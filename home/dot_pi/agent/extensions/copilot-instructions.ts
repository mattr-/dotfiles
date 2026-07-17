/**
 * GitHub Copilot Instructions Extension
 *
 * Loads GitHub Copilot instruction files into the system prompt so Pi treats
 * them like additional project context, similar to AGENTS.md.
 *
 * Files discovered from the current working directory up through the git repo
 * root (or filesystem root when not in a repo):
 * - .github/copilot-instructions.md
 * - recursive .github/instructions/*.instructions.md files
 * - other top-level .github/copilot*.md files
 */

import * as fs from "node:fs";
import * as path from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

interface InstructionFile {
	path: string;
	content: string;
}

function findGitRoot(from: string): string | null {
	let dir = path.resolve(from);

	while (true) {
		if (fs.existsSync(path.join(dir, ".git"))) {
			return dir;
		}

		const parent = path.dirname(dir);
		if (parent === dir) {
			return null;
		}
		dir = parent;
	}
}

function getSearchRoots(from: string): string[] {
	const start = path.resolve(from);
	const gitRoot = findGitRoot(start);
	const roots: string[] = [];

	let dir = start;
	while (true) {
		roots.push(dir);

		if (gitRoot ? dir === gitRoot : path.dirname(dir) === dir) {
			break;
		}

		const parent = path.dirname(dir);
		if (parent === dir) {
			break;
		}
		dir = parent;
	}

	return roots.reverse();
}

function readTextFile(filePath: string): string | null {
	try {
		const stat = fs.statSync(filePath);
		if (!stat.isFile()) {
			return null;
		}
		return fs.readFileSync(filePath, "utf8");
	} catch {
		return null;
	}
}

function findInstructionMarkdownFiles(dir: string): string[] {
	const results: string[] = [];

	if (!fs.existsSync(dir)) {
		return results;
	}

	let entries: fs.Dirent[] = [];
	try {
		entries = fs.readdirSync(dir, { withFileTypes: true });
	} catch {
		return results;
	}

	entries.sort((a, b) => a.name.localeCompare(b.name));

	for (const entry of entries) {
		const fullPath = path.join(dir, entry.name);

		if (entry.isDirectory()) {
			results.push(...findInstructionMarkdownFiles(fullPath));
			continue;
		}

		if (entry.isFile() && entry.name.endsWith(".instructions.md")) {
			results.push(fullPath);
		}
	}

	return results;
}

function discoverInstructionPaths(rootDir: string): string[] {
	const githubDir = path.join(rootDir, ".github");
	if (!fs.existsSync(githubDir)) {
		return [];
	}

	const discovered: string[] = [];
	const seen = new Set<string>();
	const add = (filePath: string) => {
		const normalized = path.resolve(filePath);
		if (seen.has(normalized)) {
			return;
		}
		seen.add(normalized);
		discovered.push(normalized);
	};

	const primary = path.join(githubDir, "copilot-instructions.md");
	if (fs.existsSync(primary)) {
		add(primary);
	}

	for (const filePath of findInstructionMarkdownFiles(path.join(githubDir, "instructions"))) {
		add(filePath);
	}

	try {
		const entries = fs.readdirSync(githubDir, { withFileTypes: true })
			.filter((entry) => entry.isFile() && entry.name.startsWith("copilot") && entry.name.endsWith(".md"))
			.sort((a, b) => a.name.localeCompare(b.name));

		for (const entry of entries) {
			add(path.join(githubDir, entry.name));
		}
	} catch {
		// Ignore discovery errors in .github/
	}

	return discovered;
}

function loadInstructionFiles(cwd: string): InstructionFile[] {
	const files: InstructionFile[] = [];
	const seen = new Set<string>();

	for (const rootDir of getSearchRoots(cwd)) {
		for (const filePath of discoverInstructionPaths(rootDir)) {
			const normalized = path.resolve(filePath);
			if (seen.has(normalized)) {
				continue;
			}

			const content = readTextFile(normalized);
			if (!content || !content.trim()) {
				continue;
			}

			seen.add(normalized);
			files.push({ path: normalized, content: content.trim() });
		}
	}

	return files;
}

function buildSystemPromptSection(files: InstructionFile[]): string {
	const fileBlocks = files
		.map((file) =>
			[
				`<github-copilot-instruction path="${file.path}">`,
				file.content,
				"</github-copilot-instruction>",
			].join("\n"),
		)
		.join("\n\n");

	return `

## GitHub Copilot Instruction Files

Treat the following files as additional project instructions, similar to AGENTS.md.
If an instruction file includes frontmatter such as \`applyTo\`, only apply it when it is relevant to the current task or touched files.

${fileBlocks}
`;
}

export default function copilotInstructionsExtension(pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		const files = loadInstructionFiles(ctx.cwd);
		if (files.length > 0 && ctx.hasUI) {
			ctx.ui.notify(`Loaded ${files.length} GitHub Copilot instruction file(s)`, "info");
		}
	});

	pi.on("before_agent_start", async (event, ctx) => {
		const files = loadInstructionFiles(ctx.cwd);
		if (files.length === 0) {
			return;
		}

		return {
			systemPrompt: event.systemPrompt + buildSystemPromptSection(files),
		};
	});
}
