/**
 * Pi Status Panel Extension
 *
 * Opens a right split pane in cmux or tmux showing modified files, git status,
 * diff stats, and recent commits — like OpenCode's right sidebar.
 *
 * Toggle with /status or ctrl+shift+s.
 * Requires cmux or tmux. Child subagents can disable mux UI with
 * PI_DISABLE_MUX_UI=1 to avoid nested panels.
 */

import { execFile, execFileSync } from "node:child_process";
import { accessSync, mkdirSync, writeFileSync, unlinkSync } from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

type PanelHandle =
	| { backend: "cmux"; ref: string }
	| { backend: "tmux"; ref: string };

function isMuxUiDisabled(): boolean {
	const value = process.env.PI_DISABLE_MUX_UI?.toLowerCase();
	return value === "1" || value === "true" || value === "yes";
}

function isCmux(): boolean {
	if (isMuxUiDisabled()) return false;
	if (process.env.CMUX_WORKSPACE_ID) return true;
	try {
		const sockPath = process.env.CMUX_SOCKET_PATH
			?? `${process.env.HOME}/Library/Application Support/cmux/cmux.sock`;
		accessSync(sockPath);
		return true;
	} catch {
		return false;
	}
}

function isTmux(): boolean {
	if (isMuxUiDisabled()) return false;
	return !!process.env.TMUX && !!process.env.TMUX_PANE;
}

function getMuxBackend(): PanelHandle["backend"] | null {
	// Prefer explicit current-session markers first. This lets tmux win when a
	// cmux socket happens to exist on disk but Pi is actually running in tmux.
	if (process.env.CMUX_WORKSPACE_ID && !isMuxUiDisabled()) return "cmux";
	if (isTmux()) return "tmux";
	if (isCmux()) return "cmux";
	return null;
}

function shellQuote(value: string): string {
	return `'${value.replace(/'/g, `'\\''`)}'`;
}

function cmuxSync(args: string[]): string | null {
	try {
		return execFileSync("cmux", args, { timeout: 3000, encoding: "utf-8" }).trim();
	} catch {
		return null;
	}
}

function tmuxSync(args: string[]): string | null {
	try {
		return execFileSync("tmux", args, { timeout: 3000, encoding: "utf-8" }).trim();
	} catch {
		return null;
	}
}

function getStateDir(sessionFile: string | undefined): string {
	if (sessionFile) {
		return path.dirname(sessionFile);
	}

	const ephemeralDir = path.join(os.tmpdir(), `pi-session-${process.pid}`);
	try {
		mkdirSync(ephemeralDir, { recursive: true });
	} catch {}
	return ephemeralDir;
}

export default function (pi: ExtensionAPI) {
	let sessionDir = "";
	let panelHandle: PanelHandle | null = null;
	const scriptPath = path.join(path.dirname(import.meta.url.replace("file://", "")), "status-panel.sh");
	const panelStateFile = path.join(os.tmpdir(), `pi-${process.pid}-status-panel.json`);

	function writePanelState(handle: PanelHandle | null): void {
		try {
			if (handle) {
				writeFileSync(panelStateFile, JSON.stringify({
					open: true,
					backend: handle.backend,
					ref: handle.ref,
					updatedAt: Date.now(),
				}), "utf-8");
			} else {
				unlinkSync(panelStateFile);
			}
		} catch {
			if (!handle) {
				try {
					unlinkSync(panelStateFile);
				} catch {}
			}
		}
	}

	function openCmuxPanel(cwd: string): PanelHandle | null {
		if (!sessionDir) return null;

		// Create a right split — returns "OK surface:<id> workspace:<id>"
		const result = cmuxSync(["new-split", "right"]);
		if (!result) return null;

		const match = result.match(/surface:(\S+)/);
		const surfaceId = match ? `surface:${match[1]}` : null;
		if (!surfaceId) return null;

		// Resize pi's pane to ~3/4 width
		// After 50/50 split, grow pi's (left) pane right so it gets 3/4, panel gets 1/4
		try {
			const tree = cmuxSync(["tree"]);
			if (tree) {
				let piPaneId: string | null = null;
				let currentPaneId: string | null = null;
				for (const line of tree.split("\n")) {
					const paneMatch = line.match(/(pane:\d+)/);
					if (paneMatch) currentPaneId = paneMatch[1];
					if (line.includes("\u25c0 here")) {
						piPaneId = currentPaneId;
						break;
					}
				}
				if (piPaneId) {
					const cols = process.stdout.columns || 80;
					execFileSync("cmux", ["resize-pane", "--pane", piPaneId, "-R", "--amount", String(Math.round(cols * 3 / 4))], { timeout: 2000 });
					execFileSync("cmux", ["focus-pane", "--pane", piPaneId], { timeout: 2000 });
				}
			}
		} catch {}

		try {
			execFileSync("cmux", ["rename-tab", "--surface", surfaceId, "Status"], { timeout: 2000 });
		} catch {}

		// Send cd + script to the new surface and press enter
		setTimeout(() => {
			const cmd = `cd ${shellQuote(cwd)} && PI_PID=${process.pid} PI_SESSION_DIR=${shellQuote(sessionDir)} ${shellQuote(scriptPath)}`;
			execFile("cmux", ["send", "--surface", surfaceId, cmd], { timeout: 3000 }, () => {});
			setTimeout(() => {
				execFile("cmux", ["send-key", "--surface", surfaceId, "enter"], { timeout: 3000 }, () => {});
			}, 100);
		}, 300);

		return { backend: "cmux", ref: surfaceId };
	}

	function openTmuxPanel(cwd: string): PanelHandle | null {
		if (!sessionDir) return null;
		const targetPane = process.env.TMUX_PANE;
		if (!targetPane) return null;

		const paneId = tmuxSync([
			"split-window",
			"-d",
			"-h",
			"-P",
			"-F",
			"#{pane_id}",
			"-l",
			"35%",
			"-t",
			targetPane,
			"-c",
			cwd,
			"-e",
			`PI_PID=${process.pid}`,
			"-e",
			`PI_SESSION_DIR=${sessionDir}`,
			scriptPath,
		]);
		if (!paneId) return null;

		try {
			execFileSync("tmux", ["select-pane", "-t", paneId, "-T", "Status"], { timeout: 2000 });
		} catch {}

		return { backend: "tmux", ref: paneId };
	}

	function openPanel(cwd: string): boolean {
		const backend = getMuxBackend();
		if (!backend) return false;
		if (!sessionDir) return false;

		panelHandle = backend === "cmux" ? openCmuxPanel(cwd) : openTmuxPanel(cwd);
		writePanelState(panelHandle);
		return !!panelHandle;
	}

	function closePanel(): boolean {
		if (!panelHandle) return false;

		const handle = panelHandle;
		panelHandle = null;

		try {
			if (handle.backend === "cmux") {
				execFileSync("cmux", ["close-surface", "--surface", handle.ref], { timeout: 2000 });
			} else {
				execFileSync("tmux", ["kill-pane", "-t", handle.ref], { timeout: 2000 });
			}
		} catch {}

		writePanelState(null);
		return true;
	}

	function togglePanel(cwd: string): boolean {
		if (panelHandle) {
			return closePanel();
		} else {
			return openPanel(cwd);
		}
	}

	pi.on("session_start", async (_event, ctx) => {
		sessionDir = getStateDir(ctx.sessionManager.getSessionFile());
		if (getMuxBackend() && !panelHandle) {
			togglePanel(ctx.cwd);
		}
	});

	pi.on("session_shutdown", async () => {
		if (panelHandle) {
			closePanel();
		}
		writePanelState(null);
	});

	// Fallback: catch process exit in case session_shutdown doesn't fire
	process.on("exit", () => {
		if (panelHandle) {
			closePanel();
		}
		writePanelState(null);
	});

	pi.registerCommand("status", {
		description: "Toggle right status panel (cmux/tmux split)",
		handler: async (_args, ctx) => {
			if (!getMuxBackend()) {
				ctx.ui.notify("Status panel requires cmux or tmux", "warning");
				return;
			}
			const wasOpen = !!panelHandle;
			togglePanel(ctx.cwd);
			ctx.ui.notify(`Status panel ${wasOpen ? "closing" : "opening"}`, "info");
		},
	});

	pi.registerShortcut("ctrl+shift+s", {
		description: "Toggle right status panel",
		handler: async (ctx) => {
			if (getMuxBackend()) {
				togglePanel(ctx.cwd);
			}
		},
	});
}
