/**
 * CI Test Analysis Extension
 *
 * Provides a tool that fetches GitHub Actions job logs, extracts
 * ===FAILURE=== blocks, and returns structured failure information.
 */

import { Type } from "typebox";
import { defineTool, type ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { execSync } from "node:child_process";

interface FailureInfo {
  suite: string;
  name: string;
  message: string;
  path: string;
  location: string;
  exception_class: string;
  executions: Record<string, string>;
}

interface JobFailuresResult {
  job_id: string;
  job_name: string;
  failures: FailureInfo[];
  error?: string;
}

function extractFailureBlocks(logs: string): FailureInfo[] {
  const failures: FailureInfo[] = [];
  const blocks = logs.split("===FAILURE===").slice(1);

  for (const block of blocks) {
    const endIdx = block.indexOf("===END FAILURE===");
    if (endIdx === -1) continue;

    const content = block.substring(0, endIdx);

    // Strip timestamp prefixes from each line (e.g. "2026-04-23T19:31:07.9573395Z ")
    const cleaned = content
      .split("\n")
      .map((line) => line.replace(/^\d{4}-\d{2}-\d{2}T[\d:.]+Z\s*/, ""))
      .join("\n");

    try {
      const json = JSON.parse(cleaned);
      failures.push({
        suite: json.suite ?? "",
        name: json.name ?? "",
        message: json.message ?? "",
        path: json.path ?? "",
        location: json.location ?? "",
        exception_class: json.exception_class ?? "",
        executions: json.executions ?? {},
      });
    } catch {
      // If JSON parsing fails, try to extract key fields with regex
      const pathMatch = cleaned.match(/"path":\s*"([^"]+)"/);
      const suiteMatch = cleaned.match(/"suite":\s*"([^"]+)"/);
      const nameMatch = cleaned.match(/"name":\s*"([^"]+)"/);
      const messageMatch = cleaned.match(/"message":\s*"([^"]+)"/);
      const locationMatch = cleaned.match(/"location":\s*"([^"]+)"/);

      if (pathMatch) {
        failures.push({
          suite: suiteMatch?.[1] ?? "",
          name: nameMatch?.[1] ?? "",
          message: messageMatch?.[1] ?? "",
          path: pathMatch[1],
          location: locationMatch?.[1] ?? "",
          exception_class: "",
          executions: {},
        });
      }
    }
  }

  return failures;
}

const getJobFailuresTool = defineTool({
  name: "get_job_failures",
  label: "Get Job Failures",
  description:
    "Fetch the logs for a GitHub Actions job by job ID, extract the ===FAILURE=== annotation blocks, parse the JSON within each block, and return structured failure information including test file paths, suite names, test names, failure messages, and execution results.",
  promptSnippet:
    "Fetch GitHub Actions job logs and extract structured test failure data from ===FAILURE=== annotation blocks",
  promptGuidelines: [
    "Use get_job_failures to analyze CI test failures from GitHub Actions job logs instead of manually fetching and parsing logs with bash.",
    "Pass individual job IDs to get_job_failures (not run IDs). Get job IDs from `gh run view <run_id> --json jobs`.",
  ],
  parameters: Type.Object({
    job_id: Type.String({ description: "The GitHub Actions job ID (numeric)" }),
    repo: Type.Optional(
      Type.String({
        description: 'Repository in owner/repo format (default: current repo from gh context)',
      })
    ),
  }),

  async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
    const { job_id, repo } = params;

    let repoFlag = "";
    if (repo) {
      repoFlag = `--repo ${repo}`;
    } else {
      // Detect repo from gh context
      try {
        const detected = execSync("gh repo view --json nameWithOwner --jq .nameWithOwner", {
          cwd: ctx.cwd,
          encoding: "utf-8",
          timeout: 10000,
        }).trim();
        repoFlag = `--repo ${detected}`;
      } catch {
        // Fall through; gh api will use default repo context
      }
    }

    // Get job name
    let jobName = job_id;
    try {
      const repoPath = repoFlag.replace("--repo ", "");
      const jobInfo = execSync(
        `gh api repos/${repoPath}/actions/jobs/${job_id} --jq '.name'`,
        { cwd: ctx.cwd, encoding: "utf-8", timeout: 15000 }
      ).trim();
      if (jobInfo) jobName = jobInfo;
    } catch {
      // Use job_id as fallback name
    }

    // Fetch logs
    let logs: string;
    try {
      const repoPath = repoFlag.replace("--repo ", "");
      logs = execSync(`gh api repos/${repoPath}/actions/jobs/${job_id}/logs`, {
        cwd: ctx.cwd,
        encoding: "utf-8",
        maxBuffer: 100 * 1024 * 1024,
        timeout: 60000,
      });
    } catch (err: any) {
      const result: JobFailuresResult = {
        job_id,
        job_name: jobName,
        failures: [],
        error: `Failed to fetch logs for job ${job_id}: ${err.message}`,
      };
      return {
        content: [{ type: "text", text: JSON.stringify(result, null, 2) }],
        details: result,
      };
    }

    // Extract failures
    const failures = extractFailureBlocks(logs);

    const result: JobFailuresResult = {
      job_id,
      job_name: jobName,
      failures,
    };

    // Build human-readable summary
    const lines: string[] = [];
    lines.push(`Job: ${jobName} (${job_id})`);

    if (failures.length === 0) {
      lines.push("No test failures found in ===FAILURE=== annotation blocks.");
    } else {
      lines.push(`${failures.length} test failure(s):\n`);
      for (const f of failures) {
        lines.push(`  File: ${f.path}`);
        lines.push(`  Location: ${f.location}`);
        lines.push(`  Suite: ${f.suite}`);
        lines.push(`  Test: ${f.name}`);
        lines.push(`  Message: ${f.message}`);
        const execSummary = Object.entries(f.executions)
          .map(([k, v]) => `${k}=${v}`)
          .join(", ");
        if (execSummary) lines.push(`  Executions: ${execSummary}`);
        lines.push("");
      }
    }

    return {
      content: [{ type: "text", text: lines.join("\n") }],
      details: result,
    };
  },
});

export default function (pi: ExtensionAPI) {
  pi.registerTool(getJobFailuresTool);
}
