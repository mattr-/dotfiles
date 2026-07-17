---
name: ci-test-analysis
description: Analyze failing CI test results from GitHub Actions workflows. Use when the user asks about CI failures, failing tests, or wants to understand what broke in a GitHub Actions run for the current branch or PR.
---

# CI Test Analysis

Analyze GitHub Actions workflow failures for a branch, extracting the actual failing test files from job logs.

## Prerequisites

- `gh` CLI authenticated and available
- The `get_job_failures` custom tool (provided by the `ci-test-analysis` extension in `.pi/extensions/ci-test-analysis.ts`)

## Workflow

### Step 1: Identify the branch and PR

```bash
# Get the current branch
git branch --show-current

# Get the HEAD commit SHA
git rev-parse HEAD

# Find the PR for this branch
gh pr list --head "<branch-name>" --json number,title,url --jq '.[0]'
```

### Step 2: Find failing workflow runs

Use the HEAD commit SHA to scope results to the exact code currently checked out.
Adjust the `--workflow` flag to match the target workflow file.

```bash
# List failing runs for the HEAD commit on a specific workflow
gh run list \
  --commit "$(git rev-parse HEAD)" \
  --workflow "<workflow-file>.yml" \
  --status failure \
  --json databaseId,conclusion,createdAt,name
```

If no runs are found for the HEAD commit, the user may want results for the branch instead:

```bash
gh run list \
  --branch "$(git branch --show-current)" \
  --workflow "<workflow-file>.yml" \
  --status failure \
  --json databaseId,conclusion,createdAt,name \
  --limit 5
```

### Step 3: Get the failed jobs from each run

For each failing run, get the individual failed jobs:

```bash
gh run view <run_id> --json jobs \
  --jq '.jobs[] | select(.conclusion == "failure") | {name: .name, id: .databaseId}'
```

Ignore "results" aggregator jobs (e.g., jobs named `*-results`) as they just report
the status of other jobs and do not contain test logs.

### Step 4: Extract failures using the get_job_failures tool

For each failed job ID (excluding aggregator jobs), call the `get_job_failures` tool:

```
get_job_failures(job_id: "<job_id>")
```

The tool fetches the job's logs, finds all `===FAILURE===` / `===END FAILURE===` annotation
blocks, parses the JSON within each block, and returns structured failure data including:

- `path` - the test file path
- `location` - file and line number
- `suite` - the test suite/class name
- `name` - the individual test name
- `message` - the failure message
- `executions` - retry results (first_run, same_worker, time_shifted, different_worker)

### Step 5: Analyze and report

After collecting failures from all jobs:

1. **Deduplicate by test file path** - multiple failures may come from the same file
2. **Identify flaky vs real failures** - if `executions.different_worker` is `"passed"`, the test is flaky, not a real failure caused by code changes
3. **Group by file** - present a consolidated list of unique failing test files
4. **Summarize failure messages** - note the assertion errors to help understand what broke

### Output Format

Present the results as:

1. **PR info** - PR number, title, URL
2. **Run info** - run ID, workflow name, date
3. **Failing test files** - unique list of test files with real (non-flaky) failures
4. **Failure details** - for each file, the test names and failure messages

### Example

For a run with 4 failed test jobs and 1 aggregator job:

```
PR #428897: Route user deletion to Windbeam
Run: 24853801523 (GitHub CI - Actions, 2026-04-23)

Failing test files (non-flaky):
  1. test/integration/users_controller_test.rb
  2. test/integration/stafftools/users_controller_test.rb
  3. test/jobs/business_cleanup_suspended_users_job_test.rb
  4. packages/billing/test/models/business/emu_business_test.rb

Flaky (passed on retry):
  - test/integration/api/internal/twirp/copilot/.../get_token_test.rb
```
