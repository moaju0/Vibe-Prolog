from __future__ import annotations

import json
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Sequence

from . import (
    check_commands_available,
    ensure_env,
    has_staged_changes,
    run,
    stage_changes,
)
def get_issue_linked_branches(issue_number: str) -> list[str]:
    """Get list of branches linked to this issue in GitHub.

    Uses `gh issue develop --list` to find branches associated with the issue.

    Args:
        issue_number: The issue number

    Returns:
        List of branch names linked to this issue (empty if none)
    """
    try:
        output = run(["gh", "issue", "develop", "--list", issue_number]).strip()
        if not output:
            return []

        # Parse output - typically one branch per line
        branches = [line.strip() for line in output.splitlines() if line.strip()]
        return branches
    except Exception:
        # If gh command fails or issue has no linked branches, return empty list
        return []


def determine_target_branch(
    issue_number: str,
    current_branch: str,
    config: IssueWorkflowConfig,
    linked_branches: list[str] | None = None,
) -> tuple[str, bool]:
    """Determine which branch should be used for the issue.

    Args:
        issue_number: The issue number
        current_branch: The currently checked out branch
        config: Configuration for branch naming
        linked_branches: Optional list of branches linked to this issue in GitHub.
                        If provided, will use one of these branches if available.

    Returns:
        tuple[str, bool]: (branch_name, should_create)
            - branch_name: The name of the branch to use
            - should_create: True if a new branch needs to be created

    Logic:
        1. If existing_branch is set, use it (checkout, don't create)
        2. If use_new_branch is True, generate new branch via gh issue develop
        3. Otherwise:
            - If current branch is linked to this issue, stay on it
            - If linked_branches provided and non-empty, use the first one
            - Otherwise, create a new branch
    """
    # Case 1: Explicit branch specified
    if config.existing_branch:
        return config.existing_branch, False

    # Case 2: Explicitly requested new branch
    if config.use_new_branch:
        # gh issue develop will create the branch name, so return sentinel
        return "", True

    # Case 3: Auto-determine based on current branch and linked branches
    # If current branch is one of the linked branches, stay on it
    if linked_branches and current_branch in linked_branches:
        return current_branch, False

    # If there are linked branches, use the first one
    if linked_branches:
        return linked_branches[0], False

    # Otherwise, need to create a new branch
    return "", True



@dataclass(frozen=True)
class IssueWorkflowConfig:
    """Configuration for running the fix-issue workflow with a specific tool."""

    tool_cmd: Sequence[str]
    branch_prefix: str
    default_commit_message: str
    branch_suffix: str = "-"
    required_commands: Sequence[str] | None = None
    pr_model: str = "gpt-5-nano"
    timeout_seconds: int | None = None
    session_dir: Path | None = None
    use_json_output: bool = False
    input_instruction: str | None = None
    use_new_branch: bool = False
    existing_branch: str | None = None

    def required_cmds(self) -> list[str]:
        """Commands that must be present before executing the workflow."""
        base = ["gh", "llm"]
        extra = list(self.required_commands) if self.required_commands is not None else []
        if not extra and self.tool_cmd:
            extra.append(self.tool_cmd[0])
        commands: list[str] = []
        for cmd in [*extra, *base]:
            if cmd and cmd not in commands:
                commands.append(cmd)
        return commands

    def branch_requirement(self, issue_number: str) -> str:
        """Full prefix text enforced for generated branch names."""
        return f"{self.branch_prefix}/{issue_number}{self.branch_suffix}"

    def branch_prefix_token(self, issue_number: str) -> str:
        """Substring that must appear in the branch name."""
        return f"{self.branch_prefix}/{issue_number}"


def get_issue_content(issue_number: str) -> str:
    template = (
        "# Issue: \n"
        "{{.title}}\n\n"
        "# Description\n"
        "{{.body}}\n\n"
        "# Comments\n\n"
        "{{range .comments}}## Comment\n"
        "{{.body}}\n\n"
        "{{end}}"
    )
    return run(
        [
            "gh",
            "issue",
            "view",
            issue_number,
            "--json",
            "title,body,comments",
            "--template",
            template,
        ]
    )


def create_branch_name(issue_number: str, issue_content: str, config: IssueWorkflowConfig) -> str:
    prompt = (
        "create a good git branch title for a branch that addresses this issue. "
        f"It should start with `{config.branch_requirement(issue_number)}` and must be a valid branch name"
    )
    raw_output = run(["llm", prompt], input_text=issue_content).strip()
    lines = [line.strip() for line in raw_output.splitlines() if line.strip()]

    temp_branch_name = ""
    required_token = config.branch_prefix_token(issue_number)
    for line in lines:
        if required_token in line:
            temp_branch_name = line
            break
    if not temp_branch_name and lines:
        temp_branch_name = lines[0]
    if not temp_branch_name:
        raise SystemExit(f"llm did not return a usable branch name: {raw_output!r}")

    normalized = run(["git", "check-ref-format", "--normalize", temp_branch_name]).strip()
    return normalized


def get_or_create_branch(issue_number: str, config: IssueWorkflowConfig) -> str:
    """Get existing branch or create a new one for the given issue, returning the
    name of the branch that is checked out.

    This function:
    1. Refreshes from remote
    2. Gets branches linked to this issue from GitHub
    3. Determines the appropriate branch to use
    4. Creates or checks out the branch as needed
    5. Returns the final branch name
    """
    # Refresh from remote
    run(["git", "fetch", "origin"], capture_output=False)
    run(["git", "pull"], capture_output=False)

    # Get current branch
    current_branch = run(["git", "branch", "--show-current"]).strip()

    # Get branches linked to this issue from GitHub
    linked_branches = get_issue_linked_branches(issue_number)

    # Determine what to do
    target_branch, should_create = determine_target_branch(
        issue_number, current_branch, config, linked_branches
    )

    if should_create:
        # Use gh issue develop to create and checkout the branch
        run(["gh", "issue", "develop", issue_number, "--checkout"], capture_output=False)
        return run(["git", "branch", "--show-current"]).strip()
    elif target_branch != current_branch:
        # Need to checkout existing branch
        run(["git", "checkout", target_branch], capture_output=False)
        return target_branch
    else:
        # Already on the right branch
        return current_branch


def run_tool(issue_content: str, config: IssueWorkflowConfig) -> None:
    """Run the configured tool with optional timeout and session management."""
    cmd = list(config.tool_cmd)

    # Add JSON output flag if configured
    if config.use_json_output and "--output" not in cmd:
        cmd.extend(["--output", "json"])

    # If no timeout, run normally
    if config.timeout_seconds is None:
        run(cmd, input_text=issue_content, capture_output=False)
        return

    # Run with timeout
    try:
        process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )

        # Send input and wait with timeout
        try:
            stdout, stderr = process.communicate(input=issue_content, timeout=config.timeout_seconds)

            # If using JSON output, try to parse and display
            if stdout:
                if config.use_json_output:
                    try:
                        result = json.loads(stdout)
                        # Extract session ID if present
                        if isinstance(result, dict) and "session_id" in result:
                            session_id = result["session_id"]
                            if config.session_dir:
                                save_session_id(session_id, config.session_dir)
                                print(f"Session ID saved: {session_id}")
                        print(json.dumps(result, indent=2))
                    except json.JSONDecodeError:
                        print(stdout)
                else:
                    print(stdout)

            if stderr:
                print(stderr, file=sys.stderr)

            if process.returncode != 0:
                raise SystemExit(f"Tool failed with exit code {process.returncode}")

        except subprocess.TimeoutExpired:
            # Kill the process
            process.kill()
            stdout_data, stderr_data = process.communicate()

            session_id = extract_session_id_from_output(stdout_data, stderr_data)

            if session_id:
                if config.session_dir:
                    save_session_id(session_id, config.session_dir)
                print(f"\n⏱️  Timeout after {config.timeout_seconds}s. Session ID: {session_id}", file=sys.stderr)
            else:
                print(f"\n⏱️  Timeout after {config.timeout_seconds}s. No session ID found.", file=sys.stderr)

            raise SystemExit(124)  # Standard timeout exit code

    except FileNotFoundError:
        raise SystemExit(f"Command not found: {cmd[0]}")


def save_session_id(session_id: str, session_dir: Path) -> None:
    """Save a session ID to the session directory."""
    session_dir.mkdir(parents=True, exist_ok=True)
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    session_file = session_dir / f"session_{timestamp}_{session_id}.txt"
    session_file.write_text(f"{session_id}\n")


def extract_session_id_from_output(stdout: str, stderr: str) -> str | None:
    """Try to extract session ID from command output."""
    combined = stdout + "\n" + stderr

    # Try to parse as JSON first
    for stream_content in (stdout, stderr):
        if not stream_content:
            continue
        try:
            data = json.loads(stream_content)
            if isinstance(data, dict) and "session_id" in data:
                return str(data["session_id"])
        except (json.JSONDecodeError, ValueError):
            pass

    # Look for common session ID patterns
    # Match patterns like "session-xxxxx" or "Session ID: xxxxx"
    patterns = [
        r'session[_-]id[:\s]+([a-zA-Z0-9_-]+)',
        r'session[:\s]+([a-zA-Z0-9_-]+)',
        r'"session_id"[:\s]+"([^"]+)"',
    ]

    for pattern in patterns:
        match = re.search(pattern, combined, re.IGNORECASE)
        if match:
            return match.group(1)

    return None


def create_commit_if_needed(default_message: str) -> None:
    if not has_staged_changes():
        print("No changes to commit.")
        return

    diff = run(["git", "diff", "--cached"])
    commit_message = run(
        ["llm", "-s", "give me a git commit message for these changes"],
        input_text=diff,
    ).strip()
    if not commit_message:
        commit_message = default_message
    run(["git", "commit", "-m", commit_message], capture_output=False)


def push_branch(branch_name: str) -> None:
    run(["git", "push", "--set-upstream", "origin", branch_name], capture_output=False)


def build_pr_title_body(issue_number: str, model: str) -> dict[str, Any]:
    git_log_output = run(["git", "log", "origin/main.."])
    prompt = (
        "Looking at this git log output, summarise into a `title` and `body` suitable for a pull request. "
        f"The `body` MUST start with `Closes #{issue_number}`. "
        "We are going to paste your message directly into the PR so provide no commentary and do not offer suggestions of what to do next."
    )
    pr_title_body_json = run(
        [
            "llm",
            "--schema",
            "title,body",
            "-m",
            model,
            prompt,
        ],
        input_text=git_log_output,
    )
    try:
        data = json.loads(pr_title_body_json)
    except json.JSONDecodeError as exc:
        raise SystemExit(f"Failed to parse JSON from llm output: {exc}\nRaw output:\n{pr_title_body_json}")
    if not isinstance(data, dict):
        raise SystemExit(f"Unexpected JSON type from llm: {type(data)}")
    return data


def create_pr(issue_number: str, model: str) -> None:
    pr_data = build_pr_title_body(issue_number, model)
    title = str(pr_data.get("title", "")).strip()
    body = str(pr_data.get("body", "")).strip()
    run(
        [
            "gh",
            "pr",
            "create",
            "--title",
            title,
            "--body",
            body,
        ],
        capture_output=False,
    )


def run_issue_workflow(issue_number: str, config: IssueWorkflowConfig) -> None:
    check_commands_available(config.required_cmds())
    ensure_env()

    issue_content = get_issue_content(issue_number)
    tool_input = issue_content
    if config.input_instruction:
        tool_input = f"{config.input_instruction}\n\n{issue_content}"

    branch_name = get_or_create_branch(issue_number, config)

    run_tool(tool_input, config)
    stage_changes()
    create_commit_if_needed(config.default_commit_message)
    push_branch(branch_name)
    create_pr(issue_number, config.pr_model)
