from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

from .gh_pr_helper import fetch_pr_comments, format_comments_as_markdown


from . import (
    check_commands_available,
    ensure_env,
    get_owner_repo,
    get_repo_root,
    has_staged_changes,
    run,
    stage_changes,
)


@dataclass(frozen=True)
class PRCommentWorkflowConfig:
    """Configuration for running PR comment workflow with a specific tool."""

    tool_name: str
    """Display name of the tool (e.g., 'kilocode', 'claude')."""

    tool_cmd: Sequence[str] | None = None
    """Command to run the tool. If None, uses preprocessing only."""

    timeout_seconds: int | None = None
    """Timeout for tool execution in seconds."""

    session_dir: Path | None = None
    """Directory to save session IDs for resumable sessions."""

    use_json_output: bool = False
    """Whether the tool outputs JSON with session information."""

    preprocess_prompt: str | None = None
    """Optional LLM prompt to preprocess PR comments before sending to tool."""

    input_instruction: str | None = None
    """Optional preamble prepended to the tool input."""


def get_pr_info(owner: str, repo: str, pr_number: str) -> dict:
    """
    Check that the PR exists and return basic info, including headRefName.
    """
    json_output = run(
        [
            "gh",
            "pr",
            "view",
            pr_number,
            "--repo",
            f"{owner}/{repo}",
            "--json",
            "number,headRefName",
        ]
    )
    try:
        data = json.loads(json_output)
    except json.JSONDecodeError as exc:
        raise SystemExit(f"Failed to parse JSON from gh output: {exc}\nRaw output:\n{json_output}")
    if not isinstance(data, dict):
        raise SystemExit(f"Unexpected JSON type from gh: {type(data)}")
    return data


def checkout_pr_branch(branch_name: str) -> None:
    """
    Switch to the branch the PR is for.

    If the branch does not exist locally, attempt to create it tracking origin.
    """
    try:
        run(["git", "checkout", branch_name], capture_output=False)
        return
    except SystemExit:
        # Try creating the branch from origin/<branch_name>
        run(["git", "checkout", "-b", branch_name, f"origin/{branch_name}"], capture_output=False)


def ensure_gh_pr_helper(repo_root: Path) -> Path:
    helper_path = repo_root / "ai-tools" / "gh-pr-helper" / "gh-pr-helper"
    if not helper_path.is_file():
        raise SystemExit(f"Unable to locate gh-pr-helper at {helper_path}")
    if not os.access(helper_path, os.X_OK):
        raise SystemExit(f"gh-pr-helper is not executable: {helper_path}")
    return helper_path


def get_gh_pr_output(helper_path: Path, owner: str, repo: str, pr_number: str) -> str:
    return run(
        [
            str(helper_path),
            "--owner",
            owner,
            "--repo",
            repo,
            "--pr",
            pr_number,
        ]
    )


def build_changes_to_make(pr_output: str, custom_prompt: str | None = None) -> str:
    """Build instructions for addressing PR comments using LLM preprocessing."""
    default_prompt = (
        "Read the PR comments below and generate precise instructions to address them. "
        "We only want to include things that we want to fix, so be sure to remove comments have been marked as resolved or are made redundant by subsequent updates. "
        "Include the diff blocks and line numbers. Format as Markdown."
    )
    prompt = custom_prompt if custom_prompt is not None else default_prompt
    return run(["llm", "-s", prompt], input_text=pr_output)


def run_kilocode_with_changes(changes_to_make: str) -> None:
    run(["kilocode", "--auto"], input_text=changes_to_make, capture_output=False)


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


def run_tool_with_changes(changes_to_make: str, config: PRCommentWorkflowConfig) -> None:
    """Run the configured tool with optional timeout and session management."""
    if config.tool_cmd is None:
        print("No tool command configured, skipping tool execution.")
        return

    cmd = list(config.tool_cmd)

    # Add JSON output flag if configured
    if config.use_json_output and "--output" not in cmd:
        cmd.extend(["--output", "json"])

    # If no timeout, run normally
    if config.timeout_seconds is None:
        run(cmd, input_text=changes_to_make, capture_output=False)
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
            stdout, stderr = process.communicate(input=changes_to_make, timeout=config.timeout_seconds)

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


def create_commit_from_pr_output(pr_output: str) -> None:
    if not has_staged_changes():
        print("No changes to commit.")
        return

    commit_message = run(
        [
            "llm",
            "--extract",
            "-s",
            """Give me a git commit message for changes that address these review comments. 
            I want just the comment I can paste directly, so additonal commentary.
              Do not offer to do what to do next. Here are the commits:\n""",
        ],
        input_text=pr_output,
    ).strip()
    if not commit_message:
        commit_message = "Address review comments"
    run(["git", "commit", "-m", commit_message], capture_output=False)


def push_current_branch() -> None:
    run(["git", "push"], capture_output=False)


def get_current_branch_name() -> str:
    branch_name = run(["git", "rev-parse", "--abbrev-ref", "HEAD"]).strip()
    if not branch_name or branch_name == "HEAD":
        raise SystemExit(
            "Unable to determine current branch. Please check out a branch or pass a PR number."
        )
    return branch_name


def get_upstream_remote_branch() -> tuple[str, str]:
    try:
        upstream_ref = run(
            ["git", "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"]
        ).strip()
    except SystemExit as exc:
        raise SystemExit(
            "Current branch has no upstream tracking branch. Configure an upstream or pass a PR number."
        ) from exc
    if "/" not in upstream_ref:
        raise SystemExit(
            f"Unexpected upstream ref format: {upstream_ref!r}. Please pass a PR number."
        )
    remote_name, remote_branch = upstream_ref.split("/", 1)
    if not remote_name or not remote_branch:
        raise SystemExit(
            f"Unable to parse upstream ref {upstream_ref!r}. Please pass a PR number."
        )
    return remote_name, remote_branch


def get_git_remotes() -> list[str]:
    """Get list of configured git remotes."""
    output = run(["git", "remote"]).strip()
    if not output:
        return []
    return [line.strip() for line in output.split("\n") if line.strip()]


def find_base_repo_remote(tracking_remote: str) -> str:
    """
    Find the base repository remote for PR searches.

    For fork-based workflows, PRs live in the upstream repo, not the fork.
    This function tries to find the upstream remote, falling back to the
    tracking remote if no upstream is found.

    Args:
        tracking_remote: The remote that the current branch tracks

    Returns:
        The remote name to use for PR searches (tries "upstream" first)
    """
    remotes = get_git_remotes()

    # For fork workflows, try "upstream" first
    if "upstream" in remotes and tracking_remote != "upstream":
        return "upstream"

    # Fall back to the tracking remote
    return tracking_remote


def find_pr_number_for_branch(
    base_owner: str,
    base_repo: str,
    head_owner: str,
    branch_name: str,
    display_branch: str,
) -> str:
    """
    Find PR number by searching in the base repository for a branch from the fork.

    Args:
        base_owner: Owner of the base repository (where the PR lives)
        base_repo: Name of the base repository (where the PR lives)
        head_owner: Owner of the fork (whose branch has the changes)
        branch_name: Name of the branch with changes
        display_branch: Branch name for display in error messages

    Returns:
        PR number as string
    """
    head_ref = f"{head_owner}:{branch_name}"
    json_output = run(
        [
            "gh",
            "api",
            f"/repos/{base_owner}/{base_repo}/pulls",
            "-f",
            "state=open",
            "-f",
            f"head={head_ref}",
            "-f",
            "per_page=50",
        ]
    )
    try:
        data = json.loads(json_output)
    except json.JSONDecodeError as exc:
        raise SystemExit(f"Failed to parse JSON from gh api output: {exc}") from exc
    if not isinstance(data, list):
        raise SystemExit("Unexpected response when searching for pull requests.")
    if len(data) == 0:
        raise SystemExit(
            f"No open pull request found for branch {display_branch!r}. Please pass a PR number."
        )
    if len(data) > 1:
        pr_numbers = [str(pr.get("number")) for pr in data if pr.get("number")]
        pr_list = ", ".join([f"#{n}" for n in pr_numbers])
        raise SystemExit(
            f"Multiple open pull requests match branch {display_branch!r}: {pr_list}. Please pass a PR number."
        )
    pr_number = data[0].get("number")
    if not pr_number:
        raise SystemExit("Unable to determine PR number for the current branch.")
    return str(pr_number)


def resolve_pr_from_current_branch() -> tuple[str, str, str]:
    """
    Auto-detect PR number from current branch.

    For fork-based workflows, searches in the upstream repository
    while using the fork owner in the head ref.

    Returns:
        Tuple of (base_owner, base_repo, pr_number)
    """
    local_branch = get_current_branch_name()
    tracking_remote, remote_branch = get_upstream_remote_branch()

    # Get fork owner from tracking remote (only need owner for head ref)
    fork_owner, _ = get_owner_repo(tracking_remote)

    # Find base repo (tries "upstream" first for fork workflows)
    base_remote = find_base_repo_remote(tracking_remote)
    base_owner, base_repo = get_owner_repo(base_remote)

    # Search in base repo for fork's branch
    pr_number = find_pr_number_for_branch(
        base_owner, base_repo, fork_owner, remote_branch, local_branch
    )

    # Return base repo since that's where the PR lives
    return base_owner, base_repo, pr_number


def run_pr_comment_workflow(pr_number_arg: str | None, config: PRCommentWorkflowConfig) -> None:
    """Common workflow for addressing PR comments with any tool."""
    required_commands = ["gh", "llm"]
    if config.tool_cmd is not None:
        required_commands.append(config.tool_cmd[0])
    check_commands_available(required_commands)
    ensure_env()

    repo_root = get_repo_root()
    os.chdir(repo_root)

    if pr_number_arg is None:
        owner, repo, pr_number = resolve_pr_from_current_branch()
    else:
        pr_number = pr_number_arg
        owner, repo = get_owner_repo()

    pr_info = get_pr_info(owner, repo, pr_number)
    branch_name = str(pr_info.get("headRefName", "")).strip()
    if not branch_name:
        raise SystemExit(f"Unable to determine headRefName for PR #{pr_number}")

    checkout_pr_branch(branch_name)
    run(["git", "pull"], capture_output=False)

    review_comments, issue_comments = fetch_pr_comments(owner, repo, pr_number)
    pr_output = format_comments_as_markdown(
        review_comments, issue_comments, owner, repo, pr_number
    )

    changes_to_make = build_changes_to_make(pr_output, config.preprocess_prompt)
    tool_input = changes_to_make
    if config.input_instruction:
        tool_input = f"{config.input_instruction}\n\n{changes_to_make}"
    run_tool_with_changes(tool_input, config)
    stage_changes()
    create_commit_from_pr_output(pr_output)
    push_current_branch()


def address_pr_comments_with_kilocode(
    pr_number: str | None = None, timeout_seconds: int | None = 1200
) -> None:
    """Address PR comments using Kilocode."""
    config = PRCommentWorkflowConfig(
        tool_name="kilocode",
        tool_cmd=["kilocode", "--auto"],
        timeout_seconds=timeout_seconds,
    )
    run_pr_comment_workflow(pr_number, config)


def address_pr_comments_with_claude(
    pr_number: str | None = None, timeout_seconds: int | None = 180
) -> None:
    """Address PR comments using Claude Code in headless mode."""
    # Determine session directory (prefer ./paige relative to cwd)
    session_dir = Path.cwd() / "paige"

    config = PRCommentWorkflowConfig(
        tool_name="claude",
        tool_cmd=[
            "claude",
            "-p",
            "Address these PR review comments",
            "--permission-mode",
            "acceptEdits",
        ],
        timeout_seconds=timeout_seconds,
        session_dir=session_dir,
        use_json_output=True,
    )
    run_pr_comment_workflow(pr_number, config)


def address_pr_comments_with_codex(
    pr_number: str | None = None, timeout_seconds: int | None = 180
) -> None:
    """Address PR comments using the Codex CLI headless mode."""
    config = PRCommentWorkflowConfig(
        tool_name="codex",
        tool_cmd=[
            "codex",
            "exec",
            "--full-auto",
            "--sandbox",
            "danger-full-access",
            "-",
        ],
        timeout_seconds=timeout_seconds,
        input_instruction=(
            "You are Codex running headless. Address the PR review comments described "
            "below by editing this repository, running tests as appropriate, and summarizing "
            "your updates before finishing."
        ),
    )
    run_pr_comment_workflow(pr_number, config)


def address_pr_comments_with_amp(
    pr_number: str | None = None, timeout_seconds: int | None = 180
) -> None:
    """Address PR comments using the Amp CLI headless mode."""
    config = PRCommentWorkflowConfig(
        tool_name="amp",
        tool_cmd=[
            "amp",
            "-x",
        ],
        timeout_seconds=timeout_seconds,
        input_instruction=(
            "You are Amp running headless. Address the PR review comments described "
            "below by editing this repository, running tests as appropriate, and summarizing "
            "your updates before finishing."
        ),
    )
    run_pr_comment_workflow(pr_number, config)
