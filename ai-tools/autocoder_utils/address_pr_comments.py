from __future__ import annotations

import json
import os
import sys
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


def _argv_or_sys(argv: Sequence[str] | None) -> Sequence[str]:
    return argv if argv is not None else sys.argv


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


def build_changes_to_make(pr_output: str) -> str:
    prompt = (
        "Read the PR comments below and generate precise instructions to address them. "
        "We only want to include things that we want to fix, so be sure to remove comments have been marked as resolved or are made redundant by subsequent updates. "
        "Include the diff blocks and line numbers. Format as Markdown"
    )
    return run(["llm", "-s", prompt], input_text=pr_output)


def run_kilocode_with_changes(changes_to_make: str) -> None:
    run(["kilocode", "--auto"], input_text=changes_to_make, capture_output=False)


def create_commit_from_pr_output(pr_output: str) -> None:
    if not has_staged_changes():
        print("No changes to commit.")
        return

    commit_message = run(
        [
            "llm",
            "--extract",
            "-s",
            "give me a git commit message for changes that address these review comments",
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


def address_pr_comments_with_kilocode(argv: Sequence[str] | None = None) -> None:
    check_commands_available(["kilocode", "gh", "llm"])
    ensure_env()

    args = _argv_or_sys(argv)
    if len(args) > 1 and args[1] in {"-h", "--help"}:
        print(f"Usage: {args[0]} [pr-number]", file=sys.stderr)
        raise SystemExit(1)

    pr_number = args[1] if len(args) > 1 else None

    repo_root = get_repo_root()
    os.chdir(repo_root)

    if pr_number is None:
        owner, repo, pr_number = resolve_pr_from_current_branch()
    else:
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

    changes_to_make = build_changes_to_make(pr_output)
    run_kilocode_with_changes(changes_to_make)
    stage_changes()
    create_commit_from_pr_output(pr_output)
    push_current_branch()
