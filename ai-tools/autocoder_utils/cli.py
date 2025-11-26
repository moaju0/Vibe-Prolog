
from __future__ import annotations

import argparse
from pathlib import Path
from typing import Sequence

from .address_pr_comments import (
    address_pr_comments_with_codex as _address_pr_comments_with_codex,
    address_pr_comments_with_claude as _address_pr_comments_with_claude,
    address_pr_comments_with_kilocode as _address_pr_comments_with_kilocode,
)
from .change_tracker import generate_changelog as _generate_changelog
from .gh_pr_helper import gh_pr_helper as _gh_pr_helper
from .issue_workflow import IssueWorkflowConfig, run_issue_workflow


def _parser_inputs(argv: Sequence[str] | None) -> tuple[list[str] | None, str | None]:
    """Return (args_without_prog, prog_name) given a raw argv style sequence."""
    if argv is None:
        return None, None
    values = list(argv)
    if not values:
        return [], None
    prog = Path(values[0]).name
    return values[1:], prog



def fix_issue_with_kilocode(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the kilocode issue workflow."""
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Fix a GitHub issue by orchestrating kilocode with git and gh.",
    )
    parser.add_argument("issue", help="Issue number to fix")
    parser.add_argument(
        "--timeout",
        type=int,
        default=1200,
        help="Seconds before kilocode automation times out (default: %(default)s)",
    )
    args = parser.parse_args(arg_list)
    config = IssueWorkflowConfig(
        tool_cmd=["kilocode", "--auto"],
        branch_prefix="fix-kilocode",
        default_commit_message="Update from kilocode",
        timeout_seconds=args.timeout,
    )
    run_issue_workflow(args.issue, config)


def fix_issue_with_claude(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the claude issue workflow with headless mode."""
    # Determine session directory (prefer ./paige relative to cwd)
    session_dir = Path.cwd() / "paige"

    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Fix a GitHub issue using the Claude CLI headless workflow.",
    )
    parser.add_argument("issue", help="Issue number to fix")
    parser.add_argument(
        "--timeout",
        type=int,
        default=180,
        help="Seconds before Claude automation times out (default: %(default)s)",
    )
    args = parser.parse_args(arg_list)
    config = IssueWorkflowConfig(
        tool_cmd=[
            "claude",
            "-p",
            "fix this issue",
            "--permission-mode",
            "acceptEdits",
        ],
        branch_prefix="fix-claude",
        default_commit_message="Update from claude",
        timeout_seconds=args.timeout,
        session_dir=session_dir,
        use_json_output=True,
    )
    run_issue_workflow(args.issue, config)


def fix_issue_with_codex(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the Codex issue workflow."""
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Fix a GitHub issue using Codex headless mode.",
    )
    parser.add_argument("issue", help="Issue number to fix")
    parser.add_argument(
        "--timeout",
        type=int,
        default=180,
        help="Seconds before Codex automation times out (default: %(default)s)",
    )
    args = parser.parse_args(arg_list)
    config = IssueWorkflowConfig(
        tool_cmd=[
            "codex",
            "exec",
            "--full-auto",
            "--sandbox",
            "danger-full-access",
            "-",
        ],
        branch_prefix="fix-codex",
        default_commit_message="Update from codex",
        timeout_seconds=args.timeout,
        input_instruction=(
            "You are Codex running headless. Fix the GitHub issue described below using this "
            "repository. Apply edits, run relevant tests, and finish with a brief summary."
        ),
    )
    run_issue_workflow(args.issue, config)


def address_pr_comments_with_kilocode(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for addressing PR comments using Kilocode."""
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Address PR review comments automatically with Kilocode.",
    )
    parser.add_argument(
        "pr_number",
        nargs="?",
        help="PR number to update (auto-detect from current branch when omitted)",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=1200,
        help="Seconds before Kilocode processing times out (default: %(default)s)",
    )
    args = parser.parse_args(arg_list)
    _address_pr_comments_with_kilocode(pr_number=args.pr_number, timeout_seconds=args.timeout)


def address_pr_comments_with_claude(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for addressing PR comments using Claude Code."""
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Address PR review comments automatically with Claude headless mode.",
    )
    parser.add_argument(
        "pr_number",
        nargs="?",
        help="PR number to update (auto-detect from current branch when omitted)",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=180,
        help="Seconds before Claude processing times out (default: %(default)s)",
    )
    args = parser.parse_args(arg_list)
    _address_pr_comments_with_claude(pr_number=args.pr_number, timeout_seconds=args.timeout)


def address_pr_comments_with_codex(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for addressing PR comments using Codex."""
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Address PR review comments automatically with Codex headless mode.",
    )
    parser.add_argument(
        "pr_number",
        nargs="?",
        help="PR number to update (auto-detect from current branch when omitted)",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=180,
        help="Seconds before Codex processing times out (default: %(default)s)",
    )
    args = parser.parse_args(arg_list)
    _address_pr_comments_with_codex(pr_number=args.pr_number, timeout_seconds=args.timeout)


def generate_changelog(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the change tracker."""
    _generate_changelog(argv)


def gh_pr_helper(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the PR helper."""
    _gh_pr_helper(argv)
