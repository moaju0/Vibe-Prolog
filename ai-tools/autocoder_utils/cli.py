
from __future__ import annotations

import argparse
from pathlib import Path
from typing import Sequence

from .address_pr_comments import (
    address_pr_comments_with_amp as _address_pr_comments_with_amp,
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


def parse_timeout(value: str) -> int | None:
    """Parse timeout argument, allowing 'off' for no timeout."""
    if value.lower() == "off":
        return None
    try:
        return int(value)
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid timeout value: {value!r}. Must be an integer or 'off'.")


def _run_issue_workflow(
    argv: Sequence[str] | None,
    tool_cmd: list[str],
    branch_prefix: str,
    tool_name: str,
    default_timeout: int = 180,
    input_instruction: str | None = None,
    session_dir: Path | None = None,
    use_json_output: bool = False,
) -> None:
    """Common issue workflow runner."""
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description=f"Fix a GitHub issue using {tool_name} headless mode.",
    )
    parser.add_argument("issue", help="Issue number to fix")
    parser.add_argument(
        "--timeout",
        type=parse_timeout,
        default=default_timeout,
        help=f"Seconds before {tool_name} automation times out (default: %(default)s). Use 'off' to disable.",
    )
    parser.add_argument(
        "--newbranch",
        action="store_true",
        help="Always create a new branch for this issue (ignore existing branches)",
    )
    parser.add_argument(
        "--existingbranch",
        type=str,
        help="Name of existing branch to use when multiple branches exist for this issue",
    )
    args = parser.parse_args(arg_list)
    config = IssueWorkflowConfig(
        tool_cmd=tool_cmd,
        branch_prefix=branch_prefix,
        default_commit_message=f"Update from {tool_name.lower()}",
        timeout_seconds=args.timeout,
        input_instruction=input_instruction,
        session_dir=session_dir,
        use_json_output=use_json_output,
        use_new_branch=args.newbranch,
        existing_branch=args.existingbranch,
    )
    run_issue_workflow(args.issue, config)


def fix_issue_with_kilocode(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the kilocode issue workflow."""
    _run_issue_workflow(
        argv,
        tool_cmd=["kilocode", "--auto"],
        branch_prefix="fix-kilocode",
        tool_name="kilocode",
        default_timeout=1200,
    )


def fix_issue_with_claude(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the claude issue workflow with headless mode."""
    session_dir = Path.cwd() / "paige"
    _run_issue_workflow(
        argv,
        tool_cmd=[
            "claude",
            "-p",
            "fix this issue",
            "--permission-mode",
            "acceptEdits",
        ],
        branch_prefix="fix-claude",
        tool_name="Claude",
        session_dir=session_dir,
        use_json_output=True,
    )


def fix_issue_with_codex(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the Codex issue workflow."""
    _run_issue_workflow(
        argv,
        tool_cmd=[
            "codex",
            "exec",
            "--full-auto",
            "--sandbox",
            "danger-full-access",
            "-",
        ],
        branch_prefix="fix-codex",
        tool_name="Codex",
        input_instruction=(
            "You are Codex running headless. Fix the GitHub issue described below using this "
            "repository. Apply edits, run relevant tests, and finish with a brief summary."
        ),
    )


def fix_issue_with_amp(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the Amp issue workflow."""
    _run_issue_workflow(
        argv,
        tool_cmd=["amp", "-x"],
        branch_prefix="fix-amp",
        tool_name="Amp",
        input_instruction=(
            "You are Amp running headless. Fix the GitHub issue described below using this "
            "repository. Apply edits, run relevant tests, and finish with a brief summary."
        ),
    )


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
        type=parse_timeout,
        default=1200,
        help="Seconds before Kilocode processing times out (default: %(default)s). Use 'off' to disable.",
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
        type=parse_timeout,
        default=180,
        help="Seconds before Claude processing times out (default: %(default)s). Use 'off' to disable.",
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
        type=parse_timeout,
        default=180,
        help="Seconds before Codex processing times out (default: %(default)s). Use 'off' to disable.",
    )
    args = parser.parse_args(arg_list)
    _address_pr_comments_with_codex(pr_number=args.pr_number, timeout_seconds=args.timeout)


def address_pr_comments_with_amp(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for addressing PR comments using Amp."""
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Address PR review comments automatically with Amp headless mode.",
    )
    parser.add_argument(
        "pr_number",
        nargs="?",
        help="PR number to update (auto-detect from current branch when omitted)",
    )
    parser.add_argument(
        "--timeout",
        type=parse_timeout,
        default=180,
        help="Seconds before Amp processing times out (default: %(default)s). Use 'off' to disable.",
    )
    args = parser.parse_args(arg_list)
    _address_pr_comments_with_amp(pr_number=args.pr_number, timeout_seconds=args.timeout)


def generate_changelog(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the change tracker."""
    _generate_changelog(argv)


def gh_pr_helper(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the PR helper."""
    _gh_pr_helper(argv)
