from __future__ import annotations

import sys
from pathlib import Path
from typing import Sequence

from .address_pr_comments import address_pr_comments_with_kilocode as _address_pr_comments_with_kilocode
from .change_tracker import generate_changelog as _generate_changelog
from .gh_pr_helper import gh_pr_helper as _gh_pr_helper
from .issue_workflow import IssueWorkflowConfig, run_issue_workflow


def _argv(argv: Sequence[str] | None) -> Sequence[str]:
    return argv if argv is not None else sys.argv


def fix_issue_with_kilocode(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the kilocode issue workflow."""
    config = IssueWorkflowConfig(
        tool_cmd=["kilocode", "--auto"],
        branch_prefix="fix-kilocode",
        default_commit_message="Update from kilocode",
    )
    run_issue_workflow(_argv(argv), config)


def fix_issue_with_claude(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the claude issue workflow with headless mode."""
    # Determine session directory (prefer ./paige relative to cwd)
    session_dir = Path.cwd() / "paige"

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
        timeout_seconds=180,  # 3 minutes
        session_dir=session_dir,
        use_json_output=True,
    )
    run_issue_workflow(_argv(argv), config)


def address_pr_comments_with_kilocode(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for addressing PR comments using Kilocode."""
    _address_pr_comments_with_kilocode(_argv(argv))


def generate_changelog(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the change tracker."""
    _generate_changelog(_argv(argv))


def gh_pr_helper(argv: Sequence[str] | None = None) -> None:
    """CLI wrapper for the PR helper."""
    _gh_pr_helper(_argv(argv))
