from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
from datetime import datetime, timedelta
from pathlib import Path
from typing import Sequence


def find_most_recent_change_file(changes_dir: Path) -> datetime | None:
    """
    Find the most recent change file by parsing filenames in year/month subdirectories.
    """
    if not changes_dir.exists():
        changes_dir.mkdir(parents=True, exist_ok=True)
        return None

    pattern = re.compile(r"^(\d{4}-\d{2}-\d{2})-CHANGES\.md$")

    dates = []
    for file in changes_dir.rglob("*-CHANGES.md"):
        if file.is_file():
            match = pattern.match(file.name)
            if match:
                try:
                    date = datetime.strptime(match.group(1), "%Y-%m-%d")
                    dates.append(date)
                except ValueError:
                    continue

    return max(dates) if dates else None


def get_git_changes(since_date: datetime | None) -> list[dict] | None:
    """
    Get git changes since the specified date using committer date.
    Uses committer date (when merged) instead of author date (when originally written).
    """
    cmd = [
        "git",
        "log",
        "--pretty=format:%H|%ci|%an|%s",
    ]

    # Don't filter by date in git - we'll filter in Python by committer date
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as exc:
        print(f"Error running git log: {exc}", file=sys.stderr)
        return None

    commits = []
    since_str = None
    if since_date:
        since_str = (since_date + timedelta(days=1)).strftime("%Y-%m-%d")

    for line in result.stdout.strip().split("\n"):
        if not line:
            continue

        parts = line.split("|", 3)
        if len(parts) == 4:
            commit_date = parts[1].split()[0]  # Extract date from committer timestamp

            # Filter by committer date if since_date is specified
            if since_str and commit_date < since_str:
                continue

            commits.append(
                {
                    "hash": parts[0][:8],
                    "date": commit_date,
                    "author": parts[2],
                    "message": parts[3],
                }
            )

    return commits


def get_git_stats(since_date: datetime | None) -> dict | None:
    """
    Aggregate file statistics since the specified date by summing per-commit stats.
    Uses committer date to match when commits were merged, not authored.
    """
    cmd = ["git", "log", "--numstat", "--format=%H|%ci"]
    cmd.append("HEAD")

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    except subprocess.CalledProcessError as exc:
        print(f"Error running git log for stats: {exc}", file=sys.stderr)
        return None

    files_touched: set[str] = set()
    insertions_total = 0
    deletions_total = 0
    current_commit_date = None
    since_str = None
    if since_date:
        since_str = (since_date + timedelta(days=1)).strftime("%Y-%m-%d")

    for line in result.stdout.splitlines():
        # Check if this is a commit header (hash|date)
        if "|" in line and "\t" not in line:
            parts = line.split("|", 1)
            if len(parts) == 2:
                current_commit_date = parts[1].split()[0]  # Extract date from timestamp
            continue

        # This is a numstat line
        if "\t" not in line:
            continue

        # Skip stats if commit is before since_date
        if since_str and current_commit_date and current_commit_date < since_str:
            continue

        parts = line.split("\t", 2)
        if len(parts) != 3:
            continue
        insertions_raw, deletions_raw, path = parts
        if not path:
            continue

        try:
            insertions = int(insertions_raw)
        except ValueError:
            insertions = 0
        try:
            deletions = int(deletions_raw)
        except ValueError:
            deletions = 0

        files_touched.add(path)
        insertions_total += insertions
        deletions_total += deletions

    return {
        "files_changed": len(files_touched),
        "insertions": insertions_total,
        "deletions": deletions_total,
    }


def get_closed_issues(since_date: datetime | None) -> list[dict] | None:
    """
    Get closed GitHub issues since the specified date, including which PR closed them.
    """
    cmd = [
        "gh",
        "issue",
        "list",
        "--state",
        "closed",
        "--json",
        "number,title,closedAt,url,closedByPullRequestsReferences",
        "--limit",
        "1000",
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as exc:
        print(f"Error running gh issue list: {exc}", file=sys.stderr)
        print("Make sure 'gh' CLI is installed and authenticated", file=sys.stderr)
        return None

    try:
        all_issues = json.loads(result.stdout)
    except json.JSONDecodeError as exc:
        print(f"Error parsing issue JSON: {exc}", file=sys.stderr)
        return None

    if since_date:
        since_str = (since_date + timedelta(days=1)).strftime("%Y-%m-%d")
        filtered_issues = []
        for issue in all_issues:
            if issue["closedAt"]:
                closed_date = issue["closedAt"].split("T")[0]
                if closed_date >= since_str:
                    # Extract PR numbers that closed this issue
                    closing_pr_numbers = [
                        pr["number"] for pr in issue.get("closedByPullRequestsReferences", [])
                    ]
                    filtered_issues.append(
                        {
                            "number": issue["number"],
                            "title": issue["title"],
                            "closed_at": closed_date,
                            "url": issue["url"],
                            "closing_pr_numbers": closing_pr_numbers,
                        }
                    )
        return filtered_issues

    return [
        {
            "number": issue["number"],
            "title": issue["title"],
            "closed_at": issue["closedAt"].split("T")[0] if issue["closedAt"] else "unknown",
            "url": issue["url"],
            "closing_pr_numbers": [
                pr["number"] for pr in issue.get("closedByPullRequestsReferences", [])
            ],
        }
        for issue in all_issues
    ]


def get_closed_prs(since_date: datetime | None) -> list[dict] | None:
    """
    Get merged GitHub pull requests since the specified date.
    """
    cmd = [
        "gh",
        "pr",
        "list",
        "--state",
        "merged",
        "--json",
        "number,title,mergedAt,url",
        "--limit",
        "100",
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as exc:
        print(f"Error running gh pr list: {exc}", file=sys.stderr)
        stderr_output = exc.stderr if hasattr(exc, "stderr") else "No stderr available"
        print(f"Error details: {stderr_output}", file=sys.stderr)
        print("Make sure 'gh' CLI is installed and authenticated", file=sys.stderr)
        return None

    try:
        all_prs = json.loads(result.stdout)
    except json.JSONDecodeError as exc:
        print(f"Error parsing PR JSON: {exc}", file=sys.stderr)
        return None

    if since_date:
        since_str = (since_date + timedelta(days=1)).strftime("%Y-%m-%d")
        filtered_prs = []
        for pr in all_prs:
            if pr["mergedAt"]:
                merged_date = pr["mergedAt"].split("T")[0]
                if merged_date >= since_str:
                    filtered_prs.append(
                        {
                            "number": pr["number"],
                            "title": pr["title"],
                            "merged_at": merged_date,
                            "url": pr["url"],
                        }
                    )
        return filtered_prs

    return [
        {
            "number": pr["number"],
            "title": pr["title"],
            "merged_at": pr["mergedAt"].split("T")[0] if pr["mergedAt"] else "unknown",
            "url": pr["url"],
        }
        for pr in all_prs
    ]


def format_changes_markdown(
    commits: list[dict],
    stats: dict,
    since_date: datetime | None,
    issues: list[dict] | None = None,
    prs: list[dict] | None = None,
) -> str:
    """
    Format git changes as markdown, grouping issues with their closing PRs.
    """
    if issues is None:
        issues = []
    if prs is None:
        prs = []

    today = datetime.now().strftime("%Y-%m-%d")

    lines = [
        f"# Changes for {today}",
        "",
    ]

    if since_date:
        lines.append(f"Changes since {since_date.strftime('%Y-%m-%d')}")
    else:
        lines.append("All changes")
    lines.append("")

    lines.append("## Summary")
    lines.append("")
    lines.append(f"- **Commits**: {len(commits)}")
    lines.append(f"- **Pull Requests**: {len(prs)}")
    lines.append(f"- **Issues Closed**: {len(issues)}")
    lines.append(f"- **Files Changed**: {stats['files_changed']}")
    lines.append(f"- **Insertions**: +{stats['insertions']}")
    lines.append(f"- **Deletions**: -{stats['deletions']}")
    lines.append("")

    # Create a mapping of PR number to PR object for easy lookup
    pr_by_number = {pr["number"]: pr for pr in prs}

    # Track which PRs are associated with issues
    pr_numbers_with_issues = set()

    if issues:
        lines.append("## Issues")
        lines.append("")
        for issue in sorted(issues, key=lambda x: x["closed_at"], reverse=True):
            lines.append(f"### Issue #{issue['number']}: {issue['title']}")
            lines.append(f"Closed: {issue['closed_at']}")
            lines.append(f"URL: {issue['url']}")
            lines.append("")

            # Get PRs that closed this issue from the closing_pr_numbers field
            closing_pr_numbers = issue.get("closing_pr_numbers", [])
            if closing_pr_numbers:
                lines.append("**Closed by:**")
                for pr_num in closing_pr_numbers:
                    pr_numbers_with_issues.add(pr_num)
                    closing_pr = pr_by_number.get(pr_num)
                    if closing_pr:
                        lines.append(f"- PR #{closing_pr['number']}: {closing_pr['title']}")
                        lines.append(f"  - Merged: {closing_pr['merged_at']}")
                        lines.append(f"  - URL: {closing_pr['url']}")
                    else:
                        # PR might be outside the date range we queried
                        lines.append(f"- PR #{pr_num} (not in this changelog's date range)")
                lines.append("")

    standalone_prs = [pr for pr in prs if pr["number"] not in pr_numbers_with_issues]

    if standalone_prs:
        lines.append("## Pull Requests")
        lines.append("")
        for pr in sorted(standalone_prs, key=lambda x: x["merged_at"], reverse=True):
            lines.append(f"### PR #{pr['number']}: {pr['title']}")
            lines.append(f"Merged: {pr['merged_at']}")
            lines.append(f"URL: {pr['url']}")
            lines.append("")

    commit_to_pr = {}
    for commit in commits:
        pr_match = re.search(r"#(\d+)", commit["message"])
        if pr_match:
            pr_number = int(pr_match.group(1))
            matching_pr = next((pr for pr in prs if pr["number"] == pr_number), None)
            if matching_pr:
                commit_to_pr[commit["hash"]] = matching_pr

    standalone_commits = [c for c in commits if c["hash"] not in commit_to_pr]
    if standalone_commits:
        lines.append("## Git Commits")
        lines.append("")
        for commit in sorted(standalone_commits, key=lambda x: x["date"], reverse=True):
            lines.append(f"- `{commit['hash']}` {commit['message']}")
            lines.append(f"  - Author: {commit['author']}")
            lines.append(f"  - Date: {commit['date']}")
        lines.append("")

    return "\n".join(lines)


def _parser_inputs(argv: Sequence[str] | None) -> tuple[list[str] | None, str | None]:
    if argv is None:
        return None, None
    args = list(argv)
    if not args:
        return [], None
    prog = Path(args[0]).name
    return args[1:], prog


def generate_changelog(argv: Sequence[str] | None = None) -> None:
    """
    Main entry point for the change tracker.
    """
    arg_list, prog = _parser_inputs(argv)
    parser = argparse.ArgumentParser(
        prog=prog,
        description="Generate a markdown changelog from git, GitHub issues, and PRs.",
    )
    parser.parse_args(arg_list)
    repo_root = Path.cwd()
    changes_dir = repo_root / "changes"

    required = ["git", "gh"]
    missing = [cmd for cmd in required if not shutil.which(cmd)]
    if missing:
        print(f"Error: Missing required commands: {', '.join(missing)}", file=sys.stderr)
        raise SystemExit(1)

    print("Change Tracker")
    print("=" * 50)

    most_recent_date = find_most_recent_change_file(changes_dir)
    if most_recent_date:
        print(f"Most recent change file: {most_recent_date.strftime('%Y-%m-%d')}")
    else:
        print("No previous change files found - tracking all changes")

    print("Fetching git changes...")
    commits = get_git_changes(most_recent_date)
    if commits is None:
        print("Failed to fetch git changes. Aborting.", file=sys.stderr)
        raise SystemExit(1)

    stats = get_git_stats(most_recent_date)
    if stats is None:
        print("Failed to fetch git stats. Aborting.", file=sys.stderr)
        raise SystemExit(1)

    print(f"Found {len(commits)} commits")

    print("Fetching closed GitHub issues...")
    issues = get_closed_issues(most_recent_date)
    if issues is None:
        print("Failed to fetch closed issues from GitHub. Aborting.", file=sys.stderr)
        raise SystemExit(1)
    print(f"Found {len(issues)} closed issues")

    print("Fetching merged GitHub pull requests...")
    prs = get_closed_prs(most_recent_date)
    if prs is None:
        print("Failed to fetch merged pull requests from GitHub. Aborting.", file=sys.stderr)
        raise SystemExit(1)
    print(f"Found {len(prs)} merged PRs")

    no_recent_activity = (
        most_recent_date is not None
        and not commits
        and not issues
        and not prs
        and stats.get("files_changed", 0) == 0
    )

    if no_recent_activity:
        print("No new changes since the last changelog. Skipping file generation.")
        return

    markdown = format_changes_markdown(commits, stats, most_recent_date, issues, prs)

    now = datetime.now()
    year = now.strftime("%Y")
    month = now.strftime("%B")
    today = now.strftime("%Y-%m-%d")

    output_dir = changes_dir / year / month
    output_dir.mkdir(parents=True, exist_ok=True)

    output_file = output_dir / f"{today}-CHANGES.md"
    output_file.write_text(markdown)

    print(f"Changes written to: {output_file}")
    print("=" * 50)
