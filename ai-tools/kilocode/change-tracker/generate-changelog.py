#!/usr/bin/env python3
"""
Change Tracker - Track git changes and write them to dated markdown files.

This tool:
1. Looks in ./changes/ for the most recent YYYY-MM-DD-CHANGES.md file
2. Gets git changes since that date (or all changes if no files exist)
3. Writes changes to a new file ./changes/<YYYY>/<Month>/YYYY-MM-DD-CHANGES.md
"""

import re
import subprocess
from datetime import datetime, timedelta
from pathlib import Path
import json
import sys


def find_most_recent_change_file(changes_dir: Path) -> datetime | None:
    """
    Find the most recent change file by parsing filenames in year/month subdirectories.

    Args:
        changes_dir: Path to the changes directory

    Returns:
        datetime of the most recent change file, or None if no files exist
    """
    if not changes_dir.exists():
        changes_dir.mkdir(parents=True, exist_ok=True)
        return None

    # Pattern to match YYYY-MM-DD-CHANGES.md
    pattern = re.compile(r'^(\d{4}-\d{2}-\d{2})-CHANGES\.md$')

    dates = []
    # Search recursively for change files in year/month subdirectories
    for file in changes_dir.rglob('*-CHANGES.md'):
        if file.is_file():
            match = pattern.match(file.name)
            if match:
                try:
                    date = datetime.strptime(match.group(1), '%Y-%m-%d')
                    dates.append(date)
                except ValueError:
                    continue

    return max(dates) if dates else None


def get_git_changes(since_date: datetime | None) -> list[dict] | None:
    """
    Get git changes since the specified date.

    Args:
        since_date: Date to get changes since, or None for all changes

    Returns:
        List of commit dictionaries with 'hash', 'date', 'author', 'message', or None on failure
    """
    # Build git log command
    cmd = [
        'git', 'log',
        '--pretty=format:%H|%ai|%an|%s',
        '--no-merges'
    ]

    if since_date:
        # Get changes after the last tracked date
        since_str = (since_date + timedelta(days=1)).strftime('%Y-%m-%d')
        cmd.append(f'--since={since_str}')

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True
        )

        commits = []
        for line in result.stdout.strip().split('\n'):
            if not line:
                continue

            parts = line.split('|', 3)
            if len(parts) == 4:
                commits.append({
                    'hash': parts[0][:8],  # Short hash
                    'date': parts[1].split()[0],  # Just the date part
                    'author': parts[2],
                    'message': parts[3]
                })

        return commits
    except subprocess.CalledProcessError as e:
        print(f"Error running git log: {e}", file=sys.stderr)
        return None


def get_git_stats(since_date: datetime | None) -> dict | None:
    """
    Get file change statistics since the specified date.

    Args:
        since_date: Date to get stats since, or None for all changes

    Returns:
        Dictionary with 'files_changed', 'insertions', 'deletions', or None on failure
    """
    cmd = ['git', 'diff', '--numstat']

    if since_date:
        since_str = (since_date + timedelta(days=1)).strftime('%Y-%m-%d')
        # Compare against the commit at that date
        try:
            ref_result = subprocess.run(
                ['git', 'rev-list', '-1', f'--before={since_str}', 'HEAD'],
                capture_output=True,
                text=True,
                check=True
            )
            ref = ref_result.stdout.strip()
            if ref:
                cmd = ['git', 'diff', '--numstat', ref, 'HEAD']
            else:
                cmd.append(f'--since={since_str}')
        except subprocess.CalledProcessError:
            pass
    else:
        # Get stats for all commits by diffing against the empty tree object.
        # The hash '4b825dc642cb6eb9a060e54bf8d69288fbee4904' is the well-known ID for an empty tree.
        cmd = ['git', 'diff', '--numstat', '4b825dc642cb6eb9a060e54bf8d69288fbee4904', 'HEAD']

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)

        stats = {'files_changed': 0, 'insertions': 0, 'deletions': 0}

        for line in result.stdout.strip().splitlines():
            # git diff --numstat produces tab-separated: insertions  deletions  path
            parts = line.split('\t')
            if len(parts) != 3:
                continue

            insertions_raw, deletions_raw, _path = parts

            # Binary files are indicated with '-' for insertions/deletions; treat as 0 changes but still count file
            insertions = int(insertions_raw) if insertions_raw.isdigit() else 0
            deletions = int(deletions_raw) if deletions_raw.isdigit() else 0

            stats['files_changed'] += 1
            stats['insertions'] += insertions
            stats['deletions'] += deletions

        return stats
    except subprocess.CalledProcessError:
        print("Error running git diff for stats", file=sys.stderr)
        return None


def get_closed_issues(since_date: datetime | None) -> list[dict] | None:
    """
    Get closed GitHub issues since the specified date.

    Args:
        since_date: Date to get issues since, or None for all issues

    Returns:
        List of issue dictionaries with 'number', 'title', 'closed_at', 'url', or None on failure
    """
    cmd = [
        'gh', 'issue', 'list',
        '--state', 'closed',
        '--json', 'number,title,closedAt,url',
        '--limit', '1000'
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True
        )

        all_issues = json.loads(result.stdout)

        # Filter by date if specified
        if since_date:
            since_str = (since_date + timedelta(days=1)).strftime('%Y-%m-%d')
            filtered_issues = []
            for issue in all_issues:
                if issue['closedAt']:
                    closed_date = issue['closedAt'].split('T')[0]
                    if closed_date >= since_str:
                        filtered_issues.append({
                            'number': issue['number'],
                            'title': issue['title'],
                            'closed_at': closed_date,
                            'url': issue['url']
                        })
            return filtered_issues
        else:
            return [
                {
                    'number': issue['number'],
                    'title': issue['title'],
                    'closed_at': issue['closedAt'].split('T')[0] if issue['closedAt'] else 'unknown',
                    'url': issue['url']
                }
                for issue in all_issues
            ]

    except subprocess.CalledProcessError as e:
        print(f"Error running gh issue list: {e}", file=sys.stderr)
        print("Make sure 'gh' CLI is installed and authenticated", file=sys.stderr)
        return None
    except json.JSONDecodeError as e:
        print(f"Error parsing issue JSON: {e}", file=sys.stderr)
        return None


def get_closed_prs(since_date: datetime | None) -> list[dict] | None:
    """
    Get closed/merged GitHub pull requests since the specified date.

    Args:
        since_date: Date to get PRs since, or None for all PRs

    Returns:
        List of PR dictionaries with 'number', 'title', 'merged_at', 'url', or None on failure
    """
    # Note: We don't fetch commits field due to GitHub GraphQL limits
    # Instead, we'll match commits to PRs via commit message patterns
    cmd = [
        'gh', 'pr', 'list',
        '--state', 'merged',
        '--json', 'number,title,mergedAt,url',
        '--limit', '100'
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True
        )

        all_prs = json.loads(result.stdout)

        # Filter by date if specified
        if since_date:
            since_str = (since_date + timedelta(days=1)).strftime('%Y-%m-%d')
            filtered_prs = []
            for pr in all_prs:
                if pr['mergedAt']:
                    merged_date = pr['mergedAt'].split('T')[0]
                    if merged_date >= since_str:
                        filtered_prs.append({
                            'number': pr['number'],
                            'title': pr['title'],
                            'merged_at': merged_date,
                            'url': pr['url']
                        })
            return filtered_prs
        else:
            return [
                {
                    'number': pr['number'],
                    'title': pr['title'],
                    'merged_at': pr['mergedAt'].split('T')[0] if pr['mergedAt'] else 'unknown',
                    'url': pr['url']
                }
                for pr in all_prs
            ]

    except subprocess.CalledProcessError as e:
        print(f"Error running gh pr list: {e}", file=sys.stderr)
        stderr_output = e.stderr if hasattr(e, 'stderr') else 'No stderr available'
        print(f"Error details: {stderr_output}", file=sys.stderr)
        print("Make sure 'gh' CLI is installed and authenticated", file=sys.stderr)
        return None
    except json.JSONDecodeError as e:
        print(f"Error parsing PR JSON: {e}", file=sys.stderr)
        return None


def format_changes_markdown(commits: list[dict], stats: dict, since_date: datetime | None,
                           issues: list[dict] = None, prs: list[dict] = None) -> str:
    """
    Format git changes as markdown, grouping issues with their closing PRs.

    Args:
        commits: List of commit dictionaries
        stats: Dictionary of file change statistics
        since_date: Date changes are since, or None for all changes
        issues: List of closed issue dictionaries
        prs: List of closed PR dictionaries

    Returns:
        Formatted markdown string
    """
    if issues is None:
        issues = []
    if prs is None:
        prs = []

    today = datetime.now().strftime('%Y-%m-%d')

    lines = [
        f"# Changes for {today}",
        "",
    ]

    # Add period information
    if since_date:
        lines.append(f"Changes since {since_date.strftime('%Y-%m-%d')}")
    else:
        lines.append("All changes")
    lines.append("")

    # Add statistics
    lines.append("## Summary")
    lines.append("")
    lines.append(f"- **Commits**: {len(commits)}")
    lines.append(f"- **Pull Requests**: {len(prs)}")
    lines.append(f"- **Issues Closed**: {len(issues)}")
    lines.append(f"- **Files Changed**: {stats['files_changed']}")
    lines.append(f"- **Insertions**: +{stats['insertions']}")
    lines.append(f"- **Deletions**: -{stats['deletions']}")
    lines.append("")

    # Match PRs to issues (look for patterns like "Fix #123", "Fixes #123", "Closes #123", etc.)
    issue_to_pr = {}
    for pr in prs:
        # Look for issue references in PR title
        # Patterns: "Fix #123", "Fixes #123", "Close #123", "Closes #123", "Resolve #123", "Resolves #123"
        # Also match: "fixes for #123", "#123", "(#123)"
        issue_match = re.search(r'#(\d+)', pr['title'])
        if issue_match:
            issue_number = int(issue_match.group(1))
            # Find matching issue
            matching_issue = next((issue for issue in issues if issue['number'] == issue_number), None)
            if matching_issue:
                issue_to_pr[issue_number] = pr

    # Add issues section (with their closing PRs)
    if issues:
        lines.append("## Issues")
        lines.append("")
        for issue in sorted(issues, key=lambda x: x['closed_at'], reverse=True):
            lines.append(f"### Issue #{issue['number']}: {issue['title']}")
            lines.append(f"Closed: {issue['closed_at']}")
            lines.append(f"URL: {issue['url']}")
            lines.append("")

            # Show the PR that closed this issue
            closing_pr = issue_to_pr.get(issue['number'])
            if closing_pr:
                lines.append("**Closed by:**")
                lines.append(f"- PR #{closing_pr['number']}: {closing_pr['title']}")
                lines.append(f"  - Merged: {closing_pr['merged_at']}")
                lines.append(f"  - URL: {closing_pr['url']}")
                lines.append("")

    # Add standalone PRs (not associated with issues)
    # Simpler: find PRs not in the issue_to_pr values
    pr_numbers_with_issues = set(pr['number'] for pr in issue_to_pr.values())
    standalone_prs = [pr for pr in prs if pr['number'] not in pr_numbers_with_issues]

    if standalone_prs:
        lines.append("## Pull Requests")
        lines.append("")
        for pr in sorted(standalone_prs, key=lambda x: x['merged_at'], reverse=True):
            lines.append(f"### PR #{pr['number']}: {pr['title']}")
            lines.append(f"Merged: {pr['merged_at']}")
            lines.append(f"URL: {pr['url']}")
            lines.append("")

    # Group commits by PR (match by PR number in commit message)
    commit_to_pr = {}
    for commit in commits:
        # Look for PR references in commit message (e.g., "#123", "(#123)", "Merge pull request #123")
        pr_match = re.search(r'#(\d+)', commit['message'])
        if pr_match:
            pr_number = int(pr_match.group(1))
            # Find matching PR
            matching_pr = next((pr for pr in prs if pr['number'] == pr_number), None)
            if matching_pr:
                commit_to_pr[commit['hash']] = matching_pr

    # Add git commits section (standalone commits not in any PR)
    standalone_commits = [c for c in commits if c['hash'] not in commit_to_pr]
    if standalone_commits:
        lines.append("## Git Commits")
        lines.append("")
        for commit in sorted(standalone_commits, key=lambda x: x['date'], reverse=True):
            lines.append(f"- `{commit['hash']}` {commit['message']}")
            lines.append(f"  - Author: {commit['author']}")
            lines.append(f"  - Date: {commit['date']}")
        lines.append("")

    return '\n'.join(lines)


def main():
    """Main entry point for the change tracker."""
    # Get the repository root (assumes script is run from repo)
    repo_root = Path.cwd()
    changes_dir = repo_root / 'changes'

    import shutil
    import sys

    # Check for required command-line tools
    required = ["git", "gh"]
    missing = [cmd for cmd in required if not shutil.which(cmd)]
    if missing:
        print(f"Error: Missing required commands: {', '.join(missing)}", file=sys.stderr)
        sys.exit(1)

    print("Change Tracker")
    print("=" * 50)

    # Find most recent change file
    most_recent_date = find_most_recent_change_file(changes_dir)

    if most_recent_date:
        print(f"Most recent change file: {most_recent_date.strftime('%Y-%m-%d')}")
    else:
        print("No previous change files found - tracking all changes")

    # Get git changes
    print("Fetching git changes...")
    commits = get_git_changes(most_recent_date)
    if commits is None:
        print("Failed to fetch git changes. Aborting.", file=sys.stderr)
        sys.exit(1)

    stats = get_git_stats(most_recent_date)
    if stats is None:
        print("Failed to fetch git stats. Aborting.", file=sys.stderr)
        sys.exit(1)

    print(f"Found {len(commits)} commits")

    # Get GitHub issues and PRs
    print("Fetching closed GitHub issues...")
    issues = get_closed_issues(most_recent_date)
    if issues is None:
        print("Failed to fetch closed issues from GitHub. Aborting.", file=sys.stderr)
        sys.exit(1)
    print(f"Found {len(issues)} closed issues")

    print("Fetching merged GitHub pull requests...")
    prs = get_closed_prs(most_recent_date)
    if prs is None:
        print("Failed to fetch merged pull requests from GitHub. Aborting.", file=sys.stderr)
        sys.exit(1)
    print(f"Found {len(prs)} merged PRs")

    no_recent_activity = (
        most_recent_date is not None
        and not commits
        and not issues
        and not prs
        and stats.get('files_changed', 0) == 0
    )

    if no_recent_activity:
        print("No new changes since the last changelog. Skipping file generation.")
        return

    # Format as markdown
    markdown = format_changes_markdown(commits, stats, most_recent_date, issues, prs)

    # Write to file in year/month subdirectories
    now = datetime.now()
    year = now.strftime('%Y')
    month = now.strftime('%B')  # Full month name (e.g., "November")
    today = now.strftime('%Y-%m-%d')

    # Create subdirectory structure: ./changes/YYYY/Month/
    output_dir = changes_dir / year / month
    output_dir.mkdir(parents=True, exist_ok=True)

    output_file = output_dir / f"{today}-CHANGES.md"
    output_file.write_text(markdown)

    print(f"Changes written to: {output_file}")
    print("=" * 50)


if __name__ == "__main__":
    main()
