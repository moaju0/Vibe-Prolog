from __future__ import annotations

import argparse
import json
import subprocess
import sys
from typing import Sequence


GRAPHQL_REVIEW_COMMENTS_QUERY = """
query FetchReviewComments($owner: String!, $repo: String!, $pr: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $pr) {
      reviewThreads(first: 100) {
        edges {
          node {
            isResolved
            path
            line
            startLine
            comments(first: 100) {
              nodes {
                author {
                  login
                }
                body
                url
                diffHunk
              }
            }
          }
        }
      }
    }
  }
}
"""


def parse_pr_path(pr_path: str) -> tuple[str, str, str]:
    """
    Parse a PR path like 'nlothian/Vibe-Prolog/pull/10' into (owner, repo, pr_number).
    """
    parts = pr_path.strip("/").split("/")

    if len(parts) == 4 and parts[2] == "pull":
        owner, repo, _, pr_number = parts
        return owner, repo, pr_number

    raise ValueError(
        f"Invalid PR path format: '{pr_path}'. Expected format: 'owner/repo/pull/number'"
    )


def fetch_api(api_path: str) -> list[dict]:
    """
    Fetch data from GitHub API using the gh CLI tool.
    """
    cmd = [
        "gh",
        "api",
        "-H",
        "Accept: application/vnd.github+json",
        "-H",
        "X-GitHub-Api-Version: 2022-11-28",
        api_path,
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
        )
        return json.loads(result.stdout)
    except subprocess.CalledProcessError as exc:
        print(f"Error calling gh API: {exc.stderr}", file=sys.stderr)
        raise SystemExit(1)
    except json.JSONDecodeError as exc:
        print(f"Error parsing JSON response: {exc}", file=sys.stderr)
        raise SystemExit(1)


def fetch_review_comments_graphql(owner: str, repo: str, pr_number: str) -> list[dict]:
    """
    Fetch review comments via the GitHub GraphQL API, excluding resolved threads.
    """
    cmd = [
        "gh",
        "api",
        "graphql",
        "-f",
        f"owner={owner}",
        "-f",
        f"repo={repo}",
        "-F",
        f"pr={pr_number}",
        "-f",
        f"query={GRAPHQL_REVIEW_COMMENTS_QUERY}",
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
        )
        payload = json.loads(result.stdout)
    except subprocess.CalledProcessError as exc:
        print(f"Error calling gh GraphQL API: {exc.stderr}", file=sys.stderr)
        raise SystemExit(1)
    except json.JSONDecodeError as exc:
        print(f"Error parsing GraphQL JSON response: {exc}", file=sys.stderr)
        raise SystemExit(1)

    if "errors" in payload and payload["errors"]:
        print(f"GraphQL errors: {payload['errors']}", file=sys.stderr)
        raise SystemExit(1)

    try:
        threads = payload["data"]["repository"]["pullRequest"]["reviewThreads"]["edges"]
    except (KeyError, TypeError) as exc:
        print("Unexpected GraphQL response format when fetching review comments.", file=sys.stderr)
        raise SystemExit(1) from exc

    review_comments: list[dict] = []
    for edge in threads or []:
        thread = edge.get("node") or {}
        if thread.get("isResolved"):
            continue

        path = thread.get("path", "unknown")
        line = thread.get("line")
        start_line = thread.get("startLine")
        comment_nodes = (thread.get("comments") or {}).get("nodes") or []

        for comment in comment_nodes:
            author_login = (comment.get("author") or {}).get("login", "unknown")
            review_comments.append(
                {
                    "path": path,
                    "line": line,
                    "start_line": start_line,
                    "original_line": line,
                    "diff_hunk": comment.get("diffHunk"),
                    "user": {"login": author_login},
                    "body": comment.get("body", ""),
                    "url": comment.get("url"),
                }
            )

    return review_comments


def fetch_pr_comments(owner: str, repo: str, pr_number: str) -> tuple[list[dict], list[dict]]:
    """
    Fetch both review comments (inline on diff) and issue comments (general PR comments).
    Returns (review_comments, issue_comments)
    """
    review_comments = fetch_review_comments_graphql(owner, repo, pr_number)

    issue_comments_path = f"/repos/{owner}/{repo}/issues/{pr_number}/comments"
    issue_comments = fetch_api(issue_comments_path)

    return review_comments, issue_comments


def format_comments_as_markdown(
    review_comments: list[dict],
    issue_comments: list[dict],
    owner: str,
    repo: str,
    pr_number: str,
) -> str:
    """
    Format PR comments as markdown suitable for AI coding agents.
    """
    output = [f"# PR Comments: {owner}/{repo}#{pr_number}\n"]

    if issue_comments:
        output.append("## General PR Comments\n")
        for comment in issue_comments:
            user = comment.get("user", {}).get("login", "unknown")
            body = comment.get("body", "").strip()

            output.append(f"### @{user}\n")
            output.append(body)
            output.append("")

    if review_comments:
        output.append("## Inline Code Review Comments\n")

        comments_by_file: dict[str, list[dict]] = {}
        for comment in review_comments:
            file_path = comment.get("path", "unknown")
            comments_by_file.setdefault(file_path, []).append(comment)

        for file_path, file_comments in sorted(comments_by_file.items()):
            output.append(f"\n### File: `{file_path}`\n")
            file_comments.sort(key=lambda c: (c.get("line") or c.get("original_line") or 0))

            for comment in file_comments:
                user = comment.get("user", {}).get("login", "unknown")
                body = comment.get("body", "").strip()
                line = comment.get("line")
                start_line = comment.get("start_line")
                diff_hunk = comment.get("diff_hunk", "").strip()

                if start_line and line and start_line != line:
                    line_ref = f"**Lines {start_line}-{line}**"
                elif line:
                    line_ref = f"**Line {line}**"
                else:
                    line_ref = "**Position in diff**"

                output.append(f"#### {line_ref} (@{user})\n")

                if diff_hunk:
                    output.append("**Code context:**")
                    output.append("```diff")
                    output.append(diff_hunk)
                    output.append("```\n")

                output.append("**Comment:**")
                output.append(body)
                output.append("")

    if not issue_comments and not review_comments:
        return "No comments found on this PR.\n"

    return "\n".join(output)


def gh_pr_helper(argv: Sequence[str] | None = None) -> None:
    parser = argparse.ArgumentParser(
        description="Fetch and format GitHub PR comments for AI coding agents.",
        epilog="Example: %(prog)s nlothian/Vibe-Prolog/pull/10",
    )

    parser.add_argument(
        "pr_path",
        nargs="?",
        help="PR path in format 'owner/repo/pull/number'",
    )
    parser.add_argument("--owner", help="Repository owner (alternative to pr_path)")
    parser.add_argument("--repo", help="Repository name (alternative to pr_path)")
    parser.add_argument("--pr", dest="pr_number", help="PR number (alternative to pr_path)")

    args = parser.parse_args(list(argv[1:]) if argv is not None else None)

    if args.pr_path:
        try:
            owner, repo, pr_number = parse_pr_path(args.pr_path)
        except ValueError as exc:
            print(f"Error: {exc}", file=sys.stderr)
            raise SystemExit(1)
    elif args.owner and args.repo and args.pr_number:
        owner = args.owner
        repo = args.repo
        pr_number = args.pr_number
    else:
        parser.print_help()
        print("\nError: Provide either a PR path or --owner, --repo, and --pr", file=sys.stderr)
        raise SystemExit(1)

    review_comments, issue_comments = fetch_pr_comments(owner, repo, pr_number)
    markdown_output = format_comments_as_markdown(review_comments, issue_comments, owner, repo, pr_number)
    print(markdown_output)
