from __future__ import annotations

import argparse
import json
import subprocess
import sys
from typing import Sequence


class GitHubAPIError(Exception):
    """Base exception for GitHub API errors."""

    pass


class GitHubAPICallError(GitHubAPIError):
    """Raised when the gh CLI command fails."""

    pass


class GitHubJSONError(GitHubAPIError):
    """Raised when JSON parsing fails."""

    pass


class GitHubGraphQLError(GitHubAPIError):
    """Raised when GraphQL returns errors."""

    pass


class GitHubResponseError(GitHubAPIError):
    """Raised when the API response format is unexpected."""

    pass


GRAPHQL_REVIEW_COMMENTS_QUERY = """
query FetchReviewComments($owner: String!, $repo: String!, $pr: Int!, $threadsAfter: String, $commentsAfter: String) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $pr) {
      reviewThreads(first: 100, after: $threadsAfter) {
        pageInfo {
          hasNextPage
          endCursor
        }
        edges {
          node {
            isResolved
            path
            line
            startLine
            comments(first: 100, after: $commentsAfter) {
              pageInfo {
                hasNextPage
                endCursor
              }
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

    Raises:
        GitHubAPICallError: If the gh CLI command fails.
        GitHubJSONError: If JSON parsing fails.
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
        raise GitHubAPICallError(f"gh API call failed: {exc.stderr}") from exc
    except json.JSONDecodeError as exc:
        raise GitHubJSONError(f"Failed to parse JSON response: {exc}") from exc


def _fetch_review_threads_page(
    owner: str, repo: str, pr_number: str, threads_after: str | None = None
) -> tuple[list[dict], bool, str | None]:
    """
    Fetch a single page of review threads from the GraphQL API.

    Args:
        owner: Repository owner
        repo: Repository name
        pr_number: Pull request number
        threads_after: Cursor for pagination

    Returns:
        Tuple of (threads, has_next_page, next_cursor)

    Raises:
        GitHubAPICallError: If the gh CLI command fails.
        GitHubJSONError: If JSON parsing fails.
        GitHubGraphQLError: If GraphQL returns errors.
        GitHubResponseError: If the response format is unexpected.
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
    ]
    
    if threads_after:
        cmd.extend(["-f", f"threadsAfter={threads_after}"])
    else:
        cmd.extend(["-f", "threadsAfter=null"])
    
    cmd.extend(["-f", "commentsAfter=null"])
    cmd.extend(["-f", f"query={GRAPHQL_REVIEW_COMMENTS_QUERY}"])

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        payload = json.loads(result.stdout)
    except subprocess.CalledProcessError as exc:
        raise GitHubAPICallError(f"gh GraphQL API call failed: {exc.stderr}") from exc
    except json.JSONDecodeError as exc:
        raise GitHubJSONError(f"Failed to parse GraphQL response: {exc}") from exc

    if "errors" in payload and payload["errors"]:
        raise GitHubGraphQLError(f"GraphQL query returned errors: {payload['errors']}")

    try:
        review_threads_data = payload["data"]["repository"]["pullRequest"]["reviewThreads"]
        threads = review_threads_data.get("edges") or []
        page_info = review_threads_data.get("pageInfo") or {}
    except (KeyError, TypeError) as exc:
        raise GitHubResponseError(
            "Unexpected GraphQL response format when fetching review threads"
        ) from exc
    
    return threads, page_info.get("hasNextPage", False), page_info.get("endCursor")


def _fetch_thread_comments_page(
    owner: str, repo: str, pr_number: str, comments_after: str | None = None
) -> tuple[list[dict], bool, str | None]:
    """
    Fetch a single page of comments for a specific thread.

    Args:
        owner: Repository owner
        repo: Repository name
        pr_number: Pull request number
        comments_after: Cursor for pagination

    Returns:
        Tuple of (comments, has_next_page, next_cursor)

    Raises:
        GitHubAPICallError: If the gh CLI command fails.
        GitHubJSONError: If JSON parsing fails.
        GitHubGraphQLError: If GraphQL returns errors.
        GitHubResponseError: If the response format is unexpected.
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
        "threadsAfter=null",
    ]
    
    if comments_after:
        cmd.extend(["-f", f"commentsAfter={comments_after}"])
    else:
        cmd.extend(["-f", "commentsAfter=null"])
    
    cmd.extend(["-f", f"query={GRAPHQL_REVIEW_COMMENTS_QUERY}"])

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        payload = json.loads(result.stdout)
    except subprocess.CalledProcessError as exc:
        raise GitHubAPICallError(f"gh GraphQL API call failed: {exc.stderr}") from exc
    except json.JSONDecodeError as exc:
        raise GitHubJSONError(f"Failed to parse GraphQL response: {exc}") from exc

    if "errors" in payload and payload["errors"]:
        raise GitHubGraphQLError(f"GraphQL query returned errors: {payload['errors']}")

    try:
        threads = payload["data"]["repository"]["pullRequest"]["reviewThreads"]["edges"]
        if threads:
            comments_data = threads[0].get("node", {}).get("comments", {})
            comment_nodes = comments_data.get("nodes") or []
            page_info = comments_data.get("pageInfo") or {}
        else:
            comment_nodes = []
            page_info = {}
    except (KeyError, TypeError) as exc:
        raise GitHubResponseError(
            "Unexpected GraphQL response format when fetching thread comments"
        ) from exc
    
    return comment_nodes, page_info.get("hasNextPage", False), page_info.get("endCursor")


def fetch_review_comments_graphql(owner: str, repo: str, pr_number: str) -> list[dict]:
    """
    Fetch review comments via the GitHub GraphQL API, excluding resolved threads.
    Implements pagination to handle more than 100 review threads or 100 comments per thread.
    """
    review_comments: list[dict] = []
    threads_after: str | None = None
    
    # Paginate through review threads
    while True:
        threads, has_next_thread_page, next_thread_cursor = _fetch_review_threads_page(
            owner, repo, pr_number, threads_after
        )
        
        for edge in threads:
            thread = edge.get("node") or {}
            if thread.get("isResolved"):
                continue
            
            path = thread.get("path", "unknown")
            line = thread.get("line")
            start_line = thread.get("startLine")
            
            # Collect all comments for this thread with pagination
            comments_after: str | None = None
            while True:
                # Get first page of comments from the thread
                comment_nodes = (thread.get("comments") or {}).get("nodes") or []
                comments_page_info = (thread.get("comments") or {}).get("pageInfo") or {}
                
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
                
                # Check if there are more comments in this thread
                if not comments_page_info.get("hasNextPage"):
                    break
                
                # Fetch next page of comments
                comments_after = comments_page_info.get("endCursor")
                comment_nodes, has_next_comment_page, next_comment_cursor = (
                    _fetch_thread_comments_page(owner, repo, pr_number, comments_after)
                )
                
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
                
                if not has_next_comment_page:
                    break
        
        if not has_next_thread_page:
            break
        
        threads_after = next_thread_cursor

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
    """
    Main entry point for fetching and formatting GitHub PR comments.

    Parses command-line arguments, fetches PR comments, and prints formatted output.
    Catches and handles all exceptions, printing error messages before exiting.
    """
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

    try:
        if args.pr_path:
            owner, repo, pr_number = parse_pr_path(args.pr_path)
        elif args.owner and args.repo and args.pr_number:
            owner = args.owner
            repo = args.repo
            pr_number = args.pr_number
        else:
            parser.print_help()
            print("\nError: Provide either a PR path or --owner, --repo, and --pr", file=sys.stderr)
            raise SystemExit(1)

        review_comments, issue_comments = fetch_pr_comments(owner, repo, pr_number)
        markdown_output = format_comments_as_markdown(
            review_comments, issue_comments, owner, repo, pr_number
        )
        print(markdown_output)

    except ValueError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        raise SystemExit(1)
    except GitHubAPIError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        raise SystemExit(1)
