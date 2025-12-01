"""Unit tests for gh_pr_helper module with mocked GitHub API calls."""

from __future__ import annotations

import json
import subprocess
from unittest.mock import MagicMock, patch

import pytest

from autocoder_utils.gh_pr_helper import (
    GitHubAPICallError,
    GitHubAPIError,
    GitHubGraphQLError,
    GitHubJSONError,
    GitHubResponseError,
    _fetch_review_threads_page,
    _fetch_thread_comments_page,
    fetch_api,
    fetch_pr_comments,
    fetch_review_comments_graphql,
    format_comments_as_markdown,
    parse_pr_path,
)


class TestParsePRPath:
    """Tests for parse_pr_path function."""

    def test_parse_valid_pr_path(self):
        """Test parsing a valid PR path."""
        owner, repo, pr_number = parse_pr_path("nlothian/Vibe-Prolog/pull/10")
        assert owner == "nlothian"
        assert repo == "Vibe-Prolog"
        assert pr_number == "10"

    def test_parse_valid_pr_path_with_leading_slash(self):
        """Test parsing a PR path with a leading slash."""
        owner, repo, pr_number = parse_pr_path("/nlothian/Vibe-Prolog/pull/42")
        assert owner == "nlothian"
        assert repo == "Vibe-Prolog"
        assert pr_number == "42"

    def test_parse_valid_pr_path_with_trailing_slash(self):
        """Test parsing a PR path with a trailing slash."""
        owner, repo, pr_number = parse_pr_path("nlothian/Vibe-Prolog/pull/99/")
        assert owner == "nlothian"
        assert repo == "Vibe-Prolog"
        assert pr_number == "99"

    def test_parse_invalid_pr_path_missing_pull(self):
        """Test that an invalid path raises ValueError."""
        with pytest.raises(ValueError, match="Invalid PR path format"):
            parse_pr_path("nlothian/Vibe-Prolog/issues/10")

    def test_parse_invalid_pr_path_too_few_parts(self):
        """Test that a path with too few parts raises ValueError."""
        with pytest.raises(ValueError, match="Invalid PR path format"):
            parse_pr_path("nlothian/Vibe-Prolog")

    def test_parse_invalid_pr_path_too_many_parts(self):
        """Test that a path with too many parts raises ValueError."""
        with pytest.raises(ValueError, match="Invalid PR path format"):
            parse_pr_path("nlothian/Vibe-Prolog/pull/10/extra")


class TestFetchAPI:
    """Tests for fetch_api function."""

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_api_success(self, mock_run):
        """Test successful API call."""
        mock_result = MagicMock()
        mock_result.stdout = json.dumps({"id": 1, "title": "Test PR"})
        mock_run.return_value = mock_result

        result = fetch_api("/repos/owner/repo/issues/1/comments")

        assert result == {"id": 1, "title": "Test PR"}
        mock_run.assert_called_once()

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_api_subprocess_error(self, mock_run):
        """Test that subprocess errors are converted to GitHubAPICallError."""

        mock_run.side_effect = subprocess.CalledProcessError(1, "gh", stderr="auth failed")

        with pytest.raises(GitHubAPICallError, match="gh API call failed"):
            fetch_api("/repos/owner/repo/issues/1/comments")

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_api_json_error(self, mock_run):
        """Test that JSON parsing errors are converted to GitHubJSONError."""
        mock_result = MagicMock()
        mock_result.stdout = "not valid json"
        mock_run.return_value = mock_result

        with pytest.raises(GitHubJSONError, match="Failed to parse JSON"):
            fetch_api("/repos/owner/repo/issues/1/comments")

    def test_fail(self):
        assert(True is False)


class TestFetchReviewThreadsPage:
    """Tests for _fetch_review_threads_page function."""

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_review_threads_page_success(self, mock_run):
        """Test successful review threads fetch."""
        mock_result = MagicMock()
        mock_result.stdout = json.dumps(
            {
                "data": {
                    "repository": {
                        "pullRequest": {
                            "reviewThreads": {
                                "pageInfo": {
                                    "hasNextPage": False,
                                    "endCursor": "cursor123",
                                },
                                "edges": [
                                    {
                                        "node": {
                                            "isResolved": False,
                                            "path": "file.py",
                                            "line": 10,
                                            "startLine": 8,
                                            "comments": {
                                                "pageInfo": {
                                                    "hasNextPage": False,
                                                    "endCursor": None,
                                                },
                                                "nodes": [
                                                    {
                                                        "author": {"login": "alice"},
                                                        "body": "Good change",
                                                        "url": "http://...",
                                                        "diffHunk": "@@ ...",
                                                    }
                                                ],
                                            },
                                        }
                                    }
                                ],
                            }
                        }
                    }
                }
            }
        )
        mock_run.return_value = mock_result

        threads, has_next, cursor = _fetch_review_threads_page("owner", "repo", "10")

        assert len(threads) == 1
        assert has_next is False
        assert cursor == "cursor123"
        assert threads[0]["node"]["path"] == "file.py"

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_review_threads_page_graphql_error(self, mock_run):
        """Test that GraphQL errors are converted to GitHubGraphQLError."""
        mock_result = MagicMock()
        mock_result.stdout = json.dumps(
            {"errors": [{"message": "Invalid query"}]}
        )
        mock_run.return_value = mock_result

        with pytest.raises(GitHubGraphQLError, match="GraphQL query returned errors"):
            _fetch_review_threads_page("owner", "repo", "10")

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_review_threads_page_response_error(self, mock_run):
        """Test that malformed responses raise GitHubResponseError."""
        mock_result = MagicMock()
        mock_result.stdout = json.dumps({"data": None})
        mock_run.return_value = mock_result

        with pytest.raises(GitHubResponseError, match="Unexpected GraphQL response"):
            _fetch_review_threads_page("owner", "repo", "10")

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_review_threads_page_with_cursor(self, mock_run):
        """Test pagination with cursor."""
        mock_result = MagicMock()
        mock_result.stdout = json.dumps(
            {
                "data": {
                    "repository": {
                        "pullRequest": {
                            "reviewThreads": {
                                "pageInfo": {"hasNextPage": False, "endCursor": None},
                                "edges": [],
                            }
                        }
                    }
                }
            }
        )
        mock_run.return_value = mock_result

        threads, has_next, cursor = _fetch_review_threads_page(
            "owner", "repo", "10", threads_after="cursor_prev"
        )

        assert len(threads) == 0
        assert has_next is False
        # Verify the cursor was passed in the command
        call_args = mock_run.call_args
        assert any("cursor_prev" in str(arg) for arg in call_args[0][0])


class TestFetchThreadCommentsPage:
    """Tests for _fetch_thread_comments_page function."""

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_thread_comments_page_success(self, mock_run):
        """Test successful thread comments fetch."""
        mock_result = MagicMock()
        mock_result.stdout = json.dumps(
            {
                "data": {
                    "repository": {
                        "pullRequest": {
                            "reviewThreads": {
                                "edges": [
                                    {
                                        "node": {
                                            "comments": {
                                                "pageInfo": {
                                                    "hasNextPage": True,
                                                    "endCursor": "next_cursor",
                                                },
                                                "nodes": [
                                                    {
                                                        "author": {"login": "bob"},
                                                        "body": "Looks good",
                                                        "url": "http://...",
                                                        "diffHunk": "@@ ...",
                                                    }
                                                ],
                                            }
                                        }
                                    }
                                ]
                            }
                        }
                    }
                }
            }
        )
        mock_run.return_value = mock_result

        comments, has_next, cursor = _fetch_thread_comments_page("owner", "repo", "10")

        assert len(comments) == 1
        assert has_next is True
        assert cursor == "next_cursor"
        assert comments[0]["author"]["login"] == "bob"

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_thread_comments_page_empty_threads(self, mock_run):
        """Test handling of empty threads list."""
        mock_result = MagicMock()
        mock_result.stdout = json.dumps(
            {
                "data": {
                    "repository": {
                        "pullRequest": {
                            "reviewThreads": {
                                "edges": []
                            }
                        }
                    }
                }
            }
        )
        mock_run.return_value = mock_result

        comments, has_next, cursor = _fetch_thread_comments_page("owner", "repo", "10")

        assert len(comments) == 0
        assert has_next is False
        assert cursor is None


class TestFetchReviewCommentsGraphQL:
    """Tests for fetch_review_comments_graphql function."""

    @patch("autocoder_utils.gh_pr_helper._fetch_review_threads_page")
    @patch("autocoder_utils.gh_pr_helper._fetch_thread_comments_page")
    def test_fetch_review_comments_single_thread_single_comment(
        self, mock_fetch_comments, mock_fetch_threads
    ):
        """Test fetching a single thread with a single comment."""
        mock_fetch_threads.return_value = (
            [
                {
                    "node": {
                        "isResolved": False,
                        "path": "app.py",
                        "line": 42,
                        "startLine": 40,
                        "comments": {
                            "pageInfo": {"hasNextPage": False, "endCursor": None},
                            "nodes": [
                                {
                                    "author": {"login": "reviewer1"},
                                    "body": "Add type hints",
                                    "url": "http://...",
                                    "diffHunk": "@@ -40,3 +40,3 @@",
                                }
                            ],
                        },
                    }
                }
            ],
            False,
            None,
        )

        result = fetch_review_comments_graphql("owner", "repo", "10")

        assert len(result) == 1
        assert result[0]["path"] == "app.py"
        assert result[0]["user"]["login"] == "reviewer1"
        assert result[0]["body"] == "Add type hints"

    @patch("autocoder_utils.gh_pr_helper._fetch_review_threads_page")
    def test_fetch_review_comments_skip_resolved(self, mock_fetch_threads):
        """Test that resolved threads are skipped."""
        mock_fetch_threads.return_value = (
            [
                {
                    "node": {
                        "isResolved": True,
                        "path": "app.py",
                        "line": 42,
                        "startLine": 40,
                        "comments": {"nodes": []},
                    }
                }
            ],
            False,
            None,
        )

        result = fetch_review_comments_graphql("owner", "repo", "10")

        assert len(result) == 0

    @patch("autocoder_utils.gh_pr_helper._fetch_review_threads_page")
    @patch("autocoder_utils.gh_pr_helper._fetch_thread_comments_page")
    def test_fetch_review_comments_with_pagination(
        self, mock_fetch_comments, mock_fetch_threads
    ):
        """Test pagination through multiple pages of threads."""
        # First page has one thread and more to come
        mock_fetch_threads.side_effect = [
            (
                [
                    {
                        "node": {
                            "isResolved": False,
                            "path": "file1.py",
                            "line": 10,
                            "startLine": 10,
                            "comments": {
                                "pageInfo": {"hasNextPage": False},
                                "nodes": [
                                    {
                                        "author": {"login": "user1"},
                                        "body": "Comment 1",
                                        "url": "http://...",
                                        "diffHunk": "@@ @@",
                                    }
                                ],
                            },
                        }
                    }
                ],
                True,
                "cursor_page2",
            ),
            # Second page has one thread, no more to come
            (
                [
                    {
                        "node": {
                            "isResolved": False,
                            "path": "file2.py",
                            "line": 20,
                            "startLine": 20,
                            "comments": {
                                "pageInfo": {"hasNextPage": False},
                                "nodes": [
                                    {
                                        "author": {"login": "user2"},
                                        "body": "Comment 2",
                                        "url": "http://...",
                                        "diffHunk": "@@ @@",
                                    }
                                ],
                            },
                        }
                    }
                ],
                False,
                None,
            ),
        ]

        result = fetch_review_comments_graphql("owner", "repo", "10")

        assert len(result) == 2
        assert result[0]["path"] == "file1.py"
        assert result[1]["path"] == "file2.py"
        # Verify pagination was called with the cursor
        assert mock_fetch_threads.call_count == 2
        second_call = mock_fetch_threads.call_args_list[1]
        # Check positional args (owner, repo, pr_number, threads_after)
        assert second_call[0][3] == "cursor_page2"

    @patch("autocoder_utils.gh_pr_helper._fetch_review_threads_page")
    @patch("autocoder_utils.gh_pr_helper._fetch_thread_comments_page")
    def test_fetch_review_comments_with_comment_pagination(
        self, mock_fetch_comments, mock_fetch_threads
    ):
        """Test pagination through multiple pages of comments within a thread."""
        # Thread with first page of comments, more to come
        mock_fetch_threads.return_value = (
            [
                {
                    "node": {
                        "isResolved": False,
                        "path": "app.py",
                        "line": 42,
                        "startLine": 40,
                        "comments": {
                            "pageInfo": {"hasNextPage": True, "endCursor": "comment_cursor"},
                            "nodes": [
                                {
                                    "author": {"login": "alice"},
                                    "body": "First comment",
                                    "url": "http://...",
                                    "diffHunk": "@@ @@",
                                }
                            ],
                        },
                    }
                }
            ],
            False,
            None,
        )

        # Second page of comments has one more comment
        mock_fetch_comments.return_value = (
            [
                {
                    "author": {"login": "bob"},
                    "body": "Second comment",
                    "url": "http://...",
                    "diffHunk": "@@ @@",
                }
            ],
            False,
            None,
        )

        result = fetch_review_comments_graphql("owner", "repo", "10")

        assert len(result) == 2
        assert result[0]["user"]["login"] == "alice"
        assert result[1]["user"]["login"] == "bob"
        # Verify comment pagination was called
        mock_fetch_comments.assert_called_once()


class TestFormatCommentsAsMarkdown:
    """Tests for format_comments_as_markdown function."""

    def test_format_empty_comments(self):
        """Test formatting with no comments."""
        result = format_comments_as_markdown([], [], "owner", "repo", "10")
        assert result == "No comments found on this PR.\n"

    def test_format_issue_comments_only(self):
        """Test formatting with only issue comments."""
        issue_comments = [
            {
                "user": {"login": "alice"},
                "body": "This PR looks good overall.",
            }
        ]
        result = format_comments_as_markdown([], issue_comments, "owner", "repo", "10")

        assert "## General PR Comments" in result
        assert "@alice" in result
        assert "This PR looks good overall." in result

    def test_format_review_comments_only(self):
        """Test formatting with only review comments."""
        review_comments = [
            {
                "path": "app.py",
                "line": 42,
                "start_line": 42,  # Same as line, so should show single line
                "original_line": 42,
                "diff_hunk": "@@ -40,3 +40,3 @@\n def foo():",
                "user": {"login": "reviewer1"},
                "body": "Add type hints",
                "url": "http://...",
            }
        ]
        result = format_comments_as_markdown(review_comments, [], "owner", "repo", "10")

        assert "## Inline Code Review Comments" in result
        assert "### File: `app.py`" in result
        assert "**Line 42**" in result
        assert "@reviewer1" in result
        assert "Add type hints" in result

    def test_format_review_comments_line_range(self):
        """Test formatting review comments with line ranges."""
        review_comments = [
            {
                "path": "app.py",
                "line": 45,
                "start_line": 40,
                "original_line": 45,
                "diff_hunk": "@@ -40,6 +40,6 @@",
                "user": {"login": "reviewer1"},
                "body": "Change affects multiple lines",
                "url": "http://...",
            }
        ]
        result = format_comments_as_markdown(review_comments, [], "owner", "repo", "10")

        assert "**Lines 40-45**" in result

    def test_format_mixed_comments(self):
        """Test formatting with both issue and review comments."""
        review_comments = [
            {
                "path": "app.py",
                "line": 10,
                "start_line": 10,
                "original_line": 10,
                "diff_hunk": "@@ @@",
                "user": {"login": "reviewer1"},
                "body": "Code review comment",
                "url": "http://...",
            }
        ]
        issue_comments = [
            {
                "user": {"login": "alice"},
                "body": "General comment on PR",
            }
        ]
        result = format_comments_as_markdown(
            review_comments, issue_comments, "owner", "repo", "10"
        )

        assert "## General PR Comments" in result
        assert "## Inline Code Review Comments" in result
        assert "@alice" in result
        assert "@reviewer1" in result


class TestFetchPRComments:
    """Tests for fetch_pr_comments function."""

    @patch("autocoder_utils.gh_pr_helper.fetch_review_comments_graphql")
    @patch("autocoder_utils.gh_pr_helper.fetch_api")
    def test_fetch_pr_comments_success(self, mock_fetch_api, mock_fetch_review):
        """Test successful fetch of both review and issue comments."""
        mock_fetch_review.return_value = [
            {
                "path": "app.py",
                "line": 10,
                "start_line": 10,
                "original_line": 10,
                "diff_hunk": "@@ @@",
                "user": {"login": "reviewer1"},
                "body": "Review comment",
                "url": "http://...",
            }
        ]
        mock_fetch_api.return_value = [
            {
                "user": {"login": "alice"},
                "body": "Issue comment",
            }
        ]

        review_comments, issue_comments = fetch_pr_comments("owner", "repo", "10")

        assert len(review_comments) == 1
        assert len(issue_comments) == 1
        assert review_comments[0]["user"]["login"] == "reviewer1"
        assert issue_comments[0]["user"]["login"] == "alice"

    @patch("autocoder_utils.gh_pr_helper.fetch_review_comments_graphql")
    @patch("autocoder_utils.gh_pr_helper.fetch_api")
    def test_fetch_pr_comments_api_error(self, mock_fetch_api, mock_fetch_review):
        """Test that API errors are propagated."""
        mock_fetch_review.return_value = []
        mock_fetch_api.side_effect = GitHubAPICallError("API failed")

        with pytest.raises(GitHubAPIError):
            fetch_pr_comments("owner", "repo", "10")
