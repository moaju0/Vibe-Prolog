"""Unit tests for gh_pr_helper module with mocked GitHub API calls."""

from __future__ import annotations

import argparse
import json
import subprocess
from unittest.mock import MagicMock, patch

import pytest

from autocoder_utils.gh_pr_helper import (
    _str_to_bool,
    _summarize_ci_log,
    collect_ci_failures,
    GitHubAPICallError,
    GitHubAPIError,
    GitHubGraphQLError,
    GitHubJSONError,
    GitHubResponseError,
    _fetch_review_threads_page,
    _fetch_thread_comments_page,
    fetch_api,
    fetch_ci_run_log,
    fetch_failed_ci_runs,
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

    def test_format_includes_ci_failures(self):
        """Test that CI failure logs are included when provided."""
        ci_failures = [
            {
                "name": "CI Workflow",
                "workflow_run_id": 12345,
                "details_url": "https://example.com/run/12345",
                "log_output": "Step failed",
            }
        ]

        result = format_comments_as_markdown(
            [],
            [],
            "owner",
            "repo",
            "10",
            ci_failures=ci_failures,
        )

        assert "## CI Failures" in result
        assert "CI Workflow (Run ID 12345)" in result
        assert "Step failed" in result
        assert "https://example.com/run/12345" in result


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


class TestBooleanParsing:
    """Tests for CLI boolean parsing helper."""

    def test_str_to_bool_true_values(self):
        """Ensure various truthy strings are accepted."""
        for value in ["true", "True", "YES", "1", "On"]:
            assert _str_to_bool(value) is True

    def test_str_to_bool_false_values(self):
        """Ensure various falsy strings are accepted."""
        for value in ["false", "False", "no", "0", "Off"]:
            assert _str_to_bool(value) is False

    def test_str_to_bool_invalid_value(self):
        """Invalid strings should raise argparse errors."""
        with pytest.raises(argparse.ArgumentTypeError):
            _str_to_bool("maybe")


class TestFetchFailedCIRuns:
    """Tests for fetch_failed_ci_runs function."""

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_failed_ci_runs_success(self, mock_run):
        """Test fetching failed CI runs filters to CheckRun failures."""
        mock_payload = {
            "data": {
                "repository": {
                    "pullRequest": {
                        "commits": {
                            "nodes": [
                                {
                                    "commit": {
                                        "statusCheckRollup": {
                                            "contexts": {
                                                "nodes": [
                                                    {
                                                        "__typename": "CheckRun",
                                                        "name": "Lint",
                                                        "conclusion": "FAILURE",
                                                        "detailsUrl": "https://example.com/lint",
                                                        "checkSuite": {
                                                            "workflowRun": {
                                                                "databaseId": 111,
                                                                "url": "https://github.com/run/111",
                                                            }
                                                        },
                                                    },
                                                    {
                                                        "__typename": "CheckRun",
                                                        "name": "Tests",
                                                        "conclusion": "SUCCESS",
                                                        "checkSuite": {
                                                            "workflowRun": {
                                                                "databaseId": 222,
                                                                "url": "https://github.com/run/222",
                                                            }
                                                        },
                                                    },
                                                ]
                                            }
                                        }
                                    }
                                }
                            ]
                        }
                    }
                }
            }
        }
        mock_result = MagicMock()
        mock_result.stdout = json.dumps(mock_payload)
        mock_run.return_value = mock_result

        runs = fetch_failed_ci_runs("owner", "repo", "10")

        assert len(runs) == 1
        assert runs[0]["name"] == "Lint"
        assert runs[0]["workflow_run_id"] == 111
        assert runs[0]["details_url"] == "https://example.com/lint"


class TestFetchCIRunLog:
    """Tests for fetch_ci_run_log function."""

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_ci_run_log_success(self, mock_run):
        """Test successful retrieval of run logs."""
        mock_result = MagicMock()
        mock_result.stdout = "log output"
        mock_run.return_value = mock_result

        output = fetch_ci_run_log(123)

        assert output == "log output"
        mock_run.assert_called_once()
        assert "--log-failed" in mock_run.call_args[0][0]

    @patch("autocoder_utils.gh_pr_helper.subprocess.run")
    def test_fetch_ci_run_log_failure(self, mock_run):
        """Test that subprocess errors raise GitHubAPICallError."""
        mock_run.side_effect = subprocess.CalledProcessError(1, "gh", stderr="boom")

        with pytest.raises(GitHubAPICallError):
            fetch_ci_run_log(123)


class TestCollectCIFailures:
    """Tests for collect_ci_failures helper."""

    @patch("autocoder_utils.gh_pr_helper.fetch_ci_run_log")
    @patch("autocoder_utils.gh_pr_helper.fetch_failed_ci_runs")
    def test_collect_ci_failures_deduplicates_runs(self, mock_fetch_runs, mock_fetch_log):
        """Test that duplicate run IDs are ignored and logs are attached."""
        mock_fetch_runs.return_value = [
            {
                "name": "CI",
                "workflow_run_id": 10,
                "details_url": "https://example.com/ci",
                "workflow_url": "https://github.com/run/10",
            },
            {
                "name": "CI",
                "workflow_run_id": 10,
                "details_url": "https://example.com/ci",
                "workflow_url": "https://github.com/run/10",
            },
        ]
        mock_fetch_log.return_value = "log data"

        failures = collect_ci_failures("owner", "repo", "10")

        assert len(failures) == 1
        assert failures[0]["log_output"] == "log data"
        mock_fetch_log.assert_called_once_with(10)

    @patch("autocoder_utils.gh_pr_helper.fetch_ci_run_log")
    @patch("autocoder_utils.gh_pr_helper.fetch_failed_ci_runs")
    def test_collect_ci_failures_handles_missing_run_id(
        self, mock_fetch_runs, mock_fetch_log
    ):
        """Test that runs without IDs still produce entries."""
        mock_fetch_runs.return_value = [
            {
                "name": "External Check",
                "workflow_run_id": None,
                "details_url": "https://example.com/external",
                "workflow_url": None,
            }
        ]
        failures = collect_ci_failures("owner", "repo", "10")

        assert len(failures) == 1
        assert "No workflow run ID available" in failures[0]["log_output"]
        mock_fetch_log.assert_not_called()

    @patch("autocoder_utils.gh_pr_helper.fetch_ci_run_log")
    @patch("autocoder_utils.gh_pr_helper.fetch_failed_ci_runs")
    def test_collect_ci_failures_log_summarization(
        self, mock_fetch_runs, mock_fetch_log
    ):
        """Test that log summarization toggles between summary and full log."""
        sample_log = "\n".join(
            [
                "setup step output",
                "more details",
                "optional-tests  UNKNOWN STEP    2025-12-01T00:09:38.3786545Z =========================== short test summary info ===========================",
                "optional-tests  UNKNOWN STEP    2025-12-01T00:09:38.3787457Z FAILED tests/test_example.py::test_something - AssertionError",
                "optional-tests  UNKNOWN STEP    2025-12-01T00:09:38.3789910Z = 1 failed, 10 passed =",
            ]
        )
        mock_fetch_runs.return_value = [
            {
                "name": "optional-tests",
                "workflow_run_id": 42,
                "details_url": "https://example.com/runs/42",
                "workflow_url": "https://github.com/run/42",
            }
        ]
        mock_fetch_log.return_value = sample_log

        summary_failures = collect_ci_failures(
            "owner", "repo", "10", include_full_logs=False
        )
        assert summary_failures[0]["log_output"].startswith("optional-tests")
        assert "setup step output" not in summary_failures[0]["log_output"]

        mock_fetch_log.return_value = sample_log
        full_failures = collect_ci_failures(
            "owner", "repo", "10", include_full_logs=True
        )
        assert full_failures[0]["log_output"] == sample_log
        assert mock_fetch_log.call_count == 2


class TestSummarizeCILog:
    """Direct tests for log summarization helper."""

    def test_summarize_ci_log_uses_failed_marker(self):
        """If summary marker missing, ensure we still capture failed lines."""
        log_output = "info\nanother line\nFAILED test_sample::test_a\nTrailing"
        snippet = _summarize_ci_log(log_output, include_full_log=False)
        assert snippet.startswith("FAILED")
        assert "info" not in snippet

    def test_summarize_ci_log_full_toggle(self):
        """When include_full_log is True, return entire log."""
        log_output = "line1\nline2"
        assert _summarize_ci_log(log_output, include_full_log=True) == log_output
