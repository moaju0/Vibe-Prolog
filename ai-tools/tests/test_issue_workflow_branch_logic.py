"""
Comprehensive tests for branch determination logic in issue_workflow.py

These tests verify the branch selection behavior under different scenarios
without actually creating or checking out branches.
"""

from unittest.mock import patch

import pytest
from autocoder_utils.issue_workflow import IssueWorkflowConfig, determine_target_branch, get_issue_linked_branches


class TestDetermineTargetBranch:
    """Test suite for determine_target_branch function."""

    def test_explicit_existing_branch_specified(self):
        """When existing_branch is set, it should be used regardless of current branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
            existing_branch="feature/my-custom-branch",
        )

        # Should use explicit branch when on main
        branch, should_create = determine_target_branch("123", "main", config)
        assert branch == "feature/my-custom-branch"
        assert should_create is False

        # Should use explicit branch when on another branch
        branch, should_create = determine_target_branch("123", "fix/123-some-fix", config)
        assert branch == "feature/my-custom-branch"
        assert should_create is False

        # Should use explicit branch even when linked branches exist
        branch, should_create = determine_target_branch(
            "123", "main", config, linked_branches=["fix/123-linked"]
        )
        assert branch == "feature/my-custom-branch"
        assert should_create is False

    def test_use_new_branch_flag_true(self):
        """When use_new_branch=True, should always create a new branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
            use_new_branch=True,
        )

        # Should create new branch when on main
        branch, should_create = determine_target_branch("123", "main", config)
        assert branch == ""  # Empty means gh issue develop will create it
        assert should_create is True

        # Should create new branch even when linked branches exist
        branch, should_create = determine_target_branch(
            "123", "fix/123-existing", config, linked_branches=["fix/123-linked"]
        )
        assert branch == ""
        assert should_create is True

    def test_existing_branch_takes_precedence_over_use_new_branch(self):
        """When both existing_branch and use_new_branch are set, existing_branch wins."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
            existing_branch="my-branch",
            use_new_branch=True,
        )

        branch, should_create = determine_target_branch("123", "main", config)
        assert branch == "my-branch"
        assert should_create is False

    def test_stay_on_current_branch_if_linked(self):
        """When current branch is one of the linked branches, should stay on it."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        # Current branch is in the linked branches list
        branch, should_create = determine_target_branch(
            "123", "fix/123-my-branch", config, linked_branches=["fix/123-my-branch", "fix/123-other"]
        )
        assert branch == "fix/123-my-branch"
        assert should_create is False

    def test_use_first_linked_branch_when_not_on_it(self):
        """When not on a linked branch, should checkout the first linked branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        # On main, but linked branches exist
        branch, should_create = determine_target_branch(
            "123", "main", config, linked_branches=["fix/123-first", "fix/123-second"]
        )
        assert branch == "fix/123-first"
        assert should_create is False

        # On different branch, but linked branches exist
        branch, should_create = determine_target_branch(
            "123", "fix/456-other", config, linked_branches=["fix/123-linked"]
        )
        assert branch == "fix/123-linked"
        assert should_create is False

    def test_create_new_branch_when_no_linked_branches(self):
        """When no linked branches exist, should create a new branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        # No linked branches, on main
        branch, should_create = determine_target_branch("123", "main", config, linked_branches=[])
        assert branch == ""
        assert should_create is True

        # No linked branches, on other branch
        branch, should_create = determine_target_branch(
            "123", "fix/456-other", config, linked_branches=[]
        )
        assert branch == ""
        assert should_create is True

        # linked_branches is None (not provided)
        branch, should_create = determine_target_branch("123", "main", config, linked_branches=None)
        assert branch == ""
        assert should_create is True

    def test_priority_order(self):
        """Test that the priority order is: existing_branch > use_new_branch > linked_branches > create."""
        # Priority 1: existing_branch (highest)
        config1 = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
            existing_branch="custom",
            use_new_branch=True,
        )
        branch, should_create = determine_target_branch(
            "123", "main", config1, linked_branches=["fix/123-linked"]
        )
        assert branch == "custom"
        assert should_create is False

        # Priority 2: use_new_branch
        config2 = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
            use_new_branch=True,
        )
        branch, should_create = determine_target_branch(
            "123", "main", config2, linked_branches=["fix/123-linked"]
        )
        assert branch == ""
        assert should_create is True

        # Priority 3: linked_branches (use first linked branch)
        config3 = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )
        branch, should_create = determine_target_branch(
            "123", "main", config3, linked_branches=["fix/123-linked"]
        )
        assert branch == "fix/123-linked"
        assert should_create is False

        # Priority 4: create new (when no linked branches)
        branch, should_create = determine_target_branch("123", "main", config3, linked_branches=[])
        assert branch == ""
        assert should_create is True


class TestLinkedBranchHandling:
    """Test suite for GitHub-linked branch behavior."""

    def test_single_linked_branch(self):
        """When issue has one linked branch, use it."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        branch, should_create = determine_target_branch(
            "123", "main", config, linked_branches=["fix/123-feature"]
        )
        assert branch == "fix/123-feature"
        assert should_create is False

    def test_multiple_linked_branches_uses_first(self):
        """When issue has multiple linked branches, use the first one."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        branch, should_create = determine_target_branch(
            "123",
            "main",
            config,
            linked_branches=["fix/123-first", "fix/123-second", "fix/123-third"],
        )
        assert branch == "fix/123-first"
        assert should_create is False

    def test_current_branch_in_linked_branches_middle(self):
        """When current branch is in the middle of linked branches, stay on it."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        branch, should_create = determine_target_branch(
            "123",
            "fix/123-second",
            config,
            linked_branches=["fix/123-first", "fix/123-second", "fix/123-third"],
        )
        assert branch == "fix/123-second"
        assert should_create is False

    def test_empty_linked_branches_list(self):
        """When linked_branches is empty list, create new branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        branch, should_create = determine_target_branch("123", "main", config, linked_branches=[])
        assert branch == ""
        assert should_create is True

    def test_none_linked_branches(self):
        """When linked_branches is None, create new branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )

        branch, should_create = determine_target_branch("123", "main", config, linked_branches=None)
        assert branch == ""
        assert should_create is True


class TestBranchPrefixToken:
    """Test the branch_prefix_token method used in branch naming."""

    def test_branch_prefix_token_format(self):
        """Verify the format of branch_prefix_token."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
        )
        assert config.branch_prefix_token("123") == "fix/123"

    def test_branch_requirement_format(self):
        """Verify the format of branch_requirement (full prefix)."""
        config = IssueWorkflowConfig(
            tool_cmd=["echo"],
            branch_prefix="fix",
            default_commit_message="fix",
            branch_suffix="-",
        )
        assert config.branch_requirement("123") == "fix/123-"


class TestRealWorldScenarios:
    """Integration-style tests simulating real-world usage patterns."""

    def test_developer_workflow_starting_from_main_no_existing_branch(self):
        """Simulate: Developer on main, runs fix-issue, no existing branch for issue."""
        config = IssueWorkflowConfig(
            tool_cmd=["kilocode"],
            branch_prefix="fix",
            default_commit_message="fix: apply kilocode changes",
        )

        # Developer is on main, working on issue 42, no linked branches
        branch, should_create = determine_target_branch("42", "main", config, linked_branches=[])
        assert should_create is True  # Should create new branch
        assert branch == ""  # gh issue develop will create it

    def test_developer_workflow_starting_from_main_with_existing_branch(self):
        """Simulate: Developer on main, runs fix-issue, branch already exists for issue."""
        config = IssueWorkflowConfig(
            tool_cmd=["kilocode"],
            branch_prefix="fix",
            default_commit_message="fix: apply kilocode changes",
        )

        # Developer is on main, but issue 42 already has a linked branch
        branch, should_create = determine_target_branch(
            "42", "main", config, linked_branches=["fix/42-memory-leak"]
        )
        assert should_create is False  # Should checkout existing branch
        assert branch == "fix/42-memory-leak"

    def test_developer_workflow_continuing_work(self):
        """Simulate: Developer continues work on existing issue branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["kilocode"],
            branch_prefix="fix",
            default_commit_message="fix: apply kilocode changes",
        )

        # Developer already on the issue branch, continues work
        branch, should_create = determine_target_branch(
            "42", "fix/42-memory-leak", config, linked_branches=["fix/42-memory-leak"]
        )
        assert should_create is False  # Should stay on current branch
        assert branch == "fix/42-memory-leak"

    def test_developer_workflow_switching_issues(self):
        """Simulate: Developer switches from one issue to another."""
        config = IssueWorkflowConfig(
            tool_cmd=["kilocode"],
            branch_prefix="fix",
            default_commit_message="fix: apply kilocode changes",
        )

        # Developer on issue 42's branch, but wants to work on issue 99 (no linked branch)
        branch, should_create = determine_target_branch(
            "99", "fix/42-memory-leak", config, linked_branches=[]
        )
        assert should_create is True  # Should create new branch for issue 99
        assert branch == ""

        # Developer on issue 42's branch, wants to work on issue 99 (has linked branch)
        branch, should_create = determine_target_branch(
            "99", "fix/42-memory-leak", config, linked_branches=["fix/99-performance"]
        )
        assert should_create is False  # Should checkout issue 99's branch
        assert branch == "fix/99-performance"

    def test_explicit_branch_for_testing(self):
        """Simulate: Developer wants to test changes on a specific branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["kilocode"],
            branch_prefix="fix",
            default_commit_message="fix: apply kilocode changes",
            existing_branch="experimental/test-branch",
        )

        # Should use the explicit branch regardless of current location or linked branches
        branch, should_create = determine_target_branch(
            "42", "main", config, linked_branches=["fix/42-existing"]
        )
        assert should_create is False
        assert branch == "experimental/test-branch"

    def test_force_new_branch_every_time(self):
        """Simulate: Tool configured to always create fresh branches."""
        config = IssueWorkflowConfig(
            tool_cmd=["kilocode"],
            branch_prefix="fix",
            default_commit_message="fix: apply kilocode changes",
            use_new_branch=True,
        )

        # Should create new branch even if on matching branch with linked branches
        branch, should_create = determine_target_branch(
            "42", "fix/42-existing", config, linked_branches=["fix/42-existing"]
        )
        assert should_create is True
        assert branch == ""

    def test_collaborator_picks_up_existing_work(self):
        """Simulate: Another developer picking up work on an existing issue branch."""
        config = IssueWorkflowConfig(
            tool_cmd=["kilocode"],
            branch_prefix="fix",
            default_commit_message="fix: apply kilocode changes",
        )

        # Developer on main, issue has a linked branch from another developer
        branch, should_create = determine_target_branch(
            "42", "main", config, linked_branches=["fix/42-started-by-alice"]
        )
        assert should_create is False  # Should checkout Alice's branch
        assert branch == "fix/42-started-by-alice"


class TestGetIssueLinkedBranches:
    """Test suite for get_issue_linked_branches parsing logic."""

    @patch("autocoder_utils.issue_workflow.run")
    def test_parse_single_branch(self, mock_run):
        """Should correctly parse output with a single linked branch."""
        mock_run.return_value = """Showing linked branches for nlothian/Vibe-Prolog#190

BRANCH                               URL
190-implement-retractall1-predicate  https://github.com/nlothian/Vibe-Prolog/tree/190-implement-retractall1-predicate
"""
        result = get_issue_linked_branches("190")
        assert result == ["190-implement-retractall1-predicate"]
        mock_run.assert_called_once_with(["gh", "issue", "develop", "--list", "190"])

    @patch("autocoder_utils.issue_workflow.run")
    def test_parse_multiple_branches(self, mock_run):
        """Should correctly parse output with multiple linked branches."""
        mock_run.return_value = """Showing linked branches for owner/repo#42

BRANCH                   URL
fix/42-first-attempt     https://github.com/owner/repo/tree/fix/42-first-attempt
fix/42-second-try        https://github.com/owner/repo/tree/fix/42-second-try
feature/42-new-approach  https://github.com/owner/repo/tree/feature/42-new-approach
"""
        result = get_issue_linked_branches("42")
        assert result == [
            "fix/42-first-attempt",
            "fix/42-second-try",
            "feature/42-new-approach",
        ]

    @patch("autocoder_utils.issue_workflow.run")
    def test_parse_empty_output(self, mock_run):
        """Should return empty list when output is empty."""
        mock_run.return_value = ""
        result = get_issue_linked_branches("999")
        assert result == []

    @patch("autocoder_utils.issue_workflow.run")
    def test_parse_only_headers_no_branches(self, mock_run):
        """Should return empty list when only headers present (no branches)."""
        mock_run.return_value = """Showing linked branches for owner/repo#123

BRANCH                               URL
"""
        result = get_issue_linked_branches("123")
        assert result == []

    @patch("autocoder_utils.issue_workflow.run")
    def test_parse_with_extra_whitespace(self, mock_run):
        """Should handle varying amounts of whitespace correctly."""
        mock_run.return_value = """Showing linked branches for owner/repo#42

BRANCH                               URL
my-branch                            https://github.com/owner/repo/tree/my-branch
another-branch-with-long-name        https://github.com/owner/repo/tree/another-branch-with-long-name
"""
        result = get_issue_linked_branches("42")
        assert result == [
            "my-branch",
            "another-branch-with-long-name",
        ]

    @patch("autocoder_utils.issue_workflow.run")
    def test_parse_with_blank_lines(self, mock_run):
        """Should skip blank lines in output."""
        mock_run.return_value = """Showing linked branches for owner/repo#42


BRANCH                               URL

fix/42-branch                        https://github.com/owner/repo/tree/fix/42-branch

"""
        result = get_issue_linked_branches("42")
        assert result == ["fix/42-branch"]

    @patch("autocoder_utils.issue_workflow.run")
    def test_handle_command_failure(self, mock_run):
        """Should return empty list when gh command fails."""
        mock_run.side_effect = Exception("gh command failed")
        result = get_issue_linked_branches("123")
        assert result == []

    @patch("autocoder_utils.issue_workflow.run")
    def test_extract_only_branch_name_not_url(self, mock_run):
        """Should extract only branch name, not the URL (regression test)."""
        mock_run.return_value = """Showing linked branches for owner/repo#190

BRANCH                               URL
190-implement-retractall1-predicate  https://github.com/owner/repo/tree/190-implement-retractall1-predicate
"""
        result = get_issue_linked_branches("190")
        # Should NOT contain the URL or tab character
        assert len(result) == 1
        assert result[0] == "190-implement-retractall1-predicate"
        assert "https://" not in result[0]
        assert "\t" not in result[0]
