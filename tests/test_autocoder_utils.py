"""Tests for autocoder_utils module."""

import os
import subprocess
import sys
from pathlib import Path
from unittest.mock import patch

import pytest

# Add ai-tools to path for importing
sys.path.insert(0, str(Path(__file__).parent.parent / "ai-tools"))

from autocoder_utils import (
    check_commands_available,
    ensure_env,
    get_owner_repo,
    get_repo_root,
    has_staged_changes,
    run,
    stage_changes,
)


class TestCheckCommandsAvailable:
    """Tests for check_commands_available function."""

    def test_all_commands_available(self):
        """Test when all required commands are available."""
        with patch("shutil.which", return_value="/usr/bin/gh"):
            # Should not raise
            check_commands_available(["gh"])

    def test_missing_command(self):
        """Test when a required command is missing."""
        with patch("shutil.which", side_effect=lambda cmd: None if cmd == "missing" else "/usr/bin/gh"):
            with pytest.raises(SystemExit):
                check_commands_available(["gh", "missing"])


class TestEnsureEnv:
    """Tests for ensure_env function."""

    def test_openai_key_set(self):
        """Test when OPENAI_API_KEY is set."""
        with patch.dict(os.environ, {"OPENAI_API_KEY": "test_key"}):
            # Should not raise
            ensure_env()

    def test_openai_key_not_set(self):
        """Test when OPENAI_API_KEY is not set."""
        with patch.dict(os.environ, {}, clear=True):
            with pytest.raises(SystemExit):
                ensure_env()

    def test_llm_model_default(self):
        """Test that LLM_MODEL gets default value."""
        with patch.dict(os.environ, {"OPENAI_API_KEY": "test_key"}, clear=True):
            ensure_env()
            assert os.environ["LLM_MODEL"] == "gpt-5-nano"


class TestRun:
    """Tests for run function."""

    def test_successful_command(self):
        """Test successful command execution."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess(
                args=["echo", "test"],
                returncode=0,
                stdout=b"output",
                stderr=b""
            )
            result = run(["echo", "test"])
            assert result == "output"

    def test_failed_command(self):
        """Test failed command execution."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess(
                args=["false"],
                returncode=1,
                stdout=b"",
                stderr=b"error"
            )
            with pytest.raises(SystemExit):
                run(["false"])

    def test_no_capture_output(self):
        """Test with capture_output=False."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess(
                args=["echo", "test"],
                returncode=0,
                stdout=None,
                stderr=b""
            )
            result = run(["echo", "test"], capture_output=False)
            assert result == ""


class TestGitHelpers:
    """Tests for git-related helper functions."""

    def test_get_repo_root(self):
        """Test get_repo_root function."""
        with patch("autocoder_utils.run", return_value="/path/to/repo\n"):
            result = get_repo_root()
            assert result == Path("/path/to/repo")

    def test_get_owner_repo_https(self):
        """Test get_owner_repo with HTTPS URL."""
        with patch("autocoder_utils.run", return_value="https://github.com/owner/repo.git\n"):
            owner, repo = get_owner_repo()
            assert owner == "owner"
            assert repo == "repo"

    def test_get_owner_repo_ssh(self):
        """Test get_owner_repo with SSH URL."""
        with patch("autocoder_utils.run", return_value="git@github.com:owner/repo.git\n"):
            owner, repo = get_owner_repo()
            assert owner == "owner"
            assert repo == "repo"

    def test_get_owner_repo_no_url(self):
        """Test get_owner_repo when no URL is configured."""
        with patch("autocoder_utils.run", return_value=""):
            with pytest.raises(SystemExit):
                get_owner_repo()

    def test_has_staged_changes_true(self):
        """Test has_staged_changes when there are staged changes."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess(
                args=["git", "diff", "--cached", "--quiet"],
                returncode=1,
                stdout=b"",
                stderr=b""
            )
            assert has_staged_changes() is True

    def test_has_staged_changes_false(self):
        """Test has_staged_changes when there are no staged changes."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess(
                args=["git", "diff", "--cached", "--quiet"],
                returncode=0,
                stdout=b"",
                stderr=b""
            )
            assert has_staged_changes() is False

    def test_stage_changes(self):
        """Test stage_changes function."""
        with patch("autocoder_utils.run") as mock_run:
            stage_changes()
            mock_run.assert_called_once_with(["git", "add", "-A"], capture_output=False)