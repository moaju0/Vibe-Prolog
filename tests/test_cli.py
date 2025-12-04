"""Tests for the Vibe-Prolog CLI functionality."""

import subprocess
import sys
from pathlib import Path
import pytest
import os


class TestCLI:
    """Test the command-line interface functionality."""

    def run_cli(self, args, input_text=None, cwd=None):
        """Run the CLI with given arguments and return result."""
        cmd = [sys.executable, "vibeprolog.py"] + args
        env = os.environ.copy()
        env["PYTHONPATH"] = str(Path(__file__).parent.parent)

        result = subprocess.run(
            cmd,
            input=input_text,
            text=True,
            capture_output=True,
            cwd=cwd or Path(__file__).parent.parent
        )
        return result

    def test_interactive_mode_without_file(self):
        """Test starting interactive mode without a program file."""
        # Test that interactive mode starts without error
        result = self.run_cli([], input_text="quit.\n")

        # Should start successfully and show the prompt
        assert result.returncode == 0
        assert "Prolog Interactive Mode" in result.stdout
        assert "Enter queries ending with '.' or 'quit.' to exit" in result.stdout

    def test_query_without_file(self):
        """Test executing a query without loading a program file."""
        result = self.run_cli(["-q", "member(X, [1,2,3])"])

        assert result.returncode == 0
        assert "X = 1" in result.stdout or "X = 1 ;" in result.stdout

    def test_query_once_without_file(self):
        """Test executing a query with --once flag without a file."""
        result = self.run_cli(["-q", "member(X, [1,2,3])", "--once"])

        assert result.returncode == 0
        assert "X = 1" in result.stdout
        # Should not have multiple solutions
        assert " ;" not in result.stdout

    @pytest.mark.parametrize("exit_command", ["quit.", "exit.", "halt.", "quit", "exit", "halt"])
    def test_interactive_with_exit_commands(self, exit_command):
        """Test interactive mode with various exit commands."""
        result = self.run_cli([], input_text=f"{exit_command}\n")

        assert result.returncode == 0
        assert "Goodbye!" in result.stdout

    def test_query_with_file_still_works(self, tmp_path):
        """Test that loading a file and executing a query still works."""
        prolog_file = tmp_path / "test.pl"
        prolog_file.write_text("test_pred(a).\ntest_pred(b).\n")

        result = self.run_cli([str(prolog_file), "-q", "test_pred(X)"])

        assert result.returncode == 0
        assert "X = a" in result.stdout or "X = a ;" in result.stdout

    def test_interactive_with_file_still_works(self, tmp_path):
        """Test that loading a file and starting interactive mode still works."""
        prolog_file = tmp_path / "test.pl"
        prolog_file.write_text("test_pred(a).\ntest_pred(b).\n")

        result = self.run_cli([str(prolog_file)], input_text="test_pred(X).\nquit.\n")

        assert result.returncode == 0
        assert "X = a" in result.stdout or "X = a ;" in result.stdout

    def test_invalid_file_error(self):
        """Test error handling for non-existent file."""
        result = self.run_cli(["nonexistent.pl"])

        assert result.returncode == 1
        assert "Error: File 'nonexistent.pl' not found" in result.stderr

    def test_directory_as_file_error(self):
        """Test error handling when passing a directory instead of a file."""
        result = self.run_cli(["."])

        assert result.returncode == 1
        assert "is not a file" in result.stderr

    def test_query_with_builtin_predicates(self):
        """Test that built-in predicates work without loading a file."""
        result = self.run_cli(["-q", "length([a,b,c], L)"])

        assert result.returncode == 0
        assert "L = 3" in result.stdout

    def test_query_failure_without_file(self):
        """Test query failure without loading a file."""
        result = self.run_cli(["-q", "false"])

        assert result.returncode == 1
        assert "false." in result.stdout

    def test_verbose_mode_without_file(self):
        """Test verbose mode works without a file."""
        result = self.run_cli(["-q", "true", "-v"])

        assert result.returncode == 0
        assert "Query: true." in result.stdout

    def test_show_bindings_without_file(self):
        """Test --show-bindings flag works without a file."""
        result = self.run_cli(["-q", "X = 5", "--show-bindings"])

        assert result.returncode == 0
        assert "X = 5" in result.stdout

    def test_program_args_without_file(self):
        """Test that program arguments work without a file."""
        result = self.run_cli(["-q", "current_prolog_flag(argv, Args)"], input_text="")

        # This should work even without a file
        assert result.returncode == 0

    def test_help_display(self):
        """Test that help text is displayed correctly."""
        result = self.run_cli(["--help"])

        assert result.returncode == 0
        assert "Prolog program file to load (.pl)" in result.stdout
        assert "interactive mode" in result.stdout
        assert "Start interactive mode without loading a file" in result.stdout

    def test_interactive_mode_with_empty_input(self):
        """Test interactive mode handles empty input gracefully."""
        result = self.run_cli([], input_text="\n\nquit.\n")

        assert result.returncode == 0
        assert "Goodbye!" in result.stdout

    def test_query_with_arithmetic_without_file(self):
        """Test arithmetic queries work without loading a file."""
        result = self.run_cli(["-q", "X is 2 + 3"])

        assert result.returncode == 0
        assert "X = 5" in result.stdout