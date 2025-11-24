"""Tests for setup_call_cleanup and call_cleanup binding handling."""

import io
import sys

from vibeprolog import PrologInterpreter


class TestCleanupBindings:
    """Test that cleanup goals can see bindings from the main goal."""

    def test_call_cleanup_sees_goal_bindings(self):
        """Cleanup goal should see bindings from the main goal."""
        prolog = PrologInterpreter()

        # Capture stdout
        old_stdout = sys.stdout
        sys.stdout = captured_output = io.StringIO()
        try:
            # call_cleanup(X=1, write(X)) should write "1", not an unbound variable
            prolog.query("call_cleanup(X=1, write(X))")
            output = captured_output.getvalue()
        finally:
            sys.stdout = old_stdout

        # The cleanup should have written "1"
        assert "1" in output, f"Expected '1' in output, got: {repr(output)}"

    def test_setup_call_cleanup_sees_goal_bindings(self):
        """Cleanup goal should see bindings from the goal, not just from setup."""
        prolog = PrologInterpreter()

        # Capture stdout
        old_stdout = sys.stdout
        sys.stdout = captured_output = io.StringIO()
        try:
            # setup_call_cleanup(true, X=1, write(X)) should write "1"
            prolog.query("setup_call_cleanup(true, X=1, write(X))")
            output = captured_output.getvalue()
        finally:
            sys.stdout = old_stdout

        # The cleanup should have written "1"
        assert "1" in output, f"Expected '1' in output, got: {repr(output)}"

    def test_setup_call_cleanup_cleanup_sees_setup_and_goal_bindings(self):
        """Cleanup goal should see bindings from both setup and goal."""
        prolog = PrologInterpreter()

        # Capture stdout
        old_stdout = sys.stdout
        sys.stdout = captured_output = io.StringIO()
        try:
            # Setup binds Y=setup, Goal binds X=goal, cleanup should see both
            prolog.query("setup_call_cleanup(Y=setup, X=goal, format('~w-~w', [Y, X]))")
            output = captured_output.getvalue()
        finally:
            sys.stdout = old_stdout

        # The cleanup should have written "setup-goal"
        assert "setup-goal" in output, f"Expected 'setup-goal' in output, got: {repr(output)}"

    def test_call_cleanup_with_multiple_solutions(self):
        """Cleanup should see bindings from the last solution."""
        prolog = PrologInterpreter()

        # Capture stdout
        old_stdout = sys.stdout
        sys.stdout = captured_output = io.StringIO()
        try:
            # Test that cleanup sees the last value of X when goal has multiple solutions
            # Using once to take only the first solution, but cleanup should still see X=1
            prolog.query("once(call_cleanup(member(X, [1, 2, 3]), write(X)))")
            output = captured_output.getvalue()
        finally:
            sys.stdout = old_stdout

        # The cleanup should have written "1" (the first/last solution taken)
        assert "1" in output, f"Expected '1' in output, got: {repr(output)}"

    def test_setup_call_cleanup_with_multiple_solutions(self):
        """Cleanup should see bindings from the last solution with setup."""
        prolog = PrologInterpreter()

        # Capture stdout
        old_stdout = sys.stdout
        sys.stdout = captured_output = io.StringIO()
        try:
            # Setup succeeds, goal has multiple solutions, take first with once
            prolog.query("once(setup_call_cleanup(true, member(X, [a, b, c]), write(X)))")
            output = captured_output.getvalue()
        finally:
            sys.stdout = old_stdout

        # The cleanup should have written "a"
        assert "a" in output, f"Expected 'a' in output, got: {repr(output)}"
