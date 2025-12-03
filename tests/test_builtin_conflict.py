"""Tests for --builtin-conflict flag functionality."""

import subprocess
import sys
import tempfile
from pathlib import Path

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestBuiltinConflictSkipMode:
    """Tests for skip mode (default behavior)."""

    def test_skip_is_default(self):
        """Skip mode should be the default."""
        prolog = PrologInterpreter()
        assert prolog.builtin_conflict == "skip"

    def test_skip_mode_loads_library_silently(self):
        """Loading a library that defines a built-in should succeed silently in skip mode."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # Define a predicate that conflicts with the built-in length/2
        prolog.consult_string("""
            length([], zero).
            length([_|T], s(N)) :- length(T, N).
        """)
        # Should not raise an error

    def test_skip_mode_uses_builtin_version(self):
        """In skip mode, the built-in version should be used, not the library version."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # Try to define a conflicting length/2 that uses peano numbers
        prolog.consult_string("""
            length([], zero).
            length([_|T], s(N)) :- length(T, N).
        """)
        # The built-in length/2 should still work with integer lengths
        result = prolog.query_once("length([a, b, c], L)")
        assert result is not None
        assert result["L"] == 3  # Built-in returns integers, not peano numbers

    def test_skip_mode_preserves_member_builtin(self):
        """Skipping a redefinition of member/2 should preserve the built-in."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # Try to define a broken member/2
        prolog.consult_string("""
            member(_, _) :- fail.
        """)
        # Built-in member/2 should still work
        assert prolog.has_solution("member(2, [1, 2, 3])")

    def test_skip_mode_with_multiple_conflicts(self):
        """Multiple conflicting predicates should all be skipped."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        prolog.consult_string("""
            length(_, broken).
            append(_, _, broken).
            member(broken, _).
        """)
        # All built-ins should still work correctly
        assert prolog.query_once("length([1, 2], L)")["L"] == 2
        assert prolog.query_once("append([1], [2], R)")["R"] == [1, 2]
        assert prolog.has_solution("member(1, [1, 2, 3])")


class TestBuiltinConflictErrorMode:
    """Tests for error mode."""

    def test_error_mode_raises_permission_error(self):
        """Error mode should raise permission_error when a library redefines a built-in."""
        prolog = PrologInterpreter(builtin_conflict="error")
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                length([], 0).
                length([_|T], N) :- length(T, N1), N is N1 + 1.
            """)
        error_str = str(exc_info.value)
        assert "permission_error" in error_str
        assert "length" in error_str

    def test_error_mode_iso_error_format(self):
        """Error should be in ISO format: error(permission_error(...), context(...))."""
        prolog = PrologInterpreter(builtin_conflict="error")
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("member(X, [X|_]).")
        error_str = str(exc_info.value)
        assert "permission_error" in error_str
        assert "modify" in error_str
        assert "static_procedure" in error_str

    def test_error_mode_allows_non_conflicting_predicates(self):
        """Error mode should allow predicates that don't conflict."""
        prolog = PrologInterpreter(builtin_conflict="error")
        prolog.consult_string("""
            my_custom_predicate(x).
            another_predicate(1, 2).
        """)
        assert prolog.has_solution("my_custom_predicate(x)")
        assert prolog.has_solution("another_predicate(1, 2)")


class TestBuiltinConflictShadowMode:
    """Tests for shadow mode (placeholder - not yet implemented)."""

    def test_shadow_mode_falls_back_to_skip_programmatically(self):
        """Programmatically, shadow mode should fall back to 'skip' behavior."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        # Define a predicate that conflicts with the built-in length/2
        prolog.consult_string("""
            length([], zero).
        """)
        # The built-in length/2 should still work, proving the redefinition was skipped.
        result = prolog.query_once("length([a, b], L)")
        assert result is not None
        assert result["L"] == 2


class TestBuiltinConflictCLI:
    """Tests for command-line argument parsing."""

    def test_cli_default_is_skip(self):
        """Default mode when flag is not provided should be skip."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".pl", delete=False) as f:
            f.write("test_fact(1).\n")
            f.flush()
            result = subprocess.run(
                [sys.executable, "vibeprolog.py", f.name, "-q", "test_fact(X)"],
                capture_output=True,
                text=True,
            )
            assert result.returncode == 0
            assert "1" in result.stdout

    def test_cli_skip_mode_explicit(self):
        """Explicit skip mode should work."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".pl", delete=False) as f:
            f.write("length([], custom). test_fact(1).\n")
            f.flush()
            result = subprocess.run(
                [
                    sys.executable,
                    "vibeprolog.py",
                    "--builtin-conflict=skip",
                    f.name,
                    "-q",
                    "test_fact(X)",
                ],
                capture_output=True,
                text=True,
            )
            assert result.returncode == 0

    def test_cli_error_mode(self):
        """Error mode via CLI should raise error on conflict."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".pl", delete=False) as f:
            f.write("length([], custom).\n")
            f.flush()
            result = subprocess.run(
                [
                    sys.executable,
                    "vibeprolog.py",
                    "--builtin-conflict=error",
                    f.name,
                    "-q",
                    "true",
                ],
                capture_output=True,
                text=True,
            )
            assert result.returncode != 0
            assert "permission_error" in result.stderr or "Error" in result.stderr

    def test_cli_shadow_mode_not_implemented(self):
        """Shadow mode should print error and exit."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".pl", delete=False) as f:
            f.write("test_fact(1).\n")
            f.flush()
            result = subprocess.run(
                [
                    sys.executable,
                    "vibeprolog.py",
                    "--builtin-conflict=shadow",
                    f.name,
                    "-q",
                    "true",
                ],
                capture_output=True,
                text=True,
            )
            assert result.returncode == 1
            assert "not yet implemented" in result.stderr

    def test_cli_invalid_mode_rejected(self):
        """Invalid mode should be rejected by argparse."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".pl", delete=False) as f:
            f.write("test_fact(1).\n")
            f.flush()
            result = subprocess.run(
                [
                    sys.executable,
                    "vibeprolog.py",
                    "--builtin-conflict=invalid",
                    f.name,
                    "-q",
                    "true",
                ],
                capture_output=True,
                text=True,
            )
            assert result.returncode != 0
            assert "invalid" in result.stderr.lower() or "choice" in result.stderr.lower()


class TestBuiltinConflictWithModules:
    """Tests for builtin_conflict behavior with modules."""

    def test_skip_mode_with_module_definition(self):
        """Skip mode should work correctly with module definitions."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        prolog.consult_string("""
            :- module(test_module, [my_length/2]).
            
            % This conflicts with built-in length/2
            length([], peano_zero).
            length([_|T], peano_succ(N)) :- length(T, N).
            
            % Non-conflicting predicate
            my_length([], 0).
            my_length([_|T], N) :- my_length(T, N1), N is N1 + 1.
        """)
        # Built-in length should still work
        result = prolog.query_once("length([1, 2, 3], L)")
        assert result is not None
        assert result["L"] == 3

    def test_error_mode_with_use_module(self):
        """Error mode should raise error when use_module loads conflicting predicates."""
        # Create a temporary module file
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".pl", delete=False
        ) as f:
            f.write("""
:- module(conflict_module, [length/2]).
length([], custom_empty).
""")
            f.flush()
            module_path = f.name

        prolog = PrologInterpreter(builtin_conflict="error")
        # Loading a module that exports a conflicting predicate should fail
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(f':- use_module("{module_path}").')
        error_str = str(exc_info.value)
        assert "permission_error" in error_str or "length" in error_str

        # Cleanup
        Path(module_path).unlink()


class TestBuiltinConflictEdgeCases:
    """Edge cases and special scenarios."""

    def test_skip_mode_different_arity(self):
        """Predicates with same name but different arity should not conflict."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # length/3 doesn't exist as a built-in (only length/2)
        prolog.consult_string("""
            length(List, N, Unit) :- length(List, N), Unit = elements.
        """)
        result = prolog.query_once("length([1, 2], N, U)")
        assert result is not None
        assert result["N"] == 2
        assert result["U"] == "elements"

    def test_skip_mode_all_clauses_skipped(self):
        """All clauses for a conflicting predicate should be skipped."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        prolog.consult_string("""
            length([], zero).
            length([_], one).
            length([_, _], two).
            length([_, _, _], three).
        """)
        # Built-in should be used for all cases
        assert prolog.query_once("length([], L)")["L"] == 0
        assert prolog.query_once("length([a], L)")["L"] == 1
        assert prolog.query_once("length([a, b], L)")["L"] == 2
        assert prolog.query_once("length([a, b, c], L)")["L"] == 3

    def test_error_mode_first_clause_errors(self):
        """In error mode, the first conflicting clause should raise the error."""
        prolog = PrologInterpreter(builtin_conflict="error")
        with pytest.raises(PrologThrow):
            prolog.consult_string("""
                length([], 0).
                % These clauses should never be reached
                length([_|T], N) :- length(T, N1), N is N1 + 1.
            """)
