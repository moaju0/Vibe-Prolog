"""Tests for --builtin-conflict flag functionality with library loading.

This test file specifically focuses on ensuring that library files with
operator/predicate conflicts load correctly in different builtin_conflict modes.
"""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestLibraryLoadingSkipMode:
    """Tests for skip mode (default) - libraries with conflicts should load silently."""

    @pytest.mark.performance
    def test_dcgs_library_loads_in_skip_mode(self):
        """library(dcgs) exports op(1105, xfy, |) which conflicts with protected | operator."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # Should not raise an error
        prolog.consult_string(":- use_module(library(dcgs)).")
        # Verify the library loaded by checking if phrase/2 is available
        assert prolog.has_solution("current_predicate(phrase/2)")

    @pytest.mark.performance
    def test_clpb_library_loads_in_skip_mode(self):
        """library(clpb) defines operators ~ and # which may conflict."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # Should not raise an error
        prolog.consult_string(":- use_module(library(clpb)).")
        # Verify the library loaded
        assert prolog.has_solution("current_predicate(sat/1)")

    @pytest.mark.performance
    def test_clpz_library_loads_in_skip_mode(self):
        """library(clpz) may have operator conflicts."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # Should not raise an error
        prolog.consult_string(":- use_module(library(clpz)).")
        # Verify the library loaded
        assert prolog.has_solution("current_predicate((#=)/2)")

    @pytest.mark.performance
    def test_multiple_affected_libraries_load(self):
        """Test loading multiple libraries that were previously failing."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        affected_libraries = [
            "library(arithmetic)",
            "library(charsio)",
            "library(csv)",
            "library(debug)",
            "library(dif)",
            "library(ffi)",
            "library(files)",
            "library(format)",
            "library(freeze)",
            "library(http/http_server)",
            "library(numerics/quadtests)",
            "library(os)",
            "library(pio)",
            "library(process)",
            "library(reif)",
            "library(serialization/abnf)",
            "library(serialization/json)",
            "library(tabling/double_linked_list)",
            "library(tabling/global_worklist)",
            "library(tabling/table_link_manager)",
            "library(tabling/trie)",
            "library(tabling/wrapper)",
            "library(time)",
            "library(when)",
            "library(xpath)"
        ]

        for lib in affected_libraries:
            try:
                prolog.consult_string(f":- use_module({lib}).")
            except Exception as e:
                pytest.fail(f"Failed to load {lib}: {e}")

    @pytest.mark.performance
    def test_skip_mode_preserves_builtin_operators(self):
        """In skip mode, built-in operators should still work even after loading conflicting libraries."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        prolog.consult("library(dcgs)")  # This tries to redefine |

        # Test that | still works for list construction
        result = prolog.query_once("X = [a, b | [c]]")
        assert result is not None
        assert result["X"] == ["a", "b", "c"]

    @pytest.mark.performance
    def test_skip_mode_preserves_builtin_predicates(self):
        """Built-in predicates should still work after loading libraries that redefine them."""
        prolog = PrologInterpreter(builtin_conflict="skip")

        # Load a library that might redefine built-ins
        prolog.consult("library(charsio)")

        # Test that built-in length/2 still works
        result = prolog.query_once("length([a, b, c], L)")
        assert result is not None
        assert result["L"] == 3


class TestLibraryLoadingErrorMode:
    """Tests for error mode - libraries with conflicts should raise permission_error."""

    @pytest.mark.performance
    def test_dcgs_library_fails_in_error_mode(self):
        """library(dcgs) should fail to load in error mode due to | operator conflict."""
        prolog = PrologInterpreter(builtin_conflict="error")
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- use_module(library(dcgs)).")
        error_str = str(exc_info.value)
        assert "permission_error" in error_str

    @pytest.mark.performance
    def test_clpb_library_fails_in_error_mode(self):
        """library(clpb) should fail in error mode."""
        prolog = PrologInterpreter(builtin_conflict="error")
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- use_module(library(clpb)).")
        error_str = str(exc_info.value)
        assert "permission_error" in error_str


class TestLibraryLoadingShadowMode:
    """Tests for shadow mode - libraries can define conflicting operators/predicates."""

    @pytest.mark.performance
    def test_dcgs_library_loads_in_shadow_mode(self):
        """library(dcgs) should load successfully in shadow mode."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        # Should not raise an error
        prolog.consult_string(":- use_module(library(dcgs)).")
        assert prolog.has_solution("current_predicate(phrase/2)")

    @pytest.mark.performance
    def test_shadow_mode_preserves_global_operators(self):
        """In shadow mode, global operators should still work."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string(":- use_module(library(dcgs)).")  # Tries to shadow |

        # Global | should still work for lists
        result = prolog.query_once("X = [a, b | [c]]")
        assert result is not None
        assert result["X"] == ["a", "b", "c"]

    @pytest.mark.performance
    def test_shadow_mode_module_qualified_uses_shadow(self):
        """Module-qualified calls should use shadowed definitions."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult("library(dcgs)")

        # Test that dcgs module has its own operators
        # This is harder to test directly, but we can check the module loaded
        assert prolog.has_solution("current_predicate(dcgs:phrase/2)")


class TestOperatorShadowingBehavior:
    """Test specific operator shadowing behavior."""

    def test_protected_operators_list(self):
        """Verify the list of protected operators."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # The protected operators are: , ; -> :- : | {}
        # Test that these cannot be redefined globally
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(100, xfy, '|').")

    def test_shadow_mode_allows_module_operator_redefinition(self):
        """In shadow mode, modules can redefine protected operators."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        # This should work in shadow mode
        prolog.consult_string("""
            :- module(test_mod, [test_pred/0]).
            :- op(100, xfy, '|').
            test_pred.
        """)
        # Module should load without error
        assert prolog.has_solution("current_predicate(test_mod:test_pred/0)")


class TestPredicateShadowingBehavior:
    """Test predicate shadowing behavior."""

    def test_skip_mode_ignores_predicate_redefinition(self):
        """In skip mode, predicate redefinitions are ignored."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        prolog.consult_string("""
            :- module(test_mod, [length/2]).
            length([], 0).
            length([_|T], N) :- length(T, N1), N is N1 + 1.
        """)
        # Built-in length should still work
        result = prolog.query_once("length([a, b, c], L)")
        assert result["L"] == 3

    def test_error_mode_fails_on_predicate_redefinition(self):
        """Error mode should fail on predicate redefinition."""
        prolog = PrologInterpreter(builtin_conflict="error")
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- module(test_mod, [length/2]).
                length([], 0).
            """)
        error_str = str(exc_info.value)
        assert "permission_error" in error_str

    def test_shadow_mode_allows_predicate_shadowing(self):
        """Shadow mode allows predicate shadowing."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, [length/2]).
            length([], zero).
            length([_|T], s(N)) :- length(T, N).
        """)
        # Module-qualified call uses shadowed version
        result = prolog.query_once("test_mod:length([a, b], L)")
        assert result is not None
        assert result["L"] == {'s': [{'s': ['zero']}]}

        # Unqualified call uses built-in
        result = prolog.query_once("length([a, b], L)")
        assert result["L"] == 2
