"""Tests for --builtin-conflict flag functionality with library loading.

This test file specifically focuses on ensuring that library files with
operator/predicate conflicts load correctly in different builtin_conflict modes.
"""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


@pytest.fixture
def skip_prolog():
    """Provides a PrologInterpreter with builtin_conflict='skip'."""
    return PrologInterpreter(builtin_conflict="skip")


@pytest.fixture
def error_prolog():
    """Provides a PrologInterpreter with builtin_conflict='error'."""
    return PrologInterpreter(builtin_conflict="error")


@pytest.fixture
def shadow_prolog():
    """Provides a PrologInterpreter with builtin_conflict='shadow'."""
    return PrologInterpreter(builtin_conflict="shadow")


class TestLibraryLoadingSkipMode:
    """Tests for skip mode (default) - libraries with conflicts should load silently."""

    @pytest.mark.performance
    def test_dcgs_library_loads_in_skip_mode(self, skip_prolog):
        """library(dcgs) exports op(1105, xfy, |) which conflicts with protected | operator."""
        prolog = skip_prolog
        # Should not raise an error
        prolog.consult_string(":- use_module(library(dcgs)).")
        # Verify the library loaded by checking if phrase/2 is available
        assert prolog.has_solution("current_predicate(phrase/2)")

    @pytest.mark.performance
    def test_clpb_library_loads_in_skip_mode(self, skip_prolog):
        """library(clpb) defines operators ~ and # which may conflict."""
        prolog = skip_prolog
        # Should not raise an error
        prolog.consult_string(":- use_module(library(clpb)).")
        # Verify the library loaded
        assert prolog.has_solution("current_predicate(sat/1)")

    @pytest.mark.performance
    def test_clpz_library_loads_in_skip_mode(self, skip_prolog):
        """library(clpz) may have operator conflicts."""
        prolog = skip_prolog
        # Should not raise an error
        prolog.consult_string(":- use_module(library(clpz)).")
        # Verify the library loaded
        assert prolog.has_solution("current_predicate((#=)/2)")

    @pytest.mark.performance
    def test_multiple_affected_libraries_load(self, skip_prolog):
        """Test loading multiple libraries that were previously failing."""
        prolog = skip_prolog
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
    def test_skip_mode_preserves_builtin_operators(self, skip_prolog):
        """In skip mode, built-in operators should still work even after loading conflicting libraries."""
        prolog = skip_prolog
        prolog.consult("library(dcgs)")  # This tries to redefine |

        # Test that | still works for list construction
        result = prolog.query_once("X = [a, b | [c]]")
        assert result is not None
        assert result["X"] == ["a", "b", "c"]

    @pytest.mark.performance
    def test_skip_mode_preserves_builtin_predicates(self, skip_prolog):
        """Built-in predicates should still work after loading libraries that redefine them."""
        prolog = skip_prolog

        # Load a library that might redefine built-ins
        prolog.consult("library(charsio)")

        # Test that built-in length/2 still works
        result = prolog.query_once("length([a, b, c], L)")
        assert result is not None
        assert result["L"] == 3


class TestLibraryLoadingErrorMode:
    """Tests for error mode - libraries with conflicts should raise permission_error."""

    @pytest.mark.performance
    def test_dcgs_library_fails_in_error_mode(self, error_prolog):
        """library(dcgs) should fail to load in error mode due to | operator conflict."""
        prolog = error_prolog
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- use_module(library(dcgs)).")
        error_str = str(exc_info.value)
        assert "permission_error" in error_str

    @pytest.mark.performance
    def test_clpb_library_fails_in_error_mode(self, error_prolog):
        """library(clpb) should fail in error mode."""
        prolog = error_prolog
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- use_module(library(clpb)).")
        error_str = str(exc_info.value)
        assert "permission_error" in error_str


class TestLibraryLoadingShadowMode:
    """Tests for shadow mode - libraries can define conflicting operators/predicates."""

    @pytest.mark.performance
    def test_dcgs_library_loads_in_shadow_mode(self, shadow_prolog):
        """library(dcgs) should load successfully in shadow mode."""
        prolog = shadow_prolog
        # Should not raise an error
        prolog.consult_string(":- use_module(library(dcgs)).")
        assert prolog.has_solution("current_predicate(phrase/2)")

    @pytest.mark.performance
    def test_shadow_mode_preserves_global_operators(self, shadow_prolog):
        """In shadow mode, global operators should still work."""
        prolog = shadow_prolog
        prolog.consult_string(":- use_module(library(dcgs)).")  # Tries to shadow |

        # Global | should still work for lists
        result = prolog.query_once("X = [a, b | [c]]")
        assert result is not None
        assert result["X"] == ["a", "b", "c"]

    @pytest.mark.performance
    def test_shadow_mode_module_qualified_uses_shadow(self, shadow_prolog):
        """Module-qualified calls should use shadowed definitions."""
        prolog = shadow_prolog
        prolog.consult("library(dcgs)")

        # Test that dcgs module has its own operators
        # This is harder to test directly, but we can check the module loaded
        assert prolog.has_solution("current_predicate(dcgs:phrase/2)")


class TestOperatorShadowingBehavior:
    """Test specific operator shadowing behavior."""

    @pytest.mark.parametrize("op", [",", ";", "->", ":-", ":", "|", "{}"])
    def test_protected_operators_list(self, skip_prolog, op):
        """Verify the list of protected operators."""
        # The protected operators are: , ; -> :- : | {}
        # Test that these cannot be redefined globally
        with pytest.raises(PrologThrow):
            skip_prolog.consult_string(f":- op(100, xfy, {repr(op)}).")

    def test_shadow_mode_allows_module_operator_redefinition(self, shadow_prolog):
        """In shadow mode, modules can redefine protected operators."""
        prolog = shadow_prolog
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

    def test_skip_mode_ignores_predicate_redefinition(self, skip_prolog):
        """In skip mode, predicate redefinitions are ignored."""
        prolog = skip_prolog
        prolog.consult_string("""
            :- module(test_mod, [length/2]).
            length([], 0).
            length([_|T], N) :- length(T, N1), N is N1 + 1.
        """)
        # Built-in length should still work
        result = prolog.query_once("length([a, b, c], L)")
        assert result["L"] == 3

    def test_error_mode_fails_on_predicate_redefinition(self, error_prolog):
        """Error mode should fail on predicate redefinition."""
        prolog = error_prolog
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- module(test_mod, [length/2]).
                length([], 0).
            """)
        error_str = str(exc_info.value)
        assert "permission_error" in error_str

    def test_shadow_mode_allows_predicate_shadowing(self, shadow_prolog):
        """Shadow mode allows predicate shadowing."""
        prolog = shadow_prolog
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
