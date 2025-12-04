"""Tests for library($project_atts) - residual goal projection for attributed variables."""

import pytest
from vibeprolog import PrologInterpreter


class TestLibraryLoading:
    """Tests for loading library($project_atts)."""

    def test_load_project_atts_library(self):
        """library($project_atts) should load without errors."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- use_module(library('$project_atts')).")
        assert True

    def test_load_iso_ext_library(self):
        """library(iso_ext) loads without $project_atts errors.
        
        Note: iso_ext.pl uses Scryer-specific directives (non_counted_backtracking)
        that are not supported, so this test is skipped until those are implemented.
        The key test is that $project_atts itself loads correctly.
        """
        pytest.skip("iso_ext.pl uses unsupported directives (non_counted_backtracking)")


class TestTermResidualGoals:
    """Tests for term_residual_goals/2."""

    def test_ground_term_returns_empty_goals(self):
        """A ground term should return an empty goals list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_residual_goals(foo(bar, 123), Goals)")
        assert result is not None
        assert result["Goals"] == []

    def test_unattributed_var_returns_empty_goals(self):
        """An unattributed variable should return an empty goals list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_residual_goals(X, Goals)")
        assert result is not None
        assert result["Goals"] == []

    def test_single_attributed_var_returns_goals(self):
        """A single attributed variable should return put_atts goals."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), term_residual_goals(X, Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) >= 1
        goal_str = str(goals[0])
        assert "put_atts" in goal_str or "color" in goal_str

    def test_multiple_attributes_on_var(self):
        """Multiple attributes on one variable should produce multiple goals."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), put_atts(X, +size(large)), "
            "term_residual_goals(X, Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) >= 2

    def test_nested_compound_with_attvar(self):
        """Attributed variables inside compound terms should be found."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +val(42)), term_residual_goals(foo(a, X, b), Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) >= 1

    def test_list_with_attvars(self):
        """Attributed variables inside lists should be found."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), put_atts(Y, +color(blue)), "
            "term_residual_goals([X, a, Y], Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) >= 2

    def test_same_var_multiple_times(self):
        """Same attributed variable appearing multiple times should only produce goals once."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), term_residual_goals(foo(X, X, X), Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) == 1

    def test_empty_list(self):
        """Empty list should return empty goals."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_residual_goals([], Goals)")
        assert result is not None
        assert result["Goals"] == []


class TestProjectAttributes:
    """Tests for project_attributes/2."""

    def test_project_with_no_attvars(self):
        """project_attributes with no attributed variables should succeed."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("project_attributes([], [])")

    def test_project_empty_query_vars(self):
        """project_attributes with empty query vars should succeed."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), project_attributes([], [X])"
        )
        assert result is not None

    def test_project_basic(self):
        """project_attributes should succeed with attributed variables."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), project_attributes([X], [X])"
        )
        assert result is not None


class TestCopyTerm3Integration:
    """Tests for copy_term/3 using $project_atts."""

    def test_copy_term3_unattributed_var(self):
        """copy_term/3 with unattributed variable returns empty goals."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- use_module(library(atts)).")
        result = prolog.query_once("copy_term(X, Y, Goals)")
        assert result is not None
        assert result["Goals"] == []

    def test_copy_term3_ground_term(self):
        """copy_term/3 with ground term returns empty goals."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- use_module(library(atts)).")
        result = prolog.query_once("copy_term(foo(bar), Copy, Goals)")
        assert result is not None
        assert result["Goals"] == []

    def test_copy_term3_attributed_var(self):
        """copy_term/3 with attributed variable returns goals."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- use_module(library(atts)).")
        result = prolog.query_once(
            "put_atts(X, +color(red)), copy_term(X, Copy, Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) >= 1


class TestEdgeCases:
    """Edge case tests for $project_atts predicates."""

    def test_deeply_nested_attvars(self):
        """Deeply nested attributed variables should be found."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +deep(value)), "
            "term_residual_goals(a(b(c(d(e(X))))), Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) >= 1

    def test_mixed_attvars_and_regular_vars(self):
        """Mixed attributed and regular variables should only produce goals for attvars."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(A, +attr(1)), "
            "term_residual_goals(foo(A, B, C), Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert isinstance(goals, list)
        assert len(goals) == 1

    def test_term_residual_goals_with_atom(self):
        """term_residual_goals with just an atom returns empty goals."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_residual_goals(hello, Goals)")
        assert result is not None
        assert result["Goals"] == []

    def test_term_residual_goals_with_number(self):
        """term_residual_goals with just a number returns empty goals."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_residual_goals(42, Goals)")
        assert result is not None
        assert result["Goals"] == []


class TestResidualGoalContent:
    """Tests verifying the content of residual goals."""

    def test_goal_contains_original_attribute(self):
        """The residual goal should contain the original attribute value."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +myattr(specialvalue)), term_residual_goals(X, Goals)"
        )
        assert result is not None
        goals = result["Goals"]
        assert len(goals) >= 1
        goal_str = str(goals[0])
        assert "specialvalue" in goal_str or "myattr" in goal_str

    def test_goals_reference_copied_variables(self):
        """Goals from copy_term/3 should reference the copied variable."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- use_module(library(atts)).")
        result = prolog.query_once(
            "put_atts(X, +color(red)), copy_term(X, Y, Goals)"
        )
        assert result is not None
        assert result["Y"] != result.get("X", object())
