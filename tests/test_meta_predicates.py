"""Tests for meta-predicates and term utilities."""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestForall:
    """Tests for forall/2."""

    def test_forall_succeeds_when_all_actions_true(self):
        """All solutions of Condition satisfy Action."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("forall(member(X, [1,2,3]), X > 0)")

    def test_forall_fails_when_action_fails(self):
        """Fails if any solution of Condition makes Action fail."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("forall(member(X, [1,2]), X = 1)")

    def test_forall_vacuous_truth(self):
        """Vacuously true when Condition has no solutions."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("forall(member(X, []), fail)")

    def test_forall_does_not_bind_outer_variables(self):
        """Bindings inside forall/2 do not leak."""
        prolog = PrologInterpreter()
        result = prolog.query_once("forall(member(X, [a,b]), true), var(X)")
        assert result is not None
        assert result["X"].startswith("_")

    def test_forall_raises_on_uninstantiated_goal(self):
        """Uninstantiated condition raises an instantiation error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.query_once("forall(Goal, true)")


class TestIgnore:
    """Tests for ignore/1."""

    def test_ignore_preserves_bindings_on_success(self):
        """Bindings from Goal propagate when it succeeds."""
        prolog = PrologInterpreter()
        result = prolog.query_once("ignore(member(X, [a,b])), X = a")
        assert result is not None
        assert result["X"] == "a"

    def test_ignore_succeeds_on_failure(self):
        """Failure of Goal still yields success."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("ignore(fail)")

    def test_ignore_raises_type_error_for_non_callable(self):
        """Non-callable goal triggers an error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.query_once("ignore(123)")

    def test_ignore_does_not_swallow_exceptions(self):
        """Exceptions from Goal propagate."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.query_once("ignore(throw(error(test_error)))")


class TestApply:
    """Tests for apply/2."""

    def test_apply_with_atom_functor(self):
        """Call atom predicate with supplied arguments."""
        prolog = PrologInterpreter()
        result = prolog.query_once("apply(append, [[1,2], [3], X])")
        assert result is not None
        assert result["X"] == [1, 2, 3]

    def test_apply_with_compound_functor(self):
        """Call compound predicate with existing arguments."""
        prolog = PrologInterpreter()
        result = prolog.query_once("apply(plus(2), [3, X])")
        assert result is not None
        assert result["X"] == 5

    def test_apply_supports_backtracking(self):
        """Backtracking over called predicate yields all solutions."""
        prolog = PrologInterpreter()
        results = prolog.query("apply(member(X), [[a, b]])")
        assert [res["X"] for res in results] == ["a", "b"]

    def test_apply_requires_list_arguments(self):
        """Second argument must be a proper list."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.query_once("apply(append, foo)")

    def test_apply_requires_callable_goal(self):
        """Non-callable goal triggers type error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.query_once("apply(123, [1])")


class TestTermVariables:
    """Tests for term_variables/2."""

    def test_collects_variables_in_order_without_duplicates(self):
        """Variables are returned once, in first-seen order."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_variables(foo(X, Y, X, Z), Vars)")
        assert result is not None
        assert result["Vars"] == ["_X", "_Y", "_Z"]

    def test_collects_variables_from_nested_structures(self):
        """Handles lists and nested terms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_variables([A, B, f(A, C)], Vars)")
        assert result is not None
        assert result["Vars"] == ["_A", "_B", "_C"]

    def test_ground_term_produces_empty_list(self):
        """Ground terms yield empty variable list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("term_variables(42, Vars)")
        assert result is not None
        assert result["Vars"] == []


class TestNumbervars:
    """Tests for numbervars/3."""

    def test_numbers_shared_variables_consistently(self):
        """Shared variables receive the same number."""
        prolog = PrologInterpreter()
        result = prolog.query_once("numbervars(foo(X, Y, X), 0, End)")
        assert result is not None
        assert result["X"] == {"$VAR": [0]}
        assert result["Y"] == {"$VAR": [1]}
        assert result["End"] == 2

    def test_numbers_with_offset(self):
        """Respects starting offset for numbering."""
        prolog = PrologInterpreter()
        result = prolog.query_once("numbervars(bar(A, B, C), 5, End)")
        assert result is not None
        assert result["A"] == {"$VAR": [5]}
        assert result["B"] == {"$VAR": [6]}
        assert result["C"] == {"$VAR": [7]}
        assert result["End"] == 8

    def test_numbervars_requires_integer_start(self):
        """Non-integer start raises a type error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.query_once("numbervars(Term, foo, End)")


class TestSubsumesTerm:
    """Tests for subsumes_term/2."""

    def test_subsumes_simple_case(self):
        """General term subsumes specific term."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("subsumes_term(f(X), f(a))")

    def test_subsumes_respects_shared_variables(self):
        """Fails when shared variable constraints do not match."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("subsumes_term(f(X, X), f(a, a))")
        assert not prolog.has_solution("subsumes_term(f(X, X), f(a, b))")

    def test_subsumes_non_subsuming_term(self):
        """Specific term should not subsume general one."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("subsumes_term(f(a), f(X))")

    def test_subsumes_does_not_bind_arguments(self):
        """Arguments remain unbound after subsumes_term/2."""
        prolog = PrologInterpreter()
        result = prolog.query_once("subsumes_term(f(X), f(a)), var(X)")
        assert result is not None
        assert result["X"].startswith("_")
