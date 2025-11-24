"""Tests for abolish/1 built-in predicate."""

import pytest
from vibeprolog import PrologInterpreter


class TestAbolishBasic:
    """Basic functionality tests for abolish/1."""

    def test_abolish_removes_all_clauses(self):
        """Test that abolish/1 removes all clauses for a predicate."""
        prolog = PrologInterpreter()

        # Add some clauses for foo/2 using consult_string
        prolog.consult_string("foo(a, b). foo(c, d). foo(X, Y) :- X = test.")

        # Verify clauses exist
        assert prolog.has_solution("foo(a, b)")
        assert prolog.has_solution("foo(c, d)")
        assert prolog.has_solution("foo(test, Y)")

        # Add unrelated predicate
        prolog.query_once("assert(bar(x)).")
        assert prolog.has_solution("bar(x)")

        # Abolish foo/2
        result = prolog.query_once("abolish(foo/2).")
        assert result == {}  # Should succeed

        # Verify foo/2 clauses are gone
        assert not prolog.has_solution("foo(a, b)")
        assert not prolog.has_solution("foo(c, d)")
        assert not prolog.has_solution("foo(test, Y)")

        # Verify unrelated predicate still works
        assert prolog.has_solution("bar(x)")

    def test_abolish_idempotent(self):
        """Test that abolish/1 succeeds even when predicate has no clauses."""
        prolog = PrologInterpreter()

        # Abolish non-existent predicate
        result = prolog.query_once("abolish(nonexistent/1).")
        assert result == {}  # Should succeed

        # Add a clause, abolish it, then abolish again
        prolog.query_once("assert(test_pred(item)).")
        assert prolog.has_solution("test_pred(item)")

        prolog.query_once("abolish(test_pred/1).")
        assert not prolog.has_solution("test_pred(item)")

        # Abolish again - should still succeed
        result = prolog.query_once("abolish(test_pred/1).")
        assert result == {}

    def test_abolish_zero_arity_predicate(self):
        """Test abolish/1 with zero-arity predicates."""
        prolog = PrologInterpreter()

        # Add zero-arity predicate
        prolog.query_once("assert(my_pred).")
        assert prolog.has_solution("my_pred")

        # Abolish it
        result = prolog.query_once("abolish(my_pred/0).")
        assert result == {}

        # Verify it's gone
        assert not prolog.has_solution("my_pred")


class TestAbolishCurrentPredicate:
    """Tests for abolish/1 interaction with current_predicate/1."""

    def test_current_predicate_after_abolish(self):
        """Test that abolished predicates no longer appear in current_predicate/1."""
        prolog = PrologInterpreter()

        # Add user-defined predicate
        prolog.query_once("assert(user_pred(a)).")
        prolog.query_once("assert(user_pred(b)).")

        # Verify it appears in current_predicate
        assert prolog.has_solution("current_predicate(user_pred/1)")

        # Abolish it
        prolog.query_once("abolish(user_pred/1).")

        # Verify it no longer appears
        assert not prolog.has_solution("current_predicate(user_pred/1)")

        # But built-ins should still appear
        assert prolog.has_solution("current_predicate(abolish/1)")
        assert prolog.has_solution("current_predicate(write/1)")


class TestAbolishClauseInteraction:
    """Tests for abolish/1 interaction with clause/2."""

    def test_clause_after_abolish(self):
        """Test that clause/2 yields no results after abolish/1."""
        prolog = PrologInterpreter()

        # Add clauses using consult_string
        prolog.consult_string("fact_pred. rule_pred(X) :- X = value.")

        # Verify clause/2 works
        assert prolog.has_solution("clause(fact_pred, true)")
        assert prolog.has_solution("clause(rule_pred(X), (X = value))")

        # Abolish predicates
        prolog.query_once("abolish(fact_pred/0).")
        prolog.query_once("abolish(rule_pred/1).")

        # Verify clause/2 yields no results
        assert not prolog.has_solution("clause(fact_pred, true)")
        assert not prolog.has_solution("clause(rule_pred(X), (X = value))")


class TestAbolishInputValidation:
    """Tests for input validation in abolish/1."""

    def test_abolish_invalid_indicator_format(self):
        """Test that malformed indicators fail."""
        prolog = PrologInterpreter()

        # Not a compound term
        assert not prolog.has_solution("abolish(foo)")

        # Wrong functor
        assert not prolog.has_solution("abolish(foo-bar)")

        # Non-atom name
        assert not prolog.has_solution("abolish(123/1)")

        # Float arity (must fail)
        assert not prolog.has_solution("abolish(foo/1.0)")

        # Non-number arity
        assert not prolog.has_solution("abolish(foo/bar)")

        # Negative arity
        assert not prolog.has_solution("abolish(foo/-1)")

        # Too many args in indicator
        assert not prolog.has_solution("abolish(foo/1/extra)")

    def test_abolish_builtins_protected(self):
        """Test that built-in predicates cannot be abolished."""
        prolog = PrologInterpreter()

        # These should succeed without effect (built-ins are protected)
        result = prolog.query_once("abolish(write/1).")
        assert result == {}

        result = prolog.query_once("abolish(abolish/1).")
        assert result == {}

        # Built-ins should still work
        assert prolog.has_solution("current_predicate(write/1)")
        assert prolog.has_solution("current_predicate(abolish/1)")


class TestAbolishBuiltinProperties:
    """Tests for abolish/1 built-in properties."""

    def test_abolish_builtin_property(self):
        """Test that abolish is recognized as a built-in predicate."""
        prolog = PrologInterpreter()
        result = prolog.query_once("predicate_property(abolish(_), built_in).")
        assert result == {}

    def test_abolish_deterministic(self):
        """Test that abolish is deterministic (single solution)."""
        prolog = PrologInterpreter()
        results = list(prolog.query("abolish(nonexistent/1)."))
        assert len(results) == 1
        assert results[0] == {}