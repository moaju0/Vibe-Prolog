"""
Tests for ISO term manipulation relation predicates: compare/3, unify_with_occurs_check/2.
"""

import pytest
from vibeprolog import PrologInterpreter


class TestCompare:
    """Tests for compare/3 predicate."""

    def test_compare_atoms(self):
        """Test compare/3 with atoms."""
        prolog = PrologInterpreter()

        result = prolog.query_once("compare(Order, foo, bar)")
        assert result is not None
        assert result['Order'] == '>'

        result = prolog.query_once("compare(Order, bar, foo)")
        assert result is not None
        assert result['Order'] == '<'

        result = prolog.query_once("compare(Order, foo, foo)")
        assert result is not None
        assert result['Order'] == '='

    def test_compare_numbers(self):
        """Test compare/3 with numbers."""
        prolog = PrologInterpreter()

        result = prolog.query_once("compare(Order, 1, 2)")
        assert result is not None
        assert result['Order'] == '<'

        result = prolog.query_once("compare(Order, 2, 1)")
        assert result is not None
        assert result['Order'] == '>'

        result = prolog.query_once("compare(Order, 5, 5)")
        assert result is not None
        assert result['Order'] == '='

    def test_compare_compounds(self):
        """Test compare/3 with compound terms."""
        prolog = PrologInterpreter()

        result = prolog.query_once("compare(Order, f(a), f(b))")
        assert result is not None
        assert result['Order'] == '<'  # a < b

        result = prolog.query_once("compare(Order, f(b), f(a))")
        assert result is not None
        assert result['Order'] == '>'  # b > a

        result = prolog.query_once("compare(Order, f(a), f(a))")
        assert result is not None
        assert result['Order'] == '='

    def test_compare_mixed_types(self):
        """Test compare/3 with mixed term types (standard order: vars < nums < atoms < compounds)."""
        prolog = PrologInterpreter()

        # Variables come first
        result = prolog.query_once("compare(Order, X, 1)")
        assert result is not None
        assert result['Order'] == '<'

        # Numbers before atoms
        result = prolog.query_once("compare(Order, 1, foo)")
        assert result is not None
        assert result['Order'] == '<'

        # Atoms before compounds
        result = prolog.query_once("compare(Order, foo, f(a))")
        assert result is not None
        assert result['Order'] == '<'

    def test_compare_bound_order(self):
        """Test compare/3 with bound Order argument."""
        prolog = PrologInterpreter()

        assert prolog.has_solution("compare('<', 1, 2)")
        assert prolog.has_solution("compare('=', foo, foo)")
        assert prolog.has_solution("compare('>', foo, bar)")  # foo > bar

        assert not prolog.has_solution("compare('>', 1, 2)")
        assert not prolog.has_solution("compare('<', foo, bar)")  # foo > bar

    def test_compare_errors(self):
        """Test error conditions for compare/3."""
        prolog = PrologInterpreter()

        # Non-atom Order (when bound)
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("compare(123, foo, bar)")

        # Invalid order atom
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("compare(invalid, foo, bar)")


class TestUnifyWithOccursCheck:
    """Tests for unify_with_occurs_check/2 predicate."""

    def test_unify_with_occurs_check_normal(self):
        """Test unify_with_occurs_check/2 with normal unification."""
        prolog = PrologInterpreter()

        result = prolog.query_once("unify_with_occurs_check(X, f(a, b))")
        assert result is not None
        assert result['X'] == {'f': ['a', 'b']}

        result = prolog.query_once("unify_with_occurs_check(X, Y)")
        assert result is not None
        # X and Y should be unified

    def test_unify_with_occurs_check_occurs_fail(self):
        """Test unify_with_occurs_check/2 prevents cyclic structures."""
        prolog = PrologInterpreter()

        # This should fail due to occurs check
        result = prolog.query_once("unify_with_occurs_check(X, f(X))")
        assert result is None

        # More complex cycle
        result = prolog.query_once("unify_with_occurs_check(X, f(g(X)))")
        assert result is None

    def test_unify_with_occurs_check_vs_regular_unify(self):
        """Test that unify_with_occurs_check/2 behaves like =/2 for non-cyclic cases."""
        prolog = PrologInterpreter()

        # Both should succeed for non-cyclic unification
        assert prolog.has_solution("X = f(a)")
        assert prolog.has_solution("unify_with_occurs_check(X, f(a))")

        # Both should fail for cyclic unification
        assert not prolog.has_solution("X = f(X)")
        assert not prolog.has_solution("unify_with_occurs_check(X, f(X))")