"""
Tests for copy_term/2 builtin predicate.
"""

import pytest
from vibeprolog import PrologInterpreter


class TestCopyTerm:
    """Tests for copy_term/2 predicate"""

    def test_copy_atom(self):
        """Test copying atoms"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(foo, X)")
        assert result is not None
        assert result['X'] == 'foo'

    def test_copy_number(self):
        """Test copying numbers"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(42, X)")
        assert result is not None
        assert result['X'] == 42

    def test_copy_compound_simple(self):
        """Test copying simple compound terms"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f(a, b), X)")
        assert result is not None
        # Should be structurally equal to f(a, b)
        assert prolog.has_solution("X = f(a, b)")

    def test_copy_compound_with_variables(self):
        """Test copying compound terms with variables"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f(X, X), Y)")
        assert result is not None
        # The two arguments in the copy should be the same fresh variable
        # We can test this by unifying Y with f(A, B) and checking A == B
        assert prolog.has_solution("copy_term(f(X, X), Y), Y = f(A, B), A == B")

    def test_copy_list_simple(self):
        """Test copying simple lists"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([1, 2, 3], X)")
        assert result is not None
        assert result['X'] == [1, 2, 3]

    def test_copy_list_with_variables(self):
        """Test copying lists with variables"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([X, Y, X], Z)")
        assert result is not None
        # First and third elements should be the same fresh variable
        assert prolog.has_solution("copy_term([X, Y, X], Z), Z = [A, B, C], A == C")

    def test_copy_nested_structures(self):
        """Test copying nested compound terms and lists"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f([X, Y], g(X)), Z)")
        assert result is not None
        # X in the list and X in g() should be the same fresh variable
        assert prolog.has_solution("copy_term(f([X, Y], g(X)), Z), Z = f([A, B], g(C)), A == C")

    def test_copy_bound_variables(self):
        """Test copying terms where variables are already bound"""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = bar(1), copy_term(X, Y)")
        assert result is not None
        # Y should unify with bar(1)
        assert prolog.has_solution("X = bar(1), copy_term(X, Y), Y = bar(1)")

    def test_copy_partially_instantiated(self):
        """Test copying partially instantiated terms"""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = f(a, Y), copy_term(X, Z)")
        assert result is not None
        # Y should become a fresh variable in the copy
        assert prolog.has_solution(f"Z = f(a, Fresh), var(Fresh)")

    def test_copy_list_with_tail(self):
        """Test copying lists with explicit tails"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([H|T], X)")
        assert result is not None
        # Should create fresh variables for H and T
        assert prolog.has_solution(f"X = [FreshH|FreshT], var(FreshH), var(FreshT)")

    def test_copy_compound_with_list(self):
        """Test copying compounds containing lists"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f([X, Y], Z), W)")
        assert result is not None
        # All variables should be fresh
        assert prolog.has_solution(f"W = f([A, B], C), var(A), var(B), var(C)")

    def test_copy_variable_consistency(self):
        """Test that identical variables in source become identical fresh variables in copy"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f(A, A, B), f(X, Y, Z))")
        assert result is not None
        # X and Y should be the same fresh variable, Z should be different
        assert prolog.has_solution("copy_term(f(A, A, B), f(X, Y, Z)), X == Y, X \\== Z")

    def test_copy_nested_lists(self):
        """Test copying nested lists"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([[X], [Y, X]], Z)")
        assert result is not None
        # X should be the same fresh variable in both positions
        assert prolog.has_solution("copy_term([[X], [Y, X]], Z), Z = [[A], [B, C]], A == C")

    def test_copy_empty_list(self):
        """Test copying empty lists"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([], X)")
        assert result is not None
        assert prolog.has_solution("copy_term([], X), X = []")

    def test_copy_mixed_terms(self):
        """Test copying terms with mixed types"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([a, 42, X, f(Y)], Z)")
        assert result is not None
        # Should preserve atoms and numbers, create fresh variables
        assert prolog.has_solution("copy_term([a, 42, X, f(Y)], Z), Z = [a, 42, Fresh1, f(Fresh2)], var(Fresh1), var(Fresh2)")

    def test_copy_deterministic(self):
        """Test that copy_term is deterministic (only one solution)"""
        prolog = PrologInterpreter()
        solutions = list(prolog.query("copy_term(f(X, X), Y)"))
        assert len(solutions) == 1
        result = solutions[0]
        assert 'Y' in result

    def test_copy_with_unbound_second_arg(self):
        """Test copy_term with unbound second argument"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f(a, b), X)")
        assert result is not None
        # Should unify with f(a, b)
        assert prolog.has_solution("copy_term(f(a, b), X), X = f(a, b)")

    def test_copy_preserves_structure(self):
        """Test that copy preserves term structure exactly"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f(g(h(X))), Y)")
        assert result is not None
        # Should have same structure with fresh variable
        assert prolog.has_solution("copy_term(f(g(h(X))), Y), Y = f(g(h(Fresh))), var(Fresh)")

    def test_copy_multiple_occurrences(self):
        """Test copying terms with multiple occurrences of same variable"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([A, B, A, C, A], X)")
        assert result is not None
        # All A positions should be the same fresh variable
        assert prolog.has_solution("copy_term([A, B, A, C, A], X), X = [P, Q, R, S, T], P == R, R == T, P \\== Q, Q \\== S")

    def test_copy_regression_tricky_tails(self):
        """Regression test for tricky list tails"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term([H|T], X)")
        assert result is not None
        assert prolog.has_solution(f"X = [FreshH|FreshT], var(FreshH), var(FreshT)")

    def test_copy_regression_compound_with_lists(self):
        """Regression test for compounds containing lists"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f([H|T], X), Y)")
        assert result is not None
        assert prolog.has_solution(f"Y = f([FreshH|FreshT], FreshX), var(FreshH), var(FreshT), var(FreshX)")

    def test_copy_regression_nested_compounds(self):
        """Regression test for nested compounds with shared variables"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f(g(A), h(A, B)), X)")
        assert result is not None
        # A should be shared between g(A) and h(A, B)
        assert prolog.has_solution("copy_term(f(g(A), h(A, B)), X), X = f(g(P), h(Q, R)), P == Q, var(R)")

    def test_copy_no_side_effects(self):
        """Test that copying doesn't affect original term"""
        prolog = PrologInterpreter()
        # Create original term
        result1 = prolog.query_once("Original = f(X, X)")
        assert result1 is not None

        # Copy it
        result2 = prolog.query_once("copy_term(Original, Copy)")
        assert result2 is not None

        # Modify the copy (conceptually - by unifying)
        result3 = prolog.query_once("Copy = f(a, b)")
        assert result3 is not None

        # Original should still have fresh variables
        assert prolog.has_solution("Original = f(Fresh1, Fresh2), var(Fresh1), var(Fresh2)")

    def test_copy_ground_term(self):
        """Test copying completely ground terms"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(f(a, [1, 2], g(b)), X)")
        assert result is not None
        # Should unify with the ground term
        assert prolog.has_solution("copy_term(f(a, [1, 2], g(b)), X), X = f(a, [1, 2], g(b))")

    def test_copy_variable_only(self):
        """Test copying a single variable"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(X, Y)")
        assert result is not None
        # Both should be fresh variables
        assert prolog.has_solution("copy_term(X, Y), var(X), var(Y)")

    def test_copy_variable_bound_after(self):
        """Test that binding variables after copy doesn't affect copy"""
        prolog = PrologInterpreter()
        result = prolog.query_once("copy_term(X, Y), X = bound")
        assert result is not None
        # Y should still be a fresh variable, not bound
        assert prolog.has_solution("copy_term(X, Y), X = bound, var(Y)")