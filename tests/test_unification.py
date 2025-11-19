"""
Advanced unification tests.
Tests complex unification scenarios.
"""

import pytest
from prolog import PrologInterpreter


class TestBasicUnification:
    """Basic unification tests"""

    def test_atom_unification(self):
        prolog = PrologInterpreter()
        assert prolog.has_solution("a = a")
        assert not prolog.has_solution("a = b")

    def test_number_unification(self):
        prolog = PrologInterpreter()
        assert prolog.has_solution("42 = 42")
        assert prolog.has_solution("3.14 = 3.14")
        assert not prolog.has_solution("42 = 43")

    def test_variable_unification(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("X = a")
        assert result is not None
        assert result['X'] == 'a'

        result = prolog.query_once("X = Y, Y = 5")
        assert result is not None
        assert result['X'] == 5
        assert result is not None
        assert result['Y'] == 5


class TestCompoundUnification:
    """Unification with compound terms"""

    def test_simple_compound(self):
        prolog = PrologInterpreter()
        assert prolog.has_solution("f(a) = f(a)")
        assert not prolog.has_solution("f(a) = f(b)")
        assert not prolog.has_solution("f(a) = g(a)")

    def test_compound_with_variables(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("f(X, b) = f(a, Y)")
        assert result is not None
        assert result['X'] == 'a'
        assert result is not None
        assert result['Y'] == 'b'

    def test_nested_compound(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("f(g(X), Y) = f(g(a), b)")
        assert result is not None
        assert result['X'] == 'a'
        assert result is not None
        assert result['Y'] == 'b'

        result = prolog.query_once("f(X, g(Y, Z)) = f(a, g(b, c))")
        assert result is not None
        assert result['X'] == 'a'
        assert result is not None
        assert result['Y'] == 'b'
        assert result is not None
        assert result['Z'] == 'c'

    def test_deep_nesting(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("f(g(h(X))) = f(g(h(42)))")
        assert result is not None
        assert result['X'] == 42


class TestListUnification:
    """Unification with lists"""

    def test_empty_list(self):
        prolog = PrologInterpreter()
        assert prolog.has_solution("[] = []")
        assert not prolog.has_solution("[] = [a]")

    def test_simple_list(self):
        prolog = PrologInterpreter()
        assert prolog.has_solution("[a, b, c] = [a, b, c]")
        assert not prolog.has_solution("[a, b] = [a, b, c]")

    def test_list_with_variables(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("[X, Y, Z] = [1, 2, 3]")
        assert result is not None
        assert result['X'] == 1
        assert result is not None
        assert result['Y'] == 2
        assert result is not None
        assert result['Z'] == 3

    def test_list_head_tail(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("[H|T] = [1, 2, 3]")
        assert result is not None
        assert result['H'] == 1
        assert result is not None
        assert result['T'] == [2, 3]

        result = prolog.query_once("[H1, H2|T] = [a, b, c, d]")
        assert result is not None
        assert result['H1'] == 'a'
        assert result is not None
        assert result['H2'] == 'b'
        assert result is not None
        assert result['T'] == ['c', 'd']

    def test_list_construction(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("L = [a|[b, c]]")
        assert result is not None
        assert result['L'] == ['a', 'b', 'c']

        result = prolog.query_once("L = [1, 2|[3, 4]]")
        assert result is not None
        assert result['L'] == [1, 2, 3, 4]


class TestUnificationChains:
    """Chained unifications"""

    def test_simple_chain(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("X = Y, Y = Z, Z = 42")
        assert result is not None
        assert result['X'] == 42
        assert result is not None
        assert result['Y'] == 42
        assert result is not None
        assert result['Z'] == 42

    def test_compound_chain(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("f(X) = Y, Y = f(a)")
        assert result is not None
        assert result['X'] == 'a'

    def test_list_chain(self):
        prolog = PrologInterpreter()

        # Test list unification chain
        result = prolog.query_once("L = [1, 2, 3], [H|T] = L")
        assert result is not None
        assert result['H'] == 1
        assert result is not None
        assert result['T'] == [2, 3]
        assert result is not None
        assert result['L'] == [1, 2, 3]


class TestUnificationWithFunctor:
    """Using functor/3 for unification"""

    def test_functor_decomposition(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("functor(foo(a, b, c), F, N)")
        assert result is not None
        assert result['F'] == 'foo'
        assert result is not None
        assert result['N'] == 3

    def test_functor_construction(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("functor(T, foo, 2)")
        assert result is not None
        assert 'T' in result

    def test_functor_atom(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("functor(hello, F, N)")
        assert result is not None
        assert result['F'] == 'hello'
        assert result is not None
        assert result['N'] == 0


class TestUnificationWithUniv:
    """Using =../2 (univ) for unification"""

    def test_univ_decompose(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("foo(a, b, c) =.. L")
        assert result is not None
        assert result['L'] == ['foo', 'a', 'b', 'c']

    def test_univ_compose(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("T =.. [foo, a, b]")
        assert result is not None

    def test_univ_with_atom(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("atom =.. L")
        assert result is not None
        assert result['L'] == ['atom']

    def test_univ_bidirectional(self):
        prolog = PrologInterpreter()

        assert prolog.has_solution("f(a, b) =.. [f, a, b]")
        assert prolog.has_solution("foo(x, y, z) =.. [foo, x, y, z]")


class TestPartialUnification:
    """Partial unification scenarios"""

    def test_partial_structure(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("f(X, b) = f(a, b)")
        assert result is not None
        assert result['X'] == 'a'

        result = prolog.query_once("f(a, Y) = f(a, b)")
        assert result is not None
        assert result['Y'] == 'b'

    def test_partial_list(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("[1, X, 3] = [1, 2, 3]")
        assert result is not None
        assert result['X'] == 2

        result = prolog.query_once("[H|[2, 3]] = [1, 2, 3]")
        assert result is not None
        assert result['H'] == 1


class TestUnificationFailures:
    """Cases where unification should fail"""

    def test_different_functors(self):
        prolog = PrologInterpreter()
        assert not prolog.has_solution("f(a) = g(a)")

    def test_different_arity(self):
        prolog = PrologInterpreter()
        assert not prolog.has_solution("f(a) = f(a, b)")

    def test_different_lists(self):
        prolog = PrologInterpreter()
        assert not prolog.has_solution("[a, b] = [a, b, c]")

    def test_atom_vs_compound(self):
        prolog = PrologInterpreter()
        assert not prolog.has_solution("a = f(a)")

    def test_number_mismatch(self):
        prolog = PrologInterpreter()
        assert not prolog.has_solution("1 = 2")


class TestComplexUnificationPatterns:
    """Complex unification patterns"""

    def test_symmetric_unification(self):
        prolog = PrologInterpreter()

        # f(X, Y) = f(Y, X) should unify with X = Y
        result = prolog.query_once("f(X, Y) = f(Y, X), X = a")
        assert result is not None
        assert result['X'] == 'a'
        assert result is not None
        assert result['Y'] == 'a'

    def test_nested_variables(self):
        prolog = PrologInterpreter()

        result = prolog.query_once("f(g(X, Y), h(Y, Z)) = f(g(1, 2), h(2, 3))")
        assert result is not None
        assert result['X'] == 1
        assert result is not None
        assert result['Y'] == 2
        assert result is not None
        assert result['Z'] == 3

    def test_list_pattern_matching(self):
        prolog = PrologInterpreter()

        # Pattern: [[X, Y], Z]
        result = prolog.query_once("[[X, Y], Z] = [[1, 2], 3]")
        assert result is not None
        assert result['X'] == 1
        assert result is not None
        assert result['Y'] == 2
        assert result is not None
        assert result['Z'] == 3
