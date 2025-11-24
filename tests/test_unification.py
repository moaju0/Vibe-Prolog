"""
Advanced unification tests.
Tests complex unification scenarios.
"""

import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.unification import vars
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Variable, Compound


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


class TestTermComparison:
    r"""Term comparison operators: ==, \==, @<, @=<, @>, @>="""

    def test_term_identity_equal(self):
        """Test ==/2 - term identity (structural equality)"""
        prolog = PrologInterpreter()

        # Atoms
        assert prolog.has_solution("a == a")
        assert not prolog.has_solution("a == b")

        # Numbers
        assert prolog.has_solution("42 == 42")
        assert prolog.has_solution("3.14 == 3.14")
        assert not prolog.has_solution("42 == 43")

        # Compound terms
        assert prolog.has_solution("f(a, b) == f(a, b)")
        assert not prolog.has_solution("f(a, b) == f(a, c)")
        assert not prolog.has_solution("f(a, b) == g(a, b)")

        # Lists
        assert prolog.has_solution("[] == []")
        assert prolog.has_solution("[1, 2, 3] == [1, 2, 3]")
        assert not prolog.has_solution("[1, 2] == [1, 2, 3]")
        assert not prolog.has_solution("[1, 2, 3] == [1, 3, 2]")

        # Variables (unbound are equal to themselves)
        assert prolog.has_solution("X == X")

    def test_term_identity_not_equal(self):
        r"""Test \==/2 - term non-identity"""
        prolog = PrologInterpreter()

        # Atoms
        assert not prolog.has_solution("a \\== a")
        assert prolog.has_solution("a \\== b")

        # Numbers
        assert not prolog.has_solution("42 \\== 42")
        assert prolog.has_solution("42 \\== 43")

        # Compound terms
        assert not prolog.has_solution("f(a, b) \\== f(a, b)")
        assert prolog.has_solution("f(a, b) \\== f(a, c)")
        assert prolog.has_solution("f(a, b) \\== g(a, b)")

        # Lists
        assert not prolog.has_solution("[] \\== []")
        assert not prolog.has_solution("[1, 2, 3] \\== [1, 2, 3]")
        assert prolog.has_solution("[1, 2] \\== [1, 2, 3]")
        assert prolog.has_solution("[1, 2, 3] \\== [1, 3, 2]")

        # Variables
        assert not prolog.has_solution("X \\== X")

    def test_term_less_than(self):
        """Test @</2 - term less than"""
        prolog = PrologInterpreter()

        # Atoms (lexicographic order)
        assert prolog.has_solution("a @< b")
        assert not prolog.has_solution("b @< a")
        assert not prolog.has_solution("a @< a")

        # Numbers
        assert prolog.has_solution("1 @< 2")
        assert prolog.has_solution("1.5 @< 2.0")
        assert not prolog.has_solution("2 @< 1")
        assert not prolog.has_solution("2 @< 2")

        # Mixed types (Variables < Numbers < Atoms < Compounds)
        assert prolog.has_solution("X @< 1")
        assert prolog.has_solution("1 @< a")
        assert prolog.has_solution("a @< f(x)")

        # Compound terms
        assert prolog.has_solution("f(a) @< f(b)")
        assert prolog.has_solution("f(a, b) @< f(a, c)")
        assert prolog.has_solution("f(a) @< g(a)")  # Different functors

        # Lists
        assert prolog.has_solution("[] @< [a]")
        assert prolog.has_solution("[a] @< [b]")
        assert prolog.has_solution("[a, b] @< [a, c]")

    def test_term_less_equal(self):
        """Test @=</2 - term less than or equal"""
        prolog = PrologInterpreter()

        # Atoms
        assert prolog.has_solution("a @=< b")
        assert prolog.has_solution("a @=< a")
        assert not prolog.has_solution("b @=< a")

        # Numbers
        assert prolog.has_solution("1 @=< 2")
        assert prolog.has_solution("2 @=< 2")
        assert not prolog.has_solution("2 @=< 1")

        # Mixed types
        assert prolog.has_solution("X @=< 1")
        assert prolog.has_solution("1 @=< a")
        assert prolog.has_solution("a @=< f(x)")
        assert prolog.has_solution("X @=< X")  # Variables equal to themselves

        # Compound terms
        assert prolog.has_solution("f(a) @=< f(b)")
        assert prolog.has_solution("f(a) @=< f(a)")
        assert prolog.has_solution("f(a) @=< g(a)")

        # Lists
        assert prolog.has_solution("[] @=< [a]")
        assert prolog.has_solution("[a] @=< [a]")
        assert prolog.has_solution("[a, b] @=< [a, c]")

    def test_term_greater_than(self):
        """Test @>/2 - term greater than"""
        prolog = PrologInterpreter()

        # Atoms
        assert prolog.has_solution("b @> a")
        assert not prolog.has_solution("a @> b")
        assert not prolog.has_solution("a @> a")

        # Numbers
        assert prolog.has_solution("2 @> 1")
        assert not prolog.has_solution("1 @> 2")
        assert not prolog.has_solution("2 @> 2")

        # Mixed types
        assert prolog.has_solution("a @> 1")
        assert prolog.has_solution("f(x) @> a")

        # Compound terms
        assert prolog.has_solution("f(b) @> f(a)")
        assert prolog.has_solution("g(a) @> f(a)")

        # Lists
        assert prolog.has_solution("[a] @> []")
        assert prolog.has_solution("[b] @> [a]")

    def test_term_greater_equal(self):
        """Test @>=/2 - term greater than or equal"""
        prolog = PrologInterpreter()

        # Atoms
        assert prolog.has_solution("b @>= a")
        assert prolog.has_solution("a @>= a")
        assert not prolog.has_solution("a @>= b")

        # Numbers
        assert prolog.has_solution("2 @>= 1")
        assert prolog.has_solution("2 @>= 2")
        assert not prolog.has_solution("1 @>= 2")

        # Mixed types
        assert prolog.has_solution("a @>= 1")
        assert prolog.has_solution("f(x) @>= a")
        assert prolog.has_solution("X @>= X")

        # Compound terms
        assert prolog.has_solution("f(b) @>= f(a)")
        assert prolog.has_solution("f(a) @>= f(a)")
        assert prolog.has_solution("g(a) @>= f(a)")

        # Lists
        assert prolog.has_solution("[a] @>= []")
        assert prolog.has_solution("[a] @>= [a]")

    def test_term_comparison_with_variables(self):
        """Test term comparisons involving variables"""
        prolog = PrologInterpreter()

        # Variables are equal to themselves
        assert prolog.has_solution("X == X")
        assert not prolog.has_solution("X \\== X")
        assert not prolog.has_solution("X @< X")
        assert prolog.has_solution("X @=< X")
        assert not prolog.has_solution("X @> X")
        assert prolog.has_solution("X @>= X")

        # Variables vs constants
        assert prolog.has_solution("X @< a")
        assert prolog.has_solution("a @> X")
        assert prolog.has_solution("X @=< a")
        assert prolog.has_solution("a @>= X")

    def test_term_comparison_complex_terms(self):
        """Test term comparisons with complex nested terms"""
        prolog = PrologInterpreter()

        # Nested compounds
        assert prolog.has_solution("f(g(a)) @< f(g(b))")
        assert prolog.has_solution("f(a, b) @< f(a, c)")
        assert prolog.has_solution("f(a, b) @< f(b, a)")  # Lexicographic on args

        # Lists with compounds
        assert prolog.has_solution("[f(a)] @< [f(b)]")
        assert prolog.has_solution("[a, b] @< [a, b, c]")

        # Mixed structures
        assert prolog.has_solution("f(a) @< [a]")
class TestVarsFunction:
    """Tests for the vars() function in unification.py"""

    def test_vars_empty_term(self):
        term = Atom('a')
        result = vars(term)
        assert result == set()

    def test_vars_single_variable(self):
        var = Variable('X')
        result = vars(var)
        assert result == {var}

    def test_vars_compound_term(self):
        var_x = Variable('X')
        var_y = Variable('Y')
        term = Compound('f', [var_x, Atom('a'), var_y])
        result = vars(term)
        assert result == {var_x, var_y}

    def test_vars_list(self):
        var_x = Variable('X')
        var_y = Variable('Y')
        term = List([var_x, Atom('b'), var_y], None)
        result = vars(term)
        assert result == {var_x, var_y}

    def test_vars_nested_compound(self):
        var_x = Variable('X')
        var_z = Variable('Z')
        inner = Compound('g', [var_x])
        term = Compound('f', [inner, var_z])
        result = vars(term)
        assert result == {var_x, var_z}

    def test_vars_with_tail(self):
        var_x = Variable('X')
        var_y = Variable('Y')
        tail = List([var_y], None)
        term = List([Atom('a'), var_x], tail)
        result = vars(term)
        assert result == {var_x, var_y}
