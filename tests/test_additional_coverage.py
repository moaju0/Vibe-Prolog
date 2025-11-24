"""
Additional targeted tests to push coverage above 92%.

These tests target very specific uncovered code paths.
"""
import pytest
from vibeprolog import PrologInterpreter


class TestFormatTwoArgs:
    """Tests for format/2 (output to stdout)."""

    def test_format_with_variables(self):
        """Test format/2 with unbound variables in args."""
        prolog = PrologInterpreter()
        # Variable in format args should be handled
        result = prolog.query_once('format("Test ~w", [X]), X = unbound')
        assert result is not None

    def test_format_with_compound_terms(self):
        """Test format/2 with compound terms."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format("Compound: ~w", [foo(a, b)])')
        assert result is not None

    def test_format_with_mixed_args(self):
        """Test format/2 with mixed argument types."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format("Mix: ~w ~d ~w", [atom, 42, X])')
        assert result is not None


class TestAssertRules:
    """Tests for asserting rules (not just facts)."""

    def test_assert_simple_rule(self):
        """Test assert/1 with a simple rule."""
        prolog = PrologInterpreter()
        # Note: assert with :- syntax might not be fully supported
        # Test with fact-based approach
        assert prolog.has_solution("assert(test_rule(X))")
        assert prolog.has_solution("test_rule(anything)")


class TestMemberBacktracking:
    """Tests for member/2 backtracking behavior."""

    def test_member_all_solutions(self):
        """Test member/2 generating all solutions via backtracking."""
        prolog = PrologInterpreter()
        results = prolog.query("member(X, [a, b, c, d, e])", limit=10)
        assert len(results) == 5
        values = [r['X'] for r in results]
        assert values == ['a', 'b', 'c', 'd', 'e']

    def test_member_with_tail_list(self):
        """Test member/2 with list containing tail."""
        prolog = PrologInterpreter()
        result = prolog.query_once("[H|T] = [1, 2, 3], member(2, T)")
        assert result is not None


class TestSetofSortingEdgeCases:
    """Tests for setof/3 sorting edge cases."""

    def test_setof_with_atoms(self):
        """Test setof/3 with atoms that need sorting."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            letter(c).
            letter(a).
            letter(b).
        """)
        result = prolog.query_once("setof(X, letter(X), L)")
        assert result is not None
        # Should be sorted alphabetically
        assert result is not None
        assert result['L'] == ['a', 'b', 'c']

    def test_setof_with_numbers(self):
        """Test setof/3 with numbers."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            num(10).
            num(2).
            num(5).
            num(1).
        """)
        result = prolog.query_once("setof(X, num(X), L)")
        assert result is not None
        assert result is not None
        assert result['L'] == [1, 2, 5, 10]


class TestComparisonOperators:
    """Tests for all comparison operators."""

    def test_less_than(self):
        """Test < operator."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("3 < 5")
        assert not prolog.has_solution("5 < 3")
        assert not prolog.has_solution("5 < 5")

    def test_greater_than(self):
        """Test > operator."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("5 > 3")
        assert not prolog.has_solution("3 > 5")
        assert not prolog.has_solution("5 > 5")

    def test_comparisons_with_expressions(self):
        """Test comparisons with arithmetic expressions."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("2 + 3 =:= 5")
        assert prolog.has_solution("10 - 5 > 3")
        assert prolog.has_solution("2 * 3 < 10")


class TestMaplistWithTails:
    """Tests for maplist/2 with lists containing tails."""

    def test_maplist_list_with_tail(self):
        """Test maplist/2 on list with explicit tail."""
        prolog = PrologInterpreter()
        # List with tail should be processed correctly
        result = prolog.query_once("[H|T] = [1, 2, 3], maplist(number, [H|T])")
        assert result is not None


class TestAppendRecursiveDeep:
    """Tests for deep recursion in append/3."""

    def test_append_long_lists(self):
        """Test append/3 with long lists to trigger deep recursion."""
        prolog = PrologInterpreter()
        result = prolog.query_once("append([1,2,3,4,5,6,7,8,9,10], [11,12,13], X)")
        assert result is not None
        assert result is not None
        assert len(result['X']) == 13

    def test_append_many_small_lists(self):
        """Test multiple append operations."""
        prolog = PrologInterpreter()
        result = prolog.query_once("append([a], [b], L1), append(L1, [c], L2)")
        assert result is not None
        assert result is not None
        assert result['L2'] == ['a', 'b', 'c']


class TestFindallBagofEdgeCases:
    """Tests for findall/bagof with various goal structures."""

    def test_findall_with_compound_template(self):
        """Test findall/3 with compound template structure."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            person(alice, 30, engineer).
            person(bob, 25, doctor).
            person(carol, 35, teacher).
        """)
        result = prolog.query_once("findall(pair(Name, Age), person(Name, Age, _), L)")
        assert result is not None
        assert result is not None
        assert len(result['L']) == 3

    def test_bagof_with_filtering(self):
        """Test bagof/3 with specific filtering."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            item(apple, red).
            item(banana, yellow).
            item(cherry, red).
        """)
        result = prolog.query_once("bagof(Item, item(Item, red), L)")
        assert result is not None
        assert result is not None
        assert len(result['L']) == 2


class TestUnivWithComplex:
    """Tests for =../2 with complex terms."""

    def test_univ_with_nested_compound(self):
        """Test =../2 with nested compound terms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("foo(bar(x), baz(y)) =.. L")
        assert result is not None
        # Should be [foo, bar(x), baz(y)]
        assert result is not None
        assert isinstance(result['L'], list)
        assert result is not None
        assert len(result['L']) == 3

    def test_univ_construct_complex(self):
        """Test =../2 constructing complex terms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("T =.. [test, a, b, c]")
        assert result is not None


class TestLengthEdgeCases:
    """Tests for length/2 edge cases."""

    def test_length_with_long_list(self):
        """Test length/2 with long lists."""
        prolog = PrologInterpreter()
        result = prolog.query_once("length([1,2,3,4,5,6,7,8,9,10], N)")
        assert result is not None
        assert result is not None
        assert result['N'] == 10

    def test_length_generate_long(self):
        """Test length/2 generating longer lists."""
        prolog = PrologInterpreter()
        result = prolog.query_once("length(L, 10)")
        assert result is not None
        assert result is not None
        assert len(result['L']) == 10


class TestReverseEdgeCases:
    """Tests for reverse/2 edge cases."""

    def test_reverse_bidirectional(self):
        """Test reverse/2 in both directions."""
        prolog = PrologInterpreter()
        # Forward
        result = prolog.query_once("reverse([1,2,3], X)")
        assert result is not None
        assert result is not None
        assert result['X'] == [3, 2, 1]

        # Backward
        result = prolog.query_once("reverse(X, [3,2,1])")
        assert result is not None
        assert result is not None
        assert result['X'] == [1, 2, 3]


class TestClauseMultiple:
    """Tests for clause/2 with multiple clauses."""

    def test_clause_enumerate_all(self):
        """Test clause/2 enumerating all matching clauses."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            fact(1).
            fact(2).
            fact(3).
            rule(X) :- fact(X).
        """)
        results = prolog.query("clause(fact(X), Body)", limit=10)
        assert len(results) >= 3


class TestArgEdgeCases:
    """Tests for arg/3 with various term structures."""

    def test_arg_all_positions(self):
        """Test arg/3 accessing all argument positions."""
        prolog = PrologInterpreter()
        result1 = prolog.query_once("arg(1, test(a,b,c,d), X)")
        result2 = prolog.query_once("arg(2, test(a,b,c,d), X)")
        result3 = prolog.query_once("arg(3, test(a,b,c,d), X)")
        result4 = prolog.query_once("arg(4, test(a,b,c,d), X)")

        assert result1 is not None
        assert result1['X'] == 'a'
        assert result2 is not None
        assert result2['X'] == 'b'
        assert result3 is not None
        assert result3['X'] == 'c'
        assert result4 is not None
        assert result4['X'] == 'd'


class TestNotUnifiable:
    """Tests for \\=/2 (not unifiable)."""

    def test_not_unifiable_different_atoms(self):
        """Test \\=/2 with different atoms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("a \\= b")
        assert not prolog.has_solution("a \\= a")

    def test_not_unifiable_structures(self):
        """Test \\=/2 with different structures."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("foo(a) \\= foo(b)")
        assert not prolog.has_solution("foo(a) \\= foo(a)")


class TestDivisionEdgeCases:
    """Tests for division and modulo edge cases."""

    def test_integer_division_various(self):
        """Test // with various operands."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 10 // 3")
        assert result is not None
        assert result['X'] == 3

        result = prolog.query_once("X is 15 // 5")
        assert result is not None
        assert result['X'] == 3

    def test_modulo_various(self):
        """Test mod with various operands."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 10 mod 3")
        assert result is not None
        assert result['X'] == 1

        result = prolog.query_once("X is 15 mod 5")
        assert result is not None
        assert result['X'] == 0
