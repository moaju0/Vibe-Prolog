"""Tests for built-in predicates in the Prolog engine."""

import pytest
from vibeprolog import PrologInterpreter


class TestUnification:
    """Tests for unification predicates (=/2, \\=/2)."""

    def test_unify_atoms(self):
        """Test unifying two atoms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("foo = foo")
        assert not prolog.has_solution("foo = bar")

    def test_unify_numbers(self):
        """Test unifying numbers."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("42 = 42")
        assert prolog.has_solution("3.14 = 3.14")
        assert not prolog.has_solution("42 = 43")

    def test_unify_variable_with_atom(self):
        """Test unifying a variable with an atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = foo")
        assert result is not None
        assert result is not None
        assert result['X'] == 'foo'

    def test_unify_variable_with_number(self):
        """Test unifying a variable with a number."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = 42")
        assert result is not None
        assert result is not None
        assert result['X'] == 42

    def test_unify_two_variables(self):
        """Test unifying two variables."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = Y, Y = foo")
        assert result is not None
        assert result is not None
        assert result['X'] == 'foo'
        assert result is not None
        assert result['Y'] == 'foo'

    def test_unify_compound_terms(self):
        """Test unifying compound terms."""
        prolog = PrologInterpreter()
        prolog.consult_string("person(john, 25).")
        assert prolog.has_solution("person(john, 25) = person(john, 25)")
        result = prolog.query_once("person(Name, Age) = person(john, 25)")
        assert result is not None
        assert result is not None
        assert result['Name'] == 'john'
        assert result is not None
        assert result['Age'] == 25

    def test_unify_lists(self):
        """Test unifying lists."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("[1, 2, 3] = [1, 2, 3]")
        result = prolog.query_once("[H|T] = [1, 2, 3]")
        assert result is not None
        assert result is not None
        assert result['H'] == 1
        assert result is not None
        assert result['T'] == [2, 3]

    def test_not_unifiable(self):
        """Test \\=/2 (not unifiable) predicate."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("foo \\= bar")
        assert not prolog.has_solution("foo \\= foo")
        assert prolog.has_solution("42 \\= 43")
        assert not prolog.has_solution("42 \\= 42")


class TestArithmetic:
    """Tests for arithmetic evaluation (is/2)."""

    def test_is_simple_number(self):
        """Test evaluating a simple number."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 42")
        assert result is not None
        assert result is not None
        assert result['X'] == 42

    def test_is_addition(self):
        """Test addition."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 2 + 3")
        assert result is not None
        assert result is not None
        assert result['X'] == 5

    def test_is_subtraction(self):
        """Test subtraction."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 10 - 3")
        assert result is not None
        assert result is not None
        assert result['X'] == 7

    def test_is_multiplication(self):
        """Test multiplication."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 4 * 5")
        assert result is not None
        assert result is not None
        assert result['X'] == 20

    def test_is_division(self):
        """Test division."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 15 / 3")
        assert result is not None
        assert result is not None
        assert result['X'] == 5.0

    def test_is_integer_division(self):
        """Test integer division."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 17 // 5")
        assert result is not None
        assert result is not None
        assert result['X'] == 3

    def test_is_modulo(self):
        """Test modulo operation."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 17 mod 5")
        assert result is not None
        assert result is not None
        assert result['X'] == 2

    def test_is_complex_expression(self):
        """Test complex arithmetic expression."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is (2 + 3) * 4")
        assert result is not None
        assert result is not None
        assert result['X'] == 20

    def test_is_with_variables(self):
        """Test arithmetic with variables."""
        prolog = PrologInterpreter()
        result = prolog.query_once("Y = 5, X is Y + 3")
        assert result is not None
        assert result is not None
        assert result['X'] == 8
        assert result is not None
        assert result['Y'] == 5

    def test_is_nested_operations(self):
        """Test nested operations."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 2 + 3 * 4")
        assert result is not None
        assert result is not None
        assert result['X'] == 14  # Multiplication has higher precedence


class TestArithmeticComparisons:
    """Tests for arithmetic comparison predicates."""

    def test_arithmetic_equal(self):
        """Test =:=/2 (arithmetic equality)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("5 =:= 5")
        assert prolog.has_solution("2 + 3 =:= 5")
        assert not prolog.has_solution("5 =:= 6")
        assert not prolog.has_solution("2 + 2 =:= 5")

    def test_less_than(self):
        """Test </2 (less than)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("3 < 5")
        assert prolog.has_solution("2 + 1 < 4")
        assert not prolog.has_solution("5 < 3")
        assert not prolog.has_solution("5 < 5")

    def test_greater_than(self):
        """Test >/2 (greater than)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("5 > 3")
        assert prolog.has_solution("10 > 2 + 3")
        assert not prolog.has_solution("3 > 5")
        assert not prolog.has_solution("5 > 5")

    def test_less_than_or_equal(self):
        """Test =</2 (less than or equal)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("3 =< 5")
        assert prolog.has_solution("5 =< 5")
        assert prolog.has_solution("2 + 1 =< 3")
        assert not prolog.has_solution("5 =< 3")

    def test_greater_than_or_equal(self):
        """Test >=/2 (greater than or equal)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("5 >= 3")
        assert prolog.has_solution("5 >= 5")
        assert prolog.has_solution("10 >= 2 + 3")
        assert not prolog.has_solution("3 >= 5")

    def test_comparison_with_variables(self):
        """Test comparisons with variables."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("X is 5, X =:= 5")
        assert prolog.has_solution("X is 3, Y is 5, X < Y")
        assert prolog.has_solution("X is 10, Y is 5, X > Y")


class TestListOperations:
    """Tests for list operations (member/2)."""

    def test_member_check_existence(self):
        """Test checking if element is in list."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("member(2, [1, 2, 3])")
        assert prolog.has_solution("member(1, [1, 2, 3])")
        assert prolog.has_solution("member(3, [1, 2, 3])")
        assert not prolog.has_solution("member(4, [1, 2, 3])")

    def test_member_find_elements(self):
        """Test finding all members of a list."""
        prolog = PrologInterpreter()
        results = prolog.query("member(X, [a, b, c])")
        assert len(results) == 3
        values = [r['X'] for r in results]
        assert 'a' in values
        assert 'b' in values
        assert 'c' in values

    def test_member_with_variable_list(self):
        """Test member with a variable in the list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("member(2, [1, X, 3]), X = 2")
        assert result is not None
        assert result is not None
        assert result['X'] == 2

    def test_member_empty_list(self):
        """Test member on empty list."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("member(X, [])")

    def test_member_single_element(self):
        """Test member with single element list."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("member(1, [1])")
        assert not prolog.has_solution("member(2, [1])")

    def test_member_with_duplicates(self):
        """Test member with duplicate elements."""
        prolog = PrologInterpreter()
        results = prolog.query("member(a, [a, b, a, c])")
        assert len(results) == 2  # Should find 'a' twice


class TestFormat:
    """Tests for format/3 predicate."""

    def test_format_simple_string(self):
        """Test formatting a simple string."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "Hello", [])')
        assert result is not None
        assert result is not None
        assert result['X'] == "Hello"

    def test_format_with_w_placeholder(self):
        """Test ~w placeholder (write any term)."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "Value: ~w", [42])')
        assert result is not None
        assert result is not None
        assert result['X'] == "Value: 42"

    def test_format_multiple_placeholders(self):
        """Test multiple ~w placeholders."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "~w and ~w", [foo, bar])')
        assert result is not None
        assert result is not None
        assert result['X'] == "foo and bar"

    def test_format_with_d_placeholder(self):
        """Test ~d placeholder (integer)."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "Number: ~d", [42])')
        assert result is not None
        assert result is not None
        assert result['X'] == "Number: 42"

    def test_format_with_f_placeholder(self):
        """Test ~f placeholder (float)."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "Pi: ~f", [3.14])')
        assert result is not None
        assert result is not None
        assert result['X'] == "Pi: 3.14"

    def test_format_with_precision(self):
        """Test ~Nf placeholder with precision."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "Value: ~2f", [3.14159])')
        assert result is not None
        assert result is not None
        assert result['X'] == "Value: 3.14"

    def test_format_with_newline(self):
        """Test ~n placeholder (newline)."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "Line 1~nLine 2", [])')
        assert result is not None
        assert result is not None
        assert result['X'] == "Line 1\nLine 2"

    def test_format_with_literal_tilde(self):
        """Test ~~ for literal tilde."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "A ~~ B", [])')
        assert result is not None
        assert result is not None
        assert result['X'] == "A ~ B"

    def test_format_complex_template(self):
        """Test complex format template."""
        prolog = PrologInterpreter()
        result = prolog.query_once('format(atom(X), "Name: ~w, Age: ~d, Score: ~2f", [john, 25, 98.756])')
        assert result is not None
        assert result is not None
        assert result['X'] == "Name: john, Age: 25, Score: 98.76"


class TestIntegration:
    """Integration tests combining multiple built-ins."""

    def test_arithmetic_in_rules(self):
        """Test arithmetic evaluation in rules."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            double(X, Y) :- Y is X * 2.
            triple(X, Y) :- Y is X * 3.
        """)
        result = prolog.query_once("double(5, Y)")
        assert result is not None
        assert result is not None
        assert result['Y'] == 10

        result = prolog.query_once("triple(4, Y)")
        assert result is not None
        assert result is not None
        assert result['Y'] == 12

    def test_list_processing_with_member(self):
        """Test list processing with member."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            contains_even(List) :- member(X, List), 0 =:= X mod 2.
        """)
        assert prolog.has_solution("contains_even([1, 2, 3])")
        assert prolog.has_solution("contains_even([4, 5, 6])")
        assert not prolog.has_solution("contains_even([1, 3, 5])")

    def test_unification_with_arithmetic(self):
        """Test unification combined with arithmetic."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            sum_equals(X, Y, Sum) :- Z is X + Y, Z = Sum.
        """)
        assert prolog.has_solution("sum_equals(2, 3, 5)")
        assert not prolog.has_solution("sum_equals(2, 3, 6)")

    def test_multiple_solutions_with_member_and_arithmetic(self):
        """Test finding multiple solutions with member and arithmetic."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            square_member(X, List) :- member(Y, List), X is Y * Y.
        """)
        results = prolog.query("square_member(X, [2, 3, 4])")
        assert len(results) == 3
        values = sorted([r['X'] for r in results])
        assert values == [4, 9, 16]

    def test_format_with_computed_values(self):
        """Test format with computed values."""
        prolog = PrologInterpreter()
        result = prolog.query_once('X is 2 + 3, format(atom(Y), "Result: ~d", [X])')
        assert result is not None
        assert result is not None
        assert result['X'] == 5
        assert result is not None
        assert result['Y'] == "Result: 5"
