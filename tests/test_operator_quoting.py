"""Tests for operator quoting in write_term_to_chars."""

from prolog import PrologInterpreter
from prolog.parser import Compound, Atom, Variable, Number
from prolog.utils.list_utils import python_to_list


class TestOperatorQuoting:
    """Test that operators are not incorrectly quoted."""

    def test_arithmetic_operators_no_quotes(self):
        """Common arithmetic operators should not be quoted."""
        prolog = PrologInterpreter()

        # Test + operator - use =.. to construct the term
        prolog.consult_string("test_op(Chars) :- Term =.. ['+', 1, 2], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_op(Chars)")
        assert result is not None
        chars = result['Chars']
        # Should be +(1,2) not '+'(1,2)
        assert ''.join(chars) == "+(1,2)"

        # Test - operator
        prolog.consult_string("test_minus(Chars) :- Term =.. ['-', 5, 3], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_minus(Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == "-(5,3)"

        # Test * operator
        prolog.consult_string("test_mult(Chars) :- Term =.. ['*', 4, 5], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_mult(Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == "*(4,5)"

        # Test / operator
        prolog.consult_string("test_div(Chars) :- Term =.. ['/', 10, 2], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_div(Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == "/(10,2)"

    def test_comparison_operators_no_quotes(self):
        """Comparison operators should not be quoted."""
        prolog = PrologInterpreter()

        # Test = operator
        prolog.consult_string("test_eq(Chars) :- Term =.. ['=', X, 1], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_eq(Chars)")
        assert result is not None
        chars = ''.join(result['Chars'])
        # Should be =(var,1) with no quotes around =
        assert chars.startswith("=(_X") and chars.endswith(",1)")
        assert not chars.startswith("'='")  # = should not be quoted

        # Test < operator
        prolog.consult_string("test_lt(Chars) :- Term =.. ['<', 1, 2], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_lt(Chars)")
        assert result is not None
        chars = ''.join(result['Chars'])
        assert chars == "<(1,2)"
        assert not chars.startswith("'<'")  # < should not be quoted

        # Test > operator
        prolog.consult_string("test_gt(Chars) :- Term =.. ['>', 2, 1], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_gt(Chars)")
        assert result is not None
        chars = ''.join(result['Chars'])
        assert chars == ">(2,1)"
        assert not chars.startswith("'>'")  # > should not be quoted

    def test_special_atoms_no_quotes(self):
        """Special atoms like !, [], {} should not be quoted."""
        prolog = PrologInterpreter()

        # Test ! (cut)
        result = prolog.query_once("write_term_to_chars(!, [quoted(true)], Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == "!"

        # Test [] (empty list atom)
        result = prolog.query_once("write_term_to_chars('[]', [quoted(true)], Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == "[]"

        # Test {} (empty braces atom)
        result = prolog.query_once("write_term_to_chars('{}', [quoted(true)], Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == "{}"

    def test_compound_operators_no_quotes(self):
        """Compound operators like :-, ->, =.. should not be quoted."""
        prolog = PrologInterpreter()

        # Test :- operator
        prolog.consult_string("test_rule(Chars) :- Term =.. [':-', a, b], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_rule(Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == ":-(a,b)"

        # Test -> operator
        prolog.consult_string("test_if(Chars) :- Term =.. ['->', a, b], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_if(Chars)")
        assert result is not None
        chars = result['Chars']
        assert ''.join(chars) == "->(a,b)"

        # Test =.. operator
        prolog.consult_string("test_univ(Chars) :- Term =.. ['=..', X, [a, b]], write_term_to_chars(Term, [quoted(true), ignore_ops(true)], Chars).")
        result = prolog.query_once("test_univ(Chars)")
        assert result is not None
        chars = ''.join(result['Chars'])
        # Should be =..(var,[a,b]) with no quotes around =..
        assert chars.startswith("=..(_X") and chars.endswith(",[a,b])")
        assert not chars.startswith("'=..'")  # =.. should not be quoted

    def test_uppercase_atom_needs_quotes(self):
        """Atoms starting with uppercase should be quoted."""
        prolog = PrologInterpreter()

        result = prolog.query_once("write_term_to_chars('Foo', [quoted(true)], Chars)")
        assert result is not None
        chars = result['Chars']
        # Should be quoted
        assert ''.join(chars) == "'Foo'"

    def test_lowercase_alphanumeric_no_quotes(self):
        """Normal lowercase atoms don't need quotes."""
        prolog = PrologInterpreter()

        result = prolog.query_once("write_term_to_chars(hello_world123, [quoted(true)], Chars)")
        assert result is not None
        chars = result['Chars']
        # Should not be quoted
        assert ''.join(chars) == "hello_world123"
