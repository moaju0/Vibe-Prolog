"""Tests for ISO Prolog error reporting."""

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow
import pytest


class TestInstantiationErrors:
    """Test instantiation_error detection."""

    def test_arg_instantiation_error_n(self):
        """arg/3 should throw instantiation_error if N is unbound."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch(arg(N, foo(a,b), X), E, true).")
        assert result is not None
        expected_error = {'error': ['instantiation_error', {'context': ['arg/3']}]}
        assert result['E'] == expected_error

    def test_arg_instantiation_error_term(self):
        """arg/3 should throw instantiation_error if Term is unbound."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch(arg(1, T, X), E, true).")
        assert result is not None
        assert 'error' in result['E']


class TestTypeErrors:
    """Test type_error detection."""

    def test_arg_type_error_n_not_integer(self):
        """arg/3 should throw type_error if N is not an integer."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch(arg(abc, foo(a,b), X), E, true).")
        assert result is not None
        # E should be error(type_error(integer, abc), context(arg/3))
        error = result['E']
        assert 'error' in error

    def test_arg_type_error_term_not_compound(self):
        """arg/3 should throw type_error if Term is not compound."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch(arg(1, atom, X), E, true).")
        assert result is not None
        error = result['E']
        assert 'error' in error


class TestDomainErrors:
    """Test domain_error detection."""

    def test_arg_domain_error_n_less_than_one(self):
        """arg/3 should throw domain_error if N < 1."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch(arg(0, foo(a,b), X), E, true).")
        assert result is not None
        error = result['E']
        assert 'error' in error

    def test_arg_domain_error_n_too_large(self):
        """arg/3 should throw domain_error if N > arity."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch(arg(5, foo(a,b), X), E, true).")
        assert result is not None
        error = result['E']
        assert 'error' in error

    def test_length_domain_error_negative(self):
        """length/2 should throw domain_error if length is negative."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch(length(L, -5), E, true).")
        assert result is not None
        error = result['E']
        assert 'error' in error


class TestSyntaxErrors:
    """Test syntax_error detection."""

    def test_parser_syntax_error(self):
        """Parser should throw syntax_error for invalid syntax."""
        prolog = PrologInterpreter()
        # Parser raises PrologThrow with syntax_error term
        with pytest.raises(PrologThrow):
            prolog.consult_string("foo(")