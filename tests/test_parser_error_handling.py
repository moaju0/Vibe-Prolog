"""Tests ensuring parser syntax errors surface as ISO terms."""

import pytest

from prolog import PrologInterpreter
from prolog.builtins.exceptions import PrologThrow
from prolog.parser import Atom, Compound


class TestParserErrorHandling:
    """Verify interpreter parsing APIs raise ISO syntax_error terms."""

    def test_query_syntax_error_raises_iso_error(self):
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as excinfo:
            prolog.query_once("foo(")

        error_term = excinfo.value.term
        assert isinstance(error_term, Compound)
        assert error_term.functor == "error"
        assert error_term.args[1] == Atom("query/1")
        syntax_term = error_term.args[0]
        assert isinstance(syntax_term, Compound)
        assert syntax_term.functor == "syntax_error"

    def test_consult_string_syntax_error_raises_iso_error(self):
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as excinfo:
            prolog.consult_string("foo(")

        error_term = excinfo.value.term
        assert isinstance(error_term, Compound)
        assert error_term.functor == "error"
        assert error_term.args[1] == Atom("consult/1")
        syntax_term = error_term.args[0]
        assert isinstance(syntax_term, Compound)
        assert syntax_term.functor == "syntax_error"
