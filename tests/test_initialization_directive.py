"""Tests for :- initialization/1 directive."""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestInitializationDirective:
    """Tests for :- initialization/1 directive."""

    def test_single_initialization_simple_goal(self):
        """Single initialization directive with simple goal runs without error."""
        prolog = PrologInterpreter()
        prolog.consult_string(
            ":- initialization(write('Hello')).\n"
            "test."
        )

        assert prolog.has_solution("test")

    def test_multiple_initializations_in_order(self):
        """Multiple initialization directives execute in order."""
        prolog = PrologInterpreter()
        prolog.consult_string(
            ":- dynamic(fact1/0, fact2/0).\n"
            ":- initialization(asserta(fact1)).\n"
            ":- initialization(asserta(fact2)).\n"
            "test."
        )

        assert prolog.has_solution("fact2")
        assert prolog.has_solution("fact1")

    def test_initialization_with_side_effects(self):
        """Initialization that performs side effects modifies database."""
        prolog = PrologInterpreter()
        prolog.consult_string(
            ":- dynamic(side_effect/0).\n"
            ":- initialization(assertz(side_effect)).\n"
            "query :- side_effect."
        )

        assert prolog.has_solution("query")

    def test_initialization_accessing_facts(self):
        """Initialization can access facts defined in the same file."""
        prolog = PrologInterpreter()
        prolog.consult_string(
            "base_fact.\n"
            ":- dynamic(derived_fact/0).\n"
            ":- initialization((base_fact, assertz(derived_fact))).\n"
            "query :- derived_fact."
        )

        assert prolog.has_solution("query")

    def test_empty_initialization(self):
        """`:- initialization(true).` is accepted."""
        prolog = PrologInterpreter()
        prolog.consult_string(
            ":- initialization(true).\n"
            "test."
        )

        assert prolog.has_solution("test")

    def test_initialization_with_complex_goal(self):
        """Initialization supports conjunction goals."""
        prolog = PrologInterpreter()
        prolog.consult_string(
            ":- dynamic(a/0, b/0).\n"
            ":- initialization((asserta(a), asserta(b))).\n"
            "test :- a, b."
        )

        assert prolog.has_solution("test")

    def test_non_callable_goal_number(self):
        """Non-callable initialization goal raises a type error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- initialization(42).")

        error = exc_info.value.term
        assert error.functor == "error"
        assert error.args[0].functor == "type_error"
        assert error.args[0].args[0].name == "callable"

    def test_unbound_variable_goal(self):
        """Unbound variable initialization goal raises instantiation error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- initialization(X).")

        error = exc_info.value.term
        assert error.functor == "error"
        assert error.args[0].name == "instantiation_error"

    def test_initialization_goal_failure(self):
        """Initialization goal that fails does not prevent consult."""
        prolog = PrologInterpreter()
        prolog.consult_string(
            ":- initialization(fail).\n"
            "test."
        )

        assert prolog.has_solution("test")

    def test_initialization_throwing_exception(self):
        """Initialization goal that throws propagates the exception."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(
                ":- initialization(throw(test_error)).\n"
                "test."
            )

        assert exc_info.value.term.name == "test_error"

    def test_initialization_in_multiple_consults(self):
        """Initialization directives across consults all run."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- dynamic(first/0).\n:- initialization(asserta(first)).")
        prolog.consult_string(":- dynamic(second/0).\n:- initialization(asserta(second)).")

        assert prolog.has_solution("first")
        assert prolog.has_solution("second")
