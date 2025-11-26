"""Tests for throw/1 and catch/3 exception handling."""

import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow
from vibeprolog.terms import Atom


class TestThrowCatch:
    """Tests for throw/1 and catch/3 exception handling."""

    def test_catch_basic_throw(self):
        """Test basic catch/3 with throw/1 - should succeed with E = oops."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch((throw(oops)), E, true).")
        assert result is not None
        assert result['E'] == 'oops'

    def test_throw_unhandled_propagation(self):
        """Test that unhandled throw/1 raises PrologThrow."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("throw(bad).")
        assert exc_info.value.term == Atom("bad")

    def test_catch_non_matching_handler(self):
        """Test catch/3 with non-matching error pattern - should propagate."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("catch((throw(oops)), err(other), writeln(handled)).")
        assert exc_info.value.term == Atom("oops")

    def test_throw_variable_term(self):
        """Test throw/1 with variable term binding."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch((X=foo, throw(X)), E, true).")
        assert result is not None
        assert result['E'] == 'foo'

    def test_throw_compound_term(self):
        """Test throw/1 with compound term."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch((throw(error(domain_error, foo))), E, true).")
        assert result is not None
        # E should unify with error(domain_error, foo)
        assert result['E'] == {'error': ['domain_error', 'foo']}

    def test_catch_with_cut_interaction(self):
        """Test that cut doesn't mask throws - throw bypasses cut."""
        prolog = PrologInterpreter()
        # The throw happens before the cut, so it should propagate through
        result = prolog.query_once("catch((throw(error), !), E, true).")
        assert result is not None
        assert result['E'] == 'error'

    def test_throw_in_backtracking(self):
        """Test that a throw within a disjunction is caught and handled."""
        prolog = PrologInterpreter()
        # First disjunct throws, second succeeds
        results = list(prolog.query("catch((throw(error) ; X = success), E, X = caught)."))
        assert len(results) == 1
        assert results[0]['X'] == 'caught'

    def test_nested_catch_throw(self):
        """Test nested catch/3 with throw/1."""
        prolog = PrologInterpreter()
        # Inner catch handles the throw, then outer throw happens
        result = prolog.query_once("catch((catch((throw(inner)), E, true), throw(outer)), F, true).")
        assert result is not None
        assert result['F'] == 'outer'
        # E is not bound because the conjunction fails after throw(outer)

    def test_throw_with_unification_failure(self):
        """Test throw where unification in catch fails."""
        prolog = PrologInterpreter()
        # Throw a term that doesn't unify with the catcher pattern
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("catch((throw(atom_term)), compound(X, Y), true).")
        assert exc_info.value.term == Atom("atom_term")

    def test_catch_successful_goal_no_throw(self):
        """Test catch/3 with successful goal that doesn't throw."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch((X = 42), _, fail).")
        assert result is not None
        assert result['X'] == 42

    def test_catch_failing_goal_no_throw(self):
        """Test catch/3 with failing goal that doesn't throw."""
        prolog = PrologInterpreter()
        result = prolog.query_once("catch((fail), _, true).")
        assert result is None  # Should fail since fail doesn't throw

    def test_throw_in_rule_body(self):
        """Test throw/1 in a rule body."""
        prolog = PrologInterpreter()
        prolog.consult_string("test_throw :- throw(error).")
        result = prolog.query_once("catch(test_throw, E, true).")
        assert result is not None
        assert result['E'] == 'error'

    def test_multiple_catches_same_level(self):
        """Test multiple catch/3 at the same level."""
        prolog = PrologInterpreter()
        # Inner catch fails to match, re-raises, outer catch handles
        result = prolog.query_once("catch(catch(throw(specific), general(_), fail), E, true).")
        assert result is not None
        assert result['E'] == 'specific'