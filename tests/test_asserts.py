"""Tests for asserta/1, assertz/1, and assert/1 builtins."""

import pytest
from vibeprolog import PrologInterpreter


class TestAsserta:
    """Tests for asserta/1 predicate."""

    def test_asserta_fact_ordering(self):
        """Test that asserta/1 adds facts at the beginning."""
        prolog = PrologInterpreter()
        # Add initial facts
        prolog.query_once("assert(p(1)).")
        prolog.query_once("assert(p(2)).")

        # Add fact with asserta (should go to front)
        prolog.query_once("asserta(p(0)).")

        # Query should find p(0) first
        results = list(prolog.query("p(X)."))
        assert len(results) == 3
        assert results[0]['X'] == 0  # asserta fact first
        assert results[1]['X'] == 1  # original first fact
        assert results[2]['X'] == 2  # original second fact

    def test_asserta_multiple(self):
        """Test multiple asserta calls."""
        prolog = PrologInterpreter()
        prolog.query_once("assert(p(1)).")
        prolog.query_once("asserta(p(0)).")
        prolog.query_once("asserta(p(-1)).")

        results = list(prolog.query("p(X)."))
        assert len(results) == 3
        assert results[0]['X'] == -1  # Last asserta first
        assert results[1]['X'] == 0   # First asserta second
        assert results[2]['X'] == 1   # Original last


class TestAssertz:
    """Tests for assertz/1 predicate."""

    def test_assertz_fact_ordering(self):
        """Test that assertz/1 adds facts at the end."""
        prolog = PrologInterpreter()
        # Add initial facts
        prolog.query_once("assert(p(1)).")
        prolog.query_once("assert(p(2)).")

        # Add fact with assertz (should go to end)
        prolog.query_once("assertz(p(3)).")

        # Query should find original facts first, then assertz fact
        results = list(prolog.query("p(X)."))
        assert len(results) == 3
        assert results[0]['X'] == 1  # original first
        assert results[1]['X'] == 2  # original second
        assert results[2]['X'] == 3  # assertz fact last

    def test_assertz_multiple(self):
        """Test multiple assertz calls."""
        prolog = PrologInterpreter()
        prolog.query_once("assert(p(1)).")
        prolog.query_once("assertz(p(2)).")
        prolog.query_once("assertz(p(3)).")

        results = list(prolog.query("p(X)."))
        assert len(results) == 3
        assert results[0]['X'] == 1
        assert results[1]['X'] == 2
        assert results[2]['X'] == 3


class TestAssert:
    """Tests for assert/1 predicate (should behave like assertz/1)."""

    def test_assert_fact_ordering(self):
        """Test that assert/1 adds facts at the end (like assertz/1)."""
        prolog = PrologInterpreter()
        # Add initial facts
        prolog.query_once("assert(p(1)).")
        prolog.query_once("assert(p(2)).")

        # Add fact with assert (should go to end)
        prolog.query_once("assert(p(3)).")

        # Query should find original facts first, then assert fact
        results = list(prolog.query("p(X)."))
        assert len(results) == 3
        assert results[0]['X'] == 1
        assert results[1]['X'] == 2
        assert results[2]['X'] == 3  # assert fact last


class TestMixedAssert:
    """Tests mixing asserta/1, assertz/1, and assert/1."""

    def test_mixed_ordering(self):
        """Test ordering when mixing different assert variants."""
        prolog = PrologInterpreter()

        # Add initial fact
        prolog.query_once("assert(p(2)).")

        # Add with asserta (front)
        prolog.query_once("asserta(p(1)).")

        # Add with assertz (end)
        prolog.query_once("assertz(p(4)).")

        # Add with assert (end, like assertz)
        prolog.query_once("assert(p(5)).")

        # Add another asserta (front)
        prolog.query_once("asserta(p(0)).")

        results = list(prolog.query("p(X)."))
        assert len(results) == 5
        assert results[0]['X'] == 0  # Last asserta
        assert results[1]['X'] == 1  # First asserta
        assert results[2]['X'] == 2  # Original
        assert results[3]['X'] == 4  # assertz
        assert results[4]['X'] == 5  # assert (like assertz)


class TestAssertBuiltins:
    """Tests for assert builtin properties."""

    def test_asserta_builtin_property(self):
        """Test that asserta is recognized as a built-in predicate."""
        prolog = PrologInterpreter()
        result = prolog.query_once("predicate_property(asserta(_), built_in).")
        assert result == {}

    def test_assertz_builtin_property(self):
        """Test that assertz is recognized as a built-in predicate."""
        prolog = PrologInterpreter()
        result = prolog.query_once("predicate_property(assertz(_), built_in).")
        assert result == {}

    def test_assert_deterministic(self):
        """Test that assert builtins are deterministic (single solution)."""
        prolog = PrologInterpreter()
        results = list(prolog.query("assert(p(test))."))
        assert len(results) == 1
        assert results[0] == {}

        results = list(prolog.query("asserta(p(test))."))
        assert len(results) == 1
        assert results[0] == {}

        results = list(prolog.query("assertz(p(test))."))
        assert len(results) == 1
        assert results[0] == {}