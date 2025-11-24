"""
Tests for basic facts and queries.
Inspired by scryer-prolog src/tests/facts.pl
"""

import pytest
from vibeprolog import PrologInterpreter


class TestBasicFacts:
    """Basic fact queries"""

    def test_simple_fact_query(self):
        prolog = PrologInterpreter()
        prolog.consult_string("p(a).")
        assert prolog.has_solution("p(a)")
        assert not prolog.has_solution("p(b)")

    def test_fact_with_variables(self):
        prolog = PrologInterpreter()
        prolog.consult_string("p(Z, Z).")

        # Query with same variable should match
        result = prolog.query_once("p(X, X)")
        assert result is not None

        # Query with specific values
        assert prolog.has_solution("p(a, a)")
        assert not prolog.has_solution("p(a, b)")

    def test_multiple_facts(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            color(red).
            color(green).
            color(blue).
        """)

        results = list(prolog.query("color(X)"))
        assert len(results) == 3
        colors = {r['X'] for r in results}
        assert colors == {'red', 'green', 'blue'}


class TestFactsWithCompoundTerms:
    """Facts with compound terms"""

    def test_compound_fact(self):
        prolog = PrologInterpreter()
        prolog.consult_string("person(alice, 30).")

        result = prolog.query_once("person(Name, Age)")
        assert result is not None
        assert result['Name'] == 'alice'
        assert result is not None
        assert result['Age'] == 30

    def test_nested_compound(self):
        prolog = PrologInterpreter()
        prolog.consult_string("p(Z, h(Z, W), f(W)).")

        # Test with matching variables
        result = prolog.query_once("p(a, h(a, b), f(b))")
        assert result is not None

        # Test with unification
        result = prolog.query_once("p(z, h(z, W), f(w))")
        assert result is not None
        assert result is not None
        assert result['W'] == 'w'


class TestFindallWithFacts:
    """Using findall with facts"""

    def test_findall_simple(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            p(1).
            p(2).
            p(3).
        """)

        result = prolog.query_once("findall(X, p(X), L)")
        assert result is not None
        assert result['L'] == [1, 2, 3]

    def test_findall_with_condition(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            num(1).
            num(2).
            num(3).
            num(4).
        """)

        # Find all numbers greater than 2
        result = prolog.query_once("findall(X, (num(X), X > 2), L)")
        assert result is not None
        assert set(result['L']) == {3, 4}

    def test_findall_empty_result(self):
        prolog = PrologInterpreter()
        prolog.consult_string("p(a).")

        result = prolog.query_once("findall(X, p(b), L)")
        assert result is not None
        assert result['L'] == []


class TestDynamicFactModification:
    """Testing assert and retract with facts"""

    def test_assert_fact(self):
        prolog = PrologInterpreter()
        prolog.consult_string("p(a).")

        # Assert a new fact
        prolog.query_once("assert(p(b))")

        results = list(prolog.query("p(X)"))
        assert len(results) == 2
        values = {r['X'] for r in results}
        assert values == {'a', 'b'}

    def test_retract_fact(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            p(a).
            p(b).
            p(c).
        """)

        # Retract one fact
        prolog.query_once("retract(p(b))")

        results = list(prolog.query("p(X)"))
        assert len(results) == 2
        values = {r['X'] for r in results}
        assert values == {'a', 'c'}

    def test_retract_all_matching(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            p(1).
            p(2).
            p(3).
        """)

        # Retract all facts matching p(X)
        results = list(prolog.query("retract(p(X))"))
        assert len(results) == 3

        # All facts should be removed
        assert not prolog.has_solution("p(X)")


class TestFactUnification:
    """Testing unification with facts"""

    def test_unify_with_anonymous_variables(self):
        prolog = PrologInterpreter()
        prolog.consult_string("p(a, b, c).")

        # Match with some variables
        result = prolog.query_once("p(X, b, Y)")
        assert result is not None
        assert result['X'] == 'a'
        assert result is not None
        assert result['Y'] == 'c'

    def test_partial_instantiation(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            edge(a, b).
            edge(b, c).
            edge(c, d).
        """)

        # Find all edges from 'b'
        results = list(prolog.query("edge(b, X)"))
        assert len(results) == 1
        assert results[0]['X'] == 'c'

        # Find all edges to 'd'
        results = list(prolog.query("edge(X, d)"))
        assert len(results) == 1
        assert results[0]['X'] == 'c'
