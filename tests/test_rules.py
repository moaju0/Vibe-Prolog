"""
Tests for rules and complex queries.
Inspired by scryer-prolog src/tests/rules.pl
"""

import pytest
from vibeprolog import PrologInterpreter


class TestBasicRules:
    """Basic rule queries"""

    def test_simple_rule(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            parent(tom, bob).
            parent(bob, ann).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """)

        result = prolog.query_once("grandparent(tom, ann)")
        assert result is not None

        result = prolog.query_once("grandparent(tom, X)")
        assert result is not None
        assert result['X'] == 'ann'

    def test_transitive_relation(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            edge(a, b).
            edge(b, c).
            edge(c, d).

            path(X, Y) :- edge(X, Y).
            path(X, Z) :- edge(X, Y), path(Y, Z).
        """)

        # Direct path
        assert prolog.has_solution("path(a, b)")

        # Indirect paths
        assert prolog.has_solution("path(a, c)")
        assert prolog.has_solution("path(a, d)")
        assert prolog.has_solution("path(b, d)")

        # Find all reachable from 'a'
        results = list(prolog.query("path(a, X)"))
        reachable = {r['X'] for r in results}
        assert 'b' in reachable
        assert 'c' in reachable
        assert 'd' in reachable


class TestRulesWithMultipleClauses:
    """Rules with multiple clauses"""

    def test_multiple_rule_clauses(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            likes(alice, pizza).
            likes(bob, burger).
            likes(charlie, pizza).

            vegetarian(alice).

            happy(X) :- likes(X, pizza).
            happy(X) :- vegetarian(X).
        """)

        # Alice is happy (both reasons apply)
        assert prolog.has_solution("happy(alice)")

        # Charlie is happy (likes pizza)
        assert prolog.has_solution("happy(charlie)")

        # Bob is not happy
        assert not prolog.has_solution("happy(bob)")

    def test_conditional_rules(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            grade(alice, 85).
            grade(bob, 92).
            grade(charlie, 78).

            passed(X) :- grade(X, G), G >= 80.
        """)

        assert prolog.has_solution("passed(alice)")
        assert prolog.has_solution("passed(bob)")
        assert not prolog.has_solution("passed(charlie)")


class TestComplexRules:
    """Complex rules with multiple goals"""

    def test_rule_with_multiple_conditions(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            student(alice).
            student(bob).
            teacher(charlie).

            age(alice, 20).
            age(bob, 22).
            age(charlie, 35).

            young_student(X) :- student(X), age(X, A), A < 21.
        """)

        results = list(prolog.query("young_student(X)"))
        assert len(results) == 1
        assert results[0]['X'] == 'alice'

    def test_rule_with_exception(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            bird(sparrow).
            bird(penguin).
            bird(eagle).

            flies(sparrow).
            flies(eagle).

            can_fly(X) :- bird(X), flies(X).
        """)

        assert prolog.has_solution("can_fly(sparrow)")
        assert prolog.has_solution("can_fly(eagle)")
        assert not prolog.has_solution("can_fly(penguin)")


class TestRulesWithLists:
    """Rules operating on lists"""

    def test_list_membership_rule(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            contains(X, L) :- member(X, L).
        """)

        assert prolog.has_solution("contains(2, [1, 2, 3])")
        assert not prolog.has_solution("contains(4, [1, 2, 3])")

    def test_list_sum_rule(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            sum_list([], 0).
            sum_list([H|T], S) :- sum_list(T, S1), S is H + S1.
        """)

        result = prolog.query_once("sum_list([1, 2, 3, 4], S)")
        assert result is not None
        assert result['S'] == 10

        result = prolog.query_once("sum_list([], S)")
        assert result is not None
        assert result['S'] == 0


class TestRecursiveRules:
    """Recursive rules"""

    def test_factorial(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            factorial(0, 1).
            factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.
        """)

        result = prolog.query_once("factorial(5, F)")
        assert result is not None
        assert result['F'] == 120

        result = prolog.query_once("factorial(0, F)")
        assert result is not None
        assert result['F'] == 1

    def test_fibonacci(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            fib(0, 0).
            fib(1, 1).
            fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.
        """)

        result = prolog.query_once("fib(6, F)")
        assert result is not None
        assert result['F'] == 8

        result = prolog.query_once("fib(10, F)")
        assert result is not None
        assert result['F'] == 55

    def test_list_length_rule(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            len([], 0).
            len([_|T], N) :- len(T, N1), N is N1 + 1.
        """)

        result = prolog.query_once("len([a, b, c], N)")
        assert result is not None
        assert result['N'] == 3

        result = prolog.query_once("len([], N)")
        assert result is not None
        assert result['N'] == 0


class TestRulesWithFindall:
    """Using findall with rules"""

    def test_collect_rule_results(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            parent(tom, bob).
            parent(tom, liz).
            parent(bob, ann).
            parent(bob, pat).
            parent(pat, jim).

            child(C, P) :- parent(P, C).
        """)

        # Find all children of tom
        result = prolog.query_once("findall(C, child(C, tom), Children)")
        assert result is not None
        assert set(result['Children']) == {'bob', 'liz'}

        # Find all children of bob
        result = prolog.query_once("findall(C, child(C, bob), Children)")
        assert result is not None
        assert set(result['Children']) == {'ann', 'pat'}

    def test_findall_with_complex_goal(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            person(alice, 30).
            person(bob, 25).
            person(charlie, 30).
            person(diana, 35).

            same_age(X, Y) :- person(X, A), person(Y, A), X \\= Y.
        """)

        # Find all pairs with same age as alice
        result = prolog.query_once("findall(Y, same_age(alice, Y), People)")
        assert result is not None
        assert 'charlie' in result['People']
