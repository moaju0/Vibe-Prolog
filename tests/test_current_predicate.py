"""Tests for current_predicate/1 builtin."""

import pytest
from vibeprolog import PrologInterpreter


class TestCurrentPredicate:
    """Tests for current_predicate/1 predicate."""

    def test_builtin_predicates_present(self):
        """Test that built-in predicates are found by current_predicate/1."""
        prolog = PrologInterpreter()

        # Test some common built-ins
        assert prolog.has_solution("current_predicate(write/1)")
        assert prolog.has_solution("current_predicate(atom/1)")
        assert prolog.has_solution("current_predicate(call/1)")
        assert prolog.has_solution("current_predicate(findall/3)")
        assert prolog.has_solution("current_predicate(true/0)")
        assert prolog.has_solution("current_predicate(fail/0)")

    def test_user_defined_facts(self):
        """Test current_predicate/1 with user-defined facts."""
        prolog = PrologInterpreter()
        prolog.consult_string("person(john, 25). person(jane, 30).")

        # Should find the person/2 predicate
        assert prolog.has_solution("current_predicate(person/2)")

        # Should not find non-existent predicates
        assert not prolog.has_solution("current_predicate(employee/2)")

    def test_user_defined_rules(self):
        """Test current_predicate/1 with user-defined rules."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            parent(X, Y) :- father(X, Y).
            parent(X, Y) :- mother(X, Y).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """)

        # Should find all defined predicates
        assert prolog.has_solution("current_predicate(parent/2)")
        assert prolog.has_solution("current_predicate(grandparent/2)")

        # father/2 and mother/2 are not defined, so should not be found
        assert not prolog.has_solution("current_predicate(father/2)")
        assert not prolog.has_solution("current_predicate(mother/2)")

    def test_enumeration_with_findall(self):
        """Test non-deterministic enumeration of predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("test_pred(a). test_pred(b).")

        # Collect all predicates
        result = prolog.query_once("findall(F, current_predicate(F), L)")
        assert result is not None
        predicates = result['L']

        # Should contain built-ins and user-defined
        # Check for specific predicates by dict structure
        has_write_1 = any(isinstance(p, dict) and "/" in p and p["/"] == ["write", 1] for p in predicates)
        has_atom_1 = any(isinstance(p, dict) and "/" in p and p["/"] == ["atom", 1] for p in predicates)
        has_test_pred_1 = any(isinstance(p, dict) and "/" in p and p["/"] == ["test_pred", 1] for p in predicates)
        has_findall_3 = any(isinstance(p, dict) and "/" in p and p["/"] == ["findall", 3] for p in predicates)

        assert has_write_1
        assert has_atom_1
        assert has_test_pred_1
        assert has_findall_3

        # Should have no duplicates (check by converting to tuples)
        predicate_tuples = [tuple(p["/"]) for p in predicates if isinstance(p, dict) and "/" in p]
        assert len(predicates) == len(set(predicate_tuples))

    def test_arity_correctness(self):
        """Test that arities are reported correctly."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            zero_arity.
            one_arity(X).
            two_arity(X, Y).
            three_arity(A, B, C).
        """)

        # Test specific arities
        assert prolog.has_solution("current_predicate(zero_arity/0)")
        assert prolog.has_solution("current_predicate(one_arity/1)")
        assert prolog.has_solution("current_predicate(two_arity/2)")
        assert prolog.has_solution("current_predicate(three_arity/3)")

        # Wrong arities should fail
        assert not prolog.has_solution("current_predicate(zero_arity/1)")
        assert not prolog.has_solution("current_predicate(one_arity/0)")

    def test_dynamic_updates(self):
        """Test that current_predicate/1 reflects dynamic predicate additions."""
        prolog = PrologInterpreter()

        # Initially, dynamic_pred should not exist
        assert not prolog.has_solution("current_predicate(dynamic_pred/1)")

        # Add a dynamic predicate
        prolog.consult_string("dynamic_pred(x).")

        # Now it should be found
        assert prolog.has_solution("current_predicate(dynamic_pred/1)")

    def test_multiple_clauses_same_predicate(self):
        """Test that predicates with multiple clauses appear only once."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            multi_clause(a).
            multi_clause(b).
            multi_clause(c).
        """)

        # Should find multi_clause/1 exactly once
        results = prolog.query("current_predicate(multi_clause/1)")
        assert len(results) == 1

    def test_variable_argument(self):
        """Test current_predicate/1 with variable argument."""
        prolog = PrologInterpreter()
        prolog.consult_string("test_var(X).")

        # Should be able to unify with a variable
        result = prolog.query_once("current_predicate(P), P = test_var/1")
        assert result is not None
        p = result['P']
        assert isinstance(p, dict) and "/" in p
        assert p["/"] == ["test_var", 1]

    def test_format_arities(self):
        """Test that format/1, format/2 and format/3 are all recognized."""
        prolog = PrologInterpreter()

        assert prolog.has_solution("current_predicate(format/1)")
        assert prolog.has_solution("current_predicate(format/2)")
        assert prolog.has_solution("current_predicate(format/3)")

    def test_no_false_positives(self):
        """Test that non-existent predicates are not found."""
        prolog = PrologInterpreter()

        assert not prolog.has_solution("current_predicate(nonexistent/0)")
        assert not prolog.has_solution("current_predicate(fake_pred/5)")
        assert not prolog.has_solution("current_predicate(true/1)")  # true is 0, not 1

    def test_predicate_property_builtin(self):
        """Test that current_predicate/1 is recognized as built-in by predicate_property/2."""
        prolog = PrologInterpreter()

        # current_predicate/1 should be recognized as built-in
        result = prolog.query_once("predicate_property(current_predicate(_), built_in)")
        assert result == {}