"""Correctness tests for first-argument indexing."""

import pytest
from vibeprolog import PrologInterpreter


class TestFirstArgIndexing:
    """Test that indexing produces correct results."""

    def test_ground_atom_indexing(self):
        """Index on ground atoms."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            person(alice, 25).
            person(bob, 30).
            person(charlie, 35).
        """)

        result = prolog.query_once("person(bob, Age)")
        assert result is not None
        assert result['Age'] == 30

    def test_ground_number_indexing(self):
        """Index on ground numbers."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            age(25, alice).
            age(30, bob).
            age(35, charlie).
        """)

        result = prolog.query_once("age(30, Name)")
        assert result is not None
        assert result['Name'] == 'bob'

    def test_empty_list_indexing(self):
        """Index on empty list."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            list([], empty).
            list([1], single).
            list([1,2], double).
        """)

        result = prolog.query_once("list([], Type)")
        assert result is not None
        assert result['Type'] == 'empty'

    def test_compound_term_indexing(self):
        """Index on compound term functors."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            tree(node(a), left).
            tree(node(b), right).
            tree(leaf(x), bottom).
        """)

        # Should retrieve only node/1 clauses
        results = list(prolog.query("tree(node(X), Dir)"))
        assert len(results) == 2
        nodes = [r['X'] for r in results]
        assert 'a' in nodes
        assert 'b' in nodes

    def test_variable_first_arg_gets_all(self):
        """Variable first arg should match all clauses."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            fact(a, 1).
            fact(b, 2).
            fact(c, 3).
        """)

        results = list(prolog.query("fact(X, Y)"))
        assert len(results) == 3

    def test_mixed_indexed_unindexed_clauses(self):
        """Mix of indexed and unindexed clauses."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            pred(a, 1).
            pred(X, 2) :- atom(X).
            pred(b, 3).
        """)

        # Query with ground first arg should check both indexed and unindexed
        result = prolog.query_once("pred(a, Y)")
        assert result is not None

    def test_dynamic_assert_updates_index(self):
        """Asserted clauses should be indexed."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- dynamic(fact/2).")

        # Assert a clause
        prolog.query_once("assertz(fact(new, 99))")

        # Should be findable via index
        result = prolog.query_once("fact(new, X)")
        assert result is not None
        assert result['X'] == 99

    def test_retract_updates_index(self):
        """Retracted clauses should be removed from index."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- dynamic(item/1).
            item(a).
            item(b).
            item(c).
        """)

        # Retract one clause
        prolog.query_once("retract(item(b))")

        # Should not find retracted clause
        results = list(prolog.query("item(X)"))
        assert len(results) == 2
        items = [r['X'] for r in results]
        assert 'a' in items
        assert 'c' in items
        assert 'b' not in items

    def test_abolish_clears_index(self):
        """Abolish should clear index entries."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- dynamic(temp/1).
            temp(1).
            temp(2).
        """)

        # Abolish predicate
        prolog.query_once("abolish(temp/1)")

        # Should not find any clauses
        result = prolog.query_once("temp(X)")
        assert result is None

    def test_backtracking_order_preserved(self):
        """Backtracking order should match clause order."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            num(1).
            num(2).
            num(3).
        """)

        results = list(prolog.query("num(X)"))
        values = [r['X'] for r in results]
        assert values == [1, 2, 3]  # Order preserved