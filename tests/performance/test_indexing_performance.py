"""Performance benchmarks for first-argument indexing."""

import time
import pytest
from vibeprolog import PrologInterpreter


class TestIndexingPerformance:
    """Benchmark tests for first-argument indexing."""

    def test_large_database_lookup(self):
        """Query on large database should be fast with indexing."""
        prolog = PrologInterpreter()

        # Generate 10,000 person facts
        facts = "\n".join([f"person({i}, name_{i})." for i in range(10000)])
        prolog.consult_string(facts)

        # Query for a specific person (should use index)
        start = time.time()
        result = prolog.query_once("person(5000, Name)")
        elapsed = time.time() - start

        assert result is not None
        assert result['Name'] == 'name_5000'

        # With indexing, this should be very fast (< 0.01 seconds)
        # Without indexing, this would scan 5000 clauses
        assert elapsed < 0.1, f"Query too slow: {elapsed}s"

    def test_repeated_queries_performance(self):
        """Multiple queries should all be fast."""
        prolog = PrologInterpreter()

        # 5,000 facts
        facts = "\n".join([f"data({i}, value_{i})." for i in range(5000)])
        prolog.consult_string(facts)

        # Run 100 queries
        start = time.time()
        for i in range(0, 5000, 50):  # Query every 50th item
            result = prolog.query_once(f"data({i}, X)")
            assert result is not None
        elapsed = time.time() - start

        # 100 queries should be reasonably fast with indexing
        # Allow more time for CI environments and different hardware
        assert elapsed < 5.0, f"100 queries took {elapsed}s"

    def test_unindexed_variable_query(self):
        """Query with variable first arg should still work (no index)."""
        prolog = PrologInterpreter()

        facts = "\n".join([f"item({i})." for i in range(1000)])
        prolog.consult_string(facts)

        # This can't use index (variable first arg)
        results = list(prolog.query("item(X)"))
        assert len(results) == 1000

    def test_backtracking_with_index(self):
        """Backtracking should work correctly with indexing."""
        prolog = PrologInterpreter()

        prolog.consult_string("""
            color(red, warm).
            color(blue, cool).
            color(green, cool).
            color(red, primary).
            color(blue, primary).
        """)

        # Query: color(red, X) - should use index and get both solutions
        results = list(prolog.query("color(red, X)"))
        assert len(results) == 2
        types = [r['X'] for r in results]
        assert 'warm' in types
        assert 'primary' in types