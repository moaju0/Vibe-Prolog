"""Tests for higher-order list predicates (maplist/3-5, include/3, exclude/3, partition/4, foldl/4-6)."""

import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


@pytest.fixture
def prolog():
    """Provides a PrologInterpreter instance for tests."""
    return PrologInterpreter()


class TestMaplist3:
    """Tests for maplist/3 predicate."""

    def test_maplist_3_basic(self, prolog):
        """Test basic maplist/3 functionality with built-in =/2."""
        result = prolog.query_once("maplist(=, [a,b,c], [a,b,c]).")
        assert result is not None

    def test_maplist_3_fail_unify(self, prolog):
        """Test maplist/3 fails when unification fails."""
        assert not prolog.has_solution("maplist(=, [a,b,c], [a,b,d]).")

    def test_maplist_3_different_lengths(self, prolog):
        """Test maplist/3 fails with different list lengths."""
        assert not prolog.has_solution("maplist(=, [a,b], [a,b,c]).")

    def test_maplist_3_empty_lists(self, prolog):
        """Test maplist/3 with empty lists."""
        result = prolog.query_once("maplist(=, [], []).")
        assert result is not None

    def test_maplist_3_type_error_non_list(self, prolog):
        """Test maplist/3 with non-list."""
        with pytest.raises(PrologThrow):  # Should raise type_error
            prolog.query_once("maplist(=, atom, [a]).")

    def test_maplist_3_type_error_non_callable(self, prolog):
        """Test maplist/3 with non-callable predicate."""
        with pytest.raises(PrologThrow):  # Should raise type_error
            prolog.query_once("maplist(5, [a], [b]).")


class TestMaplist4:
    """Tests for maplist/4 predicate."""

    def test_maplist_4_basic(self, prolog):
        """Test basic maplist/4 functionality."""
        prolog.consult_string("sum(A, B, C) :- C is A + B.")
        result = prolog.query_once("maplist(sum, [1, 2], [10, 20], X).")
        assert result is not None
        assert result['X'] == [11, 22]

    def test_maplist_4_different_lengths(self, prolog):
        """Test maplist/4 fails with different list lengths."""
        assert not prolog.has_solution("maplist(=, [a,b], [c,d], [e]).")


class TestMaplist5:
    """Tests for maplist/5 predicate."""

    def test_maplist_5_basic(self, prolog):
        """Test basic maplist/5 functionality with a simple predicate sum4/4."""
        prolog.consult_string("sum4(A, B, C, D) :- D is A + B + C.")
        result = prolog.query_once("maplist(sum4, [1,2], [10,20], [100,200], X).")
        assert result is not None
        assert result['X'] == [111, 222]

    def test_maplist_5_different_lengths(self, prolog):
        """Test maplist/5 fails with different list lengths."""
        # Test that maplist/5 fails with different list lengths
        assert not prolog.has_solution("maplist(=, [a], [b], [c], [d,e]).")


class TestInclude:
    """Tests for include/3 predicate."""

    def test_include_basic(self, prolog):
        """Test basic include/3 functionality with built-in atom/1."""
        result = prolog.query_once("include(atom, [a,1,b,2], X).")
        assert result is not None
        assert result['X'] == ['a', 'b']

    def test_include_empty_list(self, prolog):
        """Test include/3 with empty list."""
        result = prolog.query_once("include(atom, [], X).")
        assert result is not None
        assert result['X'] == []

    def test_include_all_pass(self, prolog):
        """Test include/3 when all elements pass."""
        result = prolog.query_once("include(atom, [a,b,c], X).")
        assert result is not None
        assert result['X'] == ['a', 'b', 'c']

    def test_include_none_pass(self, prolog):
        """Test include/3 when no elements pass."""
        result = prolog.query_once("include(atom, [1,2,3], X).")
        assert result is not None
        assert result['X'] == []

    def test_include_type_error_non_list(self, prolog):
        """Test include/3 with non-list."""
        with pytest.raises(PrologThrow):  # Should raise type_error
            prolog.query_once("include(atom, not_a_list, X).")

    def test_include_type_error_non_callable(self, prolog):
        """Test include/3 with non-callable predicate."""
        with pytest.raises(PrologThrow):  # Should raise type_error
            prolog.query_once("include(5, [1,2], X).")


class TestExclude:
    """Tests for exclude/3 predicate."""

    def test_exclude_basic(self, prolog):
        """Test basic exclude/3 functionality with built-in atom/1."""
        result = prolog.query_once("exclude(atom, [a,1,b,2], X).")
        assert result is not None
        assert result['X'] == [1, 2]

    def test_exclude_empty_list(self, prolog):
        """Test exclude/3 with empty list."""
        result = prolog.query_once("exclude(atom, [], X).")
        assert result is not None
        assert result['X'] == []

    def test_exclude_all_fail(self, prolog):
        """Test exclude/3 when all elements fail."""
        result = prolog.query_once("exclude(atom, [a,b,c], X).")
        assert result is not None
        assert result['X'] == []

    def test_exclude_none_fail(self, prolog):
        """Test exclude/3 when no elements fail."""
        result = prolog.query_once("exclude(atom, [1,2,3], X).")
        assert result is not None
        assert result['X'] == [1, 2, 3]


class TestPartition:
    """Tests for partition/4 predicate."""

    def test_partition_basic(self, prolog):
        """Test basic partition/4 functionality with built-in atom/1."""
        result = prolog.query_once("partition(atom, [a,1,b,2,c,3], Included, Excluded).")
        assert result is not None
        assert result['Included'] == ['a', 'b', 'c']
        assert result['Excluded'] == [1, 2, 3]

    def test_partition_empty_list(self, prolog):
        """Test partition/4 with empty list."""
        result = prolog.query_once("partition(atom, [], Included, Excluded).")
        assert result is not None
        assert result['Included'] == []
        assert result['Excluded'] == []

    def test_partition_all_included(self, prolog):
        """Test partition/4 when all elements are included."""
        result = prolog.query_once("partition(atom, [a,b,c], Included, Excluded).")
        assert result is not None
        assert result['Included'] == ['a', 'b', 'c']
        assert result['Excluded'] == []

    def test_partition_all_excluded(self, prolog):
        """Test partition/4 when all elements are excluded."""
        result = prolog.query_once("partition(atom, [1,2,3], Included, Excluded).")
        assert result is not None
        assert result['Included'] == []
        assert result['Excluded'] == [1, 2, 3]


class TestFoldl4:
    """Tests for foldl/4 predicate."""

    def test_foldl_4_sum(self, prolog):
        """Test foldl/4 for summing a list."""
        result = prolog.query_once("foldl(plus, [1,2,3,4], 0, X).")
        assert result is not None
        assert result['X'] == 10

    def test_foldl_4_empty_list(self, prolog):
        """Test foldl/4 with empty list."""
        result = prolog.query_once("foldl(plus, [], 42, X).")
        assert result is not None
        assert result['X'] == 42

    def test_foldl_4_single_element(self, prolog):
        """Test foldl/4 with single element."""
        result = prolog.query_once("foldl(plus, [5], 10, X).")
        assert result is not None
        assert result['X'] == 15

    def test_foldl_4_type_error_non_list(self, prolog):
        """Test foldl/4 with non-list."""
        with pytest.raises(PrologThrow):  # Should raise type_error
            prolog.query_once("foldl(plus, atom, 0, X).")


class TestFoldl5:
    """Tests for foldl/5 predicate."""

    def test_foldl_5_basic(self, prolog):
        """Test basic foldl/5 functionality."""
        prolog.consult_string("sum_acc(X, Y, Acc, NewAcc) :- NewAcc is X + Y + Acc.")
        result = prolog.query_once("foldl(sum_acc, [1, 2], [10, 20], 0, Result).")
        assert result is not None
        assert result['Result'] == 33  # (0 + 1 + 10) -> 11, (11 + 2 + 20) -> 33

    def test_foldl_5_length_mismatch(self, prolog):
        """Test foldl/5 fails with lists of different lengths."""
        assert not prolog.has_solution("foldl(plus, [1,2], [3], 0, X).")


class TestFoldl6:
    """Tests for foldl/6 predicate."""

    def test_foldl_6_basic(self, prolog):
        """Test basic foldl/6 functionality."""
        # Similar to foldl/5, just test error handling
        assert not prolog.has_solution("foldl(plus, [1,2], [3], [4], 0, X).")


class TestHigherOrderIntegration:
    """Integration tests for higher-order predicates."""

    def test_include_exclude_relationship(self, prolog):
        """Test that include and exclude are complementary."""
        # Test with a list
        result_include = prolog.query_once("include(atom, [a,1,b,2], Included).")
        result_exclude = prolog.query_once("exclude(atom, [a,1,b,2], Excluded).")

        assert result_include is not None
        assert result_exclude is not None
        assert result_include['Included'] == ['a', 'b']
        assert result_exclude['Excluded'] == [1, 2]

    def test_partition_completeness(self, prolog):
        """Test that partition produces complementary results."""
        result = prolog.query_once("partition(atom, [a,1,b,2,c,3], Atoms, NonAtoms).")
        assert result is not None
        assert len(result['Atoms']) + len(result['NonAtoms']) == 6
        assert set(result['Atoms'] + result['NonAtoms']) == {'a', 1, 'b', 2, 'c', 3}