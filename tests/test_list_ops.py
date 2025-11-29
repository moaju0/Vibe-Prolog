"""Tests for list operation built-ins (msort, keysort, nth0/1, last, select, memberchk, is_list, aggregations)."""

import pytest
from vibeprolog import PrologInterpreter


class TestMsort:
    """Tests for msort/2 predicate."""

    def test_msort_basic(self):
        """Test basic msort functionality."""
        prolog = PrologInterpreter()
        result = prolog.query_once("msort([3,1,2,1], X).")
        assert result is not None
        assert result['X'] == [1, 1, 2, 3]

    def test_msort_keeps_duplicates(self):
        """Test that msort keeps duplicate elements (unlike sort/2)."""
        prolog = PrologInterpreter()
        result = prolog.query_once("msort([b,a,c,a], X).")
        assert result is not None
        assert result['X'] == ['a', 'a', 'b', 'c']

    def test_msort_empty_list(self):
        """Test msort on empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("msort([], X).")
        assert result is not None
        assert result['X'] == []

    def test_msort_single_element(self):
        """Test msort on single element list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("msort([single], X).")
        assert result is not None
        assert result['X'] == ['single']

    def test_msort_instantiation_error(self):
        """Test msort with uninstantiated list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise instantiation_error
            prolog.query_once("msort(X, Y).")

    def test_msort_type_error(self):
        """Test msort with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("msort(atom, X).")


class TestKeysort:
    """Tests for keysort/2 predicate."""

    def test_keysort_basic(self):
        """Test basic keysort functionality."""
        prolog = PrologInterpreter()
        result = prolog.query_once("keysort([3-a, 1-b, 2-c, 1-d], X).")
        assert result is not None
        expected = [
            {'-': [1, 'b']},
            {'-': [1, 'd']},
            {'-': [2, 'c']},
            {'-': [3, 'a']}
        ]
        assert result['X'] == expected

    def test_keysort_stable(self):
        """Test that keysort is stable (preserves order of equal keys)."""
        prolog = PrologInterpreter()
        result = prolog.query_once("keysort([b-2, a-1, c-3], X).")
        assert result is not None
        expected = [
            {'-': ['a', 1]},
            {'-': ['b', 2]},
            {'-': ['c', 3]}
        ]
        assert result['X'] == expected

    def test_keysort_empty_list(self):
        """Test keysort on empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("keysort([], X).")
        assert result is not None
        assert result['X'] == []

    def test_keysort_type_error_non_list(self):
        """Test keysort with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("keysort(atom, X).")

    def test_keysort_type_error_non_pair(self):
        """Test keysort with non-pair element."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("keysort([not_a_pair], X).")


class TestNth0:
    """Tests for nth0/3 predicate."""

    def test_nth0_get_element(self):
        """Test getting element at index."""
        prolog = PrologInterpreter()
        result = prolog.query_once("nth0(0, [a,b,c], X).")
        assert result is not None
        assert result['X'] == 'a'

        result = prolog.query_once("nth0(2, [a,b,c,d], X).")
        assert result is not None
        assert result['X'] == 'c'

    def test_nth0_find_index(self):
        """Test finding index of element."""
        prolog = PrologInterpreter()
        results = prolog.query("nth0(I, [a,b,c], b).")
        assert len(results) == 1
        assert results[0]['I'] == 1

        results = prolog.query("nth0(I, [a,b,a], a).")
        assert len(results) == 2
        indices = sorted([r['I'] for r in results])
        assert indices == [0, 2]

    def test_nth0_out_of_range(self):
        """Test nth0 with out of range index."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("nth0(5, [a,b,c], X).")

    def test_nth0_negative_index(self):
        """Test nth0 with negative index."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("nth0(-1, [a,b,c], X).")

    def test_nth0_non_integer_index(self):
        """Test nth0 with non-integer index."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("nth0(atom, [a,b,c], X).")


class TestNth1:
    """Tests for nth1/3 predicate."""

    def test_nth1_get_element(self):
        """Test getting element at 1-based index."""
        prolog = PrologInterpreter()
        result = prolog.query_once("nth1(1, [a,b,c], X).")
        assert result is not None
        assert result['X'] == 'a'

        result = prolog.query_once("nth1(3, [a,b,c,d], X).")
        assert result is not None
        assert result['X'] == 'c'

    def test_nth1_zero_index(self):
        """Test nth1 with index 0."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("nth1(0, [a,b,c], X).")

    def test_nth1_negative_index(self):
        """Test nth1 with negative index."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("nth1(-1, [a,b,c], X).")


class TestLast:
    """Tests for last/2 predicate."""

    def test_last_basic(self):
        """Test getting last element."""
        prolog = PrologInterpreter()
        result = prolog.query_once("last([a,b,c], X).")
        assert result is not None
        assert result['X'] == 'c'

    def test_last_single_element(self):
        """Test last with single element."""
        prolog = PrologInterpreter()
        result = prolog.query_once("last([single], X).")
        assert result is not None
        assert result['X'] == 'single'

    def test_last_check_element(self):
        """Test checking if element is last."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("last([a,b,c], c).")
        assert not prolog.has_solution("last([a,b,c], b).")

    def test_last_empty_list(self):
        """Test last with empty list."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("last([], X).")

    def test_last_type_error(self):
        """Test last with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("last(atom, X).")


class TestSelect:
    """Tests for select/3 predicate."""

    def test_select_basic(self):
        """Test basic select functionality."""
        prolog = PrologInterpreter()
        result = prolog.query_once("select(b, [a,b,c], X).")
        assert result is not None
        assert result['X'] == ['a', 'c']

    def test_select_find_element(self):
        """Test finding element with select."""
        prolog = PrologInterpreter()
        result = prolog.query_once("select(X, [a,b,c], [a,c]).")
        assert result is not None
        assert result['X'] == 'b'

    def test_select_multiple_occurrences(self):
        """Test select with multiple occurrences."""
        prolog = PrologInterpreter()
        results = prolog.query("select(a, [a,b,a,c], R).")
        assert len(results) == 2
        remainders = [r['R'] for r in results]
        assert ['b', 'a', 'c'] in remainders
        assert ['a', 'b', 'c'] in remainders

    def test_select_generate(self):
        """Test generating all possibilities with select."""
        prolog = PrologInterpreter()
        results = prolog.query("select(X, [a,b,c], R).")
        assert len(results) == 3
        elements = sorted([r['X'] for r in results])
        assert elements == ['a', 'b', 'c']

    def test_select_type_error(self):
        """Test select with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("select(a, atom, X).")


class TestMemberchk:
    """Tests for memberchk/2 predicate."""

    def test_memberchk_basic(self):
        """Test basic memberchk functionality."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("memberchk(b, [a,b,c]).")
        assert not prolog.has_solution("memberchk(d, [a,b,c]).")

    def test_memberchk_deterministic(self):
        """Test that memberchk is deterministic (no backtracking)."""
        prolog = PrologInterpreter()
        results = prolog.query("memberchk(a, [a,b,a,c]).")
        assert len(results) == 1  # Only one solution, no choicepoint

    def test_memberchk_with_variable(self):
        """Test memberchk with variable element."""
        prolog = PrologInterpreter()
        result = prolog.query_once("memberchk(X, [a,b,c]).")
        assert result is not None
        assert result['X'] == 'a'  # Only first element

    def test_memberchk_type_error(self):
        """Test memberchk with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("memberchk(a, atom).")


class TestIsList:
    """Tests for is_list/1 predicate."""

    def test_is_list_empty(self):
        """Test is_list on empty list."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("is_list([]).")

    def test_is_list_proper(self):
        """Test is_list on proper list."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("is_list([a,b,c]).")

    def test_is_list_improper(self):
        """Test is_list on improper list."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list([a|b]).")

    def test_is_list_atom(self):
        """Test is_list on atom."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list(atom).")

    def test_is_list_variable(self):
        """Test is_list on unbound variable."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list(X).")


class TestSumlist:
    """Tests for sumlist/2 predicate."""

    def test_sumlist_basic(self):
        """Test basic sumlist functionality."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sumlist([1,2,3,4], X).")
        assert result is not None
        assert result['X'] == 10

    def test_sumlist_floats(self):
        """Test sumlist with floats."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sumlist([1.5, 2.5], X).")
        assert result is not None
        assert result['X'] == 4.0

    def test_sumlist_empty(self):
        """Test sumlist on empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sumlist([], X).")
        assert result is not None
        assert result['X'] == 0

    def test_sumlist_single(self):
        """Test sumlist on single element."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sumlist([5], X).")
        assert result is not None
        assert result['X'] == 5

    def test_sumlist_type_error_non_list(self):
        """Test sumlist with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("sumlist(atom, X).")

    def test_sumlist_type_error_non_number(self):
        """Test sumlist with non-number element."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("sumlist([1, atom, 3], X).")


class TestMaxList:
    """Tests for max_list/2 predicate."""

    def test_max_list_basic(self):
        """Test basic max_list functionality."""
        prolog = PrologInterpreter()
        result = prolog.query_once("max_list([3,1,4,1,5], X).")
        assert result is not None
        assert result['X'] == 5

    def test_max_list_single(self):
        """Test max_list on single element."""
        prolog = PrologInterpreter()
        result = prolog.query_once("max_list([1], X).")
        assert result is not None
        assert result['X'] == 1

    def test_max_list_negatives(self):
        """Test max_list with negative numbers."""
        prolog = PrologInterpreter()
        result = prolog.query_once("max_list([-5, -10, -3], X).")
        assert result is not None
        assert result['X'] == -3

    def test_max_list_empty_error(self):
        """Test max_list on empty list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("max_list([], X).")

    def test_max_list_type_error_non_list(self):
        """Test max_list with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("max_list(atom, X).")

    def test_max_list_type_error_non_number(self):
        """Test max_list with non-number element."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("max_list([1, atom, 3], X).")


class TestMinList:
    """Tests for min_list/2 predicate."""

    def test_min_list_basic(self):
        """Test basic min_list functionality."""
        prolog = PrologInterpreter()
        result = prolog.query_once("min_list([3,1,4,1,5], X).")
        assert result is not None
        assert result['X'] == 1

    def test_min_list_single(self):
        """Test min_list on single element."""
        prolog = PrologInterpreter()
        result = prolog.query_once("min_list([1], X).")
        assert result is not None
        assert result['X'] == 1

    def test_min_list_negatives(self):
        """Test min_list with negative numbers."""
        prolog = PrologInterpreter()
        result = prolog.query_once("min_list([-5, -10, -3], X).")
        assert result is not None
        assert result['X'] == -10

    def test_min_list_empty_error(self):
        """Test min_list on empty list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("min_list([], X).")

    def test_min_list_type_error_non_list(self):
        """Test min_list with non-list."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("min_list(atom, X).")

    def test_min_list_type_error_non_number(self):
        """Test min_list with non-number element."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("min_list([1, atom, 3], X).")