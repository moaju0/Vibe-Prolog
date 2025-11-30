"""Tests for list operation built-ins (msort, keysort, nth0/1, last, select, memberchk, is_list, aggregations)."""

import pytest
from vibeprolog import PrologInterpreter


@pytest.fixture
def prolog():
    """Provides a PrologInterpreter instance for tests."""
    return PrologInterpreter()


class TestMsort:
    """Tests for msort/2 predicate."""

    def test_msort_basic(self, prolog):
        """Test basic msort functionality."""
        result = prolog.query_once("msort([3,1,2,1], X).")
        assert result is not None
        assert result['X'] == [1, 1, 2, 3]

    def test_msort_keeps_duplicates(self, prolog):
        """Test that msort keeps duplicate elements (unlike sort/2)."""
        result = prolog.query_once("msort([b,a,c,a], X).")
        assert result is not None
        assert result['X'] == ['a', 'a', 'b', 'c']

    def test_msort_empty_list(self, prolog):
        """Test msort on empty list."""
        result = prolog.query_once("msort([], X).")
        assert result is not None
        assert result['X'] == []

    def test_msort_single_element(self, prolog):
        """Test msort on single element list."""
        result = prolog.query_once("msort([single], X).")
        assert result is not None
        assert result['X'] == ['single']

    def test_msort_instantiation_error(self, prolog):
        """Test msort with uninstantiated list."""
        with pytest.raises(Exception):  # Should raise instantiation_error
            prolog.query_once("msort(X, Y).")

    def test_msort_type_error(self, prolog):
        """Test msort with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("msort(atom, X).")


class TestKeysort:
    """Tests for keysort/2 predicate."""

    def test_keysort_basic(self, prolog):
        """Test basic keysort functionality."""
        result = prolog.query_once("keysort([3-a, 1-b, 2-c, 1-d], X).")
        assert result is not None
        expected = [
            {'-': [1, 'b']},
            {'-': [1, 'd']},
            {'-': [2, 'c']},
            {'-': [3, 'a']}
        ]
        assert result['X'] == expected

    def test_keysort_stable(self, prolog):
        """Test that keysort is stable (preserves order of equal keys)."""
        result = prolog.query_once("keysort([b-2, a-1, c-3], X).")
        assert result is not None
        expected = [
            {'-': ['a', 1]},
            {'-': ['b', 2]},
            {'-': ['c', 3]}
        ]
        assert result['X'] == expected

    def test_keysort_empty_list(self, prolog):
        """Test keysort on empty list."""
        result = prolog.query_once("keysort([], X).")
        assert result is not None
        assert result['X'] == []

    def test_keysort_type_error_non_list(self, prolog):
        """Test keysort with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("keysort(atom, X).")

    def test_keysort_type_error_non_pair(self, prolog):
        """Test keysort with non-pair element."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("keysort([not_a_pair], X).")


class TestNth0:
    """Tests for nth0/3 predicate."""

    def test_nth0_get_element(self, prolog):
        """Test getting element at index."""
        result = prolog.query_once("nth0(0, [a,b,c], X).")
        assert result is not None
        assert result['X'] == 'a'

        result = prolog.query_once("nth0(2, [a,b,c,d], X).")
        assert result is not None
        assert result['X'] == 'c'

    def test_nth0_find_index(self, prolog):
        """Test finding index of element."""
        results = prolog.query("nth0(I, [a,b,c], b).")
        assert len(results) == 1
        assert results[0]['I'] == 1

        results = prolog.query("nth0(I, [a,b,a], a).")
        assert len(results) == 2
        indices = sorted([r['I'] for r in results])
        assert indices == [0, 2]

    def test_nth0_out_of_range(self, prolog):
        """Test nth0 with out of range index."""
        assert not prolog.has_solution("nth0(5, [a,b,c], X).")

    def test_nth0_negative_index(self, prolog):
        """Test nth0 with negative index."""
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("nth0(-1, [a,b,c], X).")

    def test_nth0_non_integer_index(self, prolog):
        """Test nth0 with non-integer index."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("nth0(atom, [a,b,c], X).")

    def test_nth0_generate_list(self, prolog):
        """Test nth0/3 generating lists with element at specific index (relational mode)."""
        # Generate a list with 'x' at index 0
        result = prolog.query_once("nth0(0, L, x).")
        assert result is not None
        assert result['L'][0] == 'x'
        assert len(result['L']) >= 1

        # Generate a list with 'y' at index 2
        result = prolog.query_once("nth0(2, L, y).")
        assert result is not None
        assert len(result['L']) >= 3
        assert result['L'][2] == 'y'

    def test_nth0_backtrack_indices(self, prolog):
        """Test nth0/3 finding all indices of an element through backtracking."""
        # Find all positions where 'a' appears in [a,b,a,c]
        results = prolog.query("nth0(I, [a,b,a,c], a).")
        indices = sorted([r['I'] for r in results])
        assert indices == [0, 2]

    def test_nth0_backtrack_elements(self, prolog):
        """Test nth0/3 enumerating all elements with their indices."""
        results = prolog.query("nth0(I, [a,b,c], E).")
        assert len(results) == 3

        # Check we get all index-element pairs
        pairs = [(r['I'], r['E']) for r in results]
        assert (0, 'a') in pairs
        assert (1, 'b') in pairs
        assert (2, 'c') in pairs


class TestNth1:
    """Tests for nth1/3 predicate."""

    def test_nth1_get_element(self, prolog):
        """Test getting element at 1-based index."""
        result = prolog.query_once("nth1(1, [a,b,c], X).")
        assert result is not None
        assert result['X'] == 'a'

        result = prolog.query_once("nth1(3, [a,b,c,d], X).")
        assert result is not None
        assert result['X'] == 'c'

    def test_nth1_zero_index(self, prolog):
        """Test nth1 with index 0."""
        assert not prolog.has_solution("nth1(0, [a,b,c], X).")

    def test_nth1_negative_index(self, prolog):
        """Test nth1 with negative index."""
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("nth1(-1, [a,b,c], X).")

    def test_nth1_generate_list(self, prolog):
        """Test nth1/3 generating lists with element at specific index (relational mode)."""
        # Generate a list with 'x' at index 1 (first position)
        result = prolog.query_once("nth1(1, L, x).")
        assert result is not None
        assert result['L'][0] == 'x'  # 1-based index 1 = 0-based index 0
        assert len(result['L']) >= 1

        # Generate a list with 'y' at index 3 (third position)
        result = prolog.query_once("nth1(3, L, y).")
        assert result is not None
        assert len(result['L']) >= 3
        assert result['L'][2] == 'y'  # 1-based index 3 = 0-based index 2

    def test_nth1_backtrack_indices(self, prolog):
        """Test nth1/3 finding all 1-based indices of an element through backtracking."""
        # Find all 1-based positions where 'a' appears in [a,b,a,c]
        results = prolog.query("nth1(I, [a,b,a,c], a).")
        indices = sorted([r['I'] for r in results])
        assert indices == [1, 3]  # 1-based indices

    def test_nth1_backtrack_elements(self, prolog):
        """Test nth1/3 enumerating all elements with their 1-based indices."""
        results = prolog.query("nth1(I, [a,b,c], E).")
        assert len(results) == 3

        # Check we get all index-element pairs (1-based)
        pairs = [(r['I'], r['E']) for r in results]
        assert (1, 'a') in pairs
        assert (2, 'b') in pairs
        assert (3, 'c') in pairs


class TestLast:
    """Tests for last/2 predicate."""

    def test_last_basic(self, prolog):
        """Test getting last element."""
        result = prolog.query_once("last([a,b,c], X).")
        assert result is not None
        assert result['X'] == 'c'

    def test_last_single_element(self, prolog):
        """Test last with single element."""
        result = prolog.query_once("last([single], X).")
        assert result is not None
        assert result['X'] == 'single'

    def test_last_check_element(self, prolog):
        """Test checking if element is last."""
        assert prolog.has_solution("last([a,b,c], c).")
        assert not prolog.has_solution("last([a,b,c], b).")

    def test_last_empty_list(self, prolog):
        """Test last with empty list."""
        assert not prolog.has_solution("last([], X).")

    def test_last_type_error(self, prolog):
        """Test last with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("last(atom, X).")

    def test_last_generate_lists(self, prolog):
        """Test last/2 generating lists ending with a given element (relational mode)."""
        # Generate first 3 lists ending with 'z' using limit parameter
        results = prolog.query("last(L, z).", limit=3)

        # Check we got 3 solutions
        assert len(results) == 3

        # First solution should be [z]
        assert results[0]['L'] == ['z']

        # Second solution should be [_, z] (one variable + z)
        assert len(results[1]['L']) == 2
        assert results[1]['L'][1] == 'z'

        # Third solution should be [_, _, z] (two variables + z)
        assert len(results[2]['L']) == 3
        assert results[2]['L'][2] == 'z'


class TestSelect:
    """Tests for select/3 predicate."""

    def test_select_basic(self, prolog):
        """Test basic select functionality."""
        result = prolog.query_once("select(b, [a,b,c], X).")
        assert result is not None
        assert result['X'] == ['a', 'c']

    def test_select_find_element(self, prolog):
        """Test finding element with select."""
        result = prolog.query_once("select(X, [a,b,c], [a,c]).")
        assert result is not None
        assert result['X'] == 'b'

    def test_select_multiple_occurrences(self, prolog):
        """Test select with multiple occurrences."""
        results = prolog.query("select(a, [a,b,a,c], R).")
        assert len(results) == 2
        remainders = [r['R'] for r in results]
        assert ['b', 'a', 'c'] in remainders
        assert ['a', 'b', 'c'] in remainders

    def test_select_generate(self, prolog):
        """Test generating all possibilities with select."""
        results = prolog.query("select(X, [a,b,c], R).")
        assert len(results) == 3
        elements = sorted([r['X'] for r in results])
        assert elements == ['a', 'b', 'c']

    def test_select_type_error(self, prolog):
        """Test select with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("select(a, atom, X).")

    def test_select_insert_mode(self, prolog):
        """Test select/3 in insert mode (relational): select(?Elem, -List, +Remainder)."""
        # Insert 'x' at all positions in [a,b]
        results = prolog.query("select(x, L, [a,b]).")
        assert len(results) == 3  # Three positions: beginning, middle, end

        lists = sorted([r['L'] for r in results])
        # Should generate: [x,a,b], [a,x,b], [a,b,x]
        assert ['a', 'b', 'x'] in lists
        assert ['a', 'x', 'b'] in lists
        assert ['x', 'a', 'b'] in lists

    def test_select_insert_empty_list(self, prolog):
        """Test select/3 inserting into empty list."""
        result = prolog.query_once("select(x, L, []).")
        assert result is not None
        assert result['L'] == ['x']

    def test_select_bidirectional(self, prolog):
        """Test select/3 works bidirectionally."""
        # Forward: remove from list
        result1 = prolog.query_once("select(b, [a,b,c], R).")
        assert result1 is not None
        assert result1['R'] == ['a', 'c']

        # Backward: insert into remainder
        result2 = prolog.query_once("select(b, L, [a,c]).")
        assert result2 is not None
        # One of the valid insertions
        assert 'b' in result2['L']
        assert len(result2['L']) == 3

    def test_select_instantiation_error(self, prolog):
        """Test select/3 with both list and remainder unbound."""
        with pytest.raises(Exception):  # Should raise instantiation_error
            prolog.query_once("select(x, L, R).")


class TestMemberchk:
    """Tests for memberchk/2 predicate."""

    def test_memberchk_basic(self, prolog):
        """Test basic memberchk functionality."""
        assert prolog.has_solution("memberchk(b, [a,b,c]).")
        assert not prolog.has_solution("memberchk(d, [a,b,c]).")

    def test_memberchk_deterministic(self, prolog):
        """Test that memberchk is deterministic (no backtracking)."""
        results = prolog.query("memberchk(a, [a,b,a,c]).")
        assert len(results) == 1  # Only one solution, no choicepoint

    def test_memberchk_with_variable(self, prolog):
        """Test memberchk with variable element."""
        result = prolog.query_once("memberchk(X, [a,b,c]).")
        assert result is not None
        assert result['X'] == 'a'  # Only first element

    def test_memberchk_type_error(self, prolog):
        """Test memberchk with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("memberchk(a, atom).")


class TestIsList:
    """Tests for is_list/1 predicate."""

    def test_is_list_empty(self, prolog):
        """Test is_list on empty list."""
        assert prolog.has_solution("is_list([]).")

    def test_is_list_proper(self, prolog):
        """Test is_list on proper list."""
        assert prolog.has_solution("is_list([a,b,c]).")

    def test_is_list_improper(self, prolog):
        """Test is_list on improper list."""
        assert not prolog.has_solution("is_list([a|b]).")

    def test_is_list_atom(self, prolog):
        """Test is_list on atom."""
        assert not prolog.has_solution("is_list(atom).")

    def test_is_list_variable(self, prolog):
        """Test is_list on unbound variable."""
        assert not prolog.has_solution("is_list(X).")


class TestSumlist:
    """Tests for sumlist/2 predicate."""

    def test_sumlist_basic(self, prolog):
        """Test basic sumlist functionality."""
        result = prolog.query_once("sumlist([1,2,3,4], X).")
        assert result is not None
        assert result['X'] == 10

    def test_sumlist_floats(self, prolog):
        """Test sumlist with floats."""
        result = prolog.query_once("sumlist([1.5, 2.5], X).")
        assert result is not None
        assert result['X'] == 4.0

    def test_sumlist_empty(self, prolog):
        """Test sumlist on empty list."""
        result = prolog.query_once("sumlist([], X).")
        assert result is not None
        assert result['X'] == 0

    def test_sumlist_single(self, prolog):
        """Test sumlist on single element."""
        result = prolog.query_once("sumlist([5], X).")
        assert result is not None
        assert result['X'] == 5

    def test_sumlist_type_error_non_list(self, prolog):
        """Test sumlist with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("sumlist(atom, X).")

    def test_sumlist_type_error_non_number(self, prolog):
        """Test sumlist with non-number element."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("sumlist([1, atom, 3], X).")


class TestMaxList:
    """Tests for max_list/2 predicate."""

    def test_max_list_basic(self, prolog):
        """Test basic max_list functionality."""
        result = prolog.query_once("max_list([3,1,4,1,5], X).")
        assert result is not None
        assert result['X'] == 5

    def test_max_list_single(self, prolog):
        """Test max_list on single element."""
        result = prolog.query_once("max_list([1], X).")
        assert result is not None
        assert result['X'] == 1

    def test_max_list_negatives(self, prolog):
        """Test max_list with negative numbers."""
        result = prolog.query_once("max_list([-5, -10, -3], X).")
        assert result is not None
        assert result['X'] == -3

    def test_max_list_empty_error(self, prolog):
        """Test max_list on empty list."""
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("max_list([], X).")

    def test_max_list_type_error_non_list(self, prolog):
        """Test max_list with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("max_list(atom, X).")

    def test_max_list_type_error_non_number(self, prolog):
        """Test max_list with non-number element."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("max_list([1, atom, 3], X).")


class TestMinList:
    """Tests for min_list/2 predicate."""

    def test_min_list_basic(self, prolog):
        """Test basic min_list functionality."""
        result = prolog.query_once("min_list([3,1,4,1,5], X).")
        assert result is not None
        assert result['X'] == 1

    def test_min_list_single(self, prolog):
        """Test min_list on single element."""
        result = prolog.query_once("min_list([1], X).")
        assert result is not None
        assert result['X'] == 1

    def test_min_list_negatives(self, prolog):
        """Test min_list with negative numbers."""
        result = prolog.query_once("min_list([-5, -10, -3], X).")
        assert result is not None
        assert result['X'] == -10

    def test_min_list_empty_error(self, prolog):
        """Test min_list on empty list."""
        with pytest.raises(Exception):  # Should raise domain_error
            prolog.query_once("min_list([], X).")

    def test_min_list_type_error_non_list(self, prolog):
        """Test min_list with non-list."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("min_list(atom, X).")

    def test_min_list_type_error_non_number(self, prolog):
        """Test min_list with non-number element."""
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("min_list([1, atom, 3], X).")