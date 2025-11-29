"""Tests for atom processing built-ins (ISO §8.16)."""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Number


class TestAtomChars:
    """Tests for atom_chars/2 predicate."""

    def test_atom_chars_decompose(self):
        """Test decomposing atom to character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_chars(hello, X).")
        assert result['X'] == ['h', 'e', 'l', 'l', 'o']

    def test_atom_chars_construct(self):
        """Test constructing atom from character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_chars(X, [h,e,l,l,o]).")
        assert result['X'] == 'hello'

    def test_atom_chars_unify_true(self):
        """Test atom_chars with both arguments instantiated (true case)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("atom_chars(hello, [h,e,l,l,o]).")

    def test_atom_chars_unify_false(self):
        """Test atom_chars with both arguments instantiated (false case)."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atom_chars(hello, [h,e,l,o]).")

    def test_atom_chars_empty_atom(self):
        """Test atom_chars with empty atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_chars('', X).")
        assert result['X'] == []

    def test_atom_chars_empty_list(self):
        """Test atom_chars with empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_chars(X, []).")
        assert result['X'] == ''

    def test_atom_chars_type_error_atom(self):
        """Test type error when first arg is not an atom."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("atom_chars(123, X).")
        assert "type_error" in str(exc_info.value)

    def test_atom_chars_type_error_list(self):
        """Test type error when second arg is not a list."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("atom_chars(hello, atom).")
        assert "type_error" in str(exc_info.value)

    def test_atom_chars_invalid_chars(self):
        """Test failure when list contains non-character atoms."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atom_chars(hello, [h,hello,o]).")


class TestAtomCodes:
    """Tests for atom_codes/2 predicate."""

    def test_atom_codes_decompose(self):
        """Test decomposing atom to code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_codes(hello, X).")
        assert result['X'] == [104, 101, 108, 108, 111]

    def test_atom_codes_construct(self):
        """Test constructing atom from code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_codes(X, [104,101,108,108,111]).")
        assert result['X'] == 'hello'

    def test_atom_codes_unify_true(self):
        """Test atom_codes with both arguments instantiated (true case)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("atom_codes('Hi', [72,105]).")

    def test_atom_codes_unify_false(self):
        """Test atom_codes with both arguments instantiated (false case)."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atom_codes('Hi', [72,106]).")

    def test_atom_codes_empty_atom(self):
        """Test atom_codes with empty atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_codes('', X).")
        assert result['X'] == []

    def test_atom_codes_empty_list(self):
        """Test atom_codes with empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_codes(X, []).")
        assert result['X'] == ''

    def test_atom_codes_type_error_atom(self):
        """Test type error when first arg is not an atom."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("atom_codes(123, X).")
        assert "type_error" in str(exc_info.value)

    def test_atom_codes_type_error_list(self):
        """Test type error when second arg is not a list."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("atom_codes(hello, atom).")
        assert "type_error" in str(exc_info.value)

    def test_atom_codes_invalid_codes(self):
        """Test failure when list contains non-integers."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atom_codes(hello, [104,hello,111]).")


class TestCharCode:
    """Tests for char_code/2 predicate."""

    def test_char_code_decompose(self):
        """Test getting character code from character."""
        prolog = PrologInterpreter()
        result = prolog.query_once("char_code(a, X).")
        assert result['X'] == 97

    def test_char_code_construct(self):
        """Test getting character from code."""
        prolog = PrologInterpreter()
        result = prolog.query_once("char_code(X, 65).")
        assert result['X'] == 'A'

    def test_char_code_unify_true(self):
        """Test char_code with both arguments instantiated (true case)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("char_code(a, 97).")

    def test_char_code_unify_false(self):
        """Test char_code with both arguments instantiated (false case)."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("char_code(a, 98).")

    def test_char_code_type_error_char(self):
        """Test type error when first arg is not a character."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("char_code(hello, X).")
        assert "type_error" in str(exc_info.value)

    def test_char_code_type_error_code(self):
        """Test type error when second arg is not an integer."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("char_code(a, hello).")
        assert "type_error" in str(exc_info.value)


class TestAtomLength:
    """Tests for atom_length/2 predicate."""

    def test_atom_length_get(self):
        """Test getting length of atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_length(hello, N).")
        assert result['N'] == 5

    def test_atom_length_check_true(self):
        """Test checking length of atom (true case)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("atom_length(hello, 5).")

    def test_atom_length_check_false(self):
        """Test checking length of atom (false case)."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atom_length(hello, 4).")

    def test_atom_length_empty(self):
        """Test length of empty atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_length('', N).")
        assert result['N'] == 0

    def test_atom_length_instantiation_error(self):
        """Test instantiation error when atom is uninstantiated."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("atom_length(X, 5).")
        assert "instantiation_error" in str(exc_info.value)

    def test_atom_length_type_error(self):
        """Test type error when first arg is not an atom."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("atom_length(123, N).")
        assert "type_error" in str(exc_info.value)


class TestAtomConcat:
    """Tests for atom_concat/3 predicate."""

    def test_atom_concat_concatenate(self):
        """Test concatenating two atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_concat(hello, world, X).")
        assert result['X'] == 'helloworld'

    def test_atom_concat_unify_true(self):
        """Test atom_concat with all args instantiated (true case)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("atom_concat(hello, world, helloworld).")

    def test_atom_concat_unify_false(self):
        """Test atom_concat with all args instantiated (false case)."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atom_concat(hello, world, goodbye).")

    def test_atom_concat_decompose(self):
        """Test decomposing atom into all possible splits."""
        prolog = PrologInterpreter()
        results = list(prolog.query("atom_concat(X, Y, helloworld)."))
        expected_splits = [
            ('', 'helloworld'),
            ('h', 'elloworld'),
            ('he', 'lloworld'),
            ('hel', 'loworld'),
            ('hell', 'oworld'),
            ('hello', 'world'),
            ('hellow', 'orld'),
            ('hellowo', 'rld'),
            ('hellowor', 'ld'),
            ('helloworl', 'd'),
            ('helloworld', ''),
        ]
        actual_splits = [(r['X'], r['Y']) for r in results]
        assert actual_splits == expected_splits

    def test_atom_concat_type_error(self):
        """Test type error when arguments are not atoms."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("atom_concat(123, world, X).")
        assert "type_error" in str(exc_info.value)


class TestSubAtom:
    """Tests for sub_atom/5 predicate."""

    def test_sub_atom_extract(self):
        """Test extracting subatom with specific position and length."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sub_atom(abracadabra, 0, 5, _, X).")
        assert result['X'] == 'abrac'

    def test_sub_atom_find_positions(self):
        """Test finding positions of substring."""
        prolog = PrologInterpreter()
        results = list(prolog.query("sub_atom(abracadabra, B, 2, A, ab)."))
        expected_results = [
            {'B': 0, 'A': 9},
            {'B': 7, 'A': 2},
        ]
        assert results == expected_results

    def test_sub_atom_middle(self):
        """Test extracting middle substring."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sub_atom(abracadabra, 3, 5, 3, S).")
        assert result['S'] == 'acada'

    def test_sub_atom_end(self):
        """Test extracting from end."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sub_atom(abracadabra, _, 5, 0, X).")
        assert result['X'] == 'dabra'

    def test_sub_atom_instantiation_error(self):
        """Test instantiation error when atom is uninstantiated."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("sub_atom(X, 0, 5, _, Y).")
        assert "instantiation_error" in str(exc_info.value)

    def test_sub_atom_type_error_atom(self):
        """Test type error when atom is not an atom."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("sub_atom(123, 0, 5, _, Y).")
        assert "type_error" in str(exc_info.value)

    def test_sub_atom_domain_error_negative(self):
        """Test domain error when position/length is negative."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("sub_atom(hello, -1, 5, _, Y).")
        assert "domain_error" in str(exc_info.value)

    def test_sub_atom_type_error_integer(self):
        """Test type error when position/length is not an integer."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("sub_atom(hello, atom, 5, _, Y).")
        assert "type_error" in str(exc_info.value)


class TestNumberChars:
    """Tests for number_chars/2 predicate."""

    def test_number_chars_decompose_integer(self):
        """Test decomposing integer to character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_chars(123, X).")
        assert result['X'] == ['1', '2', '3']

    def test_number_chars_decompose_negative(self):
        """Test decomposing negative integer to character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_chars(-456, X).")
        assert result['X'] == ['-', '4', '5', '6']

    def test_number_chars_decompose_float(self):
        """Test decomposing float to character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_chars(3.14, X).")
        assert result['X'] == ['3', '.', '1', '4']

    def test_number_chars_decompose_zero(self):
        """Test decomposing zero to character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_chars(0, X).")
        assert result['X'] == ['0']

    def test_number_chars_construct_integer(self):
        """Test constructing integer from character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_chars(X, ['4','2']).")
        assert result['X'] == 42

    def test_number_chars_construct_negative(self):
        """Test constructing negative integer from character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_chars(X, ['-','1','2','3']).")
        assert result['X'] == -123

    def test_number_chars_construct_float(self):
        """Test constructing float from character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_chars(X, ['3','.','1','4']).")
        assert result['X'] == 3.14

    def test_number_chars_unify_true(self):
        """Test number_chars with both arguments instantiated (true case)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("number_chars(123, ['1','2','3']).")

    def test_number_chars_unify_false(self):
        """Test number_chars with both arguments instantiated (false case)."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("number_chars(123, ['1','2','4']).")

    def test_number_chars_type_error_number(self):
        """Test type error when first arg is not a number."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_chars(hello, X).")
        assert "type_error" in str(exc_info.value)

    def test_number_chars_type_error_list(self):
        """Test type error when second arg is not a list."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_chars(123, atom).")
        assert "type_error" in str(exc_info.value)

    def test_number_chars_syntax_error_invalid(self):
        """Test syntax error for invalid number format."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_chars(X, ['1','2','a']).")
        assert "syntax_error" in str(exc_info.value)

    def test_number_chars_invalid_chars(self):
        """Test type error when list contains non-character atoms."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_chars(X, ['1','2',hello]).")
        assert "type_error" in str(exc_info.value)
        assert "character" in str(exc_info.value)


class TestNumberCodes:
    """Tests for number_codes/2 predicate."""

    def test_number_codes_decompose_integer(self):
        """Test decomposing integer to code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_codes(123, X).")
        assert result['X'] == [49, 50, 51]  # ASCII codes for '1','2','3'

    def test_number_codes_decompose_negative(self):
        """Test decomposing negative integer to code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_codes(-456, X).")
        assert result['X'] == [45, 52, 53, 54]  # ASCII for '-','4','5','6'

    def test_number_codes_decompose_float(self):
        """Test decomposing float to code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_codes(3.14, X).")
        assert result['X'] == [51, 46, 49, 52]  # ASCII for '3','.','1','4'

    def test_number_codes_construct_integer(self):
        """Test constructing integer from code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_codes(X, [52,50]).")
        assert result['X'] == 42

    def test_number_codes_construct_negative(self):
        """Test constructing negative integer from code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_codes(X, [45,49,50,51]).")
        assert result['X'] == -123

    def test_number_codes_construct_float(self):
        """Test constructing float from code list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("number_codes(X, [51,46,49,52]).")
        assert result['X'] == 3.14

    def test_number_codes_unify_true(self):
        """Test number_codes with both arguments instantiated (true case)."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("number_codes(123, [49,50,51]).")

    def test_number_codes_unify_false(self):
        """Test number_codes with both arguments instantiated (false case)."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("number_codes(123, [49,50,52]).")

    def test_number_codes_type_error_number(self):
        """Test type error when first arg is not a number."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_codes(hello, X).")
        assert "type_error" in str(exc_info.value)

    def test_number_codes_type_error_list(self):
        """Test type error when second arg is not a list."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_codes(123, atom).")
        assert "type_error" in str(exc_info.value)

    def test_number_codes_syntax_error_invalid(self):
        """Test syntax error for invalid number format."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_codes(X, [49,50,97]).")  # '1','2','a'
        assert "syntax_error" in str(exc_info.value)

    def test_number_codes_invalid_codes(self):
        """Test type error when list contains non-integers."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_codes(X, [49,50,hello]).")
        assert "type_error" in str(exc_info.value)
        assert "integer" in str(exc_info.value)

    def test_number_codes_invalid_character_code(self):
        """Test type error when list contains invalid character code."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once("number_codes(X, [1114112]).")
        assert "type_error" in str(exc_info.value)
        assert "character_code" in str(exc_info.value)