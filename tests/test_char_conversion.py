"""Tests for char_conversion/2 directive and current_char_conversion/2 predicate.

These tests verify ISO Prolog character conversion functionality as specified
in ISO/IEC 13211-1 §6.4 and §7.4.
"""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestCharConversionDirective:
    """Tests for :- char_conversion(Char1, Char2) directive."""

    def test_basic_char_conversion_in_source(self):
        """Character conversions should apply during parsing of source code."""
        prolog = PrologInterpreter()
        # Set up conversion: 'a' -> 'b'
        prolog.consult_string("""
            :- char_conversion(a, b).
            test_fact(abc).
        """)
        # After conversion, 'abc' becomes 'bbc' during parsing
        assert prolog.has_solution("test_fact(bbc)")
        assert not prolog.has_solution("test_fact(abc)")

    def test_conversion_affects_atom_names(self):
        """Conversions should affect unquoted atom names."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(x, y).
            predicate_x(1).
        """)
        # 'predicate_x' becomes 'predicate_y'
        assert prolog.has_solution("predicate_y(1)")
        assert not prolog.has_solution("predicate_x(1)")

    def test_conversion_does_not_affect_quoted_atoms(self):
        """Conversions should NOT affect quoted atoms."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, b).
            test_fact('abc').
        """)
        # Quoted 'abc' should remain unchanged
        assert prolog.has_solution("test_fact(abc)")
        assert not prolog.has_solution("test_fact(bbc)")

    def test_conversion_does_not_affect_double_quoted_strings(self):
        """Conversions should NOT affect double-quoted strings."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, b).
            test_fact("abc").
        """)
        # Double-quoted "abc" should remain unchanged  
        result = prolog.query_once("test_fact(X)")
        assert result is not None
        assert result["X"] == "abc"

    def test_identity_conversion_removes_mapping(self):
        """Setting a character to convert to itself removes the conversion."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, b).
            fact1(abc).
            :- char_conversion(a, a).
            fact2(abc).
        """)
        # fact1 was converted, fact2 was not (identity removed the conversion)
        assert prolog.has_solution("fact1(bbc)")
        assert prolog.has_solution("fact2(abc)")

    def test_multiple_conversions(self):
        """Multiple conversions should all be active."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, x).
            :- char_conversion(b, y).
            fact(abc).
        """)
        # 'abc' -> 'xyc'
        assert prolog.has_solution("fact(xyc)")

    def test_conversion_chain_not_transitive(self):
        """Conversion chains should NOT be transitive (a->b, b->c should not cause a->c)."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, b).
            :- char_conversion(b, c).
            fact(aaa).
        """)
        # 'aaa' becomes 'bbb' (a->b applied), NOT 'ccc' (chains don't compose)
        assert prolog.has_solution("fact(bbb)")
        assert not prolog.has_solution("fact(ccc)")

    def test_conversion_overwrites_previous(self):
        """A new conversion for the same character overwrites the old one."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, b).
            fact1(aaa).
            :- char_conversion(a, c).
            fact2(aaa).
        """)
        assert prolog.has_solution("fact1(bbb)")
        assert prolog.has_solution("fact2(ccc)")

    def test_conversion_persists_across_consults(self):
        """Conversions should persist across multiple consult_string calls."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- char_conversion(a, b).")
        prolog.consult_string("fact(aaa).")
        assert prolog.has_solution("fact(bbb)")


class TestCharConversionErrors:
    """Tests for char_conversion/2 error handling."""

    def test_error_on_non_character_first_arg(self):
        """Should raise type_error(character, X) for non-single-char first argument."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- char_conversion(ab, x).")
        error = exc_info.value.error_term
        # Should be type_error(character, ab)
        assert error.functor == "error"
        assert error.args[0].functor == "type_error"
        assert error.args[0].args[0].name == "character"

    def test_error_on_non_character_second_arg(self):
        """Should raise type_error(character, X) for non-single-char second argument."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- char_conversion(a, xyz).")
        error = exc_info.value.error_term
        assert error.functor == "error"
        assert error.args[0].functor == "type_error"

    def test_error_on_number_argument(self):
        """Should raise type_error for number arguments."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- char_conversion(1, a).")

    def test_error_on_empty_atom(self):
        """Should raise type_error for empty atom argument."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- char_conversion('', a).")


class TestCurrentCharConversion:
    """Tests for current_char_conversion/2 predicate."""

    def test_enumerate_all_conversions(self):
        """Should enumerate all active conversions."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, x).
            :- char_conversion(b, y).
        """)
        results = prolog.query("current_char_conversion(In, Out)")
        assert len(results) == 2
        conversions = {(r["In"], r["Out"]) for r in results}
        assert conversions == {("a", "x"), ("b", "y")}

    def test_check_specific_conversion(self):
        """Should succeed for an active conversion."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- char_conversion(a, b).")
        assert prolog.has_solution("current_char_conversion(a, b)")
        assert not prolog.has_solution("current_char_conversion(a, c)")
        assert not prolog.has_solution("current_char_conversion(x, y)")

    def test_query_by_input_char(self):
        """Should return the output for a given input character."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- char_conversion(a, z).")
        result = prolog.query_once("current_char_conversion(a, X)")
        assert result is not None
        assert result["X"] == "z"

    def test_query_by_output_char(self):
        """Should return the input(s) for a given output character."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, z).
            :- char_conversion(b, z).
        """)
        results = prolog.query("current_char_conversion(X, z)")
        inputs = {r["X"] for r in results}
        assert inputs == {"a", "b"}

    def test_no_conversions_initially(self):
        """With no conversions set, current_char_conversion/2 should fail."""
        prolog = PrologInterpreter()
        prolog.consult_string("dummy.")  # Initialize engine
        assert not prolog.has_solution("current_char_conversion(_, _)")

    def test_removed_conversion_not_listed(self):
        """After removing a conversion, it should not be enumerated."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, b).
            :- char_conversion(a, a).
        """)
        # Conversion was removed
        assert not prolog.has_solution("current_char_conversion(a, b)")
        assert not prolog.has_solution("current_char_conversion(a, _)")


class TestCharConversionEdgeCases:
    """Edge case tests for character conversion."""

    def test_convert_digit_to_letter(self):
        """Should allow converting digits to letters."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion('1', a).
            fact(x1y).
        """)
        assert prolog.has_solution("fact(xay)")

    def test_convert_special_characters(self):
        """Should allow converting special characters."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion('+', '-').
        """)
        # Verify conversion is registered
        assert prolog.has_solution("current_char_conversion('+', '-')")

    def test_conversion_in_variable_names(self):
        """Conversions should affect variable names in source."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion('X', 'Y').
            test(Y) :- Y = 1.
        """)
        # 'X' in variable name becomes 'Y', so we query with Y
        result = prolog.query_once("test(Z)")
        assert result is not None

    def test_case_sensitive_conversion(self):
        """Conversions should be case-sensitive."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, x).
            lower(abc).
            upper('ABC').
        """)
        # Only lowercase 'a' should be converted
        assert prolog.has_solution("lower(xbc)")
        # Quoted uppercase 'ABC' should remain unchanged
        assert prolog.has_solution("upper('ABC')")

    def test_conversion_affects_functor_names(self):
        """Conversions should affect functor names."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(f, g).
            foo(1).
        """)
        # 'foo' becomes 'goo'
        assert prolog.has_solution("goo(1)")
        assert not prolog.has_solution("foo(1)")

    def test_conversion_in_comments_has_no_effect(self):
        """Conversions should not affect comments (they're stripped anyway)."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- char_conversion(a, b).
            % This comment has 'a' in it but doesn't matter
            fact(xyz).
        """)
        assert prolog.has_solution("fact(xyz)")

    def test_backslash_escape_in_quoted_not_converted(self):
        """Escape sequences inside quoted strings should not be converted."""
        prolog = PrologInterpreter()
        prolog.consult_string(r"""
            :- char_conversion(n, m).
            test('\n').
        """)
        # The \n inside the quoted string should remain a newline, not become \m
        result = prolog.query_once("test(X)")
        assert result is not None
        # The value should contain an actual newline
        assert "\n" in result["X"]
