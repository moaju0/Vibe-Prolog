"""
Tests for ISO Prolog escape sequences in strings, quoted atoms, and character codes.
"""

import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.parser import PrologParser


class TestEscapeSequences:
    """Test ISO escape sequences in various contexts."""

    def test_single_char_escapes_in_strings(self):
        """Test single character escape sequences in double-quoted strings."""
        prolog = PrologInterpreter()
        # Test various single char escapes
        test_cases = [
            ('"\\a"', '\x07'),  # alert/bell
            ('"\\b"', '\x08'),  # backspace
            ('"\\t"', '\t'),    # tab
            ('"\\n"', '\n'),    # newline
            ('"\\v"', '\x0b'),  # vertical tab
            ('"\\f"', '\x0c'),  # form feed
            ('"\\r"', '\r'),    # carriage return
            ('"\\e"', '\x1b'),  # escape
            ('"\\d"', '\x7f'),  # delete
            ('"\\s"', ' '),     # space
        ]

        for prolog_str, expected in test_cases:
            result = prolog.query_once(f"X = {prolog_str}.")
            assert result is not None
            assert result['X'] == expected

    def test_single_char_escapes_in_quoted_atoms(self):
        """Test single character escape sequences in single-quoted atoms."""
        prolog = PrologInterpreter()
        test_cases = [
            ("'\\a'", '\x07'),  # alert/bell
            ("'\\b'", '\x08'),  # backspace
            ("'\\t'", '\t'),    # tab
            ("'\\n'", '\n'),    # newline
            ("'\\v'", '\x0b'),  # vertical tab
            ("'\\f'", '\x0c'),  # form feed
            ("'\\r'", '\r'),    # carriage return
            ("'\\e'", '\x1b'),  # escape
            ("'\\d'", '\x7f'),  # delete
            ("'\\s'", ' '),     # space
        ]

        for prolog_str, expected in test_cases:
            result = prolog.query_once(f"X = {prolog_str}.")
            assert result is not None
            assert result['X'] == expected

    def test_octal_escapes(self):
        """Test octal escape sequences."""
        prolog = PrologInterpreter()
        test_cases = [
            ('"\\0"', '\x00'),
            ('"\\7"', '\x07'),
            ('"\\10"', '\x08'),
            ('"\\77"', '\x3f'),
            ('"\\100"', '\x40'),
            ('"\\177"', '\x7f'),
            ('"\\200"', '\x80'),
            ('"\\377"', '\xff'),
        ]

        for prolog_str, expected in test_cases:
            result = prolog.query_once(f"X = {prolog_str}.")
            assert result is not None
            assert result['X'] == expected

    def test_hex_escapes(self):
        """Test hexadecimal escape sequences."""
        prolog = PrologInterpreter()
        test_cases = [
            ('"\\x41"', 'A'),
            ('"\\xFF"', '\xff'),
            ('"\\x100"', '\u0100'),  # 256 = 0x100
            ('"\\x41\\"', 'A'),      # Explicit closing backslash
        ]

        for prolog_str, expected in test_cases:
            result = prolog.query_once(f"X = {prolog_str}.")
            assert result is not None
            assert result['X'] == expected

    def test_unicode_escapes(self):
        """Test Unicode escape sequences."""
        prolog = PrologInterpreter()
        test_cases = [
            ('"\\u0000"', '\u0000'),
            ('"\\u0020"', '\u0020'),
            ('"\\u0041"', 'A'),
            ('"\\uFFFF"', '\uffff'),
            ("'\\u0021'", "!"),
        ]

        for prolog_str, expected in test_cases:
            result = prolog.query_once(f"X = {prolog_str}.")
            assert result is not None
            assert result['X'] == expected

    def test_quoting_escapes(self):
        """Test quote-related escape sequences."""
        prolog = PrologInterpreter()
        test_cases = [
            ('"\\""', '"'),       # escaped double quote in double-quoted string
            ("'\\''", "'"),       # escaped single quote in single-quoted atom
            ('"\\\\"', '\\'),     # escaped backslash
            ("'\\\\'", '\\'),     # escaped backslash in single quotes
        ]

        for prolog_str, expected in test_cases:
            result = prolog.query_once(f"X = {prolog_str}.")
            assert result is not None
            assert result['X'] == expected

    def test_doubled_quotes(self):
        """Test doubled quotes (not escaped)."""
        prolog = PrologInterpreter()
        # In Prolog, '' in single-quoted atoms represents a single '
        result = prolog.query_once("X = 'Don''t'.")
        assert result is not None
        assert result['X'] == "Don't"

    def test_char_code_escapes(self):
        """Test escape sequences in character codes like 0'\\n."""
        prolog = PrologInterpreter()
        test_cases = [
            ("0'\\n'", 10),      # newline
            ("0'\\t'", 9),       # tab
            ("0'\\a'", 7),       # bell
            ("0'\\e'", 27),      # escape
            ("0'\\0'", 0),       # null
            ("0'\\177'", 127),   # del
            ("0'\\x41'", 65),    # 'A'
            ("0'\\u0041'", 65),  # 'A' unicode
            ("0'\\x41\\'", 65),  # 'A' with trailing backslash terminator
        ]

        for prolog_str, expected in test_cases:
            result = prolog.query_once(f"X = {prolog_str}.")
            assert result is not None
            assert result['X'] == expected

    def test_writeq_escapes(self):
        """Test that writeq properly displays escape sequences."""
        prolog = PrologInterpreter()
        # writeq should show the escaped form
        # This is more of an integration test
        result = prolog.query_once("writeq('\\n').")
        # The result should succeed (no exception)
        assert result is not None

    def test_atom_codes_with_escapes(self):
        """Test atom_codes/2 with escaped atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_codes('\\n\\t', Codes).")
        assert result is not None
        assert result['Codes'] == [10, 9]  # \n\t


    def test_complex_escapes(self):
        """Test complex combinations of escapes."""
        prolog = PrologInterpreter()
        # String with multiple escapes
        result = prolog.query_once("X = \"\\t\\n\\r\\a\\b\".")
        assert result is not None
        expected = "\t\n\r\x07\x08"
        assert result['X'] == expected

    def test_escape_in_atom_names(self):
        """Test escapes in atom names used as functors."""
        parser = PrologParser()
        # This should parse correctly
        result = parser.parse("foo('\\n', bar).")
        assert len(result) == 1
        clause = result[0]
        assert clause.head.functor == "foo"
        assert len(clause.head.args) == 2
        # First arg should be the escaped atom
        assert str(clause.head.args[0]) == '\n'

    def test_line_continuation_in_strings(self):
        """Backslash-newline should continue strings without embedding newline."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = \"line\\\n  more\".")
        assert result is not None
        assert result['X'] == "linemore"

    def test_line_continuation_in_atoms(self):
        """Backslash-newline should continue quoted atoms."""
        parser = PrologParser()
        clauses = parser.parse("value('path\\\n    segment').")
        assert clauses[0].head.args[0].name == "pathsegment"

    def test_writeq_preserves_escapes(self, capsys: pytest.CaptureFixture[str]):
        """writeq/1 should output escape sequences, not raw control characters."""
        prolog = PrologInterpreter()
        prolog.query_once("writeq('a\\\\nb').", capture_output=True)
        output = capsys.readouterr().out
        assert "\\n" in output
        assert "a" in output and "b" in output

    def test_invalid_escapes(self):
        """Test that invalid escape sequences raise syntax errors."""
        prolog = PrologInterpreter()

        # Invalid escape in string
        with pytest.raises(Exception):
            prolog.query_once("X = \"\\z\".")

        # Invalid escape in char code
        with pytest.raises(Exception):
            prolog.query_once("X = 0'\\z'.")

        # Incomplete Unicode escape
        with pytest.raises(Exception):
            prolog.query_once("X = \"\\u1\".")

        # Invalid hex digit in Unicode
        with pytest.raises(Exception):
            prolog.query_once("X = \"\\uGGGG\".")

        # Trailing characters in char code literal
        with pytest.raises(Exception):
            prolog.query_once("X = 0'\\na'.")

        # \c is invalid in a char code literal
        with pytest.raises(Exception):
            prolog.query_once("X = 0'\\c'.")

    def test_conformity_test_cases(self):
        """Test the specific cases from the conformity tests that were failing."""
        prolog = PrologInterpreter()

        # Test cases from conformity_test.py that should now work
        test_queries = [
            "writeq('\\n').",
            "writeq('a\\nb').",
            "writeq('\\s').",
            "writeq('\\t').",
            "writeq('\\a').",
            "char_code('\\e', C).",
            "char_code('\\d', C).",
            "writeq('\\u0021').",
            "put_code(0'\\u0021').",
            "writeq(\"\\u0021\").",
        ]

        for query in test_queries:
            result = prolog.query_once(query)
            # Should not raise an exception
            assert result is not None
