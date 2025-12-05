"""
Tests for number parsing, including underscore digit grouping support.

Tests cover:
- Valid underscore grouping in integers, floats, and scientific notation
- Invalid underscore placements that should raise syntax errors
- Equivalence to underscore-free versions
"""

import pytest
from vibeprolog.exceptions import PrologThrow
from vibeprolog.parser import PrologParser, Number
from vibeprolog import PrologInterpreter


class TestUnderscoreGrouping:
    """Tests for valid underscore digit grouping in numbers."""

    def test_integer_grouping(self):
        """Test underscore grouping in integers."""
        parser = PrologParser()
        # Single group
        num = parser.parse_term("1_000")
        assert isinstance(num, Number)
        assert num.value == 1000

        # Multiple groups
        num = parser.parse_term("1_000_000")
        assert num.value == 1000000

        # Large number
        num = parser.parse_term("123_456_789")
        assert num.value == 123456789

    def test_float_grouping(self):
        """Test underscore grouping in floats."""
        parser = PrologParser()
        # Integer part only
        num = parser.parse_term("1_234.567")
        assert isinstance(num, Number)
        assert num.value == 1234.567

        # Fractional part only
        num = parser.parse_term("123.456_789")
        assert num.value == 123.456789

        # Both parts
        num = parser.parse_term("1_234.567_890")
        assert num.value == 1234.56789

    def test_scientific_grouping(self):
        """Test underscore grouping in scientific notation."""
        parser = PrologParser()
        # Integer part
        num = parser.parse_term("1_234e5")
        assert num.value == 123400000.0

        # Fractional part
        num = parser.parse_term("1.234_567e2")
        assert num.value == 123.4567

        # Both parts
        num = parser.parse_term("1_234.567_890e-3")
        assert num.value == 1.23456789

        # With positive exponent
        num = parser.parse_term("6.022_140_76e23")
        assert num.value == 6.02214076e23

    def test_negative_numbers(self):
        """Test underscore grouping in negative numbers."""
        parser = PrologParser()
        num = parser.parse_term("-1_000")
        assert num.value == -1000

        num = parser.parse_term("-1_234.567_890e-3")
        assert num.value == -1.23456789

    def test_interpreter_equivalence(self):
        """Test that underscored numbers are equivalent in interpreter."""
        prolog = PrologInterpreter()

        # Integer
        result1 = prolog.query_once("X = 1000000.")
        result2 = prolog.query_once("X = 1_000_000.")
        assert result1['X'] == result2['X'] == 1000000

        # Float
        result1 = prolog.query_once("X = 1234.56789.")
        result2 = prolog.query_once("X = 1_234.567_89.")
        assert result1['X'] == result2['X'] == 1234.56789

        # Scientific
        result1 = prolog.query_once("X = 6.02214076e23.")
        result2 = prolog.query_once("X = 6.022_140_76e23.")
        assert result1['X'] == result2['X'] == 6.02214076e23


class TestInvalidUnderscorePlacement:
    """Tests for invalid underscore placements that should raise syntax errors."""

    def test_leading_underscore(self):
        """Test that leading underscores are rejected."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("_123")

    def test_trailing_underscore(self):
        """Test that trailing underscores are rejected."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("123_")

    def test_double_underscore(self):
        """Test that double underscores are rejected."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("1__23")

    def test_underscore_before_dot(self):
        """Test underscore adjacent to decimal point."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("123_.456")

    def test_underscore_after_dot(self):
        """Test underscore adjacent to decimal point after dot."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("123._456")

    def test_underscore_before_exponent(self):
        """Test underscore adjacent to exponent marker."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("123_e10")

        with pytest.raises(PrologThrow):
            parser.parse_term("123_E10")

    def test_underscore_after_exponent(self):
        """Test underscore adjacent to exponent sign."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("123e_10")

        with pytest.raises(PrologThrow):
            parser.parse_term("123e+_10")

        with pytest.raises(PrologThrow):
            parser.parse_term("123e-_10")

    def test_underscore_in_exponent(self):
        """Test underscores in exponent part."""
        parser = PrologParser()
        # This should be valid
        num = parser.parse_term("123e1_0")
        assert num.value == 1230000000000.0

        # But invalid placements
        with pytest.raises(PrologThrow):
            parser.parse_term("123e_10")

    def test_interpreter_invalid_cases(self):
        """Test invalid cases in interpreter context."""
        prolog = PrologInterpreter()

        # Should raise syntax error
        with pytest.raises(PrologThrow):
            prolog.query_once("X = 1__0.")

        with pytest.raises(PrologThrow):
            prolog.query_once("X = _123.")

        with pytest.raises(PrologThrow):
            prolog.query_once("X = 123_.")


class TestEdgeCases:
    """Tests for edge cases and boundary conditions."""

    def test_single_digit_groups(self):
        """Test grouping with single digits."""
        parser = PrologParser()
        num = parser.parse_term("1_2_3")
        assert num.value == 123

    def test_minimal_grouping(self):
        """Test minimal valid grouping."""
        parser = PrologParser()
        num = parser.parse_term("1_0")
        assert num.value == 10

    def test_no_grouping(self):
        """Test that numbers without underscores still work."""
        parser = PrologParser()
        num = parser.parse_term("123")
        assert num.value == 123

        num = parser.parse_term("123.456")
        assert num.value == 123.456

        num = parser.parse_term("123e4")
        assert num.value == 1230000.0

    def test_zero_grouping(self):
        """Test grouping with zeros."""
        parser = PrologParser()
        num = parser.parse_term("0_000")
        assert num.value == 0

        num = parser.parse_term("0.000_001")
        assert num.value == 0.000001


class TestBaseNumberUnderscoreGrouping:
    """Tests for underscore grouping in Edinburgh-style base numbers."""

    def test_valid_base_number_grouping(self):
        """Test valid underscore grouping in base numbers."""
        parser = PrologParser()
        num = parser.parse_term("16'f_f")
        assert num.value == 255
        num = parser.parse_term("2'1010_0101")
        assert num.value == 165
        num = parser.parse_term("-10'1_000")
        assert num.value == -1000

    def test_invalid_base_number_grouping(self):
        """Test invalid underscore placements in base numbers."""
        parser = PrologParser()
        with pytest.raises(PrologThrow):
            parser.parse_term("16'_ff")  # Leading underscore
        with pytest.raises(PrologThrow):
            parser.parse_term("16'ff_")  # Trailing underscore
        with pytest.raises(PrologThrow):
            parser.parse_term("16'f__f")  # Double underscore