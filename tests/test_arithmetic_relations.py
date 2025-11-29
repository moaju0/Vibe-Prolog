"""
Tests for ISO arithmetic relation predicates: between/3, succ/2, plus/3, divmod/4.
"""

import pytest
from vibeprolog import PrologInterpreter


class TestBetween:
    """Tests for between/3 predicate."""

    def test_between_generate_values(self):
        """Test generating values in range."""
        prolog = PrologInterpreter()
        results = list(prolog.query("between(1, 5, X)"))
        values = [r['X'] for r in results]
        assert values == [1, 2, 3, 4, 5]

    def test_between_check_value_in_range(self):
        """Test checking if value is in range."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("between(1, 5, 3)")
        assert not prolog.has_solution("between(1, 5, 0)")
        assert not prolog.has_solution("between(1, 5, 6)")

    def test_between_edge_cases(self):
        """Test edge cases for between/3."""
        prolog = PrologInterpreter()
        # Single value range
        results = list(prolog.query("between(5, 5, X)"))
        values = [r['X'] for r in results]
        assert values == [5]

        # Empty range (low > high)
        results = list(prolog.query("between(5, 3, X)"))
        assert len(results) == 0

    def test_between_errors(self):
        """Test error conditions for between/3."""
        prolog = PrologInterpreter()

        # Uninstantiated Low
        with pytest.raises(Exception):  # Should raise instantiation_error
            list(prolog.query("between(X, 5, 3)"))

        # Uninstantiated High
        with pytest.raises(Exception):  # Should raise instantiation_error
            list(prolog.query("between(1, Y, 3)"))

        # Non-integer Low
        with pytest.raises(Exception):  # Should raise type_error
            list(prolog.query("between(1.5, 5, X)"))

        # Non-integer High
        with pytest.raises(Exception):  # Should raise type_error
            list(prolog.query("between(1, 5.5, X)"))


class TestSucc:
    """Tests for succ/2 predicate."""

    def test_succ_forward(self):
        """Test succ(Int1, Int2) where Int1 is bound."""
        prolog = PrologInterpreter()
        result = prolog.query_once("succ(3, X)")
        assert result is not None
        assert result['X'] == 4

    def test_succ_backward(self):
        """Test succ(Int1, Int2) where Int2 is bound."""
        prolog = PrologInterpreter()
        result = prolog.query_once("succ(X, 5)")
        assert result is not None
        assert result['X'] == 4

    def test_succ_both_bound_true(self):
        """Test succ(Int1, Int2) where both are bound and true."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("succ(3, 4)")

    def test_succ_both_bound_false(self):
        """Test succ(Int1, Int2) where both are bound and false."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("succ(3, 5)")

    def test_succ_zero_cases(self):
        """Test succ/2 with zero."""
        prolog = PrologInterpreter()
        result = prolog.query_once("succ(0, X)")
        assert result is not None
        assert result['X'] == 1

        result = prolog.query_once("succ(X, 0)")
        assert result is None  # Negative result not allowed

    def test_succ_errors(self):
        """Test error conditions for succ/2."""
        prolog = PrologInterpreter()

        # Both uninstantiated
        with pytest.raises(Exception):  # Should raise instantiation_error
            prolog.query_once("succ(X, Y)")

        # Non-integer first arg
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("succ(3.5, X)")

        # Non-integer second arg
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("succ(X, 4.5)")


class TestPlus:
    """Tests for plus/3 predicate."""

    def test_plus_solve_for_third(self):
        """Test plus(Int1, Int2, Int3) solving for Int3."""
        prolog = PrologInterpreter()
        result = prolog.query_once("plus(2, 3, X)")
        assert result is not None
        assert result['X'] == 5

    def test_plus_solve_for_first(self):
        """Test plus(Int1, Int2, Int3) solving for Int1."""
        prolog = PrologInterpreter()
        result = prolog.query_once("plus(X, 3, 5)")
        assert result is not None
        assert result['X'] == 2

    def test_plus_solve_for_second(self):
        """Test plus(Int1, Int2, Int3) solving for Int2."""
        prolog = PrologInterpreter()
        result = prolog.query_once("plus(2, Y, 5)")
        assert result is not None
        assert result['Y'] == 3

    def test_plus_all_bound_true(self):
        """Test plus/3 with all args bound and true."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("plus(2, 3, 5)")

    def test_plus_all_bound_false(self):
        """Test plus/3 with all args bound and false."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("plus(2, 3, 6)")

    def test_plus_negative_results(self):
        """Test plus/3 with negative results (should fail)."""
        prolog = PrologInterpreter()
        result = prolog.query_once("plus(2, X, 1)")
        assert result is None  # 2 + X = 1 => X = -1, not allowed

    def test_plus_errors(self):
        """Test error conditions for plus/3."""
        prolog = PrologInterpreter()

        # Too many uninstantiated
        with pytest.raises(Exception):  # Should raise instantiation_error
            prolog.query_once("plus(X, Y, Z)")

        # Non-integer args
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("plus(2.5, 3, X)")


class TestDivMod:
    """Tests for divmod/4 predicate."""

    def test_divmod_positive(self):
        """Test divmod/4 with positive numbers."""
        prolog = PrologInterpreter()
        result = prolog.query_once("divmod(17, 5, Q, R)")
        assert result is not None
        assert result['Q'] == 3
        assert result['R'] == 2

    def test_divmod_zero_remainder(self):
        """Test divmod/4 with zero remainder."""
        prolog = PrologInterpreter()
        result = prolog.query_once("divmod(20, 5, Q, R)")
        assert result is not None
        assert result['Q'] == 4
        assert result['R'] == 0

    def test_divmod_negative_dividend(self):
        """Test divmod/4 with negative dividend (ISO floored division)."""
        prolog = PrologInterpreter()
        result = prolog.query_once("divmod(-17, 5, Q, R)")
        assert result is not None
        assert result['Q'] == -4
        assert result['R'] == 3  # ISO floored semantics

    def test_divmod_errors(self):
        """Test error conditions for divmod/4."""
        prolog = PrologInterpreter()

        # Uninstantiated dividend
        with pytest.raises(Exception):  # Should raise instantiation_error
            prolog.query_once("divmod(X, 5, Q, R)")

        # Uninstantiated divisor
        with pytest.raises(Exception):  # Should raise instantiation_error
            prolog.query_once("divmod(17, Y, Q, R)")

        # Non-integer dividend
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("divmod(17.5, 5, Q, R)")

        # Non-integer divisor
        with pytest.raises(Exception):  # Should raise type_error
            prolog.query_once("divmod(17, 5.5, Q, R)")

        # Division by zero
        with pytest.raises(Exception):  # Should raise evaluation_error
            prolog.query_once("divmod(17, 0, Q, R)")