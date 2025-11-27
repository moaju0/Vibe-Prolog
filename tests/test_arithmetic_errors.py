"""Tests for ISO-compliant arithmetic error handling."""

import pytest
from vibeprolog import PrologInterpreter


class TestArithmeticInstantiationErrors:
    """Test instantiation_error for unbound variables."""

    def test_is_unbound_rhs(self):
        """is/2 with unbound variable raises instantiation_error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is Y + 1")
        assert "instantiation_error" in str(exc_info.value)

    def test_is_unbound_in_expression(self):
        """Unbound variable inside expression raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is 5 + Y")
        assert "instantiation_error" in str(exc_info.value)

    def test_comparison_unbound(self):
        """Arithmetic comparison with unbound raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X =:= 5")
        assert "instantiation_error" in str(exc_info.value)


class TestArithmeticTypeErrors:
    """Test type_error for non-evaluable terms."""

    def test_is_atom_not_evaluable(self):
        """Atom in arithmetic raises type_error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is atom + 1")
        assert "type_error" in str(exc_info.value)
        assert "evaluable" in str(exc_info.value)

    def test_is_list_not_evaluable(self):
        """List in arithmetic raises type_error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is [1, 2, 3] + 1")
        assert "type_error" in str(exc_info.value)

    def test_invalid_functor(self):
        """Invalid arithmetic functor raises type_error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is invalid_op(5)")
        assert "type_error" in str(exc_info.value)

    def test_comparison_atom(self):
        """Comparing atom raises type_error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("atom =:= 5")
        assert "type_error" in str(exc_info.value) and "instantiation_error" not in str(exc_info.value)


class TestEvaluationErrors:
    """Test evaluation_error for runtime errors."""

    def test_division_by_zero(self):
        """Division by zero raises evaluation_error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is 1 / 0")
        assert "evaluation_error" in str(exc_info.value)
        assert "zero_divisor" in str(exc_info.value)

    def test_integer_division_by_zero(self):
        """Integer division by zero raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is 10 // 0")
        assert "evaluation_error" in str(exc_info.value)

    def test_mod_by_zero(self):
        """Modulo by zero raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is 10 mod 0")
        assert "evaluation_error" in str(exc_info.value)

    def test_sqrt_negative(self):
        """Square root of negative raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is sqrt(-1)")
        assert "evaluation_error" in str(exc_info.value)
        assert "undefined" in str(exc_info.value)

    def test_log_negative(self):
        """Logarithm of negative raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is log(-5)")
        assert "evaluation_error" in str(exc_info.value)

    def test_log_zero(self):
        """Logarithm of zero raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is log(0)")
        assert "evaluation_error" in str(exc_info.value)

    def test_zero_to_negative_power(self):
        """0 ** -1 raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("X is 0 ** -1")
        assert "evaluation_error" in str(exc_info.value)


class TestValidArithmetic:
    """Test that valid arithmetic still works."""

    def test_simple_addition(self):
        """Simple addition works."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 2 + 3")
        assert result is not None
        assert result['X'] == 5

    def test_complex_expression(self):
        """Complex expression works."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is (5 + 3) * 2 - 1")
        assert result is not None
        assert result['X'] == 15

    def test_sqrt_positive(self):
        """Square root of positive number works."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is sqrt(16)")
        assert result is not None
        assert result['X'] == 4.0

    def test_division_normal(self):
        """Normal division works."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X is 10 / 2")
        assert result is not None
        assert result['X'] == 5.0

    def test_comparisons_work(self):
        """Arithmetic comparisons work."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("5 =:= 5")
        assert prolog.has_solution("3 < 5")
        assert prolog.has_solution("10 > 2")
        assert not prolog.has_solution("5 =:= 3")