"""Arithmetic built-ins (is/2 and arithmetic comparisons).

Implements ISO-style arithmetic predicates:
- ``is/2`` for arithmetic evaluation
- ``=:=/2`` and ``=\\=/2`` for numeric equality/inequality
- ``</2``, ``>/2``, ``=<``/2, ``>=``/2 for comparisons
- ISO math functions: abs, min, max, sqrt, sin, cos, tan, exp, log, floor, ceiling, round, sign
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import Compound, Number
from vibeprolog.terms import Variable
from vibeprolog.unification import Substitution, deref, unify

if TYPE_CHECKING:
    from vibeprolog.engine import PrologEngine


def _check_instantiated(term: Any, subst: Substitution, predicate: str) -> None:
    """Check if arithmetic expression is fully instantiated."""
    # Dereference term
    term = deref(term, subst)

    # Check if variable
    if isinstance(term, Variable):
        raise PrologThrow(PrologError.instantiation_error(predicate))

    # Recursively check compound terms
    if isinstance(term, Compound):
        for arg in term.args:  # Skip functor
            _check_instantiated(arg, subst, predicate)


def _check_evaluable(term: Any, subst: Substitution, predicate: str) -> None:
    """Check if term is a valid arithmetic expression."""
    term = deref(term, subst)

    # Numbers are always evaluable
    if isinstance(term, Number):
        return

    # Variables should have been caught by _check_instantiated
    if isinstance(term, Variable):
        raise PrologThrow(PrologError.instantiation_error(predicate))

    # Check if it's a valid arithmetic functor
    if isinstance(term, Compound):
        functor = term.functor
        arity = len(term.args)

        # List of valid arithmetic functors
        valid_functors = {
            ('+', 1), ('+', 2), ('-', 1), ('-', 2),
            ('*', 2), ('/', 2), ('//', 2), ('mod', 2), ('div', 2),
            ('**', 2), ('abs', 1), ('sign', 1),
            ('min', 2), ('max', 2),
            ('sqrt', 1), ('exp', 1), ('log', 1),
            ('sin', 1), ('cos', 1), ('tan', 1),
            ('floor', 1), ('ceiling', 1), ('round', 1),
        }

        if (functor, arity) not in valid_functors:
            raise PrologThrow(PrologError.type_error("evaluable", term, predicate))

        # Recursively check arguments
        for arg in term.args:
            _check_evaluable(arg, subst, predicate)

    else:
        # Atoms and other types are not evaluable
        raise PrologThrow(PrologError.type_error("evaluable", term, predicate))


class ArithmeticBuiltins:
    """Built-ins for arithmetic evaluation and comparison."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: Any | None) -> None:
        """Register arithmetic predicate handlers into the registry."""
        register_builtin(registry, "is", 2, ArithmeticBuiltins._builtin_is)
        for op in ["=:=", r"=\=", "<", ">", "=<", ">="]:
            register_builtin(
                registry,
                op,
                2,
                lambda args,
                subst,
                engine,
                op=op: ArithmeticBuiltins._builtin_arithmetic_compare(
                    op, args, subst, engine
                ),
            )
        register_builtin(registry, "between", 3, ArithmeticBuiltins._builtin_between)
        register_builtin(registry, "succ", 2, ArithmeticBuiltins._builtin_succ)
        register_builtin(registry, "plus", 3, ArithmeticBuiltins._builtin_plus)
        register_builtin(registry, "divmod", 4, ArithmeticBuiltins._builtin_divmod)

    @staticmethod
    def _builtin_is(
        args: BuiltinArgs, subst: Substitution, engine: PrologEngine
    ) -> Substitution | None:
        """is/2 - Arithmetic evaluation with ISO error handling."""
        result_term, expr = args

        try:
            # Evaluate the arithmetic expression
            value = ArithmeticBuiltins._evaluate_arithmetic(expr, subst, engine, "is/2")

            # Unify with result
            new_subst = unify(result_term, Number(value), subst)
            if new_subst is not None:
                yield new_subst

        except PrologThrow:
            # Re-raise Prolog errors
            raise
        except Exception as e:
            # Unexpected errors - convert to evaluation error
            raise PrologThrow(PrologError.evaluation_error("error", "is/2"))

    @staticmethod
    def _builtin_arithmetic_compare(
        op: str, args: BuiltinArgs, subst: Substitution, engine: PrologEngine
    ) -> Substitution | None:
        """Perform numeric comparison between two arithmetic expressions with error handling."""
        try:
            left_val = ArithmeticBuiltins._evaluate_arithmetic(args[0], subst, engine, f"{op}/2")
            right_val = ArithmeticBuiltins._evaluate_arithmetic(args[1], subst, engine, f"{op}/2")

            if op == "=:=" and left_val == right_val:
                yield subst
            elif op == r"=\=" and left_val != right_val:
                yield subst
            elif op == "<" and left_val < right_val:
                yield subst
            elif op == ">" and left_val > right_val:
                yield subst
            elif op == "=<" and left_val <= right_val:
                yield subst
            elif op == ">=" and left_val >= right_val:
                yield subst

        except PrologThrow:
            # Re-raise Prolog errors
            raise
        except Exception:
            # Unexpected errors - convert to evaluation error
            raise PrologThrow(PrologError.evaluation_error("error", f"{op}/2"))

    @staticmethod
    def _builtin_between(
        args: BuiltinArgs, subst: Substitution, engine: PrologEngine
    ) -> Substitution | None:
        """between/3 - Generate or test integers in a range."""
        low, high, value = args

        # Dereference arguments
        low_deref = deref(low, subst)
        high_deref = deref(high, subst)
        value_deref = deref(value, subst)

        # Check Low and High are instantiated
        if isinstance(low_deref, Variable):
            raise PrologThrow(PrologError.instantiation_error("between/3"))
        if isinstance(high_deref, Variable):
            raise PrologThrow(PrologError.instantiation_error("between/3"))

        # Check Low and High are integers
        if not isinstance(low_deref, Number) or not isinstance(low_deref.value, int):
            raise PrologThrow(PrologError.type_error("integer", low_deref, "between/3"))
        if not isinstance(high_deref, Number) or not isinstance(high_deref.value, int):
            raise PrologThrow(PrologError.type_error("integer", high_deref, "between/3"))

        low_val = int(low_deref.value)
        high_val = int(high_deref.value)

        # Check Low <= High
        if low_val > high_val:
            return  # Fail

        # If Value is bound, check if it's in range
        if not isinstance(value_deref, Variable):
            if not isinstance(value_deref, Number) or not isinstance(value_deref.value, int):
                raise PrologThrow(PrologError.type_error("integer", value_deref, "between/3"))
            val = int(value_deref.value)
            if low_val <= val <= high_val:
                yield subst
        else:
            # Generate values from Low to High
            for i in range(low_val, high_val + 1):
                new_subst = unify(value, Number(i), subst)
                if new_subst is not None:
                    yield new_subst

    @staticmethod
    def _builtin_succ(
        args: BuiltinArgs, subst: Substitution, engine: PrologEngine
    ) -> Substitution | None:
        """succ/2 - Successor relation between integers."""
        int1, int2 = args

        int1_deref = deref(int1, subst)
        int2_deref = deref(int2, subst)

        # At least one must be instantiated
        if isinstance(int1_deref, Variable) and isinstance(int2_deref, Variable):
            raise PrologThrow(PrologError.instantiation_error("succ/2"))

        # If both are instantiated, check if int2 = int1 + 1
        if not isinstance(int1_deref, Variable) and not isinstance(int2_deref, Variable):
            if not isinstance(int1_deref, Number) or not isinstance(int1_deref.value, int):
                raise PrologThrow(PrologError.type_error("integer", int1_deref, "succ/2"))
            if not isinstance(int2_deref, Number) or not isinstance(int2_deref.value, int):
                raise PrologThrow(PrologError.type_error("integer", int2_deref, "succ/2"))
            if int(int2_deref.value) == int(int1_deref.value) + 1:
                yield subst
        # If int1 is bound, unify int2 with int1 + 1
        elif not isinstance(int1_deref, Variable):
            if not isinstance(int1_deref, Number) or not isinstance(int1_deref.value, int):
                raise PrologThrow(PrologError.type_error("integer", int1_deref, "succ/2"))
            val1 = int(int1_deref.value)
            val2 = val1 + 1
            if val2 < 0:
                return  # Fail, negative not allowed
            new_subst = unify(int2, Number(val2), subst)
            if new_subst is not None:
                yield new_subst
        # If int2 is bound, unify int1 with int2 - 1
        else:
            if not isinstance(int2_deref, Number) or not isinstance(int2_deref.value, int):
                raise PrologThrow(PrologError.type_error("integer", int2_deref, "succ/2"))
            val2 = int(int2_deref.value)
            val1 = val2 - 1
            if val1 < 0:
                return  # Fail
            new_subst = unify(int1, Number(val1), subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _builtin_plus(
        args: BuiltinArgs, subst: Substitution, engine: PrologEngine
    ) -> Substitution | None:
        """plus/3 - Addition relation."""
        int1, int2, int3 = args

        int1_deref = deref(int1, subst)
        int2_deref = deref(int2, subst)
        int3_deref = deref(int3, subst)

        # Count instantiated arguments
        instantiated = [
            not isinstance(int1_deref, Variable),
            not isinstance(int2_deref, Variable),
            not isinstance(int3_deref, Variable)
        ]
        if sum(instantiated) < 2:
            raise PrologThrow(PrologError.instantiation_error("plus/3"))

        # Check types of instantiated args
        if instantiated[0] and (not isinstance(int1_deref, Number) or not isinstance(int1_deref.value, int)):
            raise PrologThrow(PrologError.type_error("integer", int1_deref, "plus/3"))
        if instantiated[1] and (not isinstance(int2_deref, Number) or not isinstance(int2_deref.value, int)):
            raise PrologThrow(PrologError.type_error("integer", int2_deref, "plus/3"))
        if instantiated[2] and (not isinstance(int3_deref, Number) or not isinstance(int3_deref.value, int)):
            raise PrologThrow(PrologError.type_error("integer", int3_deref, "plus/3"))

        # Determine which arg is unbound
        if not instantiated[0]:  # int1 unbound
            val2 = int(int2_deref.value)
            val3 = int(int3_deref.value)
            val1 = val3 - val2
            if val1 < 0:
                return
            new_subst = unify(int1, Number(val1), subst)
            if new_subst is not None:
                yield new_subst
        elif not instantiated[1]:  # int2 unbound
            val1 = int(int1_deref.value)
            val3 = int(int3_deref.value)
            val2 = val3 - val1
            if val2 < 0:
                return
            new_subst = unify(int2, Number(val2), subst)
            if new_subst is not None:
                yield new_subst
        else:  # int3 unbound
            val1 = int(int1_deref.value)
            val2 = int(int2_deref.value)
            val3 = val1 + val2
            if val3 < 0:
                return
            new_subst = unify(int3, Number(val3), subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _builtin_divmod(
        args: BuiltinArgs, subst: Substitution, engine: PrologEngine
    ) -> Substitution | None:
        """divmod/4 - Compute quotient and remainder."""
        dividend, divisor, quotient, remainder = args

        dividend_deref = deref(dividend, subst)
        divisor_deref = deref(divisor, subst)

        # Check instantiated
        if isinstance(dividend_deref, Variable):
            raise PrologThrow(PrologError.instantiation_error("divmod/4"))
        if isinstance(divisor_deref, Variable):
            raise PrologThrow(PrologError.instantiation_error("divmod/4"))

        # Check types
        if not isinstance(dividend_deref, Number) or not isinstance(dividend_deref.value, int):
            raise PrologThrow(PrologError.type_error("integer", dividend_deref, "divmod/4"))
        if not isinstance(divisor_deref, Number) or not isinstance(divisor_deref.value, int):
            raise PrologThrow(PrologError.type_error("integer", divisor_deref, "divmod/4"))

        div_val = int(dividend_deref.value)
        div_by = int(divisor_deref.value)

        if div_by == 0:
            raise PrologThrow(PrologError.evaluation_error("zero_divisor", "divmod/4"))

        quot, rem = divmod(div_val, div_by)

        # Unify quotient and remainder
        new_subst = unify(quotient, Number(quot), subst)
        if new_subst is not None:
            new_subst = unify(remainder, Number(rem), new_subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _evaluate_arithmetic(
        expr: Any, subst: Substitution, engine: PrologEngine, predicate: str = "is/2"
    ) -> int | float:
        """Evaluate arithmetic expression with ISO error handling.

        Args:
            expr: The expression to evaluate
            subst: Current substitution
            engine: Prolog engine
            predicate: Name of calling predicate (for error context)

        Returns:
            Numeric result

        Raises:
            PrologThrow with appropriate error term
        """
        # Dereference
        expr = deref(expr, subst)

        # Check for instantiation
        _check_instantiated(expr, subst, predicate)

        # Check if evaluable (valid arithmetic expression)
        _check_evaluable(expr, subst, predicate)

        # Numbers evaluate to themselves
        if isinstance(expr, Number):
            return expr.value

        # Handle compound expressions
        if isinstance(expr, Compound):
            functor = expr.functor
            args = expr.args

            # Evaluate arguments recursively
            try:
                evaluated_args = [
                    ArithmeticBuiltins._evaluate_arithmetic(arg, subst, engine, predicate)
                    for arg in args
                ]
            except PrologThrow:
                raise  # Re-raise Prolog errors
            except Exception as e:
                raise PrologThrow(PrologError.type_error("evaluable", expr, predicate))

            # Apply operation
            try:
                return ArithmeticBuiltins._apply_arithmetic_op(functor, evaluated_args, predicate)
            except ZeroDivisionError:
                raise PrologThrow(PrologError.evaluation_error("zero_divisor", predicate))
            except ValueError as e:
                # Domain errors (sqrt of negative, log of negative, etc.)
                raise PrologThrow(PrologError.evaluation_error("undefined", predicate))
            except OverflowError:
                raise PrologThrow(PrologError.evaluation_error("float_overflow", predicate))

        # Not a number or valid expression
        raise PrologThrow(PrologError.type_error("evaluable", expr, predicate))

    @staticmethod
    def _apply_arithmetic_op(functor: str, args: list[int | float], predicate: str) -> int | float:
        """Apply arithmetic operation with domain checking.

        Raises:
            ZeroDivisionError: For division by zero
            ValueError: For undefined operations (sqrt(-1), log(-1), etc.)
            OverflowError: For float overflow
        """
        # Unary operations
        if len(args) == 1:
            arg = args[0]

            if functor == '+':
                return +arg
            elif functor == '-':
                return -arg
            elif functor == 'abs':
                return abs(arg)
            elif functor == 'sign':
                return 1 if arg > 0 else (-1 if arg < 0 else 0)
            elif functor == 'sqrt':
                if arg < 0:
                    raise ValueError("sqrt of negative")
                return math.sqrt(arg)
            elif functor == 'exp':
                try:
                    return math.exp(arg)
                except OverflowError:
                    raise
            elif functor == 'log':
                if arg <= 0:
                    raise ValueError("log of non-positive")
                return math.log(arg)
            elif functor == 'sin':
                return math.sin(arg)
            elif functor == 'cos':
                return math.cos(arg)
            elif functor == 'tan':
                return math.tan(arg)
            elif functor == 'floor':
                return math.floor(arg)
            elif functor == 'ceiling':
                return math.ceil(arg)
            elif functor == 'round':
                return round(arg)
            else:
                raise ValueError(f"Unknown unary operator: {functor}")

        # Binary operations
        elif len(args) == 2:
            left, right = args

            if functor == '+':
                return left + right
            elif functor == '-':
                return left - right
            elif functor == '*':
                return left * right
            elif functor == '/':
                if right == 0:
                    raise ZeroDivisionError()
                return left / right
            elif functor == '//':
                if right == 0:
                    raise ZeroDivisionError()
                return int(left // right)
            elif functor == 'mod':
                if right == 0:
                    raise ZeroDivisionError()
                return left % right
            elif functor == 'div':
                if right == 0:
                    raise ZeroDivisionError()
                return int(left // right)
            elif functor == '**':
                # Check for undefined cases
                if left == 0 and right < 0:
                    raise ValueError("0 ** negative")
                if left < 0 and not isinstance(right, int):
                    raise ValueError("negative ** float")
                try:
                    return left ** right
                except OverflowError:
                    raise
            elif functor == 'min':
                return min(left, right)
            elif functor == 'max':
                return max(left, right)
            else:
                raise ValueError(f"Unknown binary operator: {functor}")

        raise ValueError(f"Invalid arity for {functor}")

__all__ = ["ArithmeticBuiltins"]
