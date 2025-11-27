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
            ('*', 2), ('/', 2), ('//', 2), ('mod', 2),
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
