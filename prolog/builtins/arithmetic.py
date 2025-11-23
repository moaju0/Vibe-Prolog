"""Arithmetic built-ins (is/2 and arithmetic comparisons)."""

from __future__ import annotations

from typing import Any

from prolog.builtins import register_builtin
from prolog.parser import Compound, Number
from prolog.unification import Substitution, deref, unify


class ArithmeticBuiltins:
    """Built-ins for arithmetic evaluation and comparison."""

    @staticmethod
    def register(registry, _engine) -> None:
        register_builtin(registry, "is", 2, ArithmeticBuiltins._builtin_is)
        for op in ["=:=", r"=\=", "<", ">", "=<", ">="]:
            register_builtin(
                registry,
                op,
                2,
                lambda args, subst, engine, op=op: ArithmeticBuiltins._builtin_arithmetic_compare(op, args, subst, engine),
            )

    @staticmethod
    def _builtin_is(args: tuple[Any, ...], subst: Substitution, engine) -> Substitution | None:
        value = ArithmeticBuiltins._eval_arithmetic(args[1], subst, engine)
        if value is None:
            return None
        return unify(args[0], Number(value), subst)

    @staticmethod
    def _builtin_arithmetic_compare(
        op: str, args: tuple[Any, ...], subst: Substitution, engine
    ) -> Substitution | None:
        left_val = ArithmeticBuiltins._eval_arithmetic(args[0], subst, engine)
        right_val = ArithmeticBuiltins._eval_arithmetic(args[1], subst, engine)

        if left_val is None or right_val is None:
            return None

        if op == "=:=" and left_val == right_val:
            return subst
        if op == r"=\=" and left_val != right_val:
            return subst
        if op == "<" and left_val < right_val:
            return subst
        if op == ">" and left_val > right_val:
            return subst
        if op == "=<" and left_val <= right_val:
            return subst
        if op == ">=" and left_val >= right_val:
            return subst
        return None

    @staticmethod
    def _eval_arithmetic(expr: Any, subst: Substitution, engine) -> int | float | None:
        expr = deref(expr, subst)

        if isinstance(expr, Number):
            return expr.value

        if isinstance(expr, Compound):
            if expr.functor == "-" and len(expr.args) == 1:
                arg = ArithmeticBuiltins._eval_arithmetic(expr.args[0], subst, engine)
                if arg is None:
                    return None
                return -arg

            if expr.functor in ["+", "-", "*", "/", "//", "mod", "**"] and len(expr.args) == 2:
                left = ArithmeticBuiltins._eval_arithmetic(expr.args[0], subst, engine)
                right = ArithmeticBuiltins._eval_arithmetic(expr.args[1], subst, engine)

                if left is None or right is None:
                    return None

                try:
                    if expr.functor == "+":
                        return left + right
                    if expr.functor == "-":
                        return left - right
                    if expr.functor == "*":
                        return left * right
                    if expr.functor == "/":
                        return left / right
                    if expr.functor == "//":
                        return left // right
                    if expr.functor == "mod":
                        return left % right
                    if expr.functor == "**":
                        return left ** right
                except ZeroDivisionError:
                    return None

        return None


__all__ = ["ArithmeticBuiltins"]
