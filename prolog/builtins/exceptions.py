"""Exception handling built-ins (throw/1, catch/3)."""

from __future__ import annotations

from typing import Any, Iterator

from prolog.builtins import register_builtin
from prolog.parser import Atom, Compound, Cut
from prolog.unification import Substitution, deref, unify


class PrologThrow(Exception):
    """Exception raised when throw/1 is executed to unwind the call stack."""

    def __init__(self, term):
        self.term = term


class ExceptionBuiltins:
    """Built-ins for exception handling."""

    @staticmethod
    def register(registry, _engine) -> None:
        register_builtin(registry, "throw", 1, ExceptionBuiltins._builtin_throw)
        register_builtin(registry, "catch", 3, ExceptionBuiltins._builtin_catch)

    @staticmethod
    def _builtin_throw(args: tuple[Any, ...], subst: Substitution, _engine) -> Iterator[Substitution]:
        thrown_term = deref(args[0], subst)
        raise PrologThrow(thrown_term)

    @staticmethod
    def _builtin_catch(args: tuple[Any, ...], subst: Substitution, engine) -> Iterator[Substitution]:
        goal, error, recovery = args
        goal = deref(goal, subst)

        try:
            yield from ExceptionBuiltins._execute_callable_term(goal, subst, engine)
        except PrologThrow as exc:
            new_subst = unify(error, exc.term, subst)
            if new_subst is not None:
                recovery_goal = deref(recovery, new_subst)
                yield from ExceptionBuiltins._execute_callable_term(recovery_goal, new_subst, engine)
            else:
                raise

    @staticmethod
    def _execute_callable_term(goal_term: Any, subst: Substitution, engine) -> Iterator[Substitution]:
        if isinstance(goal_term, (Compound, Atom, Cut)):
            yield from engine._solve_goals([goal_term], subst)
        # Non-callable terms fail silently


__all__ = ["ExceptionBuiltins", "PrologThrow"]
