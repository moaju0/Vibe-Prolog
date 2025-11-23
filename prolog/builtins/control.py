"""Control flow built-ins (conjunction, disjunction, negation, call/once).

Implements ISO-compatible control predicates such as conjunction, disjunction,
conditionals, call/1, and once/1.
"""

from __future__ import annotations

from typing import Iterator

from prolog.builtins import BuiltinRegistry, register_builtin
from prolog.builtins.common import BuiltinArgs, EngineContext, iter_empty
from prolog.parser import Compound
from prolog.unification import Substitution, deref, unify


class ControlBuiltins:
    """Built-ins for control structures and basic predicates."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register control predicates into the registry."""
        register_builtin(registry, "=", 2, ControlBuiltins._builtin_unify)
        register_builtin(registry, r"\=", 2, ControlBuiltins._builtin_not_unifiable)
        register_builtin(registry, r"\+", 1, ControlBuiltins._negation_as_failure)
        register_builtin(registry, ";", 2, ControlBuiltins._builtin_disjunction)
        register_builtin(registry, "->", 2, ControlBuiltins._builtin_if_then)
        register_builtin(registry, ",", 2, ControlBuiltins._builtin_conjunction)
        register_builtin(registry, "call", 1, ControlBuiltins._builtin_call)
        register_builtin(registry, "once", 1, ControlBuiltins._builtin_once)
        register_builtin(registry, "true", 0, ControlBuiltins._builtin_true)
        register_builtin(registry, "fail", 0, ControlBuiltins._builtin_fail)

    @staticmethod
    def _builtin_unify(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        return unify(args[0], args[1], subst)

    @staticmethod
    def _builtin_not_unifiable(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        return subst if unify(args[0], args[1], subst) is None else None

    @staticmethod
    def _negation_as_failure(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        goal = args[0]
        for _ in engine._solve_goals([goal], subst):
            return iter_empty()
        return iter([subst])

    @staticmethod
    def _builtin_disjunction(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        left, right = args
        if isinstance(left, Compound) and left.functor == "->" and len(left.args) == 2:
            condition = left.args[0]
            then_part = left.args[1]
            else_part = right

            for condition_subst in engine._solve_goals([condition], subst):
                yield from engine._solve_goals([then_part], condition_subst)
                return

            yield from engine._solve_goals([else_part], subst)
        else:
            yield from engine._solve_goals([left], subst)
            yield from engine._solve_goals([right], subst)

    @staticmethod
    def _builtin_if_then(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        condition, then_part = args
        for condition_subst in engine._solve_goals([condition], subst):
            yield from engine._solve_goals([then_part], condition_subst)
            return

    @staticmethod
    def _builtin_conjunction(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        left, right = args
        goals = engine._flatten_conjunction(Compound(",", (left, right)))
        yield from engine._solve_goals(goals, subst)

    @staticmethod
    def _builtin_call(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        goal = deref(args[0], subst)
        yield from engine._solve_goals([goal], subst)

    @staticmethod
    def _builtin_once(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        goal = deref(args[0], subst)
        for solution in engine._solve_goals([goal], subst):
            yield solution
            return

    @staticmethod
    def _builtin_true(
        _args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution:
        return subst

    @staticmethod
    def _builtin_fail(
        _args: BuiltinArgs, _subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        return iter_empty()


__all__ = ["ControlBuiltins"]
