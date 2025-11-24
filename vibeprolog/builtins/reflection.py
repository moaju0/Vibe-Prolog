"""Reflection built-ins (predicate_property/2, current_predicate/1).

Expose predicate metadata such as presence in the database or built-in registry.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.terms import Atom, Compound, Number
from vibeprolog.unification import Substitution, deref, unify


class ReflectionBuiltins:
    """Built-ins that expose predicate metadata."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register reflection predicate handlers."""
        register_builtin(
            registry,
            "predicate_property",
            2,
            ReflectionBuiltins._builtin_predicate_property,
        )
        register_builtin(
            registry,
            "current_predicate",
            1,
            ReflectionBuiltins._builtin_current_predicate,
        )

    @staticmethod
    def _builtin_predicate_property(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        goal_term, property_term = args
        goal_term = deref(goal_term, subst)
        property_term = deref(property_term, subst)

        builtins = set(engine._builtin_registry.keys())
        builtins.add(("!", 0))

        is_builtin = False
        if isinstance(goal_term, Compound):
            is_builtin = (goal_term.functor, len(goal_term.args)) in builtins
        elif isinstance(goal_term, Atom):
            is_builtin = (goal_term.name, 0) in builtins or goal_term.name == "!"

        if is_builtin:
            return unify(property_term, Atom("built_in"), subst)
        return None

    @staticmethod
    def _builtin_current_predicate(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        indicator = deref(args[0], subst)

        predicates = set(engine._builtin_registry.keys())
        predicates.add(("!", 0))

        for clause in engine.clauses:
            if isinstance(clause.head, Compound):
                predicates.add((clause.head.functor, len(clause.head.args)))
            elif isinstance(clause.head, Atom):
                predicates.add((clause.head.name, 0))

        sorted_predicates = sorted(predicates)

        for name, arity in sorted_predicates:
            pred_indicator = Compound("/", (Atom(name), Number(arity)))
            new_subst = unify(indicator, pred_indicator, subst)
            if new_subst is not None:
                yield new_subst


__all__ = ["ReflectionBuiltins"]
