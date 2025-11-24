"""Reflection built-ins (predicate_property/2, current_predicate/1).

Expose predicate metadata such as presence in the database or built-in registry.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution, deref, unify
from vibeprolog.utils.list_utils import python_to_list


class ReflectionBuiltins:
    """Built-ins that expose predicate metadata."""

    @staticmethod
    def register(registry: BuiltinRegistry, engine: EngineContext | None) -> None:
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
        register_builtin(
            registry,
            "argv",
            1,
            ReflectionBuiltins._builtin_argv,
        )
        register_builtin(
            registry,
            "current_prolog_flag",
            2,
            ReflectionBuiltins._builtin_current_prolog_flag,
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

    @staticmethod
    def _builtin_argv(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        """argv(Args) - Unify Args with the list of command-line arguments."""
        arg_list = deref(args[0], subst)

        # Convert argv to Prolog list of atoms
        prolog_argv = ReflectionBuiltins._get_prolog_argv(engine)

        return unify(arg_list, prolog_argv, subst)

    @staticmethod
    def _get_prolog_argv(engine: EngineContext) -> any:
        """Convert engine.argv to Prolog list of atoms."""
        return python_to_list([Atom(arg) for arg in engine.argv])

    @staticmethod
    def _builtin_current_prolog_flag(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """current_prolog_flag(Flag, Value) - Get Prolog flag values."""
        flag_term, value_term = args
        flag_term = deref(flag_term, subst)
        value_term = deref(value_term, subst)

        supported_flags = {
            "argv": ReflectionBuiltins._get_prolog_argv(engine)
        }

        if isinstance(flag_term, Variable):
            # Enumerate supported flags
            for flag_name, flag_value in supported_flags.items():
                new_subst = unify(flag_term, Atom(flag_name), subst)
                if new_subst is not None:
                    final_subst = unify(value_term, flag_value, new_subst)
                    if final_subst is not None:
                        yield final_subst
        elif isinstance(flag_term, Atom) and flag_term.name in supported_flags:
            flag_value = supported_flags[flag_term.name]
            new_subst = unify(value_term, flag_value, subst)
            if new_subst is not None:
                yield new_subst


__all__ = ["ReflectionBuiltins"]
