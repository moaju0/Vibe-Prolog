"""DCG (Definite Clause Grammar) built-in predicates."""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext, iter_empty
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import Compound, Cut, List
from vibeprolog.terms import Atom
from vibeprolog.unification import Substitution, deref


def _validate_terminal_list(rule_set_deref, phrase_id):
    # Ensure a List used as a rule-set terminal has no tail
    if isinstance(rule_set_deref, List) and rule_set_deref.tail is not None:
        error_term = PrologError.type_error("list", rule_set_deref, phrase_id)
        raise PrologThrow(error_term)


class DCGBuiltins:
    """Built-ins for DCG operations."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register DCG predicates into the registry."""
        register_builtin(registry, "phrase", 2, DCGBuiltins._builtin_phrase_2)
        register_builtin(registry, "phrase", 3, DCGBuiltins._builtin_phrase_3)

    @staticmethod
    def _builtin_phrase_2(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """
        phrase(RuleSet, List) - Invoke DCG with complete list consumption.

        Expands to: call(RuleSet, List, [])
        """
        rule_set, input_list = args

        # Validate the rule set before use
        engine._check_instantiated(rule_set, subst, "phrase/2")
        engine._check_type(rule_set, (Atom, Compound, List, Cut), "callable", subst, "phrase/2")

        rule_set_deref = deref(rule_set, subst)

        # Handle different types of rule sets
        if isinstance(rule_set_deref, List):
            # Terminal list: unify input_list with the terminal list
            # Validate that the list is proper
            _validate_terminal_list(rule_set_deref, "phrase/2")
            # Unify the input list with the terminal list
            for new_subst in engine._unify_with_attvar_support(input_list, rule_set_deref, subst):
                yield new_subst
            return
        elif isinstance(rule_set_deref, Cut):
            # Cut as ruleset: unify with empty list
            empty_list = List(elements=(), tail=None)
            for new_subst in engine._unify_with_attvar_support(input_list, empty_list, subst):
                yield new_subst
            return
        else:
            # Atom or Compound: expand to DCG call
            # phrase(RuleSet, List) ≡ RuleSet(List, [])
            empty_list = List(elements=(), tail=None)
            if isinstance(rule_set_deref, Atom):
                goal = Compound(rule_set_deref.name, (input_list, empty_list))
            else:
                # Handle compound rule sets
                goal = Compound(rule_set_deref.functor, rule_set_deref.args + (input_list, empty_list))

            # Check if the predicate exists before calling it
            engine._check_predicate_exists(goal, "phrase/2")

            yield from engine._solve_goals([goal], subst)

    @staticmethod
    def _builtin_phrase_3(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """
        phrase(RuleSet, List, Rest) - Invoke DCG with remainder.

        Expands to: call(RuleSet, List, Rest)
        """
        rule_set, input_list, rest = args

        # Validate the rule set before use
        engine._check_instantiated(rule_set, subst, "phrase/3")
        engine._check_type(rule_set, (Atom, Compound, List, Cut), "callable", subst, "phrase/3")

        rule_set_deref = deref(rule_set, subst)

        # Handle different types of rule sets
        if isinstance(rule_set_deref, List):
            # Terminal list: unify input_list with the terminal list, rest with []
            # Validate that the list is proper
            _validate_terminal_list(rule_set_deref, "phrase/3")
            # Unify the input list with the terminal list, and rest with empty list
            empty_list = List(elements=(), tail=None)
            for new_subst in engine._unify_with_attvar_support(input_list, rule_set_deref, subst):
                for final_subst in engine._unify_with_attvar_support(rest, empty_list, new_subst):
                    yield final_subst
            return
        elif isinstance(rule_set_deref, Cut):
            # Cut as ruleset: unify input_list with [], rest with []
            empty_list = List(elements=(), tail=None)
            for new_subst in engine._unify_with_attvar_support(input_list, empty_list, subst):
                for final_subst in engine._unify_with_attvar_support(rest, empty_list, new_subst):
                    yield final_subst
            return
        else:
            # Atom or Compound: expand to DCG call
            # phrase(RuleSet, List, Rest) ≡ RuleSet(List, Rest)
            if isinstance(rule_set_deref, Atom):
                goal = Compound(rule_set_deref.name, (input_list, rest))
            else:
                # Handle compound rule sets
                goal = Compound(rule_set_deref.functor, rule_set_deref.args + (input_list, rest))

            # Check if the predicate exists before calling it
            engine._check_predicate_exists(goal, "phrase/3")

            yield from engine._solve_goals([goal], subst)


__all__ = ["DCGBuiltins"]