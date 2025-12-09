"""Control flow built-ins (conjunction, disjunction, negation, call/once).

Implements ISO-compatible control predicates such as conjunction, disjunction,
conditionals, call/1, and once/1.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext, iter_empty
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import Compound, List
from vibeprolog.terms import Atom
from vibeprolog.unification import Substitution, deref, unify
from vibeprolog.utils.list_utils import list_to_python


class ControlBuiltins:
    """Built-ins for control structures and basic predicates."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register control predicates into the registry."""
        register_builtin(registry, "=", 2, ControlBuiltins._builtin_unify)
        register_builtin(registry, r"\=", 2, ControlBuiltins._builtin_not_unifiable)
        register_builtin(registry, r"\+", 1, ControlBuiltins._negation_as_failure)
        register_builtin(registry, r"\+", 2, ControlBuiltins._negation_as_failure)
        register_builtin(registry, "not", 1, ControlBuiltins._negation_as_failure)
        register_builtin(registry, "not_", 1, ControlBuiltins._negation_as_failure)
        register_builtin(registry, ";", 2, ControlBuiltins._builtin_disjunction)
        register_builtin(registry, "->", 2, ControlBuiltins._builtin_if_then)
        register_builtin(registry, ",", 2, ControlBuiltins._builtin_conjunction)
        register_builtin(registry, "call", 1, ControlBuiltins._builtin_call)
        register_builtin(registry, "once", 1, ControlBuiltins._builtin_once)
        register_builtin(registry, "true", 0, ControlBuiltins._builtin_true)
        register_builtin(registry, "fail", 0, ControlBuiltins._builtin_fail)
        register_builtin(
            registry, "setup_call_cleanup", 3, ControlBuiltins._builtin_setup_call_cleanup
        )
        register_builtin(
            registry, "call_cleanup", 2, ControlBuiltins._builtin_call_cleanup
        )
        register_builtin(registry, "forall", 2, ControlBuiltins._builtin_forall)
        register_builtin(registry, "ignore", 1, ControlBuiltins._builtin_ignore)
        register_builtin(registry, "apply", 2, ControlBuiltins._builtin_apply)

    @staticmethod
    def _builtin_unify(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """Unification with attributed variable support.
        
        When unifying attributed variables, verify_attributes/3 hooks
        are called to allow constraint propagation.
        """
        if engine is not None:
            yield from engine._unify_with_attvar_support(args[0], args[1], subst)
        else:
            result = unify(args[0], args[1], subst)
            if result is not None:
                yield result

    @staticmethod
    def _builtin_not_unifiable(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext | None
    ) -> Substitution | None:
        if engine is not None:
            for _ in engine._unify_with_attvar_support(args[0], args[1], subst):
                return None
            return subst
        return subst if unify(args[0], args[1], subst) is None else None

    @staticmethod
    def _negation_as_failure(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        r"""Negation-as-failure (\+/1 and \+/2+).
        
        \+/1: Standard negation-as-failure for a single goal.
        \+/2+: Due to operator precedence, \+(p(X), q(X)) may parse as a 2-argument
               compound. This handles that case by converting multiple arguments
               into a conjunction.
        """
        # Handle multiple arguments by converting to conjunction
        if len(args) == 1:
            goal = args[0]
        else:
            # Convert multiple arguments to a conjunction: \+(A, B, C) -> \+((A, B, C))
            goal = Compound(",", args)
        
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
        # ISO standard: call/1 with a variable should raise instantiation_error
        engine._check_instantiated(args[0], subst, "call/1")
        goal = deref(args[0], subst)
        # ISO standard: call/1 with non-callable should raise type_error
        engine._check_type(goal, (Compound, Atom), "callable", subst, "call/1")
        # Check if the predicate exists before calling it
        engine._check_predicate_exists(goal, "call/1")
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

    @staticmethod
    def _builtin_setup_call_cleanup(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """setup_call_cleanup/3 - Execute goal with setup and cleanup.

        setup_call_cleanup(+Setup, +Goal, +Cleanup)
        Calls Setup once, then calls Goal (possibly multiple times via backtracking),
        and always calls Cleanup after Goal completes (whether it succeeds or fails).
        """
        setup_goal, goal, cleanup_goal = args

        # First, execute setup once. If it fails, the loop is not entered.
        for setup_subst in engine._solve_goals([deref(setup_goal, subst)], subst):
            last_subst = setup_subst
            try:
                # Yield all solutions from the goal.
                goal_d = deref(goal, setup_subst)
                for solution_subst in engine._solve_goals([goal_d], setup_subst):
                    last_subst = solution_subst
                    yield solution_subst
            finally:
                # Always call cleanup after goal completes, fails, or is interrupted.
                cleanup_goal_d = deref(cleanup_goal, last_subst)
                for _ in engine._solve_goals([cleanup_goal_d], last_subst):
                    break  # Just call cleanup once, ignore its result
            # Only run setup once.
            break

    @staticmethod
    def _builtin_call_cleanup(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """call_cleanup/2 - Execute goal with cleanup.

        call_cleanup(+Goal, +Cleanup)
        Calls Goal (possibly multiple times via backtracking),
        and always calls Cleanup after Goal completes (whether it succeeds or fails).
        """
        goal, cleanup_goal = args
        last_subst = subst

        try:
            # Yield all solutions from the goal.
            goal_d = deref(goal, subst)
            for solution_subst in engine._solve_goals([goal_d], subst):
                last_subst = solution_subst
                yield solution_subst
        finally:
            # Always call cleanup after goal completes, fails, or is interrupted.
            cleanup_goal_d = deref(cleanup_goal, last_subst)
            for _ in engine._solve_goals([cleanup_goal_d], last_subst):
                break  # Just call cleanup once, ignore its result

    @staticmethod
    def _builtin_forall(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """forall/2 - Universal quantification."""
        condition_term, action_term = args

        engine._check_instantiated(condition_term, subst, "forall/2")
        engine._check_type(condition_term, (Compound, Atom), "callable", subst, "forall/2")

        any_solution = False
        for condition_subst in engine._solve_goals([deref(condition_term, subst)], subst):
            any_solution = True
            action_goal = deref(action_term, condition_subst)
            engine._check_instantiated(action_goal, condition_subst, "forall/2")
            engine._check_type(action_goal, (Compound, Atom), "callable", condition_subst, "forall/2")

            action_succeeded = False
            for _ in engine._solve_goals([action_goal], condition_subst):
                action_succeeded = True
                break

            if not action_succeeded:
                return iter_empty()

        if not any_solution:
            return iter([subst])

        return iter([subst])

    @staticmethod
    def _builtin_ignore(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """ignore/1 - Execute goal and always succeed."""
        goal_term = deref(args[0], subst)
        engine._check_instantiated(goal_term, subst, "ignore/1")
        engine._check_type(goal_term, (Compound, Atom), "callable", subst, "ignore/1")
        engine._check_predicate_exists(goal_term, "ignore/1")

        for solution in engine._solve_goals([goal_term], subst):
            yield solution
            return

        yield subst

    @staticmethod
    def _builtin_apply(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """apply/2 - Call a predicate with arguments supplied as a list."""
        goal_term_raw, args_term_raw = args

        engine._check_instantiated(goal_term_raw, subst, "apply/2")
        engine._check_instantiated(args_term_raw, subst, "apply/2")

        goal_term = deref(goal_term_raw, subst)
        args_term = deref(args_term_raw, subst)

        engine._check_type(goal_term, (Compound, Atom), "callable", subst, "apply/2")
        engine._check_type(args_term, List, "list", subst, "apply/2")

        try:
            arg_values = list_to_python(args_term, subst)
        except TypeError:
            error_term = PrologError.type_error("list", args_term, "apply/2")
            raise PrologThrow(error_term)

        if isinstance(goal_term, Atom):
            callable_goal = Compound(goal_term.name, tuple(arg_values))
        else:
            callable_goal = Compound(goal_term.functor, goal_term.args + tuple(arg_values))

        engine._check_predicate_exists(callable_goal, "apply/2")
        yield from engine._solve_goals([callable_goal], subst)


__all__ = ["ControlBuiltins"]
