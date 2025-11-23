"""All-solutions built-ins (findall/3, bagof/3, setof/3).

Implements ISO-style predicates that collect all solutions to a query while
respecting free and existential variables.
"""

from __future__ import annotations

from collections import OrderedDict
from typing import Any, Callable, Iterator

from prolog.builtins import BuiltinRegistry, register_builtin
from prolog.builtins.common import BuiltinArgs, EngineContext
from prolog.parser import List, Variable
from prolog.unification import Substitution, apply_substitution, deref, unify
from prolog.utils.term_utils import term_sort_key
from prolog.utils.variable_utils import (
    collect_vars,
    collect_vars_in_order,
    strip_existentials,
)


class AllSolutionsBuiltins:
    """Built-ins that gather all solutions for a query."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register all-solutions predicate handlers into the registry."""
        register_builtin(registry, "findall", 3, AllSolutionsBuiltins._builtin_findall)
        register_builtin(registry, "bagof", 3, AllSolutionsBuiltins._builtin_bagof)
        register_builtin(registry, "setof", 3, AllSolutionsBuiltins._builtin_setof)

    @staticmethod
    def _builtin_findall(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        template, goal, result = args
        solutions = []
        for solution_subst in engine._solve_goals([goal], subst):
            instantiated = apply_substitution(template, solution_subst)
            solutions.append(instantiated)

        result_list = List(tuple(solutions), None)
        return unify(result, result_list, subst)

    @staticmethod
    def _builtin_bagof(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution] | None:
        template, goal, result = args
        free_vars, groups = AllSolutionsBuiltins._collect_bagof_groups(
            template, goal, subst, engine
        )
        if not groups:
            return None
        return AllSolutionsBuiltins._yield_grouped_results(
            free_vars, groups, result, subst, lambda sols: sols
        )

    @staticmethod
    def _builtin_setof(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution] | None:
        template, goal, result = args
        free_vars, groups = AllSolutionsBuiltins._collect_bagof_groups(
            template, goal, subst, engine
        )
        if not groups:
            return None
        return AllSolutionsBuiltins._yield_grouped_results(
            free_vars,
            groups,
            result,
            subst,
            AllSolutionsBuiltins._unique_and_sort_solutions,
        )

    @staticmethod
    def _yield_grouped_results(
        free_vars: list[str],
        groups: OrderedDict,
        result: Any,
        subst: Substitution,
        transform: Callable[[list], list],
    ) -> Iterator[Substitution]:
        def _results():
            for key, solutions in groups.items():
                processed_solutions = transform(solutions)

                group_subst = subst
                for var_name, value in zip(free_vars, key):
                    group_subst = unify(Variable(var_name), value, group_subst)
                    if group_subst is None:
                        break
                if group_subst is None:
                    continue

                result_list = List(tuple(processed_solutions), None)
                final_subst = unify(result, result_list, group_subst)
                if final_subst is not None:
                    yield final_subst

        return _results()

    @staticmethod
    def _unique_and_sort_solutions(solutions: list) -> list:
        unique_solutions = list(OrderedDict.fromkeys(solutions))
        try:
            return sorted(unique_solutions, key=lambda x: term_sort_key(x))
        except TypeError:
            return unique_solutions

    @staticmethod
    def _collect_bagof_groups(
        template: Any, goal: Any, subst: Substitution, engine: EngineContext
    ) -> tuple[list[str], OrderedDict]:
        template = deref(template, subst)
        goal = deref(goal, subst)

        goal, existential_vars = strip_existentials(goal, subst)

        template_vars = collect_vars(template, subst)
        goal_vars_in_order = collect_vars_in_order(goal, subst)

        seen: set[str] = set()
        free_vars: list[str] = []
        for var in goal_vars_in_order:
            if var in seen or var in template_vars or var in existential_vars:
                continue
            seen.add(var)
            free_vars.append(var)

        groups: OrderedDict[tuple, list] = OrderedDict()
        for solution_subst in engine._solve_goals([goal], subst):
            instantiated = apply_substitution(template, solution_subst)
            key = tuple(
                apply_substitution(Variable(var), solution_subst) for var in free_vars
            )
            groups.setdefault(key, []).append(instantiated)

        return free_vars, groups


__all__ = ["AllSolutionsBuiltins"]
