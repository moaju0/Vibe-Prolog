"""Higher-order built-ins (maplist/2).

Provides higher-order predicates such as ``maplist/2``.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Compound
from vibeprolog.unification import Substitution, deref
from vibeprolog.utils.list_utils import list_to_python


class HigherOrderBuiltins:
    """Built-ins for higher-order predicates."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register higher-order predicate handlers."""
        register_builtin(registry, "maplist", 2, HigherOrderBuiltins._builtin_maplist)

    @staticmethod
    def _builtin_maplist(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        goal_template, lst = args
        goal_template = deref(goal_template, subst)
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            return

        try:
            elements = list_to_python(lst, subst)
        except TypeError:
            return

        def apply_goal(
            index: int, current_subst: Substitution
        ) -> Iterator[Substitution]:
            if index >= len(elements):
                yield current_subst
                return

            elem = elements[index]

            if isinstance(goal_template, Atom):
                goal = Compound(goal_template.name, (elem,))
            elif isinstance(goal_template, Compound):
                goal = Compound(goal_template.functor, goal_template.args + (elem,))
            else:
                return

            for solution in engine._solve_goals([goal], current_subst):
                yield from apply_goal(index + 1, solution)

        yield from apply_goal(0, subst)


__all__ = ["HigherOrderBuiltins"]
