"""Higher-order built-ins (maplist/2)."""

from __future__ import annotations

from typing import Any, Iterator

from prolog.builtins import register_builtin
from prolog.parser import Atom, Compound, List
from prolog.unification import Substitution, deref
from prolog.utils.list_utils import list_to_python


class HigherOrderBuiltins:
    """Built-ins for higher-order predicates."""

    @staticmethod
    def register(registry, _engine) -> None:
        register_builtin(registry, "maplist", 2, HigherOrderBuiltins._builtin_maplist)

    @staticmethod
    def _builtin_maplist(args: tuple[Any, ...], subst: Substitution, engine) -> Iterator[Substitution]:
        goal_template, lst = args
        goal_template = deref(goal_template, subst)
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            return

        try:
            elements = list_to_python(lst, subst)
        except TypeError:
            return

        def apply_goal(index: int, current_subst: Substitution) -> Iterator[Substitution]:
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
