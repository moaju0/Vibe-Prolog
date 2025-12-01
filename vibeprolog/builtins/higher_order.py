"""Higher-order built-ins (maplist/2).

Provides higher-order predicates such as ``maplist/2``.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Compound
from vibeprolog.unification import Substitution, deref, unify
from vibeprolog.utils.list_utils import list_to_python, python_to_list


class HigherOrderBuiltins:
    """Built-ins for higher-order predicates."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register higher-order predicate handlers."""
        register_builtin(registry, "maplist", 2, HigherOrderBuiltins._builtin_maplist)
        register_builtin(registry, "maplist", 3, HigherOrderBuiltins._builtin_maplist_3)
        register_builtin(registry, "maplist", 4, HigherOrderBuiltins._builtin_maplist_4)
        register_builtin(registry, "maplist", 5, HigherOrderBuiltins._builtin_maplist_5)
        register_builtin(registry, "include", 3, HigherOrderBuiltins._builtin_include)
        register_builtin(registry, "exclude", 3, HigherOrderBuiltins._builtin_exclude)
        register_builtin(registry, "partition", 4, HigherOrderBuiltins._builtin_partition)
        register_builtin(registry, "foldl", 4, HigherOrderBuiltins._builtin_foldl_4)
        register_builtin(registry, "foldl", 5, HigherOrderBuiltins._builtin_foldl_5)
        register_builtin(registry, "foldl", 6, HigherOrderBuiltins._builtin_foldl_6)

    @staticmethod
    def _create_goal(pred, *args, context: str):
        # Helper to create a goal from pred/args
        if isinstance(pred, Atom):
            return Compound(pred.name, args)
        elif isinstance(pred, Compound):
            return Compound(pred.functor, pred.args + args)
        else:
            raise PrologThrow(PrologError.type_error("callable", pred, context))

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

    @staticmethod
    def _maplist_n(args: BuiltinArgs, subst: Substitution, engine: EngineContext, arity: int) -> Iterator[Substitution]:
        """Generic helper for maplist/3-5 predicates."""
        pred, *input_lists, output = args
        context = f"maplist/{arity}"
        pred = deref(pred, subst)
        output = deref(output, subst)

        # Validate and convert input lists
        py_lists = []
        for l in input_lists:
            l_deref = deref(l, subst)
            if not isinstance(l_deref, List):
                raise PrologThrow(PrologError.type_error("list", l_deref, context))
            try:
                py_lists.append(list_to_python(l_deref, subst))
            except TypeError:
                return

        if not py_lists:
            return

        first_len = len(py_lists[0])
        if not all(len(pl) == first_len for pl in py_lists[1:]):
            return

        from vibeprolog.terms import Variable
        results = []
        current_subst = subst
        for i in range(first_len):
            elems = tuple(pl[i] for pl in py_lists)
            result_var = Variable(f"_maplist_{i}")
            goal = HigherOrderBuiltins._create_goal(pred, *elems, result_var, context=context)
            try:
                solution = next(engine._solve_goals([goal], current_subst))
            except StopIteration:
                return
            results.append(deref(result_var, solution))
            current_subst = solution

        result_list = python_to_list(results)
        new_subst = unify(output, result_list, current_subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_maplist_3(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """maplist/3 - Apply binary predicate to corresponding elements of two lists."""
        yield from HigherOrderBuiltins._maplist_n(args, subst, engine, 3)

    @staticmethod
    def _builtin_maplist_4(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """maplist/4 - Apply ternary predicate to corresponding elements of three lists."""
        yield from HigherOrderBuiltins._maplist_n(args, subst, engine, 4)

    @staticmethod
    def _builtin_maplist_5(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """maplist/5 - Apply 4-ary predicate to corresponding elements of four lists."""
        yield from HigherOrderBuiltins._maplist_n(args, subst, engine, 5)

    @staticmethod
    def _builtin_include(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """include/3 - Filter list keeping elements that satisfy predicate."""
        pred, list_in, list_out = args
        pred = deref(pred, subst)
        list_in = deref(list_in, subst)
        list_out = deref(list_out, subst)

        yield from HigherOrderBuiltins._filter_list(args, subst, engine, include=True)

    @staticmethod
    def _builtin_exclude(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """exclude/3 - Filter list removing elements that satisfy predicate."""
        pred, list_in, list_out = args
        pred = deref(pred, subst)
        list_in = deref(list_in, subst)
        list_out = deref(list_out, subst)

        yield from HigherOrderBuiltins._filter_list(args, subst, engine, include=False)

    @staticmethod
    def _filter_list(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext, *, include: bool
    ) -> Iterator[Substitution]:
        """Helper for include/3 and exclude/3."""
        pred, list_in, list_out = args
        context = "include/3" if include else "exclude/3"
        pred = deref(pred, subst)
        list_in = deref(list_in, subst)
        list_out = deref(list_out, subst)

        if not isinstance(list_in, List):
            raise PrologThrow(PrologError.type_error("list", list_in, context))

        try:
            py_list_in = list_to_python(list_in, subst)
        except TypeError:
            return

        filtered = []
        for elem in py_list_in:
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem,))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem,))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, context))

            succeeds = any(engine._solve_goals([goal], subst))
            if succeeds if include else not succeeds:
                filtered.append(elem)

        result_list = python_to_list(filtered)
        new_subst = unify(list_out, result_list, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_partition(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """partition/4 - Split list based on predicate."""
        pred, list_in, included, excluded = args
        pred = deref(pred, subst)
        list_in = deref(list_in, subst)
        included = deref(included, subst)
        excluded = deref(excluded, subst)

        if not isinstance(list_in, List):
            raise PrologThrow(PrologError.type_error("list", list_in, "partition/4"))

        try:
            py_list_in = list_to_python(list_in, subst)
        except TypeError:
            return

        included_list = []
        excluded_list = []

        for elem in py_list_in:
            # Test if pred(elem) succeeds
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem,))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem,))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "partition/4"))

            # Partition based on predicate result
            if any(engine._solve_goals([goal], subst)):
                included_list.append(elem)
            else:
                excluded_list.append(elem)

        included_result = python_to_list(included_list)
        excluded_result = python_to_list(excluded_list)

        new_subst = unify(included, included_result, subst)
        if new_subst is not None:
            new_subst = unify(excluded, excluded_result, new_subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _builtin_foldl_n(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext, arity: int
    ) -> Iterator[Substitution]:
        """Generic foldl/N implementation."""
        pred, *lists, acc0, acc = args
        context = f"foldl/{arity}"

        pred = deref(pred, subst)
        acc0 = deref(acc0, subst)
        acc = deref(acc, subst)

        # Validate and convert lists
        py_lists = []
        for l in lists:
            l_deref = deref(l, subst)
            if not isinstance(l_deref, List):
                raise PrologThrow(PrologError.type_error("list", l_deref, context))
            try:
                py_lists.append(list_to_python(l_deref, subst))
            except TypeError:
                return

        # Check lengths
        if py_lists and not all(len(pl) == len(py_lists[0]) for pl in py_lists[1:]):
            return

        first_len = len(py_lists[0]) if py_lists else 0
        current_acc = acc0

        for idx in range(first_len):
            goal_args = tuple(pl[idx] for pl in py_lists) + (current_acc, acc)
            if isinstance(pred, Atom):
                goal = Compound(pred.name, goal_args)
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + goal_args)
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, context))

            try:
                solution = next(engine._solve_goals([goal], subst))
            except StopIteration:
                return  # No solution, fail

            current_acc = deref(acc, solution)

        new_subst = unify(acc, current_acc, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_foldl_4(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """foldl/4 - Fold list from left with accumulator (single list)."""
        yield from HigherOrderBuiltins._builtin_foldl_n(args, subst, engine, 4)

    @staticmethod
    def _builtin_foldl_5(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """foldl/5 - Fold two lists from left with accumulator."""
        yield from HigherOrderBuiltins._builtin_foldl_n(args, subst, engine, 5)

    @staticmethod
    def _builtin_foldl_6(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """foldl/6 - Fold three lists from left with accumulator."""
        yield from HigherOrderBuiltins._builtin_foldl_n(args, subst, engine, 6)


__all__ = ["HigherOrderBuiltins"]
