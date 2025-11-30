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
    def _builtin_maplist_3(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """maplist/3 - Apply binary predicate to corresponding elements of two lists."""
        pred, list1, list2 = args
        pred = deref(pred, subst)
        list1 = deref(list1, subst)
        list2 = deref(list2, subst)

        # Type checking
        if not isinstance(list1, List):
            raise PrologThrow(PrologError.type_error("list", list1, "maplist/3"))
        if not isinstance(list2, List):
            raise PrologThrow(PrologError.type_error("list", list2, "maplist/3"))

        try:
            py_list1 = list_to_python(list1, subst)
            py_list2 = list_to_python(list2, subst)
        except TypeError:
            return

        if len(py_list1) != len(py_list2):
            return  # Lists must be same length

        def apply_pred(index: int, current_subst: Substitution) -> Iterator[Substitution]:
            if index >= len(py_list1):
                yield current_subst
                return

            elem1 = py_list1[index]
            elem2 = py_list2[index]

            # Create goal: pred(elem1, elem2)
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem1, elem2))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem1, elem2))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "maplist/3"))

            for solution in engine._solve_goals([goal], current_subst):
                yield from apply_pred(index + 1, solution)

        yield from apply_pred(0, subst)

    @staticmethod
    def _builtin_maplist_4(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """maplist/4 - Apply ternary predicate to corresponding elements of three lists."""
        pred, list1, list2, list3 = args
        pred = deref(pred, subst)
        list1 = deref(list1, subst)
        list2 = deref(list2, subst)
        list3 = deref(list3, subst)

        # Type checking
        if not isinstance(list1, List):
            raise PrologThrow(PrologError.type_error("list", list1, "maplist/4"))
        if not isinstance(list2, List):
            raise PrologThrow(PrologError.type_error("list", list2, "maplist/4"))
        if not isinstance(list3, List):
            raise PrologThrow(PrologError.type_error("list", list3, "maplist/4"))

        try:
            py_list1 = list_to_python(list1, subst)
            py_list2 = list_to_python(list2, subst)
            py_list3 = list_to_python(list3, subst)
        except TypeError:
            return

        if len(py_list1) != len(py_list2) or len(py_list2) != len(py_list3):
            return  # All lists must be same length

        def apply_pred(index: int, current_subst: Substitution) -> Iterator[Substitution]:
            if index >= len(py_list1):
                yield current_subst
                return

            elem1 = py_list1[index]
            elem2 = py_list2[index]
            elem3 = py_list3[index]

            # Create goal: pred(elem1, elem2, elem3)
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem1, elem2, elem3))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem1, elem2, elem3))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "maplist/4"))

            for solution in engine._solve_goals([goal], current_subst):
                yield from apply_pred(index + 1, solution)

        yield from apply_pred(0, subst)

    @staticmethod
    def _builtin_maplist_5(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """maplist/5 - Apply 4-ary predicate to corresponding elements of four lists."""
        pred, list1, list2, list3, list4 = args
        pred = deref(pred, subst)
        list1 = deref(list1, subst)
        list2 = deref(list2, subst)
        list3 = deref(list3, subst)
        list4 = deref(list4, subst)

        # Type checking
        if not isinstance(list1, List):
            raise PrologThrow(PrologError.type_error("list", list1, "maplist/5"))
        if not isinstance(list2, List):
            raise PrologThrow(PrologError.type_error("list", list2, "maplist/5"))
        if not isinstance(list3, List):
            raise PrologThrow(PrologError.type_error("list", list3, "maplist/5"))
        if not isinstance(list4, List):
            raise PrologThrow(PrologError.type_error("list", list4, "maplist/5"))

        try:
            py_list1 = list_to_python(list1, subst)
            py_list2 = list_to_python(list2, subst)
            py_list3 = list_to_python(list3, subst)
            py_list4 = list_to_python(list4, subst)
        except TypeError:
            return

        if len(py_list1) != len(py_list2) or len(py_list2) != len(py_list3) or len(py_list3) != len(py_list4):
            return  # All lists must be same length

        def apply_pred(index: int, current_subst: Substitution) -> Iterator[Substitution]:
            if index >= len(py_list1):
                yield current_subst
                return

            elem1 = py_list1[index]
            elem2 = py_list2[index]
            elem3 = py_list3[index]
            elem4 = py_list4[index]

            # Create goal: pred(elem1, elem2, elem3, elem4)
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem1, elem2, elem3, elem4))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem1, elem2, elem3, elem4))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "maplist/5"))

            for solution in engine._solve_goals([goal], current_subst):
                yield from apply_pred(index + 1, solution)

        yield from apply_pred(0, subst)

    @staticmethod
    def _builtin_include(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """include/3 - Filter list keeping elements that satisfy predicate."""
        pred, list_in, list_out = args
        pred = deref(pred, subst)
        list_in = deref(list_in, subst)
        list_out = deref(list_out, subst)

        if not isinstance(list_in, List):
            raise PrologThrow(PrologError.type_error("list", list_in, "include/3"))

        try:
            py_list_in = list_to_python(list_in, subst)
        except TypeError:
            return

        filtered = []
        for elem in py_list_in:
            # Test if pred(elem) succeeds
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem,))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem,))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "include/3"))

            # If predicate succeeds for this element, include it
            if any(engine._solve_goals([goal], subst)):
                filtered.append(elem)

        result_list = python_to_list(filtered)
        new_subst = unify(list_out, result_list, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_exclude(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """exclude/3 - Filter list removing elements that satisfy predicate."""
        pred, list_in, list_out = args
        pred = deref(pred, subst)
        list_in = deref(list_in, subst)
        list_out = deref(list_out, subst)

        if not isinstance(list_in, List):
            raise PrologThrow(PrologError.type_error("list", list_in, "exclude/3"))

        try:
            py_list_in = list_to_python(list_in, subst)
        except TypeError:
            return

        filtered = []
        for elem in py_list_in:
            # Test if pred(elem) succeeds
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem,))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem,))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "exclude/3"))

            # If predicate fails for this element, include it
            if not any(engine._solve_goals([goal], subst)):
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
    def _builtin_foldl_4(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """foldl/4 - Fold list from left with accumulator (single list)."""
        pred, list_in, acc0, acc = args
        pred = deref(pred, subst)
        list_in = deref(list_in, subst)
        acc0 = deref(acc0, subst)
        acc = deref(acc, subst)

        if not isinstance(list_in, List):
            raise PrologThrow(PrologError.type_error("list", list_in, "foldl/4"))

        try:
            py_list_in = list_to_python(list_in, subst)
        except TypeError:
            return

        current_acc = acc0
        for elem in py_list_in:
            # Call pred(elem, current_acc, new_acc)
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem, current_acc, acc))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem, current_acc, acc))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "foldl/4"))

            # Find solutions for the accumulator
            solutions = list(engine._solve_goals([goal], subst))
            if not solutions:
                return  # No solution, fail

            # Use the first solution and update accumulator
            current_acc = deref(acc, solutions[0])

        # Final accumulator should unify with acc
        new_subst = unify(acc, current_acc, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_foldl_5(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """foldl/5 - Fold two lists from left with accumulator."""
        pred, list1, list2, acc0, acc = args
        pred = deref(pred, subst)
        list1 = deref(list1, subst)
        list2 = deref(list2, subst)
        acc0 = deref(acc0, subst)
        acc = deref(acc, subst)

        if not isinstance(list1, List):
            raise PrologThrow(PrologError.type_error("list", list1, "foldl/5"))
        if not isinstance(list2, List):
            raise PrologThrow(PrologError.type_error("list", list2, "foldl/5"))

        try:
            py_list1 = list_to_python(list1, subst)
            py_list2 = list_to_python(list2, subst)
        except TypeError:
            return

        if len(py_list1) != len(py_list2):
            return  # Lists must be same length

        current_acc = acc0
        for elem1, elem2 in zip(py_list1, py_list2):
            # Call pred(elem1, elem2, current_acc, new_acc)
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem1, elem2, current_acc, acc))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem1, elem2, current_acc, acc))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "foldl/5"))

            # Find solutions for the accumulator
            solutions = list(engine._solve_goals([goal], subst))
            if not solutions:
                return  # No solution, fail

            # Use the first solution and update accumulator
            current_acc = deref(acc, solutions[0])

        # Final accumulator should unify with acc
        new_subst = unify(acc, current_acc, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_foldl_6(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """foldl/6 - Fold three lists from left with accumulator."""
        pred, list1, list2, list3, acc0, acc = args
        pred = deref(pred, subst)
        list1 = deref(list1, subst)
        list2 = deref(list2, subst)
        list3 = deref(list3, subst)
        acc0 = deref(acc0, subst)
        acc = deref(acc, subst)

        if not isinstance(list1, List):
            raise PrologThrow(PrologError.type_error("list", list1, "foldl/6"))
        if not isinstance(list2, List):
            raise PrologThrow(PrologError.type_error("list", list2, "foldl/6"))
        if not isinstance(list3, List):
            raise PrologThrow(PrologError.type_error("list", list3, "foldl/6"))

        try:
            py_list1 = list_to_python(list1, subst)
            py_list2 = list_to_python(list2, subst)
            py_list3 = list_to_python(list3, subst)
        except TypeError:
            return

        if len(py_list1) != len(py_list2) or len(py_list2) != len(py_list3):
            return  # All lists must be same length

        current_acc = acc0
        for elem1, elem2, elem3 in zip(py_list1, py_list2, py_list3):
            # Call pred(elem1, elem2, elem3, current_acc, new_acc)
            if isinstance(pred, Atom):
                goal = Compound(pred.name, (elem1, elem2, elem3, current_acc, acc))
            elif isinstance(pred, Compound):
                goal = Compound(pred.functor, pred.args + (elem1, elem2, elem3, current_acc, acc))
            else:
                raise PrologThrow(PrologError.type_error("callable", pred, "foldl/6"))

            # Find solutions for the accumulator
            solutions = list(engine._solve_goals([goal], subst))
            if not solutions:
                return  # No solution, fail

            # Use the first solution and update accumulator
            current_acc = deref(acc, solutions[0])

        # Final accumulator should unify with acc
        new_subst = unify(acc, current_acc, subst)
        if new_subst is not None:
            yield new_subst


__all__ = ["HigherOrderBuiltins"]
