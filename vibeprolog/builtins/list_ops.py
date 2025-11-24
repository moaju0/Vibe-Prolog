"""List operation built-ins (member/2, append/3, length/2, reverse/2, sort/2).

Implements ISO-style list predicates including membership, concatenation,
length calculation, reversal, and sorting with duplicate removal.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import List, Number, Variable
from vibeprolog.unification import Substitution, deref, unify
from vibeprolog.utils.list_utils import (
    compute_list_length,
    fresh_list_of_length,
    list_to_python,
    match_list_to_length,
    python_to_list,
)
from vibeprolog.utils.term_utils import term_sort_key, terms_equal


class ListOperationsBuiltins:
    """Built-ins for working with lists."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register list predicate handlers into the registry."""
        register_builtin(registry, "member", 2, ListOperationsBuiltins._builtin_member)
        register_builtin(registry, "append", 3, ListOperationsBuiltins._builtin_append)
        register_builtin(registry, "length", 2, ListOperationsBuiltins._builtin_length)
        register_builtin(
            registry, "reverse", 2, ListOperationsBuiltins._builtin_reverse
        )
        register_builtin(registry, "sort", 2, ListOperationsBuiltins._builtin_sort)

    @staticmethod
    def _builtin_member(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        elem, lst = args
        lst = deref(lst, subst)

        if isinstance(lst, List):
            for item in lst.elements:
                new_subst = unify(elem, item, subst)
                if new_subst is not None:
                    yield new_subst

            if lst.tail is not None:
                yield from ListOperationsBuiltins._builtin_member(
                    (elem, lst.tail), subst, engine
                )

    @staticmethod
    def _builtin_append(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        list1, list2, result = args
        list1 = deref(list1, subst)
        list2 = deref(list2, subst)
        result = deref(result, subst)

        if (
            isinstance(list1, List)
            and not isinstance(list1, Variable)
            and isinstance(list2, List)
        ):
            try:
                py_list1 = list_to_python(list1, subst)
                py_list2 = list_to_python(list2, subst)
            except TypeError:
                pass
            else:
                concatenated = py_list1 + py_list2
                result_list = python_to_list(concatenated)
                new_subst = unify(result, result_list, subst)
                if new_subst is not None:
                    yield new_subst
                return

        empty_list = List(tuple(), None)
        subst1 = unify(list1, empty_list, subst)
        if subst1 is not None:
            subst2 = unify(list2, result, subst1)
            if subst2 is not None:
                yield subst2

        if isinstance(list1, List) and list1.elements:
            head = list1.elements[0]
            tail1 = (
                List(list1.elements[1:], list1.tail)
                if len(list1.elements) > 1
                else (list1.tail if list1.tail is not None else List(tuple(), None))
            )
            tail3_var = engine._fresh_variable("AppendT3_")
            result_list = List((head,), tail3_var)
            subst1 = unify(result, result_list, subst)
            if subst1 is not None:
                yield from ListOperationsBuiltins._builtin_append(
                    (tail1, list2, tail3_var), subst1, engine
                )
        elif (
            isinstance(list1, Variable) and isinstance(result, List) and result.elements
        ):
            head = result.elements[0]
            tail3 = (
                List(result.elements[1:], result.tail)
                if len(result.elements) > 1
                else (result.tail if result.tail is not None else List(tuple(), None))
            )
            tail1_var = engine._fresh_variable("AppendT1_")
            list1_val = List((head,), tail1_var)
            subst1 = unify(list1, list1_val, subst)
            if subst1 is not None:
                yield from ListOperationsBuiltins._builtin_append(
                    (tail1_var, list2, tail3), subst1, engine
                )

    @staticmethod
    def _builtin_length(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        lst, length = args
        lst = deref(lst, subst)
        length = deref(length, subst)

        if isinstance(length, Number):
            n = int(length.value)
            # Check for negative length (domain error)
            engine._check_domain(n, lambda x: x >= 0, 'not_less_than_zero', 'length/2')

            if isinstance(lst, List):
                new_subst = match_list_to_length(
                    lst, n, subst, fresh_variable=engine._fresh_variable
                )
                if new_subst is not None:
                    yield new_subst
            else:
                result_list = fresh_list_of_length(n, engine._fresh_variable)
                new_subst = unify(lst, result_list, subst)
                if new_subst is not None:
                    yield new_subst
            return

        if isinstance(lst, List):
            count = compute_list_length(lst, subst)
            if count is None:
                return

            new_subst = unify(length, Number(count), subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _builtin_reverse(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        lst, reversed_lst = args
        lst = deref(lst, subst)
        reversed_lst = deref(reversed_lst, subst)

        if isinstance(lst, List):
            try:
                py_list = list_to_python(lst, subst)
            except TypeError:
                pass
            else:
                reversed_py = list(reversed(py_list))
                result = python_to_list(reversed_py)
                new_subst = unify(reversed_lst, result, subst)
                if new_subst is not None:
                    yield new_subst
        elif isinstance(reversed_lst, List):
            try:
                py_list = list_to_python(reversed_lst, subst)
            except TypeError:
                pass
            else:
                reversed_py = list(reversed(py_list))
                result = python_to_list(reversed_py)
                new_subst = unify(lst, result, subst)
                if new_subst is not None:
                    yield new_subst

    @staticmethod
    def _builtin_sort(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        lst, sorted_lst = args
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            return None

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            return None

        unique = []
        seen = []
        for item in py_list:
            is_duplicate = False
            for seen_item in seen:
                if terms_equal(item, seen_item):
                    is_duplicate = True
                    break
            if not is_duplicate:
                unique.append(item)
                seen.append(item)

        try:
            sorted_py = sorted(unique, key=lambda x: term_sort_key(x))
        except TypeError:
            sorted_py = unique

        result = python_to_list(sorted_py)
        return unify(sorted_lst, result, subst)


__all__ = ["ListOperationsBuiltins"]
