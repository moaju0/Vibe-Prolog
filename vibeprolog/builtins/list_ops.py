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
        register_builtin(registry, "msort", 2, ListOperationsBuiltins._builtin_msort)
        register_builtin(registry, "keysort", 2, ListOperationsBuiltins._builtin_keysort)
        register_builtin(registry, "nth0", 3, ListOperationsBuiltins._builtin_nth0)
        register_builtin(registry, "nth1", 3, ListOperationsBuiltins._builtin_nth1)
        register_builtin(registry, "last", 2, ListOperationsBuiltins._builtin_last)
        register_builtin(registry, "select", 3, ListOperationsBuiltins._builtin_select)
        register_builtin(registry, "memberchk", 2, ListOperationsBuiltins._builtin_memberchk)
        register_builtin(registry, "is_list", 1, ListOperationsBuiltins._builtin_is_list)
        register_builtin(registry, "sumlist", 2, ListOperationsBuiltins._builtin_sumlist)
        register_builtin(registry, "sum_list", 2, ListOperationsBuiltins._builtin_sumlist)  # Alias
        register_builtin(registry, "max_list", 2, ListOperationsBuiltins._builtin_max_list)
        register_builtin(registry, "min_list", 2, ListOperationsBuiltins._builtin_min_list)

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

    @staticmethod
    def _builtin_msort(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        lst, sorted_lst = args
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            sorted_py = sorted(py_list, key=term_sort_key)
        except TypeError:
            sorted_py = py_list

        result = python_to_list(sorted_py)
        return unify(sorted_lst, result, subst)

    @staticmethod
    def _builtin_keysort(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        pairs, sorted_pairs = args
        pairs = deref(pairs, subst)

        if not isinstance(pairs, List):
            raise PrologThrow(PrologError.type_error("list", pairs))

        try:
            py_list = list_to_python(pairs, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", pairs))

        # Check each element is a Key-Value pair
        for item in py_list:
            if not (hasattr(item, 'functor') and item.functor == '-' and len(item.args) == 2):
                raise PrologThrow(PrologError.type_error("pair", item))

        # Sort by key (first argument of the pair)
        try:
            sorted_py = sorted(py_list, key=lambda pair: term_sort_key(pair.args[0]))
        except TypeError:
            sorted_py = py_list

        result = python_to_list(sorted_py)
        return unify(sorted_pairs, result, subst)

    @staticmethod
    def _builtin_nth0(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        index, lst, elem = args
        index = deref(index, subst)
        lst = deref(lst, subst)
        elem = deref(elem, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        # Mode 1: Index is bound, get element
        if isinstance(index, Number) and isinstance(index.value, int):
            idx = index.value
            if idx < 0:
                raise PrologThrow(PrologError.domain_error("not_less_than_zero", index))
            if idx < len(py_list):
                new_subst = unify(elem, py_list[idx], subst)
                if new_subst is not None:
                    yield new_subst
            # Out of range: fail silently
        elif isinstance(index, Number) and not isinstance(index.value, int):
            # Index is a number but not an integer
            raise PrologThrow(PrologError.type_error("integer", index))
        elif not isinstance(index, Variable):
            # Index is bound but not a number
            raise PrologThrow(PrologError.type_error("integer", index))

        # Mode 2: Element is bound, find index (backtracking)
        elif not isinstance(elem, Variable):
            for i, item in enumerate(py_list):
                if terms_equal(elem, item):
                    new_subst = unify(index, Number(i), subst)
                    if new_subst is not None:
                        yield new_subst

        # Mode 3: Generate list with element at index
        elif isinstance(index, Number) and isinstance(index.value, int):
            idx = index.value
            if idx < 0:
                raise PrologThrow(PrologError.domain_error("not_less_than_zero", index))
            if isinstance(elem, Variable):
                # Generate list with elem at position idx
                if idx == 0:
                    # [Elem | Tail] where Tail is fresh
                    tail_var = engine._fresh_variable("Tail_")
                    result_list = List((elem,), tail_var)
                    new_subst = unify(lst, result_list, subst)
                    if new_subst is not None:
                        yield new_subst
                else:
                    # Build list with elem at idx
                    prefix = [engine._fresh_variable(f"Elem{i}_") for i in range(idx)]
                    tail_var = engine._fresh_variable("Tail_")
                    elements = tuple(prefix) + (elem,) + (tail_var,)
                    result_list = List(elements, None)
                    new_subst = unify(lst, result_list, subst)
                    if new_subst is not None:
                        yield new_subst

    @staticmethod
    def _builtin_nth1(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        index, lst, elem = args
        index = deref(index, subst)

        if isinstance(index, Number) and isinstance(index.value, int):
            if index.value < 1:
                raise PrologThrow(PrologError.domain_error("not_less_than_one", index))
            # Convert to 0-based
            zero_index = Number(index.value - 1)
            yield from ListOperationsBuiltins._builtin_nth0(
                (zero_index, lst, elem), subst, engine
            )
        else:
            # For backtracking mode, we need to handle the conversion
            for subst_result in ListOperationsBuiltins._builtin_nth0(
                (Variable(), lst, elem), subst, engine
            ):
                # Get the 0-based index from the result
                zero_idx_term = deref(Variable(), subst_result)
                if isinstance(zero_idx_term, Number) and isinstance(zero_idx_term.value, int):
                    one_idx = Number(zero_idx_term.value + 1)
                    final_subst = unify(index, one_idx, subst_result)
                    if final_subst is not None:
                        yield final_subst

    @staticmethod
    def _builtin_last(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        lst, elem = args
        lst = deref(lst, subst)
        elem = deref(elem, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        if not py_list:
            return  # Empty list, fail

        last_elem = py_list[-1]
        new_subst = unify(elem, last_elem, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_select(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        elem, lst, remainder = args
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        # Find all positions where elem matches
        for i, item in enumerate(py_list):
            new_subst = unify(elem, item, subst)
            if new_subst is not None:
                # Create remainder by removing element at i
                remainder_py = py_list[:i] + py_list[i+1:]
                remainder_list = python_to_list(remainder_py)
                final_subst = unify(remainder, remainder_list, new_subst)
                if final_subst is not None:
                    yield final_subst

    @staticmethod
    def _builtin_memberchk(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        elem, lst = args
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        # Check for first match only (deterministic)
        for item in py_list:
            new_subst = unify(elem, item, subst)
            if new_subst is not None:
                yield new_subst
                return

    @staticmethod
    def _builtin_is_list(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        term, = args
        term = deref(term, subst)

        # Unbound variables are not lists
        if isinstance(term, Variable):
            return

        current = term
        while isinstance(current, List):
            if current.tail is None:
                # Ends with []
                yield subst
                return
            current = deref(current.tail, subst)

        # Not a proper list
        return

    @staticmethod
    def _builtin_sumlist(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        lst, sum_result = args
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        total = 0
        for item in py_list:
            if not isinstance(item, Number):
                raise PrologThrow(PrologError.type_error("number", item))
            total += item.value

        new_subst = unify(sum_result, Number(total), subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_max_list(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        lst, max_result = args
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        if not py_list:
            raise PrologThrow(PrologError.domain_error("not_empty_list", lst))

        # Check all are numbers and collect values
        values = []
        for item in py_list:
            if not isinstance(item, Number):
                raise PrologThrow(PrologError.type_error("number", item))
            values.append(item.value)

        max_val = max(values)
        new_subst = unify(max_result, Number(max_val), subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_min_list(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        lst, min_result = args
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            raise PrologThrow(PrologError.type_error("list", lst))

        try:
            py_list = list_to_python(lst, subst)
        except TypeError:
            raise PrologThrow(PrologError.type_error("list", lst))

        if not py_list:
            raise PrologThrow(PrologError.domain_error("not_empty_list", lst))

        # Check all are numbers and collect values
        values = []
        for item in py_list:
            if not isinstance(item, Number):
                raise PrologThrow(PrologError.type_error("number", item))
            values.append(item.value)

        min_val = min(values)
        new_subst = unify(min_result, Number(min_val), subst)
        if new_subst is not None:
            yield new_subst


__all__ = ["ListOperationsBuiltins"]
