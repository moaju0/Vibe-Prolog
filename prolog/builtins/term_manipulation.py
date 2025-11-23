"""Term manipulation built-ins (functor/3, arg/3, =../2, copy_term/2, term comparisons).

Implements ISO-style predicates for deconstructing and constructing terms.
"""

from __future__ import annotations

from typing import Iterator

from prolog.builtins import BuiltinRegistry, register_builtin
from prolog.builtins.common import BuiltinArgs, EngineContext
from prolog.parser import Atom, Compound, List, Number, Variable
from prolog.unification import Substitution, deref, unify
from prolog.utils.term_utils import term_sort_key, terms_equal
from prolog.utils.variable_utils import copy_term_recursive


class TermManipulationBuiltins:
    """Built-ins for inspecting and constructing terms."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register term manipulation predicate handlers."""
        for op in ["==", r"\==", "@<", "@=<", "@>", "@>="]:
            register_builtin(
                registry,
                op,
                2,
                lambda args,
                subst,
                engine,
                op=op: TermManipulationBuiltins._builtin_term_compare(
                    op, args, subst, engine
                ),
            )
        register_builtin(
            registry, "functor", 3, TermManipulationBuiltins._builtin_functor
        )
        register_builtin(registry, "arg", 3, TermManipulationBuiltins._builtin_arg)
        register_builtin(registry, "=..", 2, TermManipulationBuiltins._builtin_univ)
        register_builtin(
            registry, "copy_term", 2, TermManipulationBuiltins._builtin_copy_term
        )

    @staticmethod
    def _builtin_term_compare(
        op: str, args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        left_term = deref(args[0], subst)
        right_term = deref(args[1], subst)

        if op == "==":
            if terms_equal(left_term, right_term):
                return subst
        elif op == "\\==":
            if not terms_equal(left_term, right_term):
                return subst
        else:
            left_key = term_sort_key(left_term)
            right_key = term_sort_key(right_term)

            if op == "@<" and left_key < right_key:
                return subst
            if op == "@=<" and left_key <= right_key:
                return subst
            if op == "@>" and left_key > right_key:
                return subst
            if op == "@>=" and left_key >= right_key:
                return subst

        return None

    @staticmethod
    def _builtin_functor(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        term, name, arity = args
        term = deref(term, subst)
        name = deref(name, subst)
        arity = deref(arity, subst)

        if not isinstance(term, Variable):
            if isinstance(term, Compound):
                functor_name = Atom(term.functor)
                functor_arity = Number(len(term.args))

                new_subst = unify(name, functor_name, subst)
                if new_subst is not None:
                    new_subst = unify(arity, functor_arity, new_subst)
                    if new_subst is not None:
                        yield new_subst
            elif isinstance(term, Atom):
                new_subst = unify(name, term, subst)
                if new_subst is not None:
                    new_subst = unify(arity, Number(0), new_subst)
                    if new_subst is not None:
                        yield new_subst
            elif isinstance(term, Number):
                new_subst = unify(name, term, subst)
                if new_subst is not None:
                    new_subst = unify(arity, Number(0), new_subst)
                    if new_subst is not None:
                        yield new_subst
        elif isinstance(name, Atom) and isinstance(arity, Number):
            arity_val = int(arity.value)
            if arity_val == 0:
                new_subst = unify(term, name, subst)
                if new_subst is not None:
                    yield new_subst
            else:
                args_vals = tuple(
                    engine._fresh_variable(f"Arg{i}_") for i in range(arity_val)
                )
                compound = Compound(name.name, args_vals)
                new_subst = unify(term, compound, subst)
                if new_subst is not None:
                    yield new_subst

    @staticmethod
    def _builtin_arg(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        n, term, arg = args
        n = deref(n, subst)
        term = deref(term, subst)

        if not isinstance(n, Number):
            return None

        if not isinstance(term, Compound):
            return None

        n_val = int(n.value)
        if n_val < 1 or n_val > len(term.args):
            return None

        selected_arg = term.args[n_val - 1]
        return unify(arg, selected_arg, subst)

    @staticmethod
    def _builtin_univ(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        term, lst = args
        term = deref(term, subst)
        lst = deref(lst, subst)

        if not isinstance(term, Variable):
            if isinstance(term, Compound):
                elements = [Atom(term.functor)] + list(term.args)
                result_list = List(tuple(elements), None)
                return unify(lst, result_list, subst)
            if isinstance(term, Atom):
                result_list = List((term,), None)
                return unify(lst, result_list, subst)
            if isinstance(term, Number):
                result_list = List((term,), None)
                return unify(lst, result_list, subst)
        elif isinstance(lst, List) and lst.elements:
            first = lst.elements[0]
            if isinstance(first, Atom):
                if len(lst.elements) == 1:
                    return unify(term, first, subst)
                functor = first.name
                args_vals = tuple(lst.elements[1:])
                compound = Compound(functor, args_vals)
                return unify(term, compound, subst)
            if isinstance(first, Number) and len(lst.elements) == 1:
                return unify(term, first, subst)

        return None

    @staticmethod
    def _builtin_copy_term(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        source, copy = args
        source = deref(source, subst)
        var_map: dict[Variable, Variable] = {}
        copied_term = copy_term_recursive(source, var_map, engine._fresh_variable)
        return unify(copy, copied_term, subst)


__all__ = ["TermManipulationBuiltins"]
