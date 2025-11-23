"""Prolog query engine with backtracking."""

from __future__ import annotations

import inspect
from collections.abc import Iterator as IteratorABC
from typing import Callable, Iterator, TypeAlias

from prolog.builtins.exceptions import PrologThrow
from prolog.parser import Atom, Clause, Compound, Cut, List, Variable
from prolog.unification import Substitution, apply_substitution, unify
from prolog.utils.list_utils import list_to_python, python_to_list

BuiltinResult: TypeAlias = Iterator[Substitution] | Substitution | None
BuiltinHandler: TypeAlias = Callable[
    [tuple, Substitution, "PrologEngine | None"], BuiltinResult
]
BuiltinRegistry: TypeAlias = dict[tuple[str, int], BuiltinHandler]


class CutException(Exception):
    """Exception raised when cut (!) is executed to prevent backtracking."""


class PrologEngine:
    """Prolog inference engine."""

    def __init__(self, clauses: list[Clause]):
        self.clauses = clauses
        self.call_depth = 0
        self.max_depth = 1000  # Prevent infinite recursion
        self._fresh_var_counter = 0
        self._builtin_registry = self._build_builtin_registry()

    # Compatibility wrappers retained for tests and external callers.
    def _list_to_python(
        self, prolog_list: List, subst: Substitution | None = None
    ) -> list:
        return list_to_python(prolog_list, subst)

    def _python_to_list(self, py_list: list) -> List:
        return python_to_list(py_list)

    def query(self, goals: list[Compound]) -> Iterator[Substitution]:
        """Query the knowledge base and yield all substitutions that satisfy the goals."""
        try:
            yield from self._solve_goals(goals, Substitution())
        except PrologThrow:
            # Unhandled throw - query fails with no solutions
            return

    def _solve_goals(
        self, goals: list[Compound], subst: Substitution
    ) -> Iterator[Substitution]:
        """Solve a list of goals with backtracking."""
        if not goals:
            yield subst
            return

        goal = goals[0]
        remaining_goals = goals[1:]

        goal = apply_substitution(goal, subst)

        builtin_results = self._try_builtin(goal, subst)
        if builtin_results is not None:
            try:
                for new_subst in builtin_results:
                    if new_subst is not None:
                        yield from self._solve_goals(remaining_goals, new_subst)
            except CutException:
                raise
            except PrologThrow:
                raise
            return

        cut_executed = False
        for clause in self.clauses:
            if cut_executed:
                break

            renamed_clause = self._rename_variables(clause)
            new_subst = unify(goal, renamed_clause.head, subst)
            if new_subst is not None:
                body_goals: list[Compound | Atom | Cut]
                if renamed_clause.is_fact():
                    body_goals = []
                else:
                    body_goals = self._flatten_body(renamed_clause.body)
                clause_has_cut = any(isinstance(g, Cut) for g in body_goals)
                try:
                    if renamed_clause.is_fact():
                        yield from self._solve_goals(remaining_goals, new_subst)
                    else:
                        new_goals = body_goals + remaining_goals
                        yield from self._solve_goals(new_goals, new_subst)
                except CutException:
                    if clause_has_cut:
                        cut_executed = True
                    else:
                        raise
                except PrologThrow:
                    raise

    def _try_builtin(
        self, goal: Compound, subst: Substitution
    ) -> Iterator[Substitution] | None:
        """Try to solve goal as a built-in predicate. Returns None if not a builtin."""
        if isinstance(goal, Cut):

            def cut_wrapper():
                yield subst
                raise CutException()

            return cut_wrapper()

        key: tuple[str, int] | None = None
        args: tuple = ()

        if isinstance(goal, Compound):
            key = (goal.functor, len(goal.args))
            args = goal.args
        elif isinstance(goal, Atom):
            key = (goal.name, 0)

        if key is None:
            return None

        handler = self._builtin_registry.get(key)
        if handler is None:
            return None

        result = handler(args, subst, self)
        return self._normalize_builtin_result(result)

    def _build_builtin_registry(self) -> BuiltinRegistry:
        """Create the functor/arity dispatch table for built-in predicates."""
        registry: BuiltinRegistry = {}

        from prolog.builtins import (
            all_solutions,
            arithmetic,
            control,
            database,
            exceptions,
            higher_order,
            io,
            list_ops,
            reflection,
            term_manipulation,
            type_tests,
        )

        for module in [
            type_tests.TypeTestBuiltins,
            arithmetic.ArithmeticBuiltins,
            io.IOBuiltins,
            list_ops.ListOperationsBuiltins,
            term_manipulation.TermManipulationBuiltins,
            control.ControlBuiltins,
            database.DatabaseBuiltins,
            all_solutions.AllSolutionsBuiltins,
            exceptions.ExceptionBuiltins,
            reflection.ReflectionBuiltins,
            higher_order.HigherOrderBuiltins,
        ]:
            module.register(registry, self)

        return registry

    def _register_builtin(
        self, functor: str, arity: int, registry: BuiltinRegistry, handler: Callable
    ) -> None:
        """Compatibility helper for legacy tests registering ad-hoc built-ins."""
        registry[(functor, arity)] = self._wrap_builtin_handler(handler)

    def _wrap_builtin_handler(self, handler: Callable) -> BuiltinHandler:
        """Ensure builtin handlers conform to the 3-argument calling convention."""
        try:
            signature = inspect.signature(handler)
        except (TypeError, ValueError):

            def wrapper(args, subst, _engine):
                return handler(args, subst)

            return wrapper

        required_params = [
            p
            for p in signature.parameters.values()
            if p.default is inspect.Parameter.empty
            and p.kind
            in (
                inspect.Parameter.POSITIONAL_ONLY,
                inspect.Parameter.POSITIONAL_OR_KEYWORD,
            )
        ]

        if len(required_params) >= 3:
            return handler  # type: ignore[return-value]

        def wrapper(args, subst, _engine):
            return handler(args, subst)

        return wrapper

    def _fresh_variable(self, prefix: str = "Var") -> Variable:
        """Generate a fresh variable with a stable, incrementing suffix."""
        self._fresh_var_counter += 1
        return Variable(f"_{prefix}{self._fresh_var_counter}")

    def _normalize_builtin_result(
        self, result: BuiltinResult
    ) -> Iterator[Substitution]:
        """Normalize built-in handler return values to an iterator of substitutions."""
        if result is None:
            return iter([])
        if isinstance(result, IteratorABC):
            return result
        return iter([result])

    def _flatten_body(self, body: list) -> list:
        """Flatten conjunction in clause body into a list of goals."""
        if not body:
            return []

        if len(body) == 1 and isinstance(body[0], Compound) and body[0].functor == ",":
            return self._flatten_conjunction(body[0])

        return body

    def _flatten_conjunction(self, term) -> list:
        """Recursively flatten a conjunction (,) into a list of goals."""
        if isinstance(term, Compound) and term.functor == ",":
            left = self._flatten_conjunction(term.args[0])
            right = self._flatten_conjunction(term.args[1])
            return left + right
        return [term]

    def _rename_variables(self, clause: Clause) -> Clause:
        """Rename all variables in a clause to avoid conflicts."""
        self._fresh_var_counter += 1
        suffix = str(self._fresh_var_counter)

        def rename_term(term):
            if isinstance(term, Variable):
                return Variable(f"{term.name}_{suffix}")
            if isinstance(term, Compound):
                new_args = tuple(rename_term(arg) for arg in term.args)
                return Compound(term.functor, new_args)
            if isinstance(term, List):
                new_elements = tuple(rename_term(elem) for elem in term.elements)
                new_tail = rename_term(term.tail) if term.tail is not None else None
                return List(new_elements, new_tail)
            return term

        new_head = rename_term(clause.head)
        new_body = [rename_term(goal) for goal in clause.body] if clause.body else None
        return Clause(new_head, new_body)

    def _format_to_string(self, format_term, args_term, subst):
        """Compatibility shim that delegates to the I/O built-ins formatter."""
        from prolog.builtins.io import IOBuiltins

        return IOBuiltins._format_to_string(format_term, args_term, subst)


__all__ = [
    "PrologEngine",
    "CutException",
    "PrologThrow",
    "BuiltinRegistry",
    "BuiltinHandler",
    "BuiltinResult",
]
