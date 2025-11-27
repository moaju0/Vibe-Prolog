"""Prolog query engine with backtracking."""

from __future__ import annotations

import inspect
from collections.abc import Iterator as IteratorABC
from typing import Any, Callable, Iterator, TypeAlias
import sys
from dataclasses import dataclass

from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import Clause, Cut, List
from vibeprolog.streams import Stream
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution, apply_substitution, deref, unify
from vibeprolog.utils.list_utils import list_to_python, python_to_list

BuiltinResult: TypeAlias = Iterator[Substitution] | Substitution | None
BuiltinHandler: TypeAlias = Callable[
    [tuple, Substitution, "PrologEngine | None"], BuiltinResult
]
BuiltinRegistry: TypeAlias = dict[tuple[str, int], BuiltinHandler]


class CutException(Exception):
    """Exception raised when cut (!) is executed to prevent backtracking."""


class PrologEngine:
    """Prolog inference engine."""

    def __init__(
        self,
        clauses: list[Clause],
        argv: list[str] | None = None,
        predicate_properties: dict[tuple[str, int], set[str]] | None = None,
        predicate_sources: dict[tuple[str, int], set[str]] | None = None,
        predicate_docs: dict[tuple[str, int], str] | None = None,
    ):
        self.clauses = clauses
        # Explicit dependency so engine can reference interpreter state if needed
        self.interpreter = None
        self.argv = argv or []
        self.call_depth = 0
        self.max_depth = 1000  # Prevent infinite recursion
        self._fresh_var_counter = 0
        self._builtin_registry = self._build_builtin_registry()
        self.predicate_properties: dict[tuple[str, int], set[str]] = (
            predicate_properties if predicate_properties is not None else {}
        )
        self.predicate_sources: dict[tuple[str, int], set[str]] = (
            predicate_sources if predicate_sources is not None else {}
        )
        self.predicate_docs: dict[tuple[str, int], str] = (
            predicate_docs if predicate_docs is not None else {}
        )
        self._initialize_builtin_properties()
        # Index of user-defined predicates for O(1) existence checks
        self._predicate_index: set[tuple[str, int]] = self._build_predicate_index()
        # Stream management
        self._streams: dict[str, Stream] = {}
        self._stream_counter = 0
        self._initialize_standard_streams()

    # Compatibility wrappers retained for tests and external callers.
    def _list_to_python(
        self, prolog_list: List, subst: Substitution | None = None
    ) -> list:
        return list_to_python(prolog_list, subst)

    def _python_to_list(self, py_list: list) -> List:
        return python_to_list(py_list)

    def query(self, goals: list[Compound]) -> Iterator[Substitution]:
        """Query the knowledge base and yield all substitutions that satisfy the goals."""
        yield from self._solve_goals(goals, Substitution())

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
        # Module-qualified call: Module:Goal
        if isinstance(goal, Compound) and goal.functor == ":" and len(goal.args) == 2:
            module_term = goal.args[0]
            inner_goal = goal.args[1]

            # Module must be an atom
            if not isinstance(module_term, Atom):
                error_term = PrologError.type_error("atom", module_term, "module:goal")
                raise PrologThrow(error_term)

            module_name = module_term.name
            # Interpreter must be available to check modules
            interpreter = getattr(self, "interpreter", None)
            if interpreter is None or module_name not in getattr(interpreter, "modules", {}):
                indicator = module_term
                error_term = PrologError.existence_error("module", indicator, "module:goal")
                raise PrologThrow(error_term)

            module_obj = interpreter.modules[module_name]

            # Check export list for the inner goal
            key = None
            if isinstance(inner_goal, Compound):
                key = (inner_goal.functor, len(inner_goal.args))
            elif isinstance(inner_goal, Atom):
                key = (inner_goal.name, 0)

            # Builtins are always accessible
            is_builtin = key in self._builtin_registry if key is not None else False
            if module_name != "user" and not is_builtin:
                if key not in module_obj.exports:
                    indicator = self._indicator_from_key(key[0], key[1]) if key is not None else inner_goal
                    error_term = PrologError.permission_error(
                        "access", "private_procedure", indicator, "module:goal"
                    )
                    raise PrologThrow(error_term)

            # Try builtins first (builtins are module-global)
            builtin_results = self._try_builtin(inner_goal, subst)
            if builtin_results is not None:
                return builtin_results

            # Delegate clause-search to the module's predicate index when available
            for result in self._solve_module_predicate(module_name, key, inner_goal, subst, remaining_goals):
                yield result
            return

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

    def _solve_module_predicate(self, module_name, key, inner_goal, subst, remaining_goals):
        # Resolve a module-qualified goal by consulting the module's predicate index if available.
        module = getattr(self.interpreter, "modules", {}).get(module_name)
        if module is None:
            return iter(())
        # Prefer indexed predicates if available
        preds = getattr(module, "predicates", {}).get(key, [])
        cut_executed = False
        for clause in preds:
            if cut_executed:
                break

            renamed_clause = self._rename_variables(clause)
            new_subst = unify(inner_goal, renamed_clause.head, subst)
            if new_subst is not None:
                body_goals = [] if renamed_clause.is_fact() else self._flatten_body(renamed_clause.body)
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

        from vibeprolog.builtins import (
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

    def _initialize_builtin_properties(self) -> None:
        """Mark built-in predicates as static and built_in."""

        for key in self._builtin_registry.keys():
            properties = self.predicate_properties.setdefault(key, set())
            properties.update({"static", "built_in"})

        cut_key = ("!", 0)
        properties = self.predicate_properties.setdefault(cut_key, set())
        properties.update({"static", "built_in"})

    def _build_predicate_index(self) -> set[tuple[str, int]]:
        """Build an index of all user-defined predicates for fast existence checks.

        Returns:
            Set of (functor, arity) tuples for all defined predicates
        """
        index: set[tuple[str, int]] = set()
        for clause in self.clauses:
            head = clause.head
            if isinstance(head, Compound):
                index.add((head.functor, len(head.args)))
            elif isinstance(head, Atom):
                index.add((head.name, 0))
        return index

    def _register_builtin(
        self, functor: str, arity: int, registry: BuiltinRegistry, handler: Callable
    ) -> None:
        """Compatibility helper for legacy tests registering ad-hoc built-ins."""
        registry[(functor, arity)] = self._wrap_builtin_handler(handler)
        properties = self.predicate_properties.setdefault((functor, arity), set())
        properties.update({"static", "built_in"})

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
        from vibeprolog.builtins.io import IOBuiltins

        return IOBuiltins._format_to_string(format_term, args_term, subst)

    def _check_instantiated(self, term: Any, subst: Substitution, predicate: str) -> None:
        """Raise instantiation_error if term is an unbound variable.

        Args:
            term: The term to check
            subst: Current substitution
            predicate: Name of the calling predicate (e.g., 'arg/3')

        Raises:
            PrologThrow with instantiation_error term
        """
        term = deref(term, subst)
        if isinstance(term, Variable):
            error_term = PrologError.instantiation_error(predicate)
            raise PrologThrow(error_term)

    def _check_type(self, term: Any, expected_type: type | tuple[type, ...],
                    type_name: str, subst: Substitution, predicate: str) -> None:
        """Raise type_error if term doesn't match expected type.

        Args:
            term: The term to check
            expected_type: Python type or tuple of types to check against
            type_name: ISO name of the type (e.g., 'integer', 'atom')
            subst: Current substitution
            predicate: Name of the calling predicate

        Raises:
            PrologThrow with type_error term
        """
        term = deref(term, subst)
        if not isinstance(term, expected_type):
            error_term = PrologError.type_error(type_name, term, predicate)
            raise PrologThrow(error_term)

    def _check_domain(self, value: Any, is_valid: Callable[[Any], bool],
                     domain_name: str, predicate: str) -> None:
        """Raise domain_error if value is outside valid domain.

        Args:
            value: The value to check
            is_valid: Predicate function that returns True if value is valid
            domain_name: Name of the valid domain (e.g., 'not_less_than_zero')
            predicate: Name of the calling predicate

        Raises:
            PrologThrow with domain_error term
        """
        if not is_valid(value):
            error_term = PrologError.domain_error(domain_name, value, predicate)
            raise PrologThrow(error_term)

    def _predicate_exists(self, goal: Any) -> bool:
        """Check if a predicate exists (either as builtin or user-defined).

        Args:
            goal: The goal to check (typically Compound or Atom, but accepts any term)

        Returns:
            True if the predicate exists, False otherwise
        """
        key: tuple[str, int] | None = None

        if isinstance(goal, Compound):
            key = (goal.functor, len(goal.args))
        elif isinstance(goal, Atom):
            key = (goal.name, 0)
        else:
            return False

        # Check if it's a builtin or user-defined predicate (O(1) lookup)
        return key in self._builtin_registry or key in self._predicate_index

    def _check_predicate_exists(self, goal: Any, context: str) -> None:
        """Raise existence_error if predicate doesn't exist.

        Args:
            goal: The goal to check (typically Compound or Atom, but accepts any term)
            context: Name of the calling predicate (e.g., 'call/1')

        Raises:
            PrologThrow with existence_error term
        """
        if not self._predicate_exists(goal):
            # Create the procedure indicator (functor/arity)
            if isinstance(goal, Compound):
                indicator = Compound("/", (Atom(goal.functor), Number(len(goal.args))))
            elif isinstance(goal, Atom):
                indicator = Compound("/", (Atom(goal.name), Number(0)))
            else:
                # For non-callable terms, still raise an error
                indicator = goal

            error_term = PrologError.existence_error("procedure", indicator, context)
            raise PrologThrow(error_term)

    def _indicator_from_key(self, functor: str, arity: int) -> Compound:
        """Create a predicate indicator compound from a key tuple."""

        return Compound("/", (Atom(functor), Number(arity)))

    def _get_predicate_properties(self, key: tuple[str, int]) -> set[str]:
        """Return the property set for a predicate, defaulting to static."""

        properties = self.predicate_properties.setdefault(key, set())
        if not properties:
            properties.add("static")
        if "dynamic" in properties and "static" in properties:
            properties.discard("static")
        return properties

    def _ensure_dynamic_permission(self, key: tuple[str, int], context: str) -> None:
        """Raise permission_error if predicate is not dynamic."""

        functor, arity = key
        properties = self._get_predicate_properties(key)
        if "dynamic" not in properties:
            indicator = self._indicator_from_key(functor, arity)
            error_term = PrologError.permission_error(
                "modify", "static_procedure", indicator, context
            )
            raise PrologThrow(error_term)

    def _add_predicate_to_index(self, clause: Clause) -> None:
        """Add a predicate to the index after asserting a clause.

        Args:
            clause: The clause being added
        """
        head = clause.head
        if isinstance(head, Compound):
            self._predicate_index.add((head.functor, len(head.args)))
        elif isinstance(head, Atom):
            self._predicate_index.add((head.name, 0))

    def _record_predicate_source(self, key: tuple[str, int], source: str) -> None:
        """Track which source provided clauses for a predicate."""

        sources = self.predicate_sources.setdefault(key, set())
        sources.add(source)

    def _remove_predicate_from_index_if_empty(self, functor: str, arity: int) -> None:
        """Remove a predicate from the index if no clauses remain for it.

        Args:
            functor: The predicate functor name
            arity: The predicate arity
        """
        # Check if any clauses still exist for this predicate
        for clause in self.clauses:
            head = clause.head
            if isinstance(head, Compound):
                if head.functor == functor and len(head.args) == arity:
                    return  # Still has clauses, don't remove from index
            elif isinstance(head, Atom):
                if head.name == functor and arity == 0:
                    return  # Still has clauses, don't remove from index

        # No clauses remain, remove from index
        self._predicate_index.discard((functor, arity))

    def _initialize_standard_streams(self) -> None:
        """Initialize the three standard streams: user_input, user_output, user_error."""
        standard_streams_data = [
            ("user_input", Atom("user_input"), sys.stdin, "read"),
            ("user_output", Atom("user_output"), sys.stdout, "write"),
            ("user_error", Atom("user_error"), sys.stderr, "write"),
        ]

        for name, handle, file_obj, mode in standard_streams_data:
            stream = Stream(handle=handle, file_obj=file_obj, mode=mode)
            self._streams[name] = stream

    def _generate_stream_handle(self) -> Atom:
        """Generate a unique stream handle atom."""
        self._stream_counter += 1
        return Atom(f"$stream_{self._stream_counter}")

    def get_stream(self, handle: Atom) -> Stream | None:
        """Get a stream by its handle atom."""
        return self._streams.get(handle.name)

    def add_stream(self, stream: Stream) -> None:
        """Add a stream to the registry."""
        self._streams[stream.handle.name] = stream

    def remove_stream(self, handle: Atom) -> Stream | None:
        """Remove and return a stream from the registry."""
        return self._streams.pop(handle.name, None)


__all__ = [
    "PrologEngine",
    "CutException",
    "PrologThrow",
    "BuiltinRegistry",
    "BuiltinHandler",
    "BuiltinResult",
]
