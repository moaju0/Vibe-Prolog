"""Prolog query engine with backtracking."""

from typing import Callable, Iterator, TypeAlias
from collections import OrderedDict
from collections.abc import Iterator as IteratorABC
from prolog.parser import Compound, Atom, Variable, Number, List, Clause, Cut
from prolog.unification import Substitution, apply_substitution, deref, unify
from prolog.utils.list_utils import (
    compute_list_length,
    fresh_list_of_length,
    list_to_python,
    match_list_to_length,
    python_to_list,
)
from prolog.utils.term_utils import term_sort_key, term_to_string, terms_equal
from prolog.utils.variable_utils import (
    collect_vars,
    collect_vars_in_order,
    copy_term_recursive,
    strip_existentials,
)


BuiltinResult: TypeAlias = Iterator[Substitution] | Substitution | None
BuiltinHandler: TypeAlias = Callable[[tuple, Substitution], BuiltinResult]
BuiltinRegistry: TypeAlias = dict[tuple[str, int], BuiltinHandler]


class CutException(Exception):
    """Exception raised when cut (!) is executed to prevent backtracking."""
    pass


class PrologThrow(Exception):
    """Exception raised when throw/1 is executed to unwind the call stack."""
    def __init__(self, term):
        self.term = term


class PrologEngine:
    """Prolog inference engine."""

    def __init__(self, clauses: list[Clause]):
        self.clauses = clauses
        self.call_depth = 0
        self.max_depth = 1000  # Prevent infinite recursion
        self._fresh_var_counter = 0
        self._builtin_registry = self._build_builtin_registry()

    # Compatibility wrappers retained for tests and external callers.
    def _list_to_python(self, prolog_list: List, subst: Substitution | None = None) -> list:
        return list_to_python(prolog_list, subst)

    def _python_to_list(self, py_list: list) -> List:
        return python_to_list(py_list)

    def query(self, goals: list[Compound]) -> Iterator[Substitution]:
        """
        Query the knowledge base.
        Yields all substitutions that satisfy the goals.
        """
        try:
            yield from self._solve_goals(goals, Substitution())
        except PrologThrow:
            # Unhandled throw - query fails with no solutions
            return

    def _solve_goals(self, goals: list[Compound], subst: Substitution) -> Iterator[Substitution]:
        """Solve a list of goals with backtracking."""
        if not goals:
            # No more goals - success!
            yield subst
            return

        # Take first goal
        goal = goals[0]
        remaining_goals = goals[1:]

        # Apply current substitution to the goal
        goal = apply_substitution(goal, subst)

        # Try built-in predicates first
        builtin_results = self._try_builtin(goal, subst)
        if builtin_results is not None:
            try:
                for new_subst in builtin_results:
                    if new_subst is not None:
                        yield from self._solve_goals(remaining_goals, new_subst)
            except CutException:
                # Re-raise CutException so it propagates to clause selection
                raise
            except PrologThrow:
                # Re-raise PrologThrow so it propagates up to catch/3
                raise
            return

        # Try to unify with clauses in knowledge base
        cut_executed = False
        for clause in self.clauses:
            if cut_executed:
                # Cut was executed in a previous clause - don't try more clauses
                break

            # Rename variables in clause to avoid conflicts
            renamed_clause = self._rename_variables(clause)

            # Try to unify goal with clause head
            new_subst = unify(goal, renamed_clause.head, subst)
            if new_subst is not None:
                # Determine if this clause's body contains a cut. We need this
                # to know whether to consume a CutException here (cut belongs
                # to this predicate) or let it propagate to the caller (cut
                # occurred in a deeper predicate).
                body_goals: list[Compound | Atom | Cut]
                if renamed_clause.is_fact():
                    body_goals = []
                else:
                    body_goals = self._flatten_body(renamed_clause.body)
                clause_has_cut = any(isinstance(g, Cut) for g in body_goals)
                try:
                    if renamed_clause.is_fact():
                        # It's a fact - continue with remaining goals
                        yield from self._solve_goals(remaining_goals, new_subst)
                    else:
                        # It's a rule - add body goals before remaining goals
                        new_goals = body_goals + remaining_goals
                        yield from self._solve_goals(new_goals, new_subst)
                except CutException:
                    # If the cut belonged to this clause, consume it here and
                    # prevent backtracking to alternative clauses. Otherwise,
                    # propagate so an ancestor predicate can handle the cut.
                    if clause_has_cut:
                        cut_executed = True
                    else:
                        raise
                except PrologThrow:
                    # Re-raise PrologThrow so it propagates up to catch/3
                    raise

    def _try_builtin(self, goal: Compound, subst: Substitution) -> Iterator[Substitution] | None:
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

        result = handler(args, subst)
        return self._normalize_builtin_result(result)

    def _build_builtin_registry(self) -> BuiltinRegistry:
        """Create the functor/arity dispatch table for built-in predicates."""
        registry: BuiltinRegistry = {}

        for registrar in (
            self._register_control_builtins,
            self._register_arithmetic_builtins,
            self._register_list_builtins,
            self._register_database_builtins,
            self._register_io_builtins,
            self._register_reflection_builtins,
        ):
            registrar(registry)

        return registry

    def _register_builtin(self, functor: str, arity: int, registry: BuiltinRegistry, handler: BuiltinHandler) -> None:
        registry[(functor, arity)] = handler

    def _register_control_builtins(self, registry: BuiltinRegistry) -> None:
        self._register_builtin("=", 2, registry, lambda args, subst: unify(args[0], args[1], subst))
        self._register_builtin(r"\=", 2, registry, lambda args, subst: subst if unify(args[0], args[1], subst) is None else None)
        self._register_builtin(r"\+", 1, registry, lambda args, subst: self._negation_as_failure(args[0], subst))
        self._register_builtin(";", 2, registry, lambda args, subst: self._builtin_disjunction(args[0], args[1], subst))
        self._register_builtin("->", 2, registry, lambda args, subst: self._builtin_if_then(args[0], args[1], subst))
        self._register_builtin(",", 2, registry, lambda args, subst: self._builtin_conjunction(args[0], args[1], subst))
        self._register_builtin("call", 1, registry, lambda args, subst: self._builtin_call(args[0], subst))
        self._register_builtin("once", 1, registry, lambda args, subst: self._builtin_once(args[0], subst))
        self._register_builtin("maplist", 2, registry, lambda args, subst: self._builtin_maplist(args[0], args[1], subst))
        self._register_builtin("catch", 3, registry, lambda args, subst: self._builtin_catch(args[0], args[1], args[2], subst))
        self._register_builtin("throw", 1, registry, lambda args, subst: self._builtin_throw(args[0], subst))
        self._register_builtin("true", 0, registry, lambda _args, subst: subst)
        self._register_builtin("fail", 0, registry, lambda _args, _subst: iter([]))
        self._register_builtin("nl", 0, registry, lambda _args, subst: self._newline_builtin(subst))

    def _register_arithmetic_builtins(self, registry: BuiltinRegistry) -> None:
        self._register_builtin("is", 2, registry, lambda args, subst: self._builtin_is(args[0], args[1], subst))
        for op in ["=:=", r"=\=", "<", ">", "=<", ">="]:
            self._register_builtin(
                op,
                2,
                registry,
                lambda args, subst, op=op: self._builtin_arithmetic_compare(op, args[0], args[1], subst),
            )

    def _register_list_builtins(self, registry: BuiltinRegistry) -> None:
        self._register_builtin("member", 2, registry, lambda args, subst: self._builtin_member(args[0], args[1], subst))
        self._register_builtin("append", 3, registry, lambda args, subst: self._builtin_append(args[0], args[1], args[2], subst))
        self._register_builtin("length", 2, registry, lambda args, subst: self._builtin_length(args[0], args[1], subst))
        self._register_builtin("reverse", 2, registry, lambda args, subst: self._builtin_reverse(args[0], args[1], subst))
        self._register_builtin("sort", 2, registry, lambda args, subst: self._builtin_sort(args[0], args[1], subst))

    def _register_database_builtins(self, registry: BuiltinRegistry) -> None:
        self._register_builtin("clause", 2, registry, lambda args, subst: self._builtin_clause(args[0], args[1], subst))
        self._register_builtin("findall", 3, registry, lambda args, subst: self._builtin_findall(args[0], args[1], args[2], subst))
        self._register_builtin("bagof", 3, registry, lambda args, subst: self._builtin_bagof(args[0], args[1], args[2], subst))
        self._register_builtin("setof", 3, registry, lambda args, subst: self._builtin_setof(args[0], args[1], args[2], subst))
        self._register_builtin("asserta", 1, registry, lambda args, subst: self._builtin_assert(args[0], subst, position="front"))
        self._register_builtin("assertz", 1, registry, lambda args, subst: self._builtin_assert(args[0], subst, position="back"))
        self._register_builtin("assert", 1, registry, lambda args, subst: self._builtin_assert(args[0], subst, position="back"))
        self._register_builtin("retract", 1, registry, lambda args, subst: self._builtin_retract(args[0], subst))
        self._register_builtin("abolish", 1, registry, lambda args, subst: self._builtin_abolish(args[0], subst))

    def _register_io_builtins(self, registry: BuiltinRegistry) -> None:
        self._register_builtin("write", 1, registry, lambda args, subst: self._builtin_write(args[0], subst))
        self._register_builtin("writeln", 1, registry, lambda args, subst: self._builtin_writeln(args[0], subst))
        self._register_builtin("format", 3, registry, lambda args, subst: self._builtin_format(args[0], args[1], args[2], subst))
        self._register_builtin("format", 2, registry, lambda args, subst: self._builtin_format_stdout(args[0], args[1], subst))
        self._register_builtin("format", 1, registry, lambda args, subst: self._builtin_format_stdout(args[0], List(()), subst))

    def _register_reflection_builtins(self, registry: BuiltinRegistry) -> None:
        for op in ["==", r"\==", "@<", "@=<", "@>", "@>="]:
            self._register_builtin(op, 2, registry, lambda args, subst, op=op: self._builtin_term_compare(op, args[0], args[1], subst))

        self._register_builtin("atom", 1, registry, lambda args, subst: self._builtin_atom(args[0], subst))
        self._register_builtin("number", 1, registry, lambda args, subst: self._builtin_number(args[0], subst))
        self._register_builtin("var", 1, registry, lambda args, subst: self._builtin_var(args[0], subst))
        self._register_builtin("nonvar", 1, registry, lambda args, subst: self._builtin_nonvar(args[0], subst))
        self._register_builtin("compound", 1, registry, lambda args, subst: self._builtin_compound(args[0], subst))
        self._register_builtin("integer", 1, registry, lambda args, subst: self._builtin_integer(args[0], subst))
        self._register_builtin("float", 1, registry, lambda args, subst: self._builtin_float(args[0], subst))
        self._register_builtin("atomic", 1, registry, lambda args, subst: self._builtin_atomic(args[0], subst))
        self._register_builtin("callable", 1, registry, lambda args, subst: self._builtin_callable(args[0], subst))
        self._register_builtin("ground", 1, registry, lambda args, subst: self._builtin_ground(args[0], subst))
        self._register_builtin("functor", 3, registry, lambda args, subst: self._builtin_functor(args[0], args[1], args[2], subst))
        self._register_builtin("arg", 3, registry, lambda args, subst: self._builtin_arg(args[0], args[1], args[2], subst))
        self._register_builtin("=..", 2, registry, lambda args, subst: self._builtin_univ(args[0], args[1], subst))
        self._register_builtin("copy_term", 2, registry, lambda args, subst: self._builtin_copy_term(args[0], args[1], subst))
        self._register_builtin("predicate_property", 2, registry, lambda args, subst: self._builtin_predicate_property(args[0], args[1], subst))
        self._register_builtin("current_predicate", 1, registry, lambda args, subst: self._builtin_current_predicate(args[0], subst))

    def _fresh_variable(self, prefix: str = "Var") -> Variable:
        """Generate a fresh variable with a stable, incrementing suffix."""
        self._fresh_var_counter += 1
        return Variable(f"_{prefix}{self._fresh_var_counter}")

    def _normalize_builtin_result(self, result: BuiltinResult) -> Iterator[Substitution]:
        """Normalize built-in handler return values to an iterator of substitutions."""
        if result is None:
            return iter([])
        if isinstance(result, IteratorABC):
            return result
        return iter([result])

    def _newline_builtin(self, subst: Substitution) -> Substitution:
        """Print a newline and succeed, used by the nl/0 built-in."""
        print()
        return subst

    def _negation_as_failure(self, goal: any, subst: Substitution) -> Iterator[Substitution]:
        r"""Implement \+/1 using the registry dispatch."""
        for _ in self._solve_goals([goal], subst):
            return iter([])
        return iter([subst])

    def _builtin_member(self, elem: any, lst: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in member/2 predicate."""
        lst = deref(lst, subst)

        if isinstance(lst, List):
            # Try to unify with each element
            for item in lst.elements:
                new_subst = unify(elem, item, subst)
                if new_subst is not None:
                    yield new_subst

            # Try the tail if present
            if lst.tail is not None:
                yield from self._builtin_member(elem, lst.tail, subst)

    def _builtin_is(self, result: any, expr: any, subst: Substitution) -> Substitution | None:
        """Built-in is/2 predicate for arithmetic evaluation."""
        # Evaluate the expression
        value = self._eval_arithmetic(expr, subst)
        if value is None:
            return None

        # Unify with result
        return unify(result, Number(value), subst)

    def _builtin_arithmetic_compare(self, op: str, left: any, right: any, subst: Substitution) -> Substitution | None:
        """Built-in arithmetic comparison predicates."""
        left_val = self._eval_arithmetic(left, subst)
        right_val = self._eval_arithmetic(right, subst)

        if left_val is None or right_val is None:
            return None

        if op == "=:=" and left_val == right_val:
            return subst
        elif op == r"=\=" and left_val != right_val:
            return subst
        elif op == "<" and left_val < right_val:
            return subst
        elif op == ">" and left_val > right_val:
            return subst
        elif op == "=<" and left_val <= right_val:
            return subst
        elif op == ">=" and left_val >= right_val:
            return subst

        return None

    def _builtin_term_compare(self, op: str, left: any, right: any, subst: Substitution) -> Substitution | None:
        """Built-in term comparison predicates."""
        left_term = deref(left, subst)
        right_term = deref(right, subst)

        if op == "==":
            # Term identity (structural equality)
            if terms_equal(left_term, right_term):
                return subst
        elif op == "\\==":
            # Term non-identity
            if not terms_equal(left_term, right_term):
                return subst
        else:
            # Ordering comparisons: @<, @=<, @>, @>=
            left_key = term_sort_key(left_term)
            right_key = term_sort_key(right_term)

            if op == "@<" and left_key < right_key:
                return subst
            elif op == "@=<" and left_key <= right_key:
                return subst
            elif op == "@>" and left_key > right_key:
                return subst
            elif op == "@>=" and left_key >= right_key:
                return subst

        return None

    def _eval_arithmetic(self, expr: any, subst: Substitution) -> int | float | None:
        """Evaluate an arithmetic expression."""
        expr = deref(expr, subst)

        if isinstance(expr, Number):
            return expr.value

        if isinstance(expr, Compound):
            # Unary minus
            if expr.functor == "-" and len(expr.args) == 1:
                arg = self._eval_arithmetic(expr.args[0], subst)
                if arg is None:
                    return None
                return -arg

            # Binary operators
            if expr.functor in ["+", "-", "*", "/", "//", "mod", "**"] and len(expr.args) == 2:
                left = self._eval_arithmetic(expr.args[0], subst)
                right = self._eval_arithmetic(expr.args[1], subst)

                if left is None or right is None:
                    return None

                try:
                    if expr.functor == "+":
                        return left + right
                    elif expr.functor == "-":
                        return left - right
                    elif expr.functor == "*":
                        return left * right
                    elif expr.functor == "/":
                        return left / right
                    elif expr.functor == "//":
                        return left // right
                    elif expr.functor == "mod":
                        return left % right
                    elif expr.functor == "**":
                        return left ** right
                except ZeroDivisionError:
                    return None

        return None

    def _format_to_string(self, format_term: any, args_term: any, subst: Substitution) -> str | None:
        """Render a Prolog format/2 or format/3 invocation to a Python string."""
        format_term = deref(format_term, subst)
        args_term = deref(args_term, subst)

        if not isinstance(format_term, Atom):
            return None

        try:
            format_args = list_to_python(args_term, subst)
        except TypeError:
            return None

        coerced_args: list[str | int | float] = []
        for elem in format_args:
            elem = deref(elem, subst)
            if isinstance(elem, Number):
                coerced_args.append(elem.value)
            elif isinstance(elem, Atom):
                coerced_args.append(elem.name)
            elif isinstance(elem, Variable):
                coerced_args.append(f"_{elem.name}")
            else:
                coerced_args.append(str(elem))

        fmt = format_term.name
        result = ""
        i = 0
        arg_index = 0

        while i < len(fmt):
            if fmt[i] == "~":
                if i + 1 < len(fmt):
                    next_char = fmt[i + 1]

                    if next_char == "~":
                        result += "~"
                        i += 2
                    elif next_char == "w":
                        if arg_index < len(coerced_args):
                            result += str(coerced_args[arg_index])
                            arg_index += 1
                        i += 2
                    elif next_char == "d":
                        if arg_index < len(coerced_args):
                            result += str(int(coerced_args[arg_index]))
                            arg_index += 1
                        i += 2
                    elif next_char.isdigit():
                        j = i + 1
                        while j < len(fmt) and fmt[j].isdigit():
                            j += 1
                        if j < len(fmt) and fmt[j] == "f":
                            precision = int(fmt[i + 1 : j])
                            if arg_index < len(coerced_args):
                                result += f"{float(coerced_args[arg_index]):.{precision}f}"
                                arg_index += 1
                            i = j + 1
                        else:
                            result += fmt[i]
                            i += 1
                    elif next_char == "f":
                        if arg_index < len(coerced_args):
                            result += str(float(coerced_args[arg_index]))
                            arg_index += 1
                        i += 2
                    elif next_char == "n":
                        result += "\n"
                        i += 2
                    else:
                        result += fmt[i]
                        i += 1
                else:
                    result += fmt[i]
                    i += 1
            else:
                result += fmt[i]
                i += 1

        return result

    def _builtin_format(self, output: any, format_str: any, args_list: any, subst: Substitution) -> Substitution | None:
        """Built-in format/3 predicate for string formatting."""
        # Dereference arguments
        output = deref(output, subst)
        format_str = deref(format_str, subst)
        args_list = deref(args_list, subst)

        # Extract the variable from atom(X)
        if not isinstance(output, Compound) or output.functor != "atom" or len(output.args) != 1:
            return None

        rendered = self._format_to_string(format_str, args_list, subst)
        if rendered is None:
            return None

        return unify(output.args[0], Atom(rendered), subst)

    def _builtin_append(self, list1: any, list2: any, result: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in append/3 predicate for list concatenation."""
        list1 = deref(list1, subst)
        list2 = deref(list2, subst)
        result = deref(result, subst)

        # If list1 is a concrete list, convert it to Python list, append, and unify
        if isinstance(list1, List) and not isinstance(list1, Variable) and isinstance(list2, List):
            try:
                # Convert both lists to Python lists
                py_list1 = list_to_python(list1, subst)
                py_list2 = list_to_python(list2, subst)
            except TypeError:
                # Not proper lists; fall back to relational definition.
                pass
            else:
                # Concatenate
                concatenated = py_list1 + py_list2

                # Convert back to Prolog list
                result_list = python_to_list(concatenated)

                # Unify with result
                new_subst = unify(result, result_list, subst)
                if new_subst is not None:
                    yield new_subst
                return

        # Handle the recursive definition:
        # append([], L, L).
        empty_list = List(tuple(), None)
        subst1 = unify(list1, empty_list, subst)
        if subst1 is not None:
            subst2 = unify(list2, result, subst1)
            if subst2 is not None:
                yield subst2

        # append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).
        # This handles both:
        # 1. When list1 is [H|T1] (partially or fully instantiated)
        # 2. When list1 is a variable and result is [H|T3] (split mode)

        # Case 1: list1 is a non-empty list [H|T1]
        if isinstance(list1, List) and list1.elements:
            # List1 is [H|T1]
            head = list1.elements[0]
            tail1 = List(list1.elements[1:], list1.tail) if len(list1.elements) > 1 else (list1.tail if list1.tail is not None else List(tuple(), None))

            # Result should be [H|T3] where append(T1, L2, T3)
            # Create T3 as a fresh variable
            tail3_var = self._fresh_variable("AppendT3_")

            # Result should unify with [H|T3]
            result_list = List((head,), tail3_var)
            subst1 = unify(result, result_list, subst)
            if subst1 is not None:
                # Recursively solve append(T1, L2, T3)
                yield from self._builtin_append(tail1, list2, tail3_var, subst1)

        # Case 2: list1 is a variable and result is a non-empty list (split mode)
        # Generate all possible splits of result into list1 and list2
        elif isinstance(list1, Variable) and isinstance(result, List) and result.elements:
            # Try list1 = [H|T1] where result = [H|T3] and append(T1, L2, T3)
            # Extract head from result
            head = result.elements[0]
            # Get tail of result
            tail3 = List(result.elements[1:], result.tail) if len(result.elements) > 1 else (result.tail if result.tail is not None else List(tuple(), None))

            # Create fresh variable for tail of list1
            tail1_var = self._fresh_variable("AppendT1_")

            # list1 should be [H|T1]
            list1_val = List((head,), tail1_var)
            subst1 = unify(list1, list1_val, subst)
            if subst1 is not None:
                # Recursively solve append(T1, L2, T3)
                yield from self._builtin_append(tail1_var, list2, tail3, subst1)

    def _builtin_clause(self, head: any, body: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in clause/2 predicate to retrieve clause bodies."""
        head = deref(head, subst)

        # Search through all clauses for matching heads
        for clause in self.clauses:
            # Rename variables in clause to avoid conflicts
            renamed_clause = self._rename_variables(clause)

            # Try to unify the head
            new_subst = unify(head, renamed_clause.head, subst)
            if new_subst is not None:
                # Get the body
                if renamed_clause.is_fact():
                    # Facts have body = true
                    body_term = Atom("true")
                else:
                    # Convert body list to a single term (conjunction)
                    if len(renamed_clause.body) == 1:
                        body_term = renamed_clause.body[0]
                    else:
                        # Build conjunction: (A, (B, C))
                        body_term = renamed_clause.body[0]
                        for goal in renamed_clause.body[1:]:
                            body_term = Compound(",", (body_term, goal))

                # Unify with the body parameter
                final_subst = unify(body, body_term, new_subst)
                if final_subst is not None:
                    yield final_subst

    def _builtin_call(self, goal: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in call/1 predicate to call a goal dynamically."""
        goal = deref(goal, subst)

        # Call the goal as if it were a regular goal
        yield from self._solve_goals([goal], subst)

    def _builtin_once(self, goal: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in once/1 predicate to call a goal and commit to first solution.

        once(Goal) succeeds at most once. If Goal succeeds, once/1 succeeds once
        with the first solution and does not backtrack into Goal.
        If Goal fails, once/1 fails.
        """
        goal = deref(goal, subst)

        # Try to get the first solution from the goal
        for solution in self._solve_goals([goal], subst):
            # Yield the first solution
            yield solution
            # Then stop - don't try to get more solutions
            return

    def _builtin_write(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in write/1 predicate to print a term."""
        term = deref(term, subst)

        # Convert term to string and print it
        output = term_to_string(term)
        print(output, end='')

        # write always succeeds
        return subst

    def _builtin_writeln(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in writeln/1 predicate to print a term followed by newline."""
        term = deref(term, subst)

        # Convert term to string and print it
        output = term_to_string(term)
        print(output)

        # writeln always succeeds
        return subst

    def _builtin_maplist(self, goal_template: any, lst: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in maplist/2 predicate to apply a goal to each element of a list."""
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

            for solution in self._solve_goals([goal], current_subst):
                yield from apply_goal(index + 1, solution)

        yield from apply_goal(0, subst)

    def _builtin_disjunction(self, left: any, right: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in ;/2 predicate for disjunction (or)."""
        # Special case: if left is (Cond -> Then), this is if-then-else
        if isinstance(left, Compound) and left.functor == "->" and len(left.args) == 2:
            condition = left.args[0]
            then_part = left.args[1]
            else_part = right

            for condition_subst in self._solve_goals([condition], subst):
                yield from self._solve_goals([then_part], condition_subst)
                return

            yield from self._solve_goals([else_part], subst)
        else:
            # Normal disjunction - try left branch first
            yield from self._solve_goals([left], subst)
            # Then try right branch
            yield from self._solve_goals([right], subst)

    def _builtin_if_then(self, condition: any, then_part: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in ->/2 predicate for if-then."""
        for condition_subst in self._solve_goals([condition], subst):
            # Condition succeeded - execute then part with first solution
            # and commit to it (cut-like behavior)
            yield from self._solve_goals([then_part], condition_subst)
            return

    def _builtin_conjunction(self, left: any, right: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in ,/2 predicate for conjunction (and)."""
        # Flatten the conjunction into a list of goals
        goals = self._flatten_conjunction(Compound(",", (left, right)))
        # Solve all goals in sequence
        yield from self._solve_goals(goals, subst)

    def _builtin_format_stdout(self, format_str: any, args_list: any, subst: Substitution) -> Substitution | None:
        """Built-in format/2 predicate for printing formatted strings to stdout."""
        rendered = self._format_to_string(format_str, args_list, subst)
        if rendered is None:
            return None

        print(rendered, end="")

        return subst

    def _builtin_atom(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in atom/1 predicate - succeeds if term is an atom."""
        term = deref(term, subst)
        if isinstance(term, Atom):
            return subst
        return None

    def _builtin_number(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in number/1 predicate - succeeds if term is a number."""
        term = deref(term, subst)
        if isinstance(term, Number):
            return subst
        return None

    def _builtin_var(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in var/1 predicate - succeeds if term is an unbound variable."""
        term = deref(term, subst)
        if isinstance(term, Variable):
            return subst
        return None

    def _builtin_nonvar(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in nonvar/1 predicate - succeeds if term is not an unbound variable."""
        term = deref(term, subst)
        if not isinstance(term, Variable):
            return subst
        return None

    def _builtin_compound(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in compound/1 predicate - succeeds if term is a compound term."""
        term = deref(term, subst)
        # A compound term is a Compound or a List (not atoms or numbers)
        if isinstance(term, (Compound, List)):
            return subst
        return None

    def _builtin_integer(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in integer/1 predicate - succeeds if term is an integer."""
        term = deref(term, subst)
        if isinstance(term, Number) and isinstance(term.value, int):
            return subst
        return None

    def _builtin_float(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in float/1 predicate - succeeds if term is a float."""
        term = deref(term, subst)
        if isinstance(term, Number) and isinstance(term.value, float):
            return subst
        return None

    def _builtin_atomic(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in atomic/1 predicate - succeeds if term is atomic (atom or number)."""
        term = deref(term, subst)
        if isinstance(term, (Atom, Number)) or (isinstance(term, List) and not term.elements and term.tail is None):
            return subst
        return None

    def _builtin_callable(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in callable/1 predicate - succeeds if term is callable (atom, compound, or list)."""
        term = deref(term, subst)
        if isinstance(term, (Atom, Compound, List)):
            return subst
        return None

    def _builtin_ground(self, term: any, subst: Substitution) -> Substitution | None:
        """Built-in ground/1 predicate - succeeds if term contains no variables."""
        term = deref(term, subst)
        if self._is_ground(term, subst):
            return subst
        return None

    def _is_ground(self, term: any, subst: Substitution) -> bool:
        """Helper method to check if a term is ground (contains no variables)."""
        term = deref(term, subst)
        if isinstance(term, Variable):
            return False
        elif isinstance(term, (Atom, Number)):
            return True
        elif isinstance(term, Compound):
            return all(self._is_ground(arg, subst) for arg in term.args)
        elif isinstance(term, List):
            # Check elements
            for elem in term.elements:
                if not self._is_ground(elem, subst):
                    return False
            # Check tail
            if term.tail is not None:
                return self._is_ground(term.tail, subst)
            return True
        else:
            return True

    def _builtin_functor(self, term: any, name: any, arity: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in functor/3 predicate - Extract or construct functor."""
        term = deref(term, subst)
        name = deref(name, subst)
        arity = deref(arity, subst)

        # Mode 1: term is bound, extract functor and arity
        if not isinstance(term, Variable):
            if isinstance(term, Compound):
                # Extract functor name and arity
                functor_name = Atom(term.functor)
                functor_arity = Number(len(term.args))

                new_subst = unify(name, functor_name, subst)
                if new_subst is not None:
                    new_subst = unify(arity, functor_arity, new_subst)
                    if new_subst is not None:
                        yield new_subst
            elif isinstance(term, Atom):
                # Atoms have arity 0
                new_subst = unify(name, term, subst)
                if new_subst is not None:
                    new_subst = unify(arity, Number(0), new_subst)
                    if new_subst is not None:
                        yield new_subst
            elif isinstance(term, Number):
                # Numbers are their own functor with arity 0
                new_subst = unify(name, term, subst)
                if new_subst is not None:
                    new_subst = unify(arity, Number(0), new_subst)
                    if new_subst is not None:
                        yield new_subst
        # Mode 2: term is unbound, construct from name and arity
        elif isinstance(name, Atom) and isinstance(arity, Number):
            arity_val = int(arity.value)
            if arity_val == 0:
                # Create an atom
                new_subst = unify(term, name, subst)
                if new_subst is not None:
                    yield new_subst
            else:
                # Create a compound with fresh variables as arguments
                args = tuple(self._fresh_variable(f"Arg{i}_") for i in range(arity_val))
                compound = Compound(name.name, args)
                new_subst = unify(term, compound, subst)
                if new_subst is not None:
                    yield new_subst

    def _builtin_arg(self, n: any, term: any, arg: any, subst: Substitution) -> Substitution | None:
        """Built-in arg/3 predicate - Access the Nth argument of a compound term."""
        n = deref(n, subst)
        term = deref(term, subst)

        if not isinstance(n, Number):
            return None

        if not isinstance(term, Compound):
            return None

        n_val = int(n.value)
        # Prolog uses 1-based indexing
        if n_val < 1 or n_val > len(term.args):
            return None

        # Get the argument (convert to 0-based index)
        selected_arg = term.args[n_val - 1]

        # Unify with the third argument
        return unify(arg, selected_arg, subst)

    def _builtin_univ(self, term: any, lst: any, subst: Substitution) -> Substitution | None:
        """Built-in =../2 (univ) predicate - Convert between term and list."""
        term = deref(term, subst)
        lst = deref(lst, subst)

        # Mode 1: term is bound, convert to list
        if not isinstance(term, Variable):
            if isinstance(term, Compound):
                # Compound: [functor, arg1, arg2, ...]
                elements = [Atom(term.functor)] + list(term.args)
                result_list = List(tuple(elements), None)
                return unify(lst, result_list, subst)
            elif isinstance(term, Atom):
                # Atom: [atom]
                result_list = List((term,), None)
                return unify(lst, result_list, subst)
            elif isinstance(term, Number):
                # Number: [number]
                result_list = List((term,), None)
                return unify(lst, result_list, subst)
        # Mode 2: list is bound, convert to term
        elif isinstance(lst, List) and lst.elements:
            first = lst.elements[0]
            if isinstance(first, Atom):
                if len(lst.elements) == 1:
                    # Single atom
                    return unify(term, first, subst)
                else:
                    # Compound term
                    functor = first.name
                    args = tuple(lst.elements[1:])
                    compound = Compound(functor, args)
                    return unify(term, compound, subst)
            elif isinstance(first, Number) and len(lst.elements) == 1:
                # Single number
                return unify(term, first, subst)

        return None

    def _builtin_copy_term(self, source: any, copy: any, subst: Substitution) -> Substitution | None:
        """Built-in copy_term/2 predicate - Create a copy of a term with fresh variables."""
        # Dereference the source term
        source = deref(source, subst)

        # Create a copy with fresh variables
        var_map = {}  # Map from original variables to fresh variables
        copied_term = copy_term_recursive(source, var_map, self._fresh_variable)

        # Unify the copied term with the copy argument
        return unify(copy, copied_term, subst)

    def _builtin_findall(self, template: any, goal: any, result: any, subst: Substitution) -> Substitution | None:
        """Built-in findall/3 predicate - Collect all solutions."""
        # Collect all solutions of the goal
        solutions = []
        for solution_subst in self._solve_goals([goal], subst):
            # Apply substitution to template
            instantiated = apply_substitution(template, solution_subst)
            solutions.append(instantiated)

        # Convert to Prolog list
        result_list = List(tuple(solutions), None)

        # Unify with result
        return unify(result, result_list, subst)

    def _builtin_bagof(self, template: any, goal: any, result: any, subst: Substitution) -> Iterator[Substitution] | None:
        """Built-in bagof/3 predicate - Collect solutions with duplicates."""
        free_vars, groups = self._collect_bagof_groups(template, goal, subst)
        if not groups:
            return None
        return self._yield_grouped_results(free_vars, groups, result, subst, lambda sols: sols)

    def _builtin_setof(self, template: any, goal: any, result: any, subst: Substitution) -> Iterator[Substitution] | None:
        """Built-in setof/3 predicate - Collect unique sorted solutions."""
        free_vars, groups = self._collect_bagof_groups(template, goal, subst)
        if not groups:
            return None
        return self._yield_grouped_results(
            free_vars,
            groups,
            result,
            subst,
            self._unique_and_sort_solutions,
        )

    def _yield_grouped_results(
        self,
        free_vars: list[str],
        groups: OrderedDict,
        result: any,
        subst: Substitution,
        transform: Callable[[list], list],
    ) -> Iterator[Substitution]:
        """Yield unified substitutions for each grouped bagof/setof solution list."""

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

    def _unique_and_sort_solutions(self, solutions: list) -> list:
        """Remove duplicates while preserving order, then sort if comparable."""
        unique_solutions = list(OrderedDict.fromkeys(solutions))
        try:
            return sorted(unique_solutions, key=lambda x: term_sort_key(x))
        except TypeError:
            return unique_solutions

    def _builtin_assert(self, clause_term: any, subst: Substitution, position: str = "back") -> Substitution | None:
        """Built-in assert predicates - Add a clause to the database."""
        clause_term = deref(clause_term, subst)
        clause_term = apply_substitution(clause_term, subst)

        # Parse the clause term into a Clause object
        if isinstance(clause_term, Compound) and clause_term.functor == ":-" and len(clause_term.args) == 2:
            # It's a rule: Head :- Body
            head = clause_term.args[0]
            body_term = clause_term.args[1]
            # Flatten body into list of goals
            body = self._flatten_conjunction(body_term)
            new_clause = Clause(head, body)
        else:
            # It's a fact
            new_clause = Clause(clause_term, None)

        # Add to the clause database at the specified position
        if position == "front":
            self.clauses.insert(0, new_clause)
        else:
            self.clauses.append(new_clause)

        return subst

    def _builtin_retract(self, clause_term: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in retract/1 predicate - Remove a clause from the database."""
        clause_term = deref(clause_term, subst)

        # Collect all matching clauses with their indices
        # We need to do this first because removing changes indices
        matches = []
        for i, clause in enumerate(self.clauses):
            # Rename variables in clause to avoid conflicts
            renamed_clause = self._rename_variables(clause)

            # Build the clause term for comparison
            if renamed_clause.is_fact():
                clause_as_term = renamed_clause.head
            else:
                # Build Head :- Body
                if len(renamed_clause.body) == 1:
                    body_term = renamed_clause.body[0]
                else:
                    # Build conjunction
                    body_term = renamed_clause.body[0]
                    for goal in renamed_clause.body[1:]:
                        body_term = Compound(",", (body_term, goal))
                clause_as_term = Compound(":-", (renamed_clause.head, body_term))

            # Try to unify
            new_subst = unify(clause_term, clause_as_term, subst)
            if new_subst is not None:
                matches.append((i, new_subst))

        # Remove clauses in reverse order to maintain indices
        for i, new_subst in reversed(matches):
            self.clauses.pop(i)
            yield new_subst

    def _builtin_abolish(self, indicator: any, subst: Substitution) -> Substitution | None:
        """Built-in abolish/1 predicate - Remove all clauses for a predicate indicator."""
        indicator = deref(indicator, subst)

        # Validate indicator format: Name/Arity
        if not isinstance(indicator, Compound) or indicator.functor != "/" or len(indicator.args) != 2:
            return None

        name_term, arity_term = indicator.args
        name_term = deref(name_term, subst)
        arity_term = deref(arity_term, subst)

        # Strict validation: name must be Atom, arity must be non-negative integer
        if not isinstance(name_term, Atom):
            return None
        if not isinstance(arity_term, Number) or not isinstance(arity_term.value, int) or arity_term.value < 0:
            return None
        arity = arity_term.value

        name = name_term.name

        # Filter out clauses that match the predicate indicator
        # Only remove user-defined clauses, not built-ins
        clauses_to_keep = []
        for clause in self.clauses:
            head = clause.head
            matches = False
            if isinstance(head, Compound):
                if head.functor == name and len(head.args) == arity:
                    matches = True
            elif isinstance(head, Atom):
                if head.name == name and arity == 0:
                    matches = True

            if not matches:
                clauses_to_keep.append(clause)
        self.clauses = clauses_to_keep

        # Always succeed
        return subst

    def _builtin_length(self, lst: any, length: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in length/2 predicate - List length."""
        lst = deref(lst, subst)
        length = deref(length, subst)

        # Mode 1: length is bound, generate/unify list of that length
        if isinstance(length, Number):
            n = int(length.value)
            if n < 0:
                return

            if isinstance(lst, List):
                new_subst = match_list_to_length(lst, n, subst, fresh_variable=self._fresh_variable)
                if new_subst is not None:
                    yield new_subst
            else:
                result_list = fresh_list_of_length(n, self._fresh_variable)
                new_subst = unify(lst, result_list, subst)
                if new_subst is not None:
                    yield new_subst
            return

        # Mode 2: list is bound, compute length
        if isinstance(lst, List):
            # Recursively compute length, checking for proper list
            count = compute_list_length(lst, subst)
            if count is None:
                # Not a proper list (has uninstantiated tail or improper tail)
                return

            new_subst = unify(length, Number(count), subst)
            if new_subst is not None:
                yield new_subst

    def _builtin_reverse(self, lst: any, reversed_lst: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in reverse/2 predicate - Reverse a list (bidirectional)."""
        lst = deref(lst, subst)
        reversed_lst = deref(reversed_lst, subst)

        # Mode 1: first argument is bound, reverse it and unify with second
        if isinstance(lst, List):
            # Convert to Python list, reverse, and convert back
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
        # Mode 2: second argument is bound, reverse it and unify with first
        elif isinstance(reversed_lst, List):
            # Convert to Python list, reverse, and convert back
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

    def _builtin_sort(self, lst: any, sorted_lst: any, subst: Substitution) -> Substitution | None:
        """Built-in sort/2 predicate - Sort a list and remove duplicates."""
        lst = deref(lst, subst)

        if not isinstance(lst, List):
            return None

        # Convert to Python list
        try:
                py_list = list_to_python(lst, subst)
        except TypeError:
            return None

        # Remove duplicates
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

        # Sort
        try:
            sorted_py = sorted(unique, key=lambda x: term_sort_key(x))
        except:
            # If sorting fails, just return unique
            sorted_py = unique

        result = python_to_list(sorted_py)
        return unify(sorted_lst, result, subst)

    def _collect_bagof_groups(self, template: any, goal: any, subst: Substitution) -> tuple[list[str], OrderedDict]:
        """Collect solutions for bagof/setof, grouped by free variable bindings."""
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
        for solution_subst in self._solve_goals([goal], subst):
            instantiated = apply_substitution(template, solution_subst)
            key = tuple(apply_substitution(Variable(var), solution_subst) for var in free_vars)
            groups.setdefault(key, []).append(instantiated)

        return free_vars, groups

    def _flatten_body(self, body: list) -> list:
        """Flatten conjunction in clause body into a list of goals."""
        if not body:
            return []

        # If body has a single element that's a conjunction, flatten it
        if len(body) == 1 and isinstance(body[0], Compound) and body[0].functor == ",":
            return self._flatten_conjunction(body[0])

        # Otherwise, return as is
        return body

    def _flatten_conjunction(self, term) -> list:
        """Recursively flatten a conjunction (,) into a list of goals."""
        if isinstance(term, Compound) and term.functor == ",":
            # Recursively flatten left and right
            left = self._flatten_conjunction(term.args[0])
            right = self._flatten_conjunction(term.args[1])
            return left + right
        else:
            return [term]

    def _rename_variables(self, clause: Clause) -> Clause:
        """Rename all variables in a clause to avoid conflicts."""
        self._fresh_var_counter += 1
        suffix = str(self._fresh_var_counter)

        def rename_term(term):
            if isinstance(term, Variable):
                return Variable(f"{term.name}_{suffix}")
            elif isinstance(term, Compound):
                new_args = tuple(rename_term(arg) for arg in term.args)
                return Compound(term.functor, new_args)
            elif isinstance(term, List):
                new_elements = tuple(rename_term(elem) for elem in term.elements)
                new_tail = rename_term(term.tail) if term.tail is not None else None
                return List(new_elements, new_tail)
            else:
                return term

        new_head = rename_term(clause.head)
        new_body = [rename_term(goal) for goal in clause.body] if clause.body else None
        return Clause(new_head, new_body)

    def _builtin_predicate_property(self, goal: any, property: any, subst: Substitution) -> Substitution | None:
        """Built-in predicate_property/2 - Check if a predicate has a given property."""
        goal = deref(goal, subst)
        property = deref(property, subst)

        # List of all built-in functors
        builtins = {
            "=", r"\=", r"\+", "is", "=:=", r"=\=", "<", ">", "=<", ">=", "==", r"\==", "@<", "@=<", "@>", "@>=", "=..",
            "member", "append", "length", "reverse", "sort",
            "clause", "call", "once", "true", "fail", "!",
            "write", "writeln", "nl", "format",
            ";", "->", ",",
            "atom", "number", "var", "nonvar",
            "functor", "arg",
            "findall", "bagof", "setof",
            "assert", "asserta", "assertz", "retract", "abolish",
            "maplist",
            "throw", "catch",
            "predicate_property", "current_predicate"
        }

        # Check if goal is a compound term with a built-in functor
        is_builtin = False
        if isinstance(goal, Compound):
            is_builtin = goal.functor in builtins
        elif isinstance(goal, Atom):
            is_builtin = goal.name in builtins

        # Try to unify property with built_in if the goal is a built-in
        if is_builtin:
            builtin_atom = Atom("built_in")
            return unify(property, builtin_atom, subst)

        return None

    def _builtin_current_predicate(self, indicator: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in current_predicate/1 - Enumerate defined predicates."""
        indicator = deref(indicator, subst)

        # Collect all unique predicate indicators
        predicates = set()

        # Add built-in predicates with their arities
        builtin_predicates = [
            # Control constructs
            ("true", 0), ("fail", 0), ("!", 0), ("nl", 0),
            # Unification and comparison
            ("=", 2), (r"\=", 2), ("==", 2), (r"\==", 2), ("@<", 2), ("@=<", 2), ("@>", 2), ("@>=", 2),
            # Arithmetic
            ("is", 2), ("=:=", 2), (r"=\=", 2), ("<", 2), (">", 2), ("=<", 2), (">=", 2),
            # Type testing
            ("var", 1), ("nonvar", 1), ("atom", 1), ("number", 1), ("integer", 1), ("float", 1),
            ("atomic", 1), ("compound", 1), ("callable", 1), ("ground", 1),
            # Term manipulation
            ("functor", 3), ("arg", 3), ("=..", 2),
            # List operations
            ("member", 2), ("append", 3), ("length", 2), ("reverse", 2), ("sort", 2),
            # All solutions
            ("findall", 3), ("bagof", 3), ("setof", 3),
            # Database modification
            ("asserta", 1), ("assertz", 1), ("assert", 1), ("retract", 1), ("abolish", 1), ("clause", 2),
            # Meta-logical
            ("call", 1), ("once", 1),
            # Control flow
            (";", 2), ("->", 2), (",", 2), ("\\+", 1),
            # I/O
            ("write", 1), ("writeln", 1), ("format", 1), ("format", 2), ("format", 3),
            # Higher-order
            ("maplist", 2),
            # Exception handling
            ("throw", 1), ("catch", 3),
            # Reflection
            ("predicate_property", 2), ("current_predicate", 1)
        ]

        for name, arity in builtin_predicates:
            predicates.add((name, arity))

        # Add user-defined predicates from clauses
        for clause in self.clauses:
            if isinstance(clause.head, Compound):
                predicates.add((clause.head.functor, len(clause.head.args)))
            elif isinstance(clause.head, Atom):
                predicates.add((clause.head.name, 0))

        # Sort for deterministic order
        sorted_predicates = sorted(predicates)

        # Try to unify each predicate indicator with the argument
        for name, arity in sorted_predicates:
            pred_indicator = Compound("/", (Atom(name), Number(arity)))
            new_subst = unify(indicator, pred_indicator, subst)
            if new_subst is not None:
                yield new_subst

    def _builtin_throw(self, term: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in throw/1 - Throw an exception term."""
        # Dereference the term to be thrown
        thrown_term = deref(term, subst)
        # Raise the PrologThrow exception to unwind the call stack
        raise PrologThrow(thrown_term)

    def _execute_callable_term(self, goal_term: any, subst: Substitution) -> Iterator[Substitution]:
        """Executes a term if it is a valid goal (Compound, Atom, or Cut), otherwise fails silently."""
        if isinstance(goal_term, (Compound, Atom, Cut)):
            yield from self._solve_goals([goal_term], subst)
        # No else: non-callable terms yield no results (fails)

    def _builtin_catch(self, goal: any, error: any, recovery: any, subst: Substitution) -> Iterator[Substitution]:
        """Built-in catch/3 - Exception handling with throw/1 support.

        catch(Goal, Error, Recovery) - Execute Goal. If Goal throws an exception
        that unifies with Error, execute Recovery in the unified substitution environment.
        If the thrown term doesn't unify with Error, re-raise the exception.
        """
        goal = deref(goal, subst)

        try:
            yield from self._execute_callable_term(goal, subst)
        except PrologThrow as e:
            # Try to unify the thrown term with the error pattern
            new_subst = unify(error, e.term, subst)
            if new_subst is not None:
                # Unification succeeded - execute recovery goal
                recovery_goal = deref(recovery, new_subst)
                yield from self._execute_callable_term(recovery_goal, new_subst)
            else:
                # Unification failed - re-raise the exception
                raise
