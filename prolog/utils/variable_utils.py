"""
Variable utility functions for Prolog variable handling.

Functions for collecting, tracking, and manipulating variables in
Prolog terms, including support for existential quantification.
"""

from typing import Any, Callable

from prolog.parser import Compound, List, Variable
from prolog.unification import Substitution, deref


def strip_existentials(goal: Any, subst: Substitution) -> tuple[Any, set[str]]:
    """Peel off existential quantifiers (Var^Goal) and collect bound variables.

    Args:
        goal: Goal term that may include existential wrappers.
        subst: Current substitution for dereferencing.

    Returns:
        A tuple of the innermost goal and the set of existentially bound variable names.
    """
    existential_vars: set[str] = set()
    while isinstance(goal, Compound) and goal.functor == "^" and len(goal.args) == 2:
        var_part = goal.args[0]
        existential_vars |= collect_vars(var_part, subst)
        goal = goal.args[1]
    return goal, existential_vars


def collect_vars(term: Any, subst: Substitution) -> set[str]:
    """Collect variable names from a term, following dereferences.

    Args:
        term: Term to inspect for variables.
        subst: Substitution used to dereference any bindings.

    Returns:
        A set of variable names encountered within the term.
    """
    term = deref(term, subst)
    vars_found: set[str] = set()

    if isinstance(term, Variable):
        vars_found.add(term.name)
    elif isinstance(term, Compound):
        for arg in term.args:
            vars_found |= collect_vars(arg, subst)
    elif isinstance(term, List):
        for elem in term.elements:
            vars_found |= collect_vars(elem, subst)
        if term.tail is not None:
            vars_found |= collect_vars(term.tail, subst)
    elif isinstance(term, list):
        # Clause bodies and goal lists are represented as Python lists internally.
        for item in term:
            vars_found |= collect_vars(item, subst)

    return vars_found


def collect_vars_in_order(
    term: Any, subst: Substitution, seen: set[str] | None = None
) -> list[str]:
    """Collect variable names in first-seen order from a term.

    Args:
        term: Term to inspect for variables.
        subst: Substitution used for dereferencing.
        seen: Optional set used to preserve ordering across recursive calls.

    Returns:
        A list of variable names in the order they are first encountered.
    """
    if seen is None:
        seen = set()

    term = deref(term, subst)

    if isinstance(term, Variable):
        if term.name not in seen:
            seen.add(term.name)
            return [term.name]
        return []

    vars_found: list[str] = []
    if isinstance(term, Compound):
        for arg in term.args:
            vars_found.extend(collect_vars_in_order(arg, subst, seen))
    elif isinstance(term, List):
        for elem in term.elements:
            vars_found.extend(collect_vars_in_order(elem, subst, seen))
        if term.tail is not None:
            vars_found.extend(collect_vars_in_order(term.tail, subst, seen))
    elif isinstance(term, list):
        for item in term:
            vars_found.extend(collect_vars_in_order(item, subst, seen))

    return vars_found


def copy_term_recursive(
    term: Any,
    var_map: dict[Variable, Variable],
    fresh_variable: Callable[[str], Variable],
) -> Any:
    """Recursively copy a term, creating fresh variables and maintaining consistency.

    Args:
        term: The term to copy.
        var_map: Mapping of original variables to their fresh copies.
        fresh_variable: Callable producing new variables.

    Returns:
        A deep copy of the term with consistent fresh variables.
    """
    if isinstance(term, Variable):
        if term in var_map:
            return var_map[term]
        fresh_var = fresh_variable(f"Copy{term.name}_")
        var_map[term] = fresh_var
        return fresh_var

    if isinstance(term, Compound):
        new_args = tuple(
            copy_term_recursive(arg, var_map, fresh_variable) for arg in term.args
        )
        return Compound(term.functor, new_args)
    if isinstance(term, List):
        new_elements = tuple(
            copy_term_recursive(elem, var_map, fresh_variable) for elem in term.elements
        )
        new_tail = (
            copy_term_recursive(term.tail, var_map, fresh_variable)
            if term.tail is not None
            else None
        )
        return List(new_elements, new_tail)

    # Atoms, numbers, etc. - return as is
    return term
