"""
Term utility functions for Prolog term manipulation.

This module provides standalone utility functions for working with Prolog terms:
- term_to_string: Convert terms to string representation
- terms_equal: Structural equality checking
- term_sort_key: Generate sort keys for term ordering

These utilities are used throughout the engine and built-in predicates.
"""

from typing import Any

from prolog.parser import Atom, Compound, List, Number, Variable
from prolog.unification import Substitution, deref


def term_to_string(term: Any) -> str:
    """Convert a Prolog term to a printable string.

    Args:
        term: The term to render.

    Returns:
        A readable string representation of the term, preserving list syntax.
    """
    if isinstance(term, Atom):
        return term.name
    if isinstance(term, Number):
        return str(term.value)
    if isinstance(term, Variable):
        return f"_{term.name}"
    if isinstance(term, List):
        if not term.elements and term.tail is None:
            return "[]"
        elements_str = ", ".join(term_to_string(e) for e in term.elements)
        if term.tail is not None and not (
            isinstance(term.tail, List) and not term.tail.elements
        ):
            return f"[{elements_str}|{term_to_string(term.tail)}]"
        return f"[{elements_str}]"
    if isinstance(term, Compound):
        if not term.args:
            return term.functor
        args_str = ", ".join(term_to_string(arg) for arg in term.args)
        return f"{term.functor}({args_str})"
    return str(term)


def terms_equal(term1: Any, term2: Any) -> bool:
    """Check if two terms are structurally equal.

    Args:
        term1: First term to compare.
        term2: Second term to compare.

    Returns:
        ``True`` if the terms match structurally; otherwise ``False``.
    """
    if type(term1) is not type(term2):
        return False

    if isinstance(term1, Atom):
        return term1.name == term2.name
    if isinstance(term1, Number):
        return term1.value == term2.value
    if isinstance(term1, Variable):
        return term1.name == term2.name
    if isinstance(term1, List):
        if len(term1.elements) != len(term2.elements):
            return False
        for e1, e2 in zip(term1.elements, term2.elements):
            if not terms_equal(e1, e2):
                return False
        if term1.tail is None and term2.tail is None:
            return True
        if term1.tail is None or term2.tail is None:
            return False
        return terms_equal(term1.tail, term2.tail)
    if isinstance(term1, Compound):
        if term1.functor != term2.functor:
            return False
        if len(term1.args) != len(term2.args):
            return False
        return all(terms_equal(a1, a2) for a1, a2 in zip(term1.args, term2.args))

    return False


def term_sort_key(term: Any, subst: Substitution | None = None) -> tuple:
    """Generate a deterministic sort key for a term.

    Args:
        term: The term to normalize.
        subst: Optional substitution used for dereferencing before ordering.

    Returns:
        A tuple that can be used as a sort key for deterministic ordering.
    """
    subst = subst or Substitution()
    term = deref(term, subst)

    # Order: Variable < Number < Atom < Compound < List
    if isinstance(term, Variable):
        return (0, term.name)
    if isinstance(term, Number):
        return (1, term.value)
    if isinstance(term, Atom):
        return (2, term.name)
    if isinstance(term, Compound):
        return (
            3,
            len(term.args),
            term.functor,
            tuple(term_sort_key(arg, subst) for arg in term.args),
        )
    if isinstance(term, List):
        normalized_elements = list(term.elements)
        tail = term.tail

        # Flatten contiguous list tails to normalize equivalent representations like [1,2] and [1|[2]].
        current_tail = deref(tail, subst) if tail is not None else None
        while isinstance(current_tail, List):
            normalized_elements.extend(current_tail.elements)
            if current_tail.tail is None or (
                isinstance(current_tail.tail, List)
                and not current_tail.tail.elements
                and current_tail.tail.tail is None
            ):
                element_keys = tuple(
                    term_sort_key(elem, subst) for elem in normalized_elements
                )
                return (4, len(normalized_elements), element_keys, (4, 0))
            current_tail = deref(current_tail.tail, subst)

        tail_key = (
            term_sort_key(current_tail, subst) if current_tail is not None else (4, 0)
        )
        element_keys = tuple(term_sort_key(elem, subst) for elem in normalized_elements)
        return (4, len(normalized_elements), element_keys, tail_key)

    return (5, str(term))
