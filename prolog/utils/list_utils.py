"""
List utility functions for Prolog list operations.

Provides conversion between Prolog List structures and Python lists,
plus helper functions for list length operations and matching.
"""

from typing import Any, Callable

from prolog.parser import Atom, List, Variable
from prolog.unification import Substitution, apply_substitution, deref, unify
from prolog.utils.term_utils import term_to_string


def list_to_python(prolog_list: List, subst: Substitution | None = None) -> list[Any]:
    """Convert a proper Prolog list to a Python list using the active substitution.

    Args:
        prolog_list: The list term to convert.
        subst: Substitution to apply before conversion.

    Raises:
        TypeError: If the list is improper or has an open tail.

    Returns:
        A Python ``list`` containing substituted elements.
    """
    subst = subst or Substitution()
    result = []
    current = deref(prolog_list, subst)

    while isinstance(current, List):
        result.extend(apply_substitution(elem, subst) for elem in current.elements)

        if current.tail is None:
            # Proper list terminated without an explicit tail.
            return result

        current = deref(current.tail, subst)

    # Allow explicit [] as a terminator.
    if isinstance(current, Atom) and current.name == "[]":
        return result

    # Otherwise, the list was not proper (improper tail or open variable).
    raise TypeError(
        "Cannot convert non-proper Prolog list with tail "
        f"'{term_to_string(current)}' to a Python list."
    )


def python_to_list(py_list: list[Any]) -> List:
    """Convert a Python list to a Prolog list."""
    if not py_list:
        return List(tuple(), None)
    return List(tuple(py_list), None)


def compute_list_length(lst: List, subst: Substitution) -> int | None:
    """
    Recursively compute the length of a proper list.

    Args:
        lst: List term to inspect.
        subst: Substitution used for dereferencing.

    Returns:
        The length of the list, or ``None`` if the tail is uninstantiated or improper.
    """
    count = len(lst.elements)

    if lst.tail is None:
        # Proper list ending with implicit []
        return count

    tail = deref(lst.tail, subst)

    if isinstance(tail, List):
        if len(tail.elements) == 0 and tail.tail is None:
            # Explicit empty list []
            return count
        # Recursively compute tail length
        tail_length = compute_list_length(tail, subst)
        if tail_length is None:
            return None
        return count + tail_length
    if isinstance(tail, Variable):
        # Uninstantiated variable tail - not a proper list
        return None

    # Improper list (tail is atom, number, or compound term)
    return None


def fresh_list_of_length(
    length: int, fresh_variable: Callable[[str], Variable]
) -> List:
    """Create a list of the requested length populated with fresh variables.

    Args:
        length: Desired list length.
        fresh_variable: Callable that yields unique :class:`Variable` instances.

    Returns:
        A Prolog list populated with fresh variables.
    """
    if length <= 0:
        return List((), None)

    elements = tuple(fresh_variable(f"E{i}_") for i in range(length))
    return List(elements, None)


def match_list_to_length(
    lst: List,
    target_length: int,
    subst: Substitution,
    *,
    fresh_variable: Callable[[str], Variable],
) -> Substitution | None:
    """Ensure a possibly open list can have the requested length.

    Args:
        lst: Possibly open list term to constrain.
        target_length: Desired length of the list.
        subst: Current substitution.
        fresh_variable: Generator used to create fresh tail variables when needed.

    Returns:
        A substitution if the list can match the length, otherwise ``None``.
    """
    remaining = target_length
    current = lst

    while True:
        element_count = len(current.elements)
        if element_count > remaining:
            return None

        remaining -= element_count

        if current.tail is None:
            return subst if remaining == 0 else None

        tail = deref(current.tail, subst)

        if isinstance(tail, List):
            current = tail
            continue

        if isinstance(tail, Variable):
            tail_list = fresh_list_of_length(remaining, fresh_variable)
            return unify(tail, tail_list, subst)

        # Tail is neither a list nor a variable – improper structure
        return None
