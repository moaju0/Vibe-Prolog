"""Unification algorithm for Prolog terms."""

from typing import Any
from vibeprolog.parser import Variable, Atom, Number, Compound, List


class Substitution:
    """Represents variable bindings."""

    def __init__(self, bindings: dict[str, Any] | None = None):
        self.bindings = bindings or {}

    def bind(self, var: str, value: Any) -> "Substitution":
        """Create new substitution with additional binding."""
        new_bindings = self.bindings.copy()
        new_bindings[var] = value
        return Substitution(new_bindings)

    def lookup(self, var: str) -> Any:
        """Look up variable binding."""
        return self.bindings.get(var)

    def __repr__(self):
        return f"Substitution({self.bindings})"

    def copy(self) -> "Substitution":
        """Create a copy of this substitution."""
        return Substitution(self.bindings.copy())


def deref(term: Any, subst: Substitution) -> Any:
    """Dereference a term by following variable bindings."""
    if isinstance(term, Variable):
        value = subst.lookup(term.name)
        if value is not None:
            return deref(value, subst)
    return term


def occurs_check(var: Variable, term: Any, subst: Substitution) -> bool:
    """Check if variable occurs in term (prevents infinite structures)."""
    term = deref(term, subst)

    if isinstance(term, Variable):
        return var.name == term.name

    if isinstance(term, Compound):
        return any(occurs_check(var, arg, subst) for arg in term.args)

    if isinstance(term, List):
        for elem in term.elements:
            if occurs_check(var, elem, subst):
                return True
        if term.tail is not None:
            return occurs_check(var, term.tail, subst)

    return False


def unify(term1: Any, term2: Any, subst: Substitution) -> Substitution | None:
    """
    Unify two terms with given substitution.
    Returns new substitution if successful, None if unification fails.
    """
    # Dereference both terms
    term1 = deref(term1, subst)
    term2 = deref(term2, subst)

    # Same term
    if term1 == term2:
        return subst

    # ISO Prolog: Empty list [] unifies with atom '[]'
    if isinstance(term1, List) and isinstance(term2, Atom):
        if not term1.elements and term1.tail is None and term2.name == "[]":
            return subst
    if isinstance(term1, Atom) and isinstance(term2, List):
        if not term2.elements and term2.tail is None and term1.name == "[]":
            return subst

    # Variable unification
    if isinstance(term1, Variable):
        if occurs_check(term1, term2, subst):
            return None
        return subst.bind(term1.name, term2)

    if isinstance(term2, Variable):
        if occurs_check(term2, term1, subst):
            return None
        return subst.bind(term2.name, term1)

    # Atom unification
    if isinstance(term1, Atom) and isinstance(term2, Atom):
        return subst if term1.name == term2.name else None

    # Number unification
    if isinstance(term1, Number) and isinstance(term2, Number):
        return subst if term1.value == term2.value else None

    # Compound unification
    if isinstance(term1, Compound) and isinstance(term2, Compound):
        if term1.functor != term2.functor or len(term1.args) != len(term2.args):
            return None

        # Unify all arguments
        for arg1, arg2 in zip(term1.args, term2.args):
            subst = unify(arg1, arg2, subst)
            if subst is None:
                return None
        return subst

    # List unification
    if isinstance(term1, List) and isinstance(term2, List):
        # Empty lists
        if (
            not term1.elements
            and not term2.elements
            and term1.tail is None
            and term2.tail is None
        ):
            return subst

        # Handle the case where one or both lists have explicit tails [H|T]
        # We need to unify element by element, then unify the remaining parts

        # If both lists have no elements, just unify the tails
        if not term1.elements and not term2.elements:
            if term1.tail is not None and term2.tail is not None:
                return unify(term1.tail, term2.tail, subst)
            return subst

        # If one list is empty but the other has elements, fail unless empty list has a tail
        if not term1.elements:
            if term1.tail is not None:
                return unify(term1.tail, term2, subst)
            return None

        if not term2.elements:
            if term2.tail is not None:
                return unify(term1, term2.tail, subst)
            return None

        # Both have at least one element - unify the first elements
        subst = unify(term1.elements[0], term2.elements[0], subst)
        if subst is None:
            return None

        # Create tails from remaining elements
        if len(term1.elements) > 1:
            tail1 = List(term1.elements[1:], term1.tail)
        elif term1.tail is not None:
            tail1 = term1.tail
        else:
            tail1 = List(())  # Empty list

        if len(term2.elements) > 1:
            tail2 = List(term2.elements[1:], term2.tail)
        elif term2.tail is not None:
            tail2 = term2.tail
        else:
            tail2 = List(())  # Empty list

        # Recursively unify the tails
        return unify(tail1, tail2, subst)

    # No unification possible
    return None


def apply_substitution(term: Any, subst: Substitution) -> Any:
    """Apply substitution to a term, replacing all variables."""
    term = deref(term, subst)

    if isinstance(term, Variable):
        return term

    if isinstance(term, Compound):
        new_args = tuple(apply_substitution(arg, subst) for arg in term.args)
        return Compound(term.functor, new_args)

    if isinstance(term, List):
        new_elements = tuple(apply_substitution(elem, subst) for elem in term.elements)
        new_tail = (
            apply_substitution(term.tail, subst) if term.tail is not None else None
        )
        return List(new_elements, new_tail)

    return term


def vars(term: Any) -> set[Variable]:
    """Collect all variables in a term."""

    def _walk(term: Any):
        """Recursively walk the term and yield variables."""
        if isinstance(term, Variable):
            yield term
        elif isinstance(term, Compound):
            for arg in term.args:
                yield from _walk(arg)
        elif isinstance(term, List):
            for elem in term.elements:
                yield from _walk(elem)
            if term.tail is not None:
                yield from _walk(term.tail)

    return set(_walk(term))
