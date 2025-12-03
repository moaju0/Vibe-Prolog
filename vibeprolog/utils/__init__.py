"""Utility helpers for reusable Prolog engine functionality."""

from vibeprolog.utils.list_utils import (
    compute_list_length,
    fresh_list_of_length,
    list_to_python,
    match_list_to_length,
    python_to_list,
)
from vibeprolog.utils.term_utils import term_sort_key, term_to_string, terms_equal
from vibeprolog.utils.variable_utils import (
    collect_vars,
    collect_vars_in_order,
    copy_term_recursive,
    strip_existentials,
)

def reconstruct_operator_name_from_term(term):
    """
Reconstruct an operator name from its AST representation as a compound term.

The representation is treated as:
- Atom-like term: has a 'name' attribute -> return that name
- Compound-like term: has 'functor' and 'args' attributes; if there is exactly one arg,
  recursively reconstruct from that arg and prepend the functor.
"""
    # Avoid importing concrete Atom/Compound classes to prevent circular imports.
    if hasattr(term, "name"):
        # Treat terms with a 'name' attribute as atoms
        return term.name
    if hasattr(term, "functor") and hasattr(term, "args"):
        if len(term.args) == 1:
            tail = reconstruct_operator_name_from_term(term.args[0])
            if tail is not None:
                return f"{term.functor}{tail}"
    return None

__all__ = [
    "compute_list_length",
    "fresh_list_of_length",
    "list_to_python",
    "match_list_to_length",
    "python_to_list",
    "reconstruct_operator_name_from_term",
    "term_sort_key",
    "term_to_string",
    "terms_equal",
    "collect_vars",
    "collect_vars_in_order",
    "copy_term_recursive",
    "strip_existentials",
]
