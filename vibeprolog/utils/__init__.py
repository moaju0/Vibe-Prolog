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

__all__ = [
    "compute_list_length",
    "fresh_list_of_length",
    "list_to_python",
    "match_list_to_length",
    "python_to_list",
    "term_sort_key",
    "term_to_string",
    "terms_equal",
    "collect_vars",
    "collect_vars_in_order",
    "copy_term_recursive",
    "strip_existentials",
]
