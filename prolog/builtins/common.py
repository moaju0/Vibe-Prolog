"""Shared utilities and typing aliases for built-in predicate modules.

This module centralizes common typing helpers used by the built-in predicate
implementations. Keeping these helpers in one place reduces duplication across
the modules in :mod:`prolog.builtins` and makes handler signatures consistent.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Iterator, TypeAlias

from prolog.unification import Substitution

if TYPE_CHECKING:
    from prolog.engine import PrologEngine

# Common aliases for handler signatures.
BuiltinArgs: TypeAlias = tuple[Any, ...]
BuiltinResult: TypeAlias = Substitution | Iterator[Substitution] | None
EngineContext: TypeAlias = "PrologEngine"


def iter_empty() -> Iterator[Substitution]:
    """Return a reusable empty iterator for predicates that deterministically fail."""
    return iter(())
