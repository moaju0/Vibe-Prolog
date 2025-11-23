"""Built-in predicate infrastructure.

This package provides the scaffolding for migrating built-in predicates from
``prolog.engine`` into modular submodules. During Phase 2 the legacy built-in
registrations in :mod:`prolog.engine` remain in place, while this package
establishes the new registration and handler signature conventions.

Built-in handlers support two calling conventions during migration:

- **Old style**: ``handler(args: tuple, subst: Substitution) -> BuiltinResult``
- **New style**: ``handler(args: tuple, subst: Substitution, engine) -> BuiltinResult``

New-style handlers receive a reference to the :class:`~prolog.engine.PrologEngine`
instance for features that need engine context. Old-style handlers can be
adapted automatically via :func:`adapt_old_handler` to maintain compatibility.

Example usage for a new built-in module:

.. code-block:: python

    from prolog.builtins import BuiltinPredicate, register_builtin

    class TypeTestBuiltins:
        @staticmethod
        def register(registry, engine_ref=None):
            def atom_handler(args, subst, engine):
                # implementation here
                return subst

            register_builtin(registry, "atom", 1, atom_handler)

Modules should expose a ``register`` static method matching
:class:`BuiltinPredicate` so they can be imported and registered centrally in
``PrologEngine._build_builtin_registry``.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Callable, Protocol, TypeAlias

if TYPE_CHECKING:
    from prolog.engine import BuiltinRegistry
else:
    BuiltinRegistry: TypeAlias = dict[tuple[str, int], Callable]
from prolog.unification import Substitution


class BuiltinPredicate(Protocol):
    """Protocol for built-in predicate handler classes."""

    @staticmethod
    def register(registry: BuiltinRegistry, engine_ref=None) -> None:
        """Register this module's built-in handlers in the registry.

        Args:
            registry: The built-in registry to populate.
            engine_ref: Optional reference to :class:`~prolog.engine.PrologEngine`.
        """
        ...


def register_builtin(
    registry: BuiltinRegistry, functor: str, arity: int, handler: Callable
) -> None:
    """Helper to register a built-in predicate handler."""
    registry[(functor, arity)] = handler


def adapt_old_handler(old_handler: Callable[[tuple, Substitution], object]) -> Callable:
    """Adapt an old-style handler to the new signature accepting an engine.

    Args:
        old_handler: A handler that only accepts ``args`` and ``subst``.

    Returns:
        A wrapper that forwards the first two parameters and ignores the engine
        argument so it can be used in the new dispatch path.
    """

    def wrapper(args, subst, _engine):  # pragma: no cover - trivial passthrough
        return old_handler(args, subst)

    return wrapper


__all__ = [
    "BuiltinPredicate",
    "BuiltinRegistry",
    "register_builtin",
    "adapt_old_handler",
]
