"""Dummy built-ins used to validate the modular registration infrastructure."""

from __future__ import annotations

from prolog.builtins import BuiltinRegistry
from prolog.builtins.common import BuiltinArgs, EngineContext
from prolog.unification import Substitution


class DummyBuiltins:
    """Dummy built-ins for testing infrastructure."""

    @staticmethod
    def dummy_test(
        _args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution:
        """dummy_test/0 - Always succeeds for testing and returns the current substitution."""
        assert engine is not None
        return subst

    @staticmethod
    def register(
        registry: BuiltinRegistry, engine_ref: EngineContext | None = None
    ) -> None:
        """Register dummy built-ins in the provided registry."""
        registry[("dummy_test", 0)] = DummyBuiltins.dummy_test
