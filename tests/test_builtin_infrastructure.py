"""Tests for the modular built-in predicate infrastructure."""

from vibeprolog import PrologInterpreter
from vibeprolog.builtins._test_dummy import DummyBuiltins
from vibeprolog.engine import PrologEngine


def test_old_style_builtins_remain_available():
    prolog = PrologInterpreter()
    assert prolog.has_solution("true."), "Legacy built-in predicates should continue to work"


def test_new_style_registration_supports_engine_reference():
    prolog = PrologInterpreter()
    prolog.engine = PrologEngine([])

    DummyBuiltins.register(prolog.engine._builtin_registry, prolog.engine)

    assert prolog.has_solution("dummy_test."), "New-style built-ins should be callable via the interpreter"


def test_old_style_handlers_are_adapted_to_new_signature():
    prolog = PrologInterpreter()
    prolog.engine = PrologEngine([])
    marker: dict[str, bool] = {}

    def legacy_handler(_args, subst):
        marker["called"] = True
        return subst

    prolog.engine._register_builtin("legacy_test", 0, prolog.engine._builtin_registry, legacy_handler)

    assert prolog.has_solution("legacy_test."), "Legacy handlers should still be invokable"
    assert marker.get("called", False) is True


def test_mixed_registration_paths_coexist():
    prolog = PrologInterpreter()
    prolog.engine = PrologEngine([])

    DummyBuiltins.register(prolog.engine._builtin_registry, prolog.engine)
    assert prolog.has_solution("true."), "Old-style built-ins should still be present"
    assert prolog.has_solution("dummy_test."), "New-style built-ins should be present alongside legacy ones"
