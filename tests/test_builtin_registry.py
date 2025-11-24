"""Tests for the built-in dispatch registry."""

from vibeprolog import PrologInterpreter


def test_registry_contains_core_builtins():
    interpreter = PrologInterpreter()
    interpreter.query("true.")

    registry = interpreter.engine._builtin_registry

    assert ("=", 2) in registry
    assert ("\\+", 1) in registry
    assert ("is", 2) in registry
    assert (";", 2) in registry


def test_registry_groups_cover_major_categories():
    interpreter = PrologInterpreter()
    interpreter.query("true.")

    registry = interpreter.engine._builtin_registry
    expected_keys = [
        (";", 2),  # control
        ("is", 2),  # arithmetic
        ("append", 3),  # lists
        ("clause", 2),  # database
        ("format", 3),  # IO
        ("var", 1),  # reflection
    ]

    for key in expected_keys:
        assert key in registry


def test_negation_and_arithmetic_through_registry():
    interpreter = PrologInterpreter()

    assert interpreter.has_solution("\\+ (1 = 2).")
    assert not interpreter.has_solution("\\+ (1 = 1).")

    result = interpreter.query_once("Z is 1 + 2.")
    assert result["Z"] == 3
    assert interpreter.has_solution("4 =:= 2 * 2.")


def test_cut_behavior_via_builtin_dispatch():
    interpreter = PrologInterpreter()

    solutions = interpreter.query("((X = 1 ; X = 2), !, true).")
    assert [solution["X"] for solution in solutions] == [1]
