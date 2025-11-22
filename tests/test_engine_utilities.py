"""Tests for shared engine utilities used by built-ins."""

from prolog import PrologInterpreter
from prolog.parser import Atom, Number, Variable, List
from prolog.unification import Substitution, unify


def test_format_to_string_shared_logic():
    interpreter = PrologInterpreter()
    interpreter.query("true.")
    engine = interpreter.engine
    rendered = engine._format_to_string(
        Atom("~w ~2f~n"),
        List((Atom("pi"), Number(3.14159)), None),
        Substitution(),
    )

    assert rendered == "pi 3.14\n"


def test_list_conversion_honors_substitution():
    interpreter = PrologInterpreter()
    interpreter.query("true.")
    engine = interpreter.engine
    x_var = Variable("X")
    prolog_list = List((x_var, Number(2)), None)
    subst = unify(x_var, Number(1), Substitution())

    assert engine._list_to_python(prolog_list, subst) == [Number(1), Number(2)]


def test_fresh_variable_generator_is_monotonic():
    interpreter = PrologInterpreter()
    interpreter.query("true.")
    engine = interpreter.engine
    first = engine._fresh_variable("Tmp")
    second = engine._fresh_variable("Tmp")

    assert first.name != second.name
