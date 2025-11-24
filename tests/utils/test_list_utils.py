import pytest

from vibeprolog.parser import List
from vibeprolog.terms import Atom, Number, Variable
from vibeprolog.unification import Substitution, deref, unify
from vibeprolog.utils.list_utils import (
    compute_list_length,
    fresh_list_of_length,
    list_to_python,
    match_list_to_length,
    python_to_list,
)


def _make_var_generator():
    counter = {"i": 0}

    def _gen(prefix: str) -> Variable:
        name = f"{prefix}{counter['i']}"
        counter["i"] += 1
        return Variable(name)

    return _gen


def test_list_to_python_proper_list():
    prolog_list = List((Number(1), Atom("b")), None)
    result = list_to_python(prolog_list, Substitution())
    assert result == [Number(1), Atom("b")]


def test_list_to_python_improper_raises_type_error():
    improper_list = List((Atom("a"),), Atom("tail"))
    with pytest.raises(TypeError):
        list_to_python(improper_list, Substitution())


def test_python_to_list_round_trip():
    py_list = [Number(1), Atom("c")]
    prolog_list = python_to_list(py_list)
    assert isinstance(prolog_list, List)
    assert list_to_python(prolog_list, Substitution()) == py_list


def test_compute_list_length_handles_open_and_proper_lists():
    subst = Substitution()
    proper = List((Number(1), Number(2)), None)
    assert compute_list_length(proper, subst) == 2

    open_tail = List((Number(1),), Variable("T"))
    assert compute_list_length(open_tail, subst) is None


def test_fresh_list_of_length_uses_generator():
    gen = _make_var_generator()
    fresh_list = fresh_list_of_length(3, gen)

    assert len(fresh_list.elements) == 3
    assert all(isinstance(elem, Variable) for elem in fresh_list.elements)


def test_match_list_to_length_instantiates_tail():
    gen = _make_var_generator()
    lst = List((Number(1),), Variable("Tail"))
    subst = Substitution()

    new_subst = match_list_to_length(lst, 3, subst, fresh_variable=gen)
    assert new_subst is not None

    tail_value = deref(lst.tail, new_subst)
    assert isinstance(tail_value, List)
    assert compute_list_length(tail_value, new_subst) == 2


def test_match_list_to_length_rejects_longer_lists():
    gen = _make_var_generator()
    lst = List((Number(1), Number(2), Number(3)), None)
    subst = Substitution()

    assert match_list_to_length(lst, 2, subst, fresh_variable=gen) is None


def test_match_list_to_length_closes_variable_tail():
    gen = _make_var_generator()
    tail_var = Variable("Tail")
    lst = List((Number(1), Number(2)), tail_var)
    subst = Substitution()

    new_subst = match_list_to_length(lst, 2, subst, fresh_variable=gen)
    assert new_subst is not None
    assert deref(tail_var, new_subst) is None or deref(tail_var, new_subst) == List(tuple(), None)


def test_list_to_python_respects_substitution():
    subst = Substitution()
    var = Variable("X")
    subst = unify(var, Number(5), subst)
    prolog_list = List((var,), None)

    assert list_to_python(prolog_list, subst) == [Number(5)]
