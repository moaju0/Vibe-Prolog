import pytest

from vibeprolog.utils import reconstruct_operator_name_from_term
from vibeprolog.terms import Atom, Compound
from vibeprolog.interpreter import PrologInterpreter

def test_reconstruct_operator_name_from_term_atom():
    result = reconstruct_operator_name_from_term(Atom("plus"))
    assert result == "plus"

def test_reconstruct_operator_name_from_term_compound_single():
    term = Compound("#\\=", [Atom("A")])
    result = reconstruct_operator_name_from_term(term)
    assert result == "#\\=A"

def test_reconstruct_operator_name_from_term_compound_multi():
    term = Compound("foo", [Atom("a"), Atom("b")])
    result = reconstruct_operator_name_from_term(term)
    assert result is None

def test_reconstruct_operator_name_from_term_nested():
    inner = Compound("#\\=", [Atom("A")])
    term = Compound("not", [inner])
    result = reconstruct_operator_name_from_term(term)
    assert result == "not#\\=A"

def test_interpreter_reconstruct_delegates():
    interpreter = PrologInterpreter()
    term = Atom("plus")
    result = interpreter._reconstruct_operator_name(term)
    assert result == "plus"