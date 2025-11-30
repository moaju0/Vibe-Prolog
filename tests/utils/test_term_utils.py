import pytest

from vibeprolog.parser import List
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution
from vibeprolog.utils.term_utils import term_sort_key, term_to_string, terms_equal


def test_term_to_string_handles_all_term_types():
    assert term_to_string(Atom("hello")) == "hello"
    assert term_to_string(Number(42)) == "42"
    assert term_to_string(Variable("X")) == "_X"
    assert term_to_string(Compound("f", (Atom("a"), Number(1)))) == "f(a, 1)"

    proper_list = List((Number(1), Atom("b")), None)
    assert term_to_string(proper_list) == "[1, b]"

    tail = Variable("Tail")
    improper_list = List((Atom("a"),), tail)
    assert term_to_string(improper_list) == "[a|_Tail]"


def test_terms_equal_compares_structures():
    term_a = Compound("f", (Number(1), Atom("b")))
    term_b = Compound("f", (Number(1), Atom("b")))
    term_c = Compound("f", (Number(2), Atom("b")))

    assert terms_equal(term_a, term_b)
    assert not terms_equal(term_a, term_c)

    list_a = List((Number(1), Atom("b")), None)
    list_b = List((Number(1), Atom("b")), None)
    list_c = List((Number(1), Atom("c")), None)

    assert terms_equal(list_a, list_b)
    assert not terms_equal(list_a, list_c)

    nested1 = List((List((Number(1),), None),), None)
    nested2 = List((List((Number(1),), None),), None)
    nested3 = List((List((Number(2),), None),), None)

    assert terms_equal(nested1, nested2)
    assert not terms_equal(nested1, nested3)


def test_term_sort_key_orders_term_types():
    subst = Substitution()
    terms = [
        List((Number(2),), None),
        Atom("atom"),
        Variable("A"),
        Compound("f", (Number(1), Atom("a"))),
        Number(3),
        List(tuple(), None),
    ]

    sorted_terms = sorted(terms, key=lambda term: term_sort_key(term, subst))

    assert isinstance(sorted_terms[0], Variable)
    assert isinstance(sorted_terms[1], Number)
    assert isinstance(sorted_terms[2], Atom)
    assert isinstance(sorted_terms[3], Compound)
    assert isinstance(sorted_terms[4], List)
    assert isinstance(sorted_terms[5], List)


def test_term_sort_key_handles_nested_lists():
    subst = Substitution()
    list1 = List((Number(1), Number(2)), None)
    list2 = List((Number(1),), List((Number(2),), None))

    assert term_sort_key(list1, subst) == term_sort_key(list2, subst)


@pytest.mark.parametrize(
    "term,expected",
    [
        (List(tuple(), None), "[]"),
        (Compound("g", tuple()), "g"),
    ],
)
def test_term_to_string_edge_cases(term, expected):
    assert term_to_string(term) == expected
