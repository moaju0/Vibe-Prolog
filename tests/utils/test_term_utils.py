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
    assert isinstance(sorted_terms[2], List)  # []
    assert isinstance(sorted_terms[3], Atom)  # "atom"
    assert isinstance(sorted_terms[4], List)  # [2]
    assert isinstance(sorted_terms[5], Compound)  # f(a,1)


def test_term_sort_key_handles_nested_lists():
    subst = Substitution()
    list1 = List((Number(1), Number(2)), None)
    list2 = List((Number(1),), List((Number(2),), None))

    assert term_sort_key(list1, subst) == term_sort_key(list2, subst)


def test_term_sort_key_iso_list_ordering():
    """Test ISO-compliant term ordering where lists are treated as compounds."""
    subst = Substitution()

    # Empty list orders as atom '[]'
    empty_list = List(tuple(), None)
    atom_test = Atom("test")
    key_empty = term_sort_key(empty_list, subst)
    key_atom = term_sort_key(atom_test, subst)
    assert key_empty < key_atom  # [] @< test

    # Non-empty lists order as compounds with functor '.'
    list1 = List((Atom("a"),), None)  # [a]
    list2 = List((Atom("b"),), None)  # [b]
    assert term_sort_key(list1, subst) < term_sort_key(list2, subst)  # [a] @< [b]

    # List length comparison via tail
    list_short = List((Number(1), Number(2)), None)  # [1,2]
    list_long = List((Number(1), Number(2), Number(3)), None)  # [1,2,3]
    assert term_sort_key(list_short, subst) < term_sort_key(list_long, subst)  # [1,2] @< [1,2,3]

    # Compound vs list: f(a) @< [1]
    compound_fa = Compound("f", (Atom("a"),))
    list_1 = List((Number(1),), None)
    assert term_sort_key(compound_fa, subst) < term_sort_key(list_1, subst)  # f(a) @< [1]

    # Mixed: list (as atom '[]') < atom < compound
    atom_z = Atom("z")
    compound_g = Compound("g", tuple())
    list_empty = List(tuple(), None)
    assert term_sort_key(list_empty, subst) < term_sort_key(atom_z, subst) < term_sort_key(compound_g, subst)

    # List with tail
    improper_list = List((Atom("x"),), Atom("tail"))  # [x|tail]
    proper_list = List((Atom("x"), Atom("y")), None)  # [x,y]
    # [x|tail] should order based on '.'(x, tail) vs '.'(x, '.'(y, []))
    # Since tail is atom "tail", and '.'(y, []) is compound, but depends on ordering
    # Actually, since '.' has arity 2, and args compared left to right
    # '.'(x, tail) vs '.'(x, '.'(y, []))
    # First arg x == x, second arg: tail (atom) vs '.'(y, []) (compound)
    # atom < compound, so [x|tail] @< [x,y]
    assert term_sort_key(improper_list, subst) < term_sort_key(proper_list, subst)


@pytest.mark.parametrize(
    "term,expected",
    [
        (List(tuple(), None), "[]"),
        (Compound("g", tuple()), "g"),
    ],
)
def test_term_to_string_edge_cases(term, expected):
    assert term_to_string(term) == expected
