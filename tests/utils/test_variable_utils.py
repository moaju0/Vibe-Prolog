from vibeprolog.parser import List
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution
from vibeprolog.utils.variable_utils import (
    collect_vars,
    collect_vars_in_order,
    copy_term_recursive,
    strip_existentials,
)


def _make_var_generator():
    counter = {"i": 0}

    def _gen(prefix: str) -> Variable:
        name = f"{prefix}{counter['i']}"
        counter["i"] += 1
        return Variable(name)

    return _gen


def test_collect_vars_finds_all_variables():
    term = Compound("f", (Variable("X"), List((Variable("Y"), Atom("a")), Variable("Z"))))
    subst = Substitution()

    assert collect_vars(term, subst) == {"X", "Y", "Z"}


def test_collect_vars_in_order_preserves_first_seen():
    term = Compound("g", (Variable("A"), Variable("B"), Variable("A"), Variable("C")))
    subst = Substitution()

    assert collect_vars_in_order(term, subst) == ["A", "B", "C"]


def test_strip_existentials_collects_bound_variables():
    inner_goal = Compound("p", (Variable("X"), Variable("Y")))
    goal = Compound("^", (Variable("X"), Compound("^", (Variable("Z"), inner_goal))))
    subst = Substitution()

    stripped_goal, existentials = strip_existentials(goal, subst)
    assert stripped_goal == inner_goal
    assert existentials == {"X", "Z"}


def test_copy_term_recursive_creates_independent_variables():
    gen = _make_var_generator()
    var_x = Variable("X")
    var_y = Variable("Y")
    term = Compound("f", (var_x, List((var_y,), var_x)))

    copied = copy_term_recursive(term, {}, gen)

    assert isinstance(copied, Compound)
    copied_x, copied_list = copied.args
    assert copied_x != var_x
    assert copied_list.tail == copied_x
    assert copied_list.elements[0] != var_y
    assert copied_list.elements[0] != var_x


def test_collect_vars_handles_python_lists():
    subst = Substitution()
    term_list = [Variable("A"), Compound("h", (Variable("B"),))]

    assert collect_vars(term_list, subst) == {"A", "B"}
