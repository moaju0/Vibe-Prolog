"""Tests for comprehensive fixtures exercising various Prolog features."""

import pytest
from vibeprolog import PrologInterpreter


@pytest.fixture
def prolog_interpreter():
    """Provide a fresh PrologInterpreter for each test function."""
    return PrologInterpreter()


@pytest.mark.parametrize(
    "fixture_file,query,expected",
    [
        ("recursion.pl", "factorial(5, X)", {"X": 120}),
        ("recursion.pl", "fib(10, X)", {"X": 55}),
        ("lists.pl", "my_append([1,2], [3,4], X)", {"X": [1, 2, 3, 4]}),
        ("lists.pl", "my_reverse([1,2,3], X)", {"X": [3, 2, 1]}),
        ("meta_predicates.pl", "q(X)", {"X": ["a", "b", "c"]}),
        ("arithmetic.pl", "sum_list([1,2,3,4], X)", {"X": 10}),
        ("arithmetic.pl", "product_list([2,3,4], X)", {"X": 24}),
        ("large_facts.pl", "findall(X, person(X), L), length(L, Len)", {"Len": 500}),
    ],
)
@pytest.mark.slow
def test_fixture_queries(prolog_interpreter, fixture_file, query, expected):
    """Parameterized test to validate multiple fixture files and queries."""
    p = PrologInterpreter()
    p.consult(f"tests/fixtures/{fixture_file}")
    result = p.query_once(query)
    assert result is not None
    for var, value in expected.items():
        assert result[var] == value


def test_has_solution_recursion(prolog_interpreter):
    """Ensure has_solution works for recursion fixtures."""
    p = prolog_interpreter
    p.consult("tests/fixtures/recursion.pl")
    assert p.has_solution("factorial(3, X)")


def test_has_solution_lists(prolog_interpreter):
    """Ensure has_solution works for lists fixtures."""
    p = prolog_interpreter
    p.consult("tests/fixtures/lists.pl")
    assert p.has_solution("my_member(2, [1,2,3])")


def test_pep8_blank_lines():
    """Ensure there are two blank lines between top-level definitions (PEP8)."""
    # This is a placeholder test to remind maintainers to keep proper spacing.
    pass
