"""Tests for comprehensive fixtures exercising various Prolog features."""

import pytest
from vibeprolog import PrologInterpreter


class TestRecursionFixture:
    """Tests for recursion fixture."""

    @pytest.fixture
    def prolog(self):
        prolog = PrologInterpreter()
        prolog.consult("tests/fixtures/recursion.pl")
        return prolog

    def test_factorial_success(self, prolog):
        result = prolog.query_once("factorial(5, X)")
        assert result is not None
        assert result['X'] == 120

    def test_fib_success(self, prolog):
        result = prolog.query_once("fib(10, X)")
        assert result is not None
        assert result['X'] == 55

    def test_factorial_failure(self, prolog):
        assert not prolog.has_solution("factorial(-1, X)")


class TestListsFixture:
    """Tests for list operations fixture."""

    @pytest.fixture
    def prolog(self):
        prolog = PrologInterpreter()
        prolog.consult("tests/fixtures/lists.pl")
        return prolog

    def test_append_success(self, prolog):
        result = prolog.query_once("my_append([1,2], [3,4], X)")
        assert result is not None
        assert result['X'] == [1, 2, 3, 4]

    def test_reverse_success(self, prolog):
        result = prolog.query_once("my_reverse([1,2,3], X)")
        assert result is not None
        assert result['X'] == [3, 2, 1]

    def test_member_success(self, prolog):
        assert prolog.has_solution("my_member(2, [1,2,3])")

    def test_member_failure(self, prolog):
        assert not prolog.has_solution("my_member(4, [1,2,3])")



class TestMetaPredicatesFixture:
    """Tests for meta-predicates fixture."""

    @pytest.fixture
    def prolog(self):
        prolog = PrologInterpreter()
        prolog.consult("tests/fixtures/meta_predicates.pl")
        return prolog

    def test_findall_success(self, prolog):
        result = prolog.query_once("q(X)")
        assert result is not None
        assert result['X'] == ['a', 'b', 'c']

    def test_call_success(self, prolog):
        assert prolog.has_solution("test_call")


class TestArithmeticFixture:
    """Tests for arithmetic fixture."""

    @pytest.fixture
    def prolog(self):
        prolog = PrologInterpreter()
        prolog.consult("tests/fixtures/arithmetic.pl")
        return prolog

    def test_sum_list_success(self, prolog):
        result = prolog.query_once("sum_list([1,2,3,4], X)")
        assert result is not None
        assert result['X'] == 10

    def test_greater_than_success(self, prolog):
        assert prolog.has_solution("greater_than(5, 3)")

    def test_greater_than_failure(self, prolog):
        assert not prolog.has_solution("greater_than(3, 5)")

    def test_product_list_success(self, prolog):
        result = prolog.query_once("product_list([2,3,4], X)")
        assert result is not None
        assert result['X'] == 24


class TestDCGFixture:
    """Tests for DCG fixture."""

    @pytest.fixture
    def prolog(self):
        prolog = PrologInterpreter()
        prolog.consult("tests/fixtures/dcg_sample.pl")
        return prolog

    def test_dcg_success1(self, prolog):
        assert prolog.has_solution("phrase(s, ['(', '(', ')', ')'])")

    def test_dcg_success2(self, prolog):
        assert prolog.has_solution("phrase(s, ['(', ')'])")

    def test_dcg_failure(self, prolog):
        assert not prolog.has_solution("phrase(s, ['(', ')', '('])")


class TestLargeFactsFixture:
    """Tests for large facts fixture."""

    @pytest.fixture
    def prolog(self):
        prolog = PrologInterpreter()
        prolog.consult("tests/fixtures/large_facts.pl")
        return prolog

    def test_person_exists(self, prolog):
        assert prolog.has_solution("person(250)")

    def test_person_not_exists(self, prolog):
        assert not prolog.has_solution("person(501)")

    def test_findall_large(self, prolog):
        result = prolog.query_once("findall(X, person(X), L), length(L, Len)")
        assert result is not None
        assert result['Len'] == 500