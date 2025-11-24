"""Tests for additional built-in predicates."""

import pytest
from vibeprolog import PrologInterpreter


class TestTypeChecking:
    """Tests for type checking predicates."""

    def test_fail_always_fails(self):
        """Test fail/0 always fails."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("fail")

    def test_fail_in_disjunction(self):
        """Test fail/0 in disjunction."""
        prolog = PrologInterpreter()
        result = prolog.query_once("(fail ; X = 1).")
        assert result is not None
        assert result is not None
        assert result['X'] == 1

    def test_atom_with_atom(self):
        """Test atom/1 succeeds with atoms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("atom(foo)")
        assert prolog.has_solution("atom(abc)")

    def test_atom_with_non_atom(self):
        """Test atom/1 fails with non-atoms."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atom(123)")
        assert not prolog.has_solution("atom(X)")
        assert not prolog.has_solution("atom(f(a))")

    def test_number_with_number(self):
        """Test number/1 succeeds with numbers."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("number(42)")
        assert prolog.has_solution("number(3.14)")
        assert prolog.has_solution("number(0)")

    def test_number_with_non_number(self):
        """Test number/1 fails with non-numbers."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("number(foo)")
        assert not prolog.has_solution("number(X)")
        assert not prolog.has_solution("number(f(1))")

    def test_var_with_variable(self):
        """Test var/1 succeeds with unbound variables."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("var(X)")

    def test_var_with_non_variable(self):
        """Test var/1 fails with bound terms."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("var(42)")
        assert not prolog.has_solution("var(foo)")
        assert not prolog.has_solution("var(f(a))")

    def test_var_after_unification(self):
        """Test var/1 after unification."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("X = 5, var(X)")

    def test_nonvar_with_bound_terms(self):
        """Test nonvar/1 succeeds with bound terms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("nonvar(42)")
        assert prolog.has_solution("nonvar(foo)")
        assert prolog.has_solution("nonvar(f(a))")

    def test_nonvar_with_variable(self):
        """Test nonvar/1 fails with unbound variables."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("nonvar(X)")

    def test_nonvar_after_unification(self):
        """Test nonvar/1 after unification."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("X = 5, nonvar(X)")


class TestTermManipulation:
    """Tests for term manipulation predicates."""

    def test_functor_extract_from_compound(self):
        """Test functor/3 extracting from compound term."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(foo(a, b, c), F, N).")
        assert result is not None
        assert result is not None
        assert result['F'] == 'foo'
        assert result is not None
        assert result['N'] == 3

    def test_functor_extract_from_atom(self):
        """Test functor/3 extracting from atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(foo, F, N).")
        assert result is not None
        assert result is not None
        assert result['F'] == 'foo'
        assert result is not None
        assert result['N'] == 0

    def test_functor_extract_from_number(self):
        """Test functor/3 extracting from number."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(42, F, N).")
        assert result is not None
        assert result is not None
        assert result['F'] == 42
        assert result is not None
        assert result['N'] == 0

    def test_functor_construct_compound(self):
        """Test functor/3 constructing compound term."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(T, foo, 2).")
        assert result is not None
        # Should create foo(_G1, _G2) with fresh variables
        assert 'T' in result

    def test_functor_construct_atom(self):
        """Test functor/3 constructing atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(T, bar, 0).")
        assert result is not None
        assert result is not None
        assert result['T'] == 'bar'

    def test_arg_access_first_argument(self):
        """Test arg/3 accessing first argument."""
        prolog = PrologInterpreter()
        result = prolog.query_once("arg(1, foo(a, b, c), X).")
        assert result is not None
        assert result is not None
        assert result['X'] == 'a'

    def test_arg_access_middle_argument(self):
        """Test arg/3 accessing middle argument."""
        prolog = PrologInterpreter()
        result = prolog.query_once("arg(2, foo(a, b, c), X).")
        assert result is not None
        assert result is not None
        assert result['X'] == 'b'

    def test_arg_access_last_argument(self):
        """Test arg/3 accessing last argument."""
        prolog = PrologInterpreter()
        result = prolog.query_once("arg(3, foo(a, b, c), X).")
        assert result is not None
        assert result is not None
        assert result['X'] == 'c'

    def test_arg_out_of_bounds(self):
        """Test arg/3 with out of bounds index."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("arg(0, foo(a, b), X)")
        assert not prolog.has_solution("arg(4, foo(a, b), X)")

    def test_arg_unification(self):
        """Test arg/3 with unification."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("arg(2, foo(a, bar, c), bar)")
        assert not prolog.has_solution("arg(2, foo(a, bar, c), baz)")

    def test_univ_compound_to_list(self):
        """Test =../2 converting compound to list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("foo(a, b, c) =.. L.")
        assert result is not None
        assert result is not None
        assert result['L'] == ['foo', 'a', 'b', 'c']

    def test_univ_atom_to_list(self):
        """Test =../2 converting atom to list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("foo =.. L.")
        assert result is not None
        assert result is not None
        assert result['L'] == ['foo']

    def test_univ_number_to_list(self):
        """Test =../2 converting number to list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("42 =.. L.")
        assert result is not None
        assert result is not None
        assert result['L'] == [42]

    def test_univ_list_to_compound(self):
        """Test =../2 converting list to compound."""
        prolog = PrologInterpreter()
        result = prolog.query_once("T =.. [foo, a, b].")
        assert result is not None
        # Should create foo(a, b)
        assert 'T' in result

    def test_univ_list_to_atom(self):
        """Test =../2 converting single-element list to atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("T =.. [bar].")
        assert result is not None
        assert result is not None
        assert result['T'] == 'bar'

    def test_univ_bidirectional(self):
        """Test =../2 working bidirectionally."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("foo(a, b) =.. [foo, a, b]")


class TestSolutionCollection:
    """Tests for solution collection predicates."""

    def test_findall_basic(self):
        """Test findall/3 basic usage."""
        prolog = PrologInterpreter()
        prolog.consult_string("num(1). num(2). num(3).")
        result = prolog.query_once("findall(X, num(X), L).")
        assert result is not None
        assert result is not None
        assert result['L'] == [1, 2, 3]

    def test_findall_no_solutions(self):
        """Test findall/3 with no solutions returns empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("findall(X, member(X, []), L).")
        assert result is not None
        assert result is not None
        assert result['L'] == []

    def test_findall_with_template(self):
        """Test findall/3 with complex template."""
        prolog = PrologInterpreter()
        prolog.consult_string("person(alice, 30). person(bob, 25). person(charlie, 30).")
        result = prolog.query_once("findall(Name, person(Name, 30), L).")
        assert result is not None
        assert result is not None
        assert result['L'] == ['alice', 'charlie']

    def test_findall_duplicates(self):
        """Test findall/3 keeps duplicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("color(red). color(blue). color(red).")
        result = prolog.query_once("findall(X, color(X), L).")
        assert result is not None
        assert result is not None
        assert result['L'] == ['red', 'blue', 'red']

    def test_bagof_basic(self):
        """Test bagof/3 basic usage."""
        prolog = PrologInterpreter()
        prolog.consult_string("num(1). num(2). num(3).")
        result = prolog.query_once("bagof(X, num(X), L).")
        assert result is not None
        assert result is not None
        assert result['L'] == [1, 2, 3]

    def test_bagof_respects_free_var_order(self):
        """Bagof should group free variables in goal order, not sorted order."""
        prolog = PrologInterpreter()
        results = list(
            prolog.query(
                "bagof(0, (member(B, [b1, b2]), member(A, [a1, a2])), L)."
            )
        )

        bindings = [(res['B'], res['A']) for res in results]
        assert bindings == [('b1', 'a1'), ('b1', 'a2'), ('b2', 'a1'), ('b2', 'a2')]
        assert all(res['L'] == [0] for res in results)

    def test_bagof_no_solutions_fails(self):
        """Test bagof/3 fails when no solutions."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("bagof(X, member(X, []), L)")

    def test_setof_basic(self):
        """Test setof/3 basic usage."""
        prolog = PrologInterpreter()
        prolog.consult_string("num(3). num(1). num(2).")
        result = prolog.query_once("setof(X, num(X), L).")
        assert result is not None
        # Should be sorted and unique
        assert result is not None
        assert result['L'] == [1, 2, 3]

    def test_setof_removes_duplicates(self):
        """Test setof/3 removes duplicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("color(red). color(blue). color(red).")
        result = prolog.query_once("setof(X, color(X), L).")
        assert result is not None
        # Should be unique and sorted
        assert result is not None
        assert result['L'] == ['blue', 'red']

    def test_setof_no_solutions_fails(self):
        """Test setof/3 fails when no solutions."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("setof(X, member(X, []), L)")


class TestDatabaseModification:
    """Tests for database modification predicates."""

    def test_assert_fact(self):
        """Test assert/1 adding a fact."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("foo(bar)")
        prolog.query_once("assert(foo(bar)).")
        assert prolog.has_solution("foo(bar)")

    def test_assert_multiple_facts(self):
        """Test assert/1 adding multiple facts."""
        prolog = PrologInterpreter()
        prolog.query_once("assert(num(1)).")
        prolog.query_once("assert(num(2)).")
        prolog.query_once("assert(num(3)).")

        results = prolog.query("num(X).")
        assert len(results) == 3
        values = [r['X'] for r in results]
        assert 1 in values
        assert 2 in values
        assert 3 in values

    def test_assert_rule(self):
        """Test assert/1 adding a rule using consult_string."""
        prolog = PrologInterpreter()
        # Note: assert with :- is tricky to parse in query context
        # Instead, we test that assert works with facts and use consult_string for rules
        prolog.consult_string("father(john, mary).")
        prolog.consult_string("parent(X, Y) :- father(X, Y).")

        assert prolog.has_solution("parent(john, mary)")

    def test_retract_fact(self):
        """Test retract/1 removing a fact."""
        prolog = PrologInterpreter()
        prolog.consult_string("foo(bar). foo(baz).")

        assert prolog.has_solution("foo(bar)")
        prolog.query_once("retract(foo(bar)).")
        assert not prolog.has_solution("foo(bar)")
        assert prolog.has_solution("foo(baz)")

    def test_retract_with_variable(self):
        """Test retract/1 with variable."""
        prolog = PrologInterpreter()
        prolog.consult_string("num(1). num(2). num(3).")

        result = prolog.query_once("retract(num(X)).")
        assert result is not None
        # Should retract one of the num facts
        assert 'X' in result

    def test_retract_all(self):
        """Test retracting multiple facts."""
        prolog = PrologInterpreter()
        prolog.consult_string("num(1). num(2). num(3).")

        # Retract all num facts
        while prolog.has_solution("retract(num(_))"):
            pass

        assert not prolog.has_solution("num(X)")


class TestListOperations:
    """Tests for list operation predicates."""

    def test_length_of_empty_list(self):
        """Test length/2 with empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("length([], N).")
        assert result is not None
        assert result is not None
        assert result['N'] == 0

    def test_length_of_list(self):
        """Test length/2 with non-empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("length([a, b, c], N).")
        assert result is not None
        assert result is not None
        assert result['N'] == 3

    def test_length_check(self):
        """Test length/2 checking length."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("length([1, 2, 3], 3)")
        assert not prolog.has_solution("length([1, 2, 3], 4)")

    def test_length_generate_list(self):
        """Test length/2 generating list of given length."""
        prolog = PrologInterpreter()
        result = prolog.query_once("length(L, 3).")
        assert result is not None
        assert 'L' in result
        # Should generate list with 3 elements

    def test_length_with_uninstantiated_tail(self):
        """Test length/2 instantiates open list tails when the length is bound."""
        prolog = PrologInterpreter()

        # Tail should be instantiated to [] when the required length matches the known prefix
        result = prolog.query_once("length([1, 2 | T], 2).")
        assert result is not None
        assert result is not None
        assert result['T'] == []

        # Longer lengths should extend the open tail with fresh variables
        result = prolog.query_once("length([1, 2 | T], 4).")
        assert result is not None
        assert result is not None
        assert isinstance(result['T'], list)
        assert result is not None
        assert len(result['T']) == 2

    def test_length_with_improper_list(self):
        """Test length/2 should fail on improper list [1,2|3]."""
        prolog = PrologInterpreter()
        # Improper list (tail is not [] or another list)
        prolog.consult_string("test_improper :- length([1, 2 | 3], N).")
        # Should fail because [1,2|3] is not a proper list
        assert not prolog.has_solution("test_improper")

    def test_length_with_nested_tail(self):
        """Test length/2 also resolves nested open list tails when length is known."""
        prolog = PrologInterpreter()

        result = prolog.query_once("length([1, 2 | [3, 4 | T]], 4).")
        assert result is not None
        assert result is not None
        assert result['T'] == []

        result = prolog.query_once("length([1, 2 | [3, 4 | T]], 6).")
        assert result is not None
        assert result is not None
        assert isinstance(result['T'], list)
        assert result is not None
        assert len(result['T']) == 2

    def test_length_proper_list_with_explicit_tail(self):
        """Test length/2 works correctly on proper list with explicit empty tail."""
        prolog = PrologInterpreter()
        # [1, 2 | []] is the same as [1, 2]
        result = prolog.query_once("length([1, 2 | []], N).")
        assert result is not None
        assert result is not None
        assert result['N'] == 2

    def test_length_proper_list_with_nested_structure(self):
        """Test length/2 works correctly on proper list with nested structure."""
        prolog = PrologInterpreter()
        # [1, 2 | [3, 4]] should have length 4
        result = prolog.query_once("length([1, 2 | [3, 4]], N).")
        assert result is not None
        assert result is not None
        assert result['N'] == 4

    def test_reverse_empty_list(self):
        """Test reverse/2 with empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("reverse([], R).")
        assert result is not None
        assert result is not None
        assert result['R'] == []

    def test_reverse_list(self):
        """Test reverse/2 with non-empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("reverse([1, 2, 3], R).")
        assert result is not None
        assert result is not None
        assert result['R'] == [3, 2, 1]

    def test_reverse_check(self):
        """Test reverse/2 checking reversal."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("reverse([a, b, c], [c, b, a])")
        assert not prolog.has_solution("reverse([a, b, c], [a, b, c])")

    def test_reverse_single_element(self):
        """Test reverse/2 with single element."""
        prolog = PrologInterpreter()
        result = prolog.query_once("reverse([x], R).")
        assert result is not None
        assert result is not None
        assert result['R'] == ['x']

    def test_reverse_bidirectional_second_arg_bound(self):
        """Test reverse/2 with second argument bound (reverse direction)."""
        prolog = PrologInterpreter()
        result = prolog.query_once("reverse(L, [3, 2, 1]).")
        assert result is not None
        assert result is not None
        assert result['L'] == [1, 2, 3]

    def test_reverse_bidirectional_empty_second(self):
        """Test reverse/2 with empty list as second argument."""
        prolog = PrologInterpreter()
        result = prolog.query_once("reverse(L, []).")
        assert result is not None
        assert result is not None
        assert result['L'] == []

    def test_reverse_bidirectional_single_element_second(self):
        """Test reverse/2 with single element as second argument."""
        prolog = PrologInterpreter()
        result = prolog.query_once("reverse(L, [x]).")
        assert result is not None
        assert result is not None
        assert result['L'] == ['x']

    def test_reverse_bidirectional_verification(self):
        """Test reverse/2 with both arguments bound for verification."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("reverse([1, 2, 3], [3, 2, 1])")
        assert not prolog.has_solution("reverse([1, 2, 3], [1, 2, 3])")
        assert prolog.has_solution("reverse([a], [a])")

    def test_reverse_bidirectional_both_unbound_fails(self):
        """Test reverse/2 with both arguments unbound fails."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("reverse(X, Y)")

    def test_sort_empty_list(self):
        """Test sort/2 with empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sort([], S).")
        assert result is not None
        assert result is not None
        assert result['S'] == []

    def test_sort_numbers(self):
        """Test sort/2 with numbers."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sort([3, 1, 2], S).")
        assert result is not None
        assert result is not None
        assert result['S'] == [1, 2, 3]

    def test_sort_atoms(self):
        """Test sort/2 with atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sort([c, a, b], S).")
        assert result is not None
        assert result is not None
        assert result['S'] == ['a', 'b', 'c']

    def test_sort_removes_duplicates(self):
        """Test sort/2 removes duplicates."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sort([3, 1, 2, 1, 3], S).")
        assert result is not None
        assert result is not None
        assert result['S'] == [1, 2, 3]

    def test_sort_already_sorted(self):
        """Test sort/2 with already sorted list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sort([1, 2, 3], S).")
        assert result is not None
        assert result is not None
        assert result['S'] == [1, 2, 3]

    def test_sort_check(self):
        """Test sort/2 checking sorted list."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("sort([3, 1, 2], [1, 2, 3])")
        assert not prolog.has_solution("sort([3, 1, 2], [3, 1, 2])")


class TestIntegration:
    """Integration tests combining multiple built-ins."""

    def test_functor_with_univ(self):
        """Test functor/3 and =../2 together."""
        prolog = PrologInterpreter()
        result = prolog.query_once("foo(a, b) =.. L, functor(foo(a, b), F, N).")
        assert result is not None
        assert result is not None
        assert result['L'] == ['foo', 'a', 'b']
        assert result is not None
        assert result['F'] == 'foo'
        assert result is not None
        assert result['N'] == 2

    def test_findall_with_functor(self):
        """Test findall/3 with functor/3."""
        prolog = PrologInterpreter()
        prolog.consult_string("term(foo(a)). term(bar(b, c)). term(baz).")
        result = prolog.query_once("findall(N, (term(T), functor(T, _, N)), L).")
        assert result is not None
        # Should collect arities: 1, 2, 0
        assert result is not None
        assert sorted(result['L']) == [0, 1, 2]

    def test_assert_and_findall(self):
        """Test dynamic database with findall."""
        prolog = PrologInterpreter()
        prolog.query_once("assert(num(1)).")
        prolog.query_once("assert(num(2)).")
        prolog.query_once("assert(num(3)).")

        result = prolog.query_once("findall(X, num(X), L).")
        assert result is not None
        assert result is not None
        assert sorted(result['L']) == [1, 2, 3]

    def test_type_checking_in_guard(self):
        """Test type checking predicates in guards."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            process(X, number) :- number(X).
            process(X, atom) :- atom(X).
        """)

        result = prolog.query_once("process(42, T).")
        assert result is not None
        assert result is not None
        assert result['T'] == 'number'

        result = prolog.query_once("process(foo, T).")
        assert result is not None
        assert result is not None
        assert result['T'] == 'atom'

    def test_length_reverse_sort(self):
        """Test combining length, reverse, and sort."""
        prolog = PrologInterpreter()
        result = prolog.query_once("""
            L = [3, 1, 2],
            length(L, N),
            reverse(L, R),
            sort(L, S).
        """)
        assert result is not None
        assert result is not None
        assert result['L'] == [3, 1, 2]
        assert result is not None
        assert result['N'] == 3
        assert result is not None
        assert result['R'] == [2, 1, 3]
        assert result is not None
        assert result['S'] == [1, 2, 3]
