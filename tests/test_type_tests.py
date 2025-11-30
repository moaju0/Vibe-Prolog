"""Tests for type testing predicates (atomic/1, callable/1, ground/1)."""

import pytest
from vibeprolog import PrologInterpreter


class TestAtomic:
    """Tests for atomic/1 predicate."""

    def test_atomic_atoms(self):
        """Test atomic/1 succeeds for atoms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("atomic(foo)")
        assert prolog.has_solution("atomic('hello')")
        assert prolog.has_solution("atomic([])")  # Empty list is an atom

    def test_atomic_numbers(self):
        """Test atomic/1 succeeds for numbers."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("atomic(42)")
        assert prolog.has_solution("atomic(3.14)")
        assert prolog.has_solution("atomic(-5)")

    def test_atomic_compounds_fail(self):
        """Test atomic/1 fails for compound terms."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atomic(f(1))")
        assert not prolog.has_solution("atomic(foo(bar))")

    def test_atomic_lists_fail(self):
        """Test atomic/1 fails for non-empty lists."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atomic([1, 2])")
        assert not prolog.has_solution("atomic([a|b])")

    def test_atomic_variables_fail(self):
        """Test atomic/1 fails for unbound variables."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("atomic(X)")

    def test_atomic_bound_variables(self):
        """Test atomic/1 succeeds for bound variables."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("X = 5, atomic(X)")
        assert prolog.has_solution("X = foo, atomic(X)")
        assert prolog.has_solution("X = [], atomic(X)")


class TestCallable:
    """Tests for callable/1 predicate."""

    def test_callable_atoms(self):
        """Test callable/1 succeeds for atoms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("callable(foo)")
        assert prolog.has_solution("callable('hello')")
        assert prolog.has_solution("callable([])")  # Empty list is an atom

    def test_callable_compounds(self):
        """Test callable/1 succeeds for compound terms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("callable(f(1))")
        assert prolog.has_solution("callable(foo(bar, baz))")
        assert prolog.has_solution("callable(+(2, 3))")

    def test_callable_numbers_fail(self):
        """Test callable/1 fails for numbers."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("callable(42)")
        assert not prolog.has_solution("callable(3.14)")

    def test_callable_lists_succeed(self):
        """Test callable/1 succeeds for lists."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("callable([1, 2])")
        assert prolog.has_solution("callable([a|b])")

    def test_callable_variables_fail(self):
        """Test callable/1 fails for unbound variables."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("callable(X)")

    def test_callable_bound_variables(self):
        """Test callable/1 succeeds for bound callable variables."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("X = foo, callable(X)")
        assert prolog.has_solution("X = f(1), callable(X)")
        assert prolog.has_solution("X = [], callable(X)")


class TestGround:
    """Tests for ground/1 predicate."""

    def test_ground_atoms(self):
        """Test ground/1 succeeds for atoms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("ground(foo)")
        assert prolog.has_solution("ground('hello')")
        assert prolog.has_solution("ground([])")

    def test_ground_numbers(self):
        """Test ground/1 succeeds for numbers."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("ground(42)")
        assert prolog.has_solution("ground(3.14)")

    def test_ground_compounds(self):
        """Test ground/1 succeeds for ground compound terms."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("ground(f(1, 2))")
        assert prolog.has_solution("ground(person(john, 25))")

    def test_ground_lists(self):
        """Test ground/1 succeeds for ground lists."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("ground([1, 2, 3])")
        assert prolog.has_solution("ground([a, b, c])")
        assert prolog.has_solution("ground([1, 2|[]])")  # Explicit empty tail

    def test_ground_nested_structures(self):
        """Test ground/1 succeeds for nested ground structures."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("ground([f(a), g(b)])")
        assert prolog.has_solution("ground(f([1, 2], g(3)))")

    def test_ground_variables_fail(self):
        """Test ground/1 fails for unbound variables."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("ground(X)")

    def test_ground_compound_with_variables_fail(self):
        """Test ground/1 fails for compounds containing variables."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("ground(f(X, 1))")
        assert not prolog.has_solution("ground(person(Name, 25))")

    def test_ground_list_with_variables_fail(self):
        """Test ground/1 fails for lists containing variables."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("ground([1, X, 3])")
        assert not prolog.has_solution("ground([H|T])")
        assert not prolog.has_solution("ground([1, 2|T])")

    def test_ground_bound_variables(self):
        """Test ground/1 succeeds for bound variables."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("X = 5, ground(X)")
        assert prolog.has_solution("X = foo, ground(X)")
        assert prolog.has_solution("X = f(1, 2), ground(X)")
        assert prolog.has_solution("X = [1, 2, 3], ground(X)")

    def test_ground_partially_bound_fail(self):
        """Test ground/1 fails when variables remain after dereferencing."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("X = f(Y), ground(X)")
        assert not prolog.has_solution("X = [1, Y], ground(X)")

    def test_ground_nested_bound_variables(self):
        """Test ground/1 succeeds when nested variables are bound."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("X = 1, Y = f(X), ground(Y)")
        assert prolog.has_solution("X = 1, Y = [X, 2], ground(Y)")


class TestIsList:
    """Tests for is_list/1 predicate."""

    def test_is_list_cyclic_term_fails(self):
        prolog = PrologInterpreter()
        assert not prolog.has_solution("X = [a|X], is_list(X)")

    def test_is_list_atom_terminator_handles_atom(self):
        prolog = PrologInterpreter()
        assert prolog.has_solution("T = '[]', is_list([a|T])")

    def test_empty_list(self):
        """Test is_list/1 succeeds for empty list."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("is_list([])")

    def test_proper_list(self):
        """Test is_list/1 succeeds for proper lists."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("is_list([1, 2, 3])")
        assert prolog.has_solution("is_list([a, b, c])")
        assert prolog.has_solution("is_list([1])")
        assert prolog.has_solution("is_list([foo(bar)])")

    def test_improper_list(self):
        """Test is_list/1 fails for improper lists."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list([1|2])")
        assert not prolog.has_solution("is_list([a|b])")
        assert not prolog.has_solution("is_list([1, 2|3])")

    def test_atom(self):
        """Test is_list/1 fails for atoms."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list(foo)")
        assert not prolog.has_solution("is_list('hello')")

    def test_number(self):
        """Test is_list/1 fails for numbers."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list(42)")
        assert not prolog.has_solution("is_list(3.14)")

    def test_variable(self):
        """Test is_list/1 fails for unbound variables."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list(X)")

    def test_compound(self):
        """Test is_list/1 fails for compound terms."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("is_list(f(1))")
        assert not prolog.has_solution("is_list(foo(bar))")

    def test_nested_list(self):
        """Test is_list/1 succeeds for nested lists."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("is_list([[1, 2], [3, 4]])")
        assert prolog.has_solution("is_list([a, [b, c], d])")

    def test_bound_variable_list(self):
        """Test is_list/1 succeeds for bound variables that are lists."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("X = [], is_list(X)")
        assert prolog.has_solution("X = [1, 2, 3], is_list(X)")
        assert prolog.has_solution("X = [a, b], is_list(X)")

    def test_bound_variable_non_list(self):
        """Test is_list/1 fails for bound variables that are not lists."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("X = foo, is_list(X)")
        assert not prolog.has_solution("X = 42, is_list(X)")
        assert not prolog.has_solution("X = f(1), is_list(X)")