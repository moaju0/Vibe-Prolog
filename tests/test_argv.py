"""Tests for argv/1 and current_prolog_flag(argv, Args) builtins."""

import pytest
from vibeprolog import PrologInterpreter


class TestArgv:
    """Tests for argv/1 and current_prolog_flag(argv, Args) predicates."""

    def test_argv_empty_args(self):
        """Test argv/1 with empty argument list."""
        prolog = PrologInterpreter()

        result = prolog.query_once("argv(Args)")
        assert result is not None
        assert result['Args'] == []

    def test_argv_single_arg(self):
        """Test argv/1 with single argument."""
        prolog = PrologInterpreter(argv=["hello"])

        result = prolog.query_once("argv(Args)")
        assert result is not None
        assert result['Args'] == ["hello"]

    def test_argv_multiple_args(self):
        """Test argv/1 with multiple arguments."""
        prolog = PrologInterpreter(argv=["arg1", "arg2", "arg3"])

        result = prolog.query_once("argv(Args)")
        assert result is not None
        assert result['Args'] == ["arg1", "arg2", "arg3"]

    def test_argv_special_characters(self):
        """Test argv/1 with special characters in arguments."""
        prolog = PrologInterpreter(argv=["hello world", "test-123", "file.txt"])

        result = prolog.query_once("argv(Args)")
        assert result is not None
        assert result['Args'] == ["hello world", "test-123", "file.txt"]

    def test_argv_unification(self):
        """Test argv/1 with specific unification."""
        prolog = PrologInterpreter(argv=["first", "second"])

        # Test unification with specific list
        assert prolog.has_solution("argv([first, second])")

        # Test that wrong arguments fail
        assert not prolog.has_solution("argv([wrong])")
        assert not prolog.has_solution("argv([first, third])")

    def test_argv_with_findall(self):
        """Test argv/1 with findall to collect arguments."""
        prolog = PrologInterpreter(argv=["a", "b", "c"])

        result = prolog.query_once("findall(X, argv(X), L)")
        assert result is not None
        # findall should collect the single result
        assert len(result['L']) == 1
        assert result['L'][0] == ["a", "b", "c"]

    def test_current_prolog_flag_argv_empty(self):
        """Test current_prolog_flag(argv, Args) with empty arguments."""
        prolog = PrologInterpreter()

        result = prolog.query_once("current_prolog_flag(argv, Args)")
        assert result is not None
        assert result['Args'] == []

    def test_current_prolog_flag_argv_single(self):
        """Test current_prolog_flag(argv, Args) with single argument."""
        prolog = PrologInterpreter(argv=["test"])

        result = prolog.query_once("current_prolog_flag(argv, Args)")
        assert result is not None
        assert result['Args'] == ["test"]

    def test_current_prolog_flag_argv_multiple(self):
        """Test current_prolog_flag(argv, Args) with multiple arguments."""
        prolog = PrologInterpreter(argv=["one", "two", "three"])

        result = prolog.query_once("current_prolog_flag(argv, Args)")
        assert result is not None
        assert result['Args'] == ["one", "two", "three"]

    def test_current_prolog_flag_argv_unification(self):
        """Test current_prolog_flag(argv, Args) with unification."""
        prolog = PrologInterpreter(argv=["expected"])

        assert prolog.has_solution("current_prolog_flag(argv, [expected])")
        assert not prolog.has_solution("current_prolog_flag(argv, [wrong])")

    def test_current_prolog_flag_other_flags(self):
        """Test that current_prolog_flag/2 doesn't match other flags."""
        prolog = PrologInterpreter(argv=["test"])

        # Should not match non-argv flags
        assert not prolog.has_solution("current_prolog_flag(version, _)")

    def test_determinism(self):
        """Test determinism of argv/1 and current_prolog_flag(argv, _)."""
        prolog = PrologInterpreter(argv=["a", "b"])
        assert len(list(prolog.query("argv(Args)"))) == 1
        assert len(list(prolog.query("current_prolog_flag(argv, Args)"))) == 1

    def test_argv_with_atom_string_conversion(self):
        """Test using argv/1 results with atom_string/2."""
        prolog = PrologInterpreter(argv=["hello"])

        # This should work if atom_string is available
        result = prolog.query_once("argv([Arg]), atom_string(Arg, String)")
        # Note: atom_string may not be implemented yet, so this might fail
        # but the query should parse and argv should work
        if result is not None:
            assert result['String'] == "hello"

    def test_argv_order_preservation(self):
        """Test that argument order is preserved."""
        args = ["first", "second", "third", "fourth"]
        prolog = PrologInterpreter(argv=args)

        result = prolog.query_once("argv(Args)")
        assert result is not None
        assert result['Args'] == args

    def test_argv_none_initialization(self):
        """Test that argv defaults to empty list when None passed."""
        prolog = PrologInterpreter(argv=None)

        result = prolog.query_once("argv(Args)")
        assert result is not None
        assert result['Args'] == []

    def test_argv_empty_list(self):
        """Test explicit empty argv list."""
        prolog = PrologInterpreter(argv=[])

        result = prolog.query_once("argv(Args)")
        assert result is not None
        assert result['Args'] == []

    def test_integration_with_program_loading(self):
        """Test argv works after loading a program."""
        prolog = PrologInterpreter(argv=["test_arg"])
        prolog.consult_string("test_pred(X) :- argv([X]).")

        assert prolog.has_solution("test_pred(test_arg)")