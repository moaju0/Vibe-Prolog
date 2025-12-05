"""Tests for operator shadowing with --builtin-conflict=shadow mode.

This module tests the ability for modules to shadow protected operators
like `|` that are used extensively in CLP libraries (clpb, clpz, etc.).
"""

import tempfile
from pathlib import Path

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestOperatorShadowingBasic:
    """Basic tests for operator shadowing behavior."""

    def test_shadow_mode_allows_pipe_operator_redefinition(self):
        """Shadow mode should allow redefining the | operator within a module."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        # This should not raise an error
        prolog.consult_string("""
            :- module(test_mod, []).
            :- op(700, xfx, '|').
        """)

    def test_shadow_mode_pipe_operator_in_module(self):
        """Shadowed pipe operator should work within the module."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, [test_pipe/2]).
            :- op(700, xfx, '|').
            test_pipe(A, B) :- A = 1, B = 2.
        """)
        result = prolog.query_once("test_mod:test_pipe(X, Y)")
        assert result is not None
        assert result["X"] == 1
        assert result["Y"] == 2

    def test_skip_mode_silently_ignores_pipe_redefinition(self):
        """Skip mode should silently ignore pipe operator redefinition."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        # This should not raise an error
        prolog.consult_string("""
            :- module(test_mod, []).
            :- op(700, xfx, '|').
        """)

    def test_error_mode_rejects_pipe_redefinition(self):
        """Error mode should reject pipe operator redefinition."""
        prolog = PrologInterpreter(builtin_conflict="error")
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- module(test_mod, []).
                :- op(700, xfx, '|').
            """)
        error_str = str(exc_info.value)
        assert "permission_error" in error_str
        assert "operator" in error_str

    def test_shadow_global_scope_unaffected(self):
        """Shadowing in a module should not affect global scope."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, []).
            :- op(700, xfx, '|').
        """)
        # The global list syntax should still work
        result = prolog.query_once("X = [1, 2 | [3]]")
        assert result is not None
        assert result["X"] == [1, 2, 3]


class TestOperatorShadowingModuleQualification:
    """Tests for module-qualified operator calls."""

    def test_shadow_mode_other_modules_unaffected(self):
        """Shadowing in one module should not affect other modules."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(mod_a, []).
            :- op(700, xfx, '|').
        """)
        prolog.consult_string("""
            :- module(mod_b, [use_list/1]).
            use_list(X) :- X = [a | [b, c]].
        """)
        result = prolog.query_once("mod_b:use_list(L)")
        assert result is not None
        assert result["L"] == ["a", "b", "c"]

    def test_shadow_mode_multiple_modules_can_shadow_same_operator(self):
        """Multiple modules can independently shadow the same operator."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(mod_a, []).
            :- op(700, xfx, '|').
        """)
        prolog.consult_string("""
            :- module(mod_b, []).
            :- op(700, xfx, '|').
        """)
        # Both should work without errors


class TestOperatorShadowingWithImports:
    """Tests for operator shadowing with module imports."""

    def test_shadow_import_shadowed_operator(self):
        """Importing from a module with shadowed operators should work."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(lib_mod, [process/2]).
            :- op(700, xfx, '|').
            process(A, B) :- A = x, B = y.
        """)
        prolog.consult_string("""
            :- use_module(lib_mod, [process/2]).
        """)
        result = prolog.query_once("process(A, B)")
        assert result is not None
        assert result["A"] == "x"
        assert result["B"] == "y"


class TestProtectedOperators:
    """Tests for all protected operators in shadow mode."""

    def test_shadow_comma_operator(self):
        """Shadow mode should allow redefining , operator."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, []).
            :- op(1000, xfy, ',').
        """)

    def test_shadow_semicolon_operator(self):
        """Shadow mode should allow redefining ; operator."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, []).
            :- op(1100, xfy, ';').
        """)

    def test_shadow_arrow_operator(self):
        """Shadow mode should allow redefining -> operator."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, []).
            :- op(1050, xfy, '->').
        """)

    def test_shadow_colon_operator(self):
        """Shadow mode should allow redefining : operator."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, []).
            :- op(600, xfy, ':').
        """)


class TestLibraryCompatibility:
    """Tests for library compatibility with shadow mode."""

    def test_can_load_iso_ext_with_shadow_mode(self):
        """library/iso_ext.pl should load with shadow mode."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string(":- use_module(library(iso_ext)).")

    def test_can_load_dcgs_with_shadow_mode(self):
        """library/dcgs.pl should load with shadow mode."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string(":- use_module(library(dcgs)).")


class TestOperatorTableMethods:
    """Tests for OperatorTable shadow-related methods."""

    def test_set_builtin_conflict(self):
        """Test set_builtin_conflict method."""
        from vibeprolog.operators import OperatorTable

        table = OperatorTable(builtin_conflict="skip")
        assert table._builtin_conflict == "skip"

        table.set_builtin_conflict("shadow")
        assert table._builtin_conflict == "shadow"

        table.set_builtin_conflict("error")
        assert table._builtin_conflict == "error"

    def test_get_module_operators(self):
        """Test get_module_operators method."""
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        table.define(Number(700), Atom("xfx"), Atom("|"), "test", module_name="test_mod")

        ops = table.get_module_operators("test_mod")
        assert ("|", "xfx") in ops
        assert ops[("|", "xfx")].precedence == 700

    def test_is_shadowed(self):
        """Test is_shadowed method."""
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        table.define(Number(700), Atom("xfx"), Atom("|"), "test", module_name="test_mod")

        assert table.is_shadowed("test_mod", "|", "xfx")
        assert not table.is_shadowed("other_mod", "|", "xfx")

    def test_clone_preserves_shadow_state(self):
        """Test that clone preserves shadow-related state."""
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        table.define(Number(700), Atom("xfx"), Atom("|"), "test", module_name="test_mod")

        cloned = table.clone()
        assert cloned._builtin_conflict == "shadow"
        assert cloned.is_shadowed("test_mod", "|", "xfx")
        ops = cloned.get_module_operators("test_mod")
        assert ("|", "xfx") in ops


class TestOperatorShadowingEdgeCases:
    """Edge cases for operator shadowing."""

    def test_shadow_undefine_operator(self):
        """Setting precedence to 0 should remove the shadowed operator."""
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        table.define(Number(700), Atom("xfx"), Atom("|"), "test", module_name="test_mod")

        # Remove the operator
        table.define(Number(0), Atom("xfx"), Atom("|"), "test", module_name="test_mod")

        ops = table.get_module_operators("test_mod")
        assert ("|", "xfx") not in ops

    def test_shadow_without_module_context(self):
        """Shadowing protected operator without module context is silently ignored.

        This prevents global pollution of the operator table while still allowing
        the op/3 directive to succeed without raising an error.
        """
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        # Define at global level (no module) - this should be silently ignored
        # for protected operators to prevent global pollution
        table.define(Number(800), Atom("xfx"), Atom("|"), "test", module_name=None)

        # The protected operator should NOT be added to global table
        # (it may not exist in global table at all, or retains original)
        info = table.lookup("|", "xfx")
        # Either None or unchanged - the key is it's not 800
        if info is not None:
            assert info.precedence != 800

    def test_shadow_non_protected_without_module_context(self):
        """Non-protected operators can still be defined at global level."""
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        # Custom operator should work at global level
        table.define(Number(500), Atom("xfx"), Atom("my_op"), "test", module_name=None)

        info = table.lookup("my_op", "xfx")
        assert info is not None
        assert info.precedence == 500

    def test_non_protected_operators_work_normally(self):
        """Non-protected operators should work the same in all modes."""
        for mode in ["skip", "error", "shadow"]:
            prolog = PrologInterpreter(builtin_conflict=mode)
            prolog.consult_string("""
                :- module(test_mod, []).
                :- op(500, xfx, custom_op).
            """)
            # Should work without issues

    def test_shadow_mode_preserves_list_syntax(self):
        """List syntax with | should still work after shadowing."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(test_mod, [test_list/1]).
            :- op(700, xfx, '|').
            test_list(X) :- X = [1, 2, 3].
        """)

        # List syntax should work in user module
        result = prolog.query_once("X = [a, b | [c, d]]")
        assert result is not None
        assert result["X"] == ["a", "b", "c", "d"]

        # List through module predicate should also work
        result = prolog.query_once("test_mod:test_list(L)")
        assert result is not None
        assert result["L"] == [1, 2, 3]


class TestModuleScopedOperatorParsing:
    """Tests that module-scoped operators are recognized during parsing."""

    def test_parser_uses_module_scoped_operator_during_consult(self):
        """Parser should use module-scoped operators when parsing module content.

        This test verifies that during consult, module-scoped operators are
        correctly used to parse code within that module.
        """
        prolog = PrologInterpreter(builtin_conflict="shadow")
        # Define a module that uses | as an infix operator in clause heads
        # The get_pipe_args/3 predicate uses pattern matching with | operator
        prolog.consult_string("""
            :- module(clp_test, [get_pipe_args/3]).
            :- op(700, xfx, '|').
            get_pipe_args(X | Y, X, Y).
        """)
        # Query the predicate with a compound term built using functor '|'
        # We use the '|'/2 functor directly to avoid query parsing issues
        result = prolog.query_once("clp_test:get_pipe_args('|'(a, b), A, B)")
        assert result is not None
        assert result["A"] == "a"
        assert result["B"] == "b"

    def test_module_operator_in_clause_body(self):
        """Module-scoped operators should work in clause bodies during consult."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        prolog.consult_string("""
            :- module(pipe_mod, [make_pipe/3]).
            :- op(700, xfx, '|').
            make_pipe(X, Y, X | Y).
        """)
        # Call the predicate and verify it creates a compound with | functor
        result = prolog.query_once("pipe_mod:make_pipe(foo, bar, R)")
        assert result is not None
        # The result should be a compound term with functor '|'
        # The Python representation is {functor: [args...]}
        assert result["R"] == {"|": ["foo", "bar"]}

    def test_parser_module_operator_different_precedence(self):
        """Module can define operator with different precedence than global."""
        prolog = PrologInterpreter(builtin_conflict="shadow")
        # Define a custom operator in module with unusual precedence
        prolog.consult_string("""
            :- module(custom_ops, [use_op/1]).
            :- op(300, xfy, '@@').
            use_op(X) :- X = (1 @@ 2 @@ 3).
        """)
        result = prolog.query_once("custom_ops:use_op(X)")
        assert result is not None
        # Should parse as right-associative @@

    def test_iter_operators_for_module_combines_global_and_module(self):
        """iter_operators_for_module should combine global and module operators."""
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        # Add a module-scoped operator for a protected operator
        table.define(Number(700), Atom("xfx"), Atom("|"), "test", module_name="my_mod")

        # The global table should have the default operators
        global_ops = list(table.iter_current_ops())
        assert len(global_ops) > 0

        # Module-scoped iteration should include both global and module ops
        mod_ops = list(table.iter_operators_for_module("my_mod"))
        mod_op_names = [name for name, _ in mod_ops]

        # Should include standard operators
        assert "+" in mod_op_names
        assert "-" in mod_op_names

        # Should include the shadowed pipe operator
        assert "|" in mod_op_names

        # The pipe operator should have the module-scoped precedence
        pipe_info = None
        for name, info in mod_ops:
            if name == "|" and info.spec == "xfx":
                pipe_info = info
                break
        assert pipe_info is not None
        assert pipe_info.precedence == 700

    def test_iter_operators_for_module_none_returns_global(self):
        """iter_operators_for_module(None) should return only global operators."""
        from vibeprolog.operators import OperatorTable
        from vibeprolog.terms import Atom, Number

        table = OperatorTable(builtin_conflict="shadow")
        # Add a module-scoped operator
        table.define(Number(700), Atom("xfx"), Atom("|"), "test", module_name="my_mod")

        # None module should not include module-scoped operators
        global_ops = list(table.iter_operators_for_module(None))
        global_op_dict = {(name, info.spec): info for name, info in global_ops}

        # Pipe might not be in global table or might have default precedence
        if ("|", "xfx") in global_op_dict:
            # If it exists, it should NOT be 700 (the module-scoped value)
            assert global_op_dict[("|", "xfx")].precedence != 700
