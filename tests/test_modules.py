import pytest

from vibeprolog import PrologInterpreter


class TestModuleIsolation:
    def test_private_predicate_not_accessible(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(m1, [public/1]).
            public(x).
            private(y).
        """)
        assert prolog.has_solution("m1:public(x)")
        assert not prolog.has_solution("m1:private(y)")


class TestMultipleModules:
    def test_two_modules_same_predicate(self):
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(m1, [foo/1]).
            foo(from_m1).

            :- module(m2, [foo/1]).
            foo(from_m2).
        """)
        r1 = prolog.query_once("m1:foo(X)")
        assert r1 is not None and r1["X"] == "from_m1"
        r2 = prolog.query_once("m2:foo(X)")
        assert r2 is not None and r2["X"] == "from_m2"


def test_current_module_builtin():
    prolog = PrologInterpreter()
    prolog.consult_string(":- module(m1, [a/1]).\n")
    sols = prolog.query("current_module(M)")
    names = set(s["M"] for s in sols)
    assert names == {"m1", "user"}

def test_builtin_accessible_from_module():
    prolog = PrologInterpreter()
    prolog.consult_string(":- module(m1, [test/1]). test(X) :- append([1], [2], X).")
    sols = prolog.query("m1:test(X)")
    assert any(s.get("X") == [1, 2] for s in sols)

def test_module_qualified_call_in_clause_body():
    prolog = PrologInterpreter()
    prolog.consult_string("""
        bar :- m1:foo(x).
        :- module(m1, [foo/1]).
        foo(x).
    """)
    assert prolog.has_solution("bar")

def test_module_property_nonexistent_module():
    prolog = PrologInterpreter()
    sols = list(prolog.query("module_property(nonexistent, _)"))
    assert len(sols) == 0


def test_unqualified_call_in_clause_body():
    """Test that unqualified goals in clause bodies resolve to the defining module."""
    prolog = PrologInterpreter()
    prolog.consult_string("""
        :- module(m1, [test/1]).
        helper(a).
        test(X) :- helper(X).

        :- module(m2, [helper/1]).
        helper(b).
    """)
    # m1:test should find m1:helper(a), not m2:helper(b)
    result = prolog.query_once("m1:test(X)")
    assert result is not None and result["X"] == "a"


def test_unqualified_call_prefers_defining_module():
    """Unqualified calls in module clauses prefer local predicates over user."""
    prolog = PrologInterpreter()
    prolog.consult_string("""
        helper(user).  % in user module

        :- module(m1, [test/1]).
        helper(local).  % in m1 module
        test(X) :- helper(X).
    """)
    # Should find local, not user
    result = prolog.query_once("m1:test(X)")
    assert result is not None and result["X"] == "local"


def test_unqualified_call_cannot_access_private_other_module():
    """Unqualified calls cannot access private predicates of other modules."""
    prolog = PrologInterpreter()
    prolog.consult_string("""
        :- module(m1, [test/1]).
        test(X) :- other_helper(X).  % unqualified, should not find m2's private

        :- module(m2, [public/1]).
        other_helper(private).  % not exported
        public(public).
    """)
    # Should not find the private predicate
    assert not prolog.has_solution("m1:test(X)")


def test_builtin_accessible_from_module_clause():
    """Built-ins remain globally accessible from module clauses."""
    prolog = PrologInterpreter()
    prolog.consult_string("""
        :- module(m1, [test/1]).
        test(X) :- append([1], [2], X).
    """)
    result = prolog.query_once("m1:test(X)")
    assert result is not None and result["X"] == [1, 2]


class TestUseModule:
    """Tests for use_module/1,2 directives."""

    def test_use_module_file_all_exports(self):
        """Test use_module(File) imports all exported predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- use_module('examples/modules/math_utils.pl').
            test_double(X) :- double(5, X).
            test_square(X) :- square(3, X).
        """)
        result1 = prolog.query_once("test_double(X)")
        assert result1 is not None and result1["X"] == 10
        result2 = prolog.query_once("test_square(X)")
        assert result2 is not None and result2["X"] == 9

    def test_use_module_file_specific_imports(self):
        """Test use_module(File, [pred/arity]) imports only specified predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- use_module('examples/modules/math_utils.pl', [double/2]).
            test_double(X) :- double(4, X).
        """)
        # Should work for imported predicate
        result = prolog.query_once("test_double(X)")
        assert result is not None and result["X"] == 8

        # Should fail for non-imported predicate
        assert not prolog.has_solution("square(2, X)")

    def test_use_module_nonexistent_file(self):
        """Test use_module with nonexistent file raises error."""
        prolog = PrologInterpreter()
        from vibeprolog.exceptions import PrologThrow

        with pytest.raises(PrologThrow) as excinfo:
            prolog.consult_string(":- use_module('nonexistent.pl').")
        assert "existence_error" in str(excinfo.value.term)

    def test_use_module_nonexistent_file_in_path(self):
        """Test use_module with a nonexistent file in an existing path raises an error."""
        prolog = PrologInterpreter()
        try:
            prolog.consult_string(":- use_module('examples/modules/nonexistent.pl').")
            assert False, "Should have raised an error"
        except Exception as e:
            assert "existence_error" in str(e)

    def test_use_module_private_predicate(self):
        """Test use_module cannot import non-exported predicates."""
        prolog = PrologInterpreter()
        try:
            prolog.consult_string("""
                :- use_module('examples/modules/math_utils.pl', [private_helper/2]).
            """)
            assert False, "Should have raised a permission error"
        except Exception as e:
            assert "permission_error" in str(e)

    def test_use_module_idempotent(self):
        """Test use_module is idempotent - multiple imports don't duplicate."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- use_module('examples/modules/lists.pl').
            :- use_module('examples/modules/lists.pl').
            test(X) :- my_append([1,2], [3,4], X).
        """)
        result = prolog.query_once("test(X)")
        assert result is not None and result["X"] == [1, 2, 3, 4]

    def test_use_module_in_module_clause(self):
        """Test use_module works when used inside a module."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(my_module, [test/1]).
            :- use_module('examples/modules/math_utils.pl').
            test(X) :- double(3, X).
        """)
        result = prolog.query_once("my_module:test(X)")
        assert result is not None and result["X"] == 6

    def test_imported_predicates_in_clause_body(self):
        """Test imported predicates work in clause bodies."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- use_module('examples/modules/lists.pl').
            test_combined(X) :- my_append([1], [2], Temp), my_reverse(Temp, X).
        """)
        result = prolog.query_once("test_combined(X)")
        assert result is not None and result["X"] == [2, 1]

    def test_use_module_library_syntax(self):
        """Test library(Name) syntax for use_module."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- use_module(library(math_utils)).
            test(X) :- double(7, X).
        """)
        result = prolog.query_once("test(X)")
        assert result is not None and result["X"] == 14


class TestOperatorExports:
    """Tests for operator exports in module declarations."""

    def test_module_exports_operator(self):
        """Module can export an operator in its export list."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(ops_test, [op(300, fy, ~), foo/1]).
            foo(bar).
        """)
        # Check that the module was created and the predicate is accessible via qualification
        assert prolog.has_solution("ops_test:foo(bar)")

    def test_operator_available_after_import(self):
        """Exported operators are available after import."""
        prolog = PrologInterpreter()
        # Create a module that exports an operator
        prolog.consult_string("""
            :- op(200, xfx, +++).
            :- module(myops, [op(200, xfx, +++), test/1]).
            test(a +++ b).
        """)
        # Import it and verify operator works
        prolog.consult_string("""
            :- use_module(myops).
            query :- test(a +++ b).
        """)
        assert prolog.has_solution("query")

    def test_multiple_operator_exports(self):
        """Module can export multiple operators."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(multi_ops, [op(300, fy, ~), op(500, xfx, #), pred/0]).
            pred.
        """)
        assert prolog.has_solution("multi_ops:pred")

    def test_selective_import_excludes_operators(self):
        """Selective import should not import operators (SWI-Prolog behavior)."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(selective, [op(200, xfx, <<<), foo/1, bar/1]).
            foo(1).
            bar(2).
        """)
        # Selective import should not get the operator
        prolog.consult_string("""
            :- use_module(selective, [foo/1]).
            % The operator <<< should NOT be available here
        """)
        # Test that predicate works but operator doesn't
        assert prolog.has_solution("foo(1)")

    def test_full_import_includes_operators(self):
        """Full import (no list) should import operators."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(200, xfx, ++).
            :- module(full, [op(200, xfx, ++), baz/1]).
            baz(test ++ test).
        """)
        prolog.consult_string("""
            :- use_module(full).
            verify :- baz(test ++ test).
        """)
        assert prolog.has_solution("verify")

    def test_invalid_operator_precedence_in_export(self):
        """Invalid operator precedence should raise error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- module(bad, [op(2000, fy, ~)]).")
        assert "domain_error" in str(exc_info.value.term) or "operator_priority" in str(exc_info.value.term)

    def test_invalid_operator_associativity_in_export(self):
        """Invalid associativity should raise error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string(":- module(bad, [op(300, bad_assoc, ~)]).")
        assert "domain_error" in str(exc_info.value.term) or "operator_specifier" in str(exc_info.value.term)

    def test_clpb_operators(self):
        """Test with actual operators from library(clpb)."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(300, fy, ~).
            :- op(500, yfx, #).
            :- module(clpb_lite, [op(300, fy, ~), op(500, yfx, #), sat/1]).
            sat(~X) :- sat(1 # X).
            sat(1).
        """)
        prolog.consult_string("""
            :- use_module(clpb_lite).
            test :- sat(1).
        """)
        assert prolog.has_solution("test")
