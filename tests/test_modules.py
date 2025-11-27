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
