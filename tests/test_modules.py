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
        :- module(m1, [foo/1]).
        foo(x).
        
        bar :- m1:foo(x).
    """)
    assert prolog.has_solution("bar")

def test_module_property_nonexistent_module():
    prolog = PrologInterpreter()
    sols = list(prolog.query("module_property(nonexistent, _)"))
    assert len(sols) == 0
