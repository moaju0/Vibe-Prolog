"""Regression tests for engine correctness fixes."""

from vibeprolog import PrologInterpreter


class TestListConversion:
    """Ensure list handling respects active substitutions."""

    def test_append_with_open_tail_substitution(self):
        """append/3 should honor bindings when flattening open lists."""

        prolog = PrologInterpreter()

        result = prolog.query_once("T = [2], append([1|T], [3], R).")

        assert result is not None
        assert result["R"] == [1, 2, 3]
        assert result["T"] == [2]

    def test_append_open_tail_bindings_not_truncated(self):
        """Open tails must be bound, not silently dropped during conversion."""

        prolog = PrologInterpreter()

        result = prolog.query_once("append([1|Tail], [2], [1,2]).")

        assert result is not None
        assert result["Tail"] == []

    def test_append_improper_list_fails(self):
        """Improper lists should cause append to fail rather than truncate."""

        prolog = PrologInterpreter()

        result = prolog.query_once("append([1|3], [], R).")

        assert result is None


class TestMaplistStreaming:
    """maplist should stream predicate solutions."""

    def test_maplist_collects_all_element_solutions(self):
        """maplist/2 should backtrack over predicate solutions per element."""

        prolog = PrologInterpreter()
        prolog.consult_string("p(a). p(b).")

        solutions = prolog.query("maplist(p, [X]).")

        assert [solution["X"] for solution in solutions] == ["a", "b"]


class TestIfThenStreaming:
    """If-then should not exhaust all condition solutions."""

    def test_if_then_consumes_only_first_solution(self):
        """->/2 and ;/2 should leave remaining condition choices untouched."""

        prolog = PrologInterpreter()
        prolog.query_once("assertz(next(1)).")
        prolog.query_once("assertz(next(2)).")

        result = prolog.query_once("(retract(next(X)) -> true ; true).")

        assert result is not None
        remaining = prolog.query_once("findall(N, next(N), Ns).")

        assert remaining is not None
        assert len(remaining["Ns"]) == 1
        assert remaining["Ns"] in ([1], [2])


class TestTermOrdering:
    """setof/3 ordering must match Prolog term order (lists after compounds)."""

    def test_setof_uses_standard_ordering(self):
        """setof should sort by variable, number, atom, compound, then list order."""

        prolog = PrologInterpreter()

        result = prolog.query_once(
            "setof(X, member(X, [b, a, foo(1), foo(0), [a], [], 2, 1.5]), Set)."
        )

        assert result is not None
        assert result["Set"] == [1.5, 2, "a", "b", {"foo": [0]}, {"foo": [1]}, [], ["a"]]
