from vibeprolog import PrologInterpreter


FIB_PROGRAM = """
:- table(fib/2).

fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.
"""


def test_table_directive_registers_predicates():
    prolog = PrologInterpreter()
    prolog.consult_string(":- table([foo/1, bar/2]). foo(a). bar(a, b).")

    assert ("foo", 1) in prolog._tabled_predicates
    assert ("bar", 2) in prolog._tabled_predicates


def test_tabled_fib_computes_result():
    prolog = PrologInterpreter()
    prolog.consult_string(FIB_PROGRAM)

    solution = prolog.query_once("fib(10, F).")
    assert solution is not None
    assert solution["F"] == 55


def test_cached_answers_are_reused():
    prolog = PrologInterpreter()
    prolog.consult_string(FIB_PROGRAM)

    first = prolog.query_once("fib(7, F).")
    assert first["F"] == 13
    initial_cache = prolog.engine._table_cache.copy()

    second = prolog.query_once("fib(7, F).")
    assert second["F"] == 13
    assert prolog.engine._table_cache == initial_cache


def test_tabled_predicate_with_variables():
    prolog = PrologInterpreter()
    prolog.consult_string(
        ":- table(member/2). member(X, [X|_]). member(X, [_|T]) :- member(X, T)."
    )

    solutions = prolog.query("member(X, [a, b]).")
    values = {sol["X"] for sol in solutions}
    assert values == {"a", "b"}


def test_tabled_variant_calls_produce_fresh_bindings():
    prolog = PrologInterpreter()
    prolog.consult_string(
        ":- table(member/2). member(X, [X|_]). member(X, [_|T]) :- member(X, T)."
    )

    first_values = {sol["X"] for sol in prolog.query("member(X, [a, b]).")}
    assert first_values == {"a", "b"}

    # Re-query with a different variable name to ensure cached answers unify correctly
    second_values = {sol["Y"] for sol in prolog.query("member(Y, [a, b]).")}
    assert second_values == {"a", "b"}
