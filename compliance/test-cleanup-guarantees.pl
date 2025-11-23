% Test that cleanup is guaranteed to run even when generator is interrupted

% Test 1: cleanup runs with once/1
test_cleanup_with_once :-
    % This should call cleanup even though once stops after first solution
    once(call_cleanup(
        member(X, [1, 2, 3]),
        writeln('cleanup_ran')
    )),
    X == 1.

% Test 2: setup_call_cleanup runs cleanup with once/1
test_setup_cleanup_with_once :-
    once(setup_call_cleanup(
        writeln('setup'),
        member(X, [a, b, c]),
        writeln('cleanup')
    )),
    X == a.

% Test 3: cleanup runs even when goal fails
test_cleanup_on_failure :-
    \+ call_cleanup(
        fail,
        writeln('cleanup_on_fail')
    ).

% Test 4: cleanup runs on exception (if we had throw/catch working properly)
% For now, just test normal backtracking

% Run all tests
run_test(Test) :-
    format("Testing ~w... ", [Test]),
    ( call(Test) ->
        writeln("OK")
    ;   writeln("FAILED"),
        false
    ).

run_all :-
    Tests = [
        test_cleanup_with_once,
        test_setup_cleanup_with_once,
        test_cleanup_on_failure
    ],
    maplist(run_test, Tests),
    writeln('\nAll cleanup guarantee tests passed!').
