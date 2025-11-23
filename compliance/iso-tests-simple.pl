% Simplified ISO Conformity Tests
% Tests basic functionality that should work with current parser

% Helper predicates
test_syntax_error(ReadString, Error) :-
    catch((once(read_from_chars(ReadString, _)),
           false),
          error(Error, _),
          true).

% Test read_from_chars/2
test_read_1 :- read_from_chars("atom.", T),
               T == atom.

test_read_2 :- read_from_chars("f(a, b).", T),
               T == f(a, b).

test_read_3 :- read_from_chars("[1, 2, 3].", T),
               T == [1, 2, 3].

% Test write_term_to_chars/3
test_write_1 :- write_term_to_chars(atom, [quoted(false)], Chars),
                Chars == [a,t,o,m].

test_write_2 :- write_term_to_chars(123, [quoted(false)], Chars),
                Chars == ['1','2','3'].

test_write_3 :- write_term_to_chars([1,2], [quoted(false)], Chars),
                Chars == ['[','1',',','2',']'].

% Test setup_call_cleanup/3
test_setup_1 :- setup_call_cleanup(true, X = 5, true),
                X == 5.

test_setup_2 :- setup_call_cleanup(X = 1, Y = 2, Z = 3),
                X == 1, Y == 2.

% Test call_cleanup/2
test_cleanup_1 :- call_cleanup(X = 10, true),
                  X == 10.

% Test runner
run_test(Test) :-
    ( call(Test) ->
        true
    ;   format("~w failed!~n", [Test]),
        false
    ).

run_all_tests :-
    findall(Test,
            ( current_predicate(Test/0),
              atom_chars(Test, Chars),
              append("test_", _, Chars)
            ),
            Tests),
    format("Running ~w tests...~n", [[Tests]]),
    ( maplist(run_test, Tests) ->
        write('All tests passed!\n')
    ;   write('Some tests failed\n'),
        false
    ).

% Manual test list (in case findall doesn't work)
run_all_tests_manual :-
    Tests = [
        test_read_1, test_read_2, test_read_3,
        test_write_1, test_write_2, test_write_3,
        test_setup_1, test_setup_2,
        test_cleanup_1
    ],
    format("Running ~w tests...~n~n", [[Tests]]),
    run_tests_helper(Tests, FailedTests),
    ( FailedTests == [] ->
        write('\nAll tests passed!\n')
    ;   format("\nFailed tests: ~w~n", [FailedTests]),
        false
    ).

run_tests_helper([], []).
run_tests_helper([Test|Tests], FailedTests) :-
    format("  Running ~w... ", [Test]),
    ( call(Test) ->
        write("OK\n"),
        run_tests_helper(Tests, FailedTests)
    ;   write("FAILED\n"),
        run_tests_helper(Tests, RestFailed),
        FailedTests = [Test|RestFailed]
    ).

% Run with: uv run python main.py compliance/iso-tests-simple.pl -q "run_all_tests_manual"
