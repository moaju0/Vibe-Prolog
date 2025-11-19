% Minimal test for meta-interpreter

% Simple facts
fact(a).
fact(b).

% Simple rule
rule(X) :- fact(X).

% Meta-interpreter (simplified version of mi from cans.pl)
mi(true, Expl, Expl) :- !.

mi((A,B), ExplIn, ExplOut) :- !,
    mi(A, ExplIn, ExplMid),
    mi(B, ExplMid, ExplOut).

mi(Goal, ExplIn, ExplOut) :-
    (   clause(Goal, Body)
    ->  mi(Body, ExplIn, ExplOut)
    ;   call(Goal),
        ExplOut = ExplIn
    ).

% Test predicate
test_mi :-
    mi(rule(X), [], Expl),
    writeln(X).
