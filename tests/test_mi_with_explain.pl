% Test meta-interpreter with explain_goal pattern from cans.pl

fact(a).
rule(X) :- fact(X).

% Explanation for rule
explain_goal(rule(X), Msg) :-
    format(atom(Msg), "Checking rule for ~w", [X]).

% Meta-interpreter with explanation (from cans.pl)
mi(true, Expl, Expl) :- !.

mi((A,B), ExplIn, ExplOut) :- !,
    mi(A, ExplIn, ExplMid),
    mi(B, ExplMid, ExplOut).

mi(Goal, ExplIn, ExplOut) :-
    explain_goal(Goal, Msg), !,
    append(ExplIn, [Msg], ExplMid),
    (   clause(Goal, Body)
    ->  mi(Body, ExplMid, ExplOut)
    ;   call(Goal),
        ExplOut = ExplMid
    ).

mi(Goal, ExplIn, ExplOut) :-
    (   clause(Goal, Body)
    ->  mi(Body, ExplIn, ExplOut)
    ;   call(Goal),
        ExplOut = ExplIn
    ).
