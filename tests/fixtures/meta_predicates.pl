% Meta-predicates fixtures: call/1, findall/3
% Tests higher-order predicates and meta-programming

p(a).
p(b).
p(c).

q(X) :- findall(Y, p(Y), X).

% Using call
test_call :- call(p(a)).

% Queries:
% ?- q(X). % X = [a,b,c]
% ?- test_call. % true
% ?- findall(X, (X=a; X=b), L). % L = [a,b]