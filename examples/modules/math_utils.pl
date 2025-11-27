:- module(math_utils, [double/2, square/2, factorial/2]).

double(X, Y) :- Y is X * 2.

square(X, Y) :- Y is X * X.

factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Private predicate
private_helper(X, Y) :- Y is X + 1.