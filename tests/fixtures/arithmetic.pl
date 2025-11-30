% Arithmetic fixtures: accumulators, comparisons
% Tests arithmetic operations and comparisons

greater_than(X, Y) :- X > Y.

product_list([], 1).
product_list([H|T], P) :- product_list(T, PT), P is PT * H.

% Queries:
% ?- greater_than(5, 3). % true
% ?- greater_than(3, 5). % false
% ?- product_list([2,3,4], X). % X = 24
