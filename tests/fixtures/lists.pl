% List operations fixtures: custom list predicates
% Tests list manipulation and recursion on lists

my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

my_reverse([], []).
my_reverse([H|T], R) :- my_reverse(T, RT), my_append(RT, [H], R).

my_member(X, [X|_]).
my_member(X, [_|T]) :- my_member(X, T).

% Simple map using my_succ (renamed to avoid conflict with built-in succ/2)
my_succ(X, Y) :- Y is X + 1.
my_map(_, [], []).
my_map(P, [H|T], [R|RT]) :- call(P, H, R), my_map(P, T, RT).

% Queries:
% ?- my_append([1,2], [3,4], X). % X = [1,2,3,4]
% ?- my_reverse([1,2,3], X). % X = [3,2,1]
% ?- my_member(2, [1,2,3]). % true
% ?- my_member(4, [1,2,3]). % false
% ?- my_map(my_succ, [1,2,3], X). % X = [2,3,4]