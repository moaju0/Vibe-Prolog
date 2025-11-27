:- module(lists, [my_append/3, my_reverse/2, my_length/2]).

my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

my_reverse([], []).
my_reverse([H|T], R) :- my_reverse(T, RT), my_append(RT, [H], R).

my_length([], 0).
my_length([_|T], N) :- my_length(T, N1), N is N1 + 1.