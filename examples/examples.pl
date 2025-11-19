% Example Prolog rules and facts

% Family relationships
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Animals and characteristics
animal(dog).
animal(cat).
animal(bird).

has_fur(dog).
has_fur(cat).

can_fly(bird).

mammal(X) :- animal(X), has_fur(X).

% Simple math facts
even(0).
even(2).
even(4).
even(6).
even(8).

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).
