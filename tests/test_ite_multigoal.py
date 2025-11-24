from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

# Test with multiple goals in else branch
prolog.consult_string("""
test1 :-
    Goal = (X is 2 + 3),
    (clause(Goal, B) ->
        write(has)
    ;
        write(no), write(clause)
    ).

test2 :-
    Goal = (X is 2 + 3),
    (clause(Goal, B) ->
        write(has)
    ;
        call(Goal), write(X)
    ).

test3 :-
    Goal = (X is 2 + 3),
    (clause(Goal, B) ->
        write(has)
    ;
        call(Goal),
        Y = X,
        write(Y)
    ).
""")

print("1. Test with multiple writes in else...")
results = prolog.query("test1.")
print(f"   Results: {results}")

print("\n2. Test with call + write in else...")
results = prolog.query("test2.")
print(f"   Results: {results}")

print("\n3. Test with call + unify + write in else...")
results = prolog.query("test3.")
print(f"   Results: {results}")
