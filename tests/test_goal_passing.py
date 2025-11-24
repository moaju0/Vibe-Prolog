from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

print("1. Test clause(Goal, Body) with Goal = myfact(a)...")
prolog.consult_string("myfact(a).")
results = prolog.query("Goal = myfact(a), clause(Goal, Body).")
print(f"   Results: {results}")

print("\n2. Test clause(Goal, Body) with Goal = (X is 2 + 3)...")
results = prolog.query("Goal = (X is 2 + 3), clause(Goal, Body).")
print(f"   Results: {results}")

print("\n3. Test if-then-else with Goal variable...")
prolog.consult_string("""
test_with_goal_var :-
    Goal = (X is 2 + 3),
    write("Goal = "), writeln(Goal),
    write("Testing clause..."), nl,
    (clause(Goal, Body) ->
        write("Has clause!")
    ;
        write("No clause, calling..."), nl,
        call(Goal),
        write("Call succeeded! X = "), writeln(X)
    ).
""")
results = prolog.query("test_with_goal_var.")
print(f"   Results: {results}")
