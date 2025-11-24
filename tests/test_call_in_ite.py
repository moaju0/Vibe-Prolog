from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

# Test if-then-else with call in the else branch
prolog.consult_string("""
test_ite_with_call(Goal, Result) :-
    (   clause(Goal, Body)
    ->  Result = has_clause
    ;   call(Goal),
        Result = called_builtin
    ).
""")

print("1. Test with a goal that has a clause...")
prolog.consult_string("myfact(a).")
results = prolog.query("test_ite_with_call(myfact(a), R).")
print(f"   Results: {results}")

print("\n2. Test with a built-in (is)...")
results = prolog.query("test_ite_with_call(X is 2 + 3, R).")
print(f"   Results: {results}")

print("\n3. Test call(X is 2 + 3) directly...")
results = prolog.query("call(X is 2 + 3).")
print(f"   Results: {results}")

print("\n4. Test the full pattern from mi...")
prolog.consult_string("""
test_full_pattern(Goal, ExplOut) :-
    ExplMid = [test],
    (   clause(Goal, Body)
    ->  ExplOut = has_clause
    ;   call(Goal),
        ExplOut = ExplMid
    ).
""")
results = prolog.query("test_full_pattern(X is 2 + 3, Expl).")
print(f"   Results: {results}")
