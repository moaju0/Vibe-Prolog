from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

# Simplest possible test
prolog.consult_string("""
test1 :- (1 =:= 2 -> write(then) ; write(else)).
test2 :- (1 =:= 1 -> write(then) ; write(else)).
test3 :- X = 5, (1 =:= 2 -> write(then) ; write(else)), write(X).
""")

print("1. Test with false condition...")
results = prolog.query("test1.")
print(f"   Results: {results}")

print("\n2. Test with true condition...")
results = prolog.query("test2.")
print(f"   Results: {results}")

print("\n3. Test with variable binding...")
results = prolog.query("test3.")
print(f"   Results: {results}")

# Test the exact pattern that's failing
prolog.consult_string("""
test4 :-
    Goal = (X is 2 + 3),
    (clause(Goal, B) -> write(has) ; write(no)).
""")

print("\n4. Test exact failing pattern...")
results = prolog.query("test4.")
print(f"   Results: {results}")
