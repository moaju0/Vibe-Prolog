from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

# Test basic if-then-else
prolog.consult_string("""
test_if_then(X, Y) :- (X = 1 -> Y = one ; Y = other).
""")

print("Testing if-then-else...")
results = prolog.query("test_if_then(1, Y).")
print(f"test_if_then(1, Y): {results}")

results = prolog.query("test_if_then(2, Y).")
print(f"test_if_then(2, Y): {results}")

# Test disjunction
prolog.consult_string("""
test_or(X) :- X = a ; X = b.
""")

print("\nTesting disjunction...")
results = prolog.query("test_or(X).")
print(f"test_or(X): {results}")
