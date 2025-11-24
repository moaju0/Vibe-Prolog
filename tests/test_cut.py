from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

# Test cut directly
prolog.consult_string("""
test_cut :- !.
test_cut_with_true :- !, true.
""")

print("Testing cut...")
results = prolog.query("test_cut.")
print(f"test_cut: {results}")

print("\nTesting cut with true...")
results = prolog.query("test_cut_with_true.")
print(f"test_cut_with_true: {results}")

# Test the exact pattern from mi
prolog.consult_string("""
test_pattern(true, X, X) :- !.
""")

print("\nTesting pattern...")
results = prolog.query("test_pattern(true, [], Y).")
print(f"test_pattern(true, [], Y): {results}")
