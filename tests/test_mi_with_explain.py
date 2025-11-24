from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()
prolog.consult("tests/test_mi_with_explain.pl")

print("1. Test explain_goal(rule(X), Msg)...")
results = prolog.query("explain_goal(rule(X), Msg).")
print(f"   Results: {results[:1]}")

print("\n2. Test mi(rule(X), [], Expl)...")
results = prolog.query("mi(rule(X), [], Expl).")
print(f"   Results: {results[:2]}")

print("\n3. Test mi(rule(a), [], Expl)...")
results = prolog.query("mi(rule(a), [], Expl).")
print(f"   Results: {results}")

print("\n4. Test mi(fact(a), [], Expl)...")
results = prolog.query("mi(fact(a), [], Expl).")
print(f"   Results: {results}")
