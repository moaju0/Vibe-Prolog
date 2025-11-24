from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

print("1. Test clause with a fact...")
prolog.consult_string("myfact(a).")
results = prolog.query("clause(myfact(a), Body).")
print(f"   Results: {results}")

print("\n2. Test clause with a built-in (is)...")
results = prolog.query("clause(X is 2 + 3, Body).")
print(f"   Results: {results}")

print("\n3. Test if clause fails properly...")
results = prolog.query("clause(X is 2 + 3, Body), write(found).")
print(f"   Results: {results}")

print("\n4. Test if-then with clause condition...")
results = prolog.query("(clause(X is 2 + 3, B) -> write(has_clause) ; write(no_clause)).")
print(f"   Results: {results}")
