from vibeprolog import PrologInterpreter

prolog = PrologInterpreter()

# Test append
print("Testing append...")
results = prolog.query("append([1, 2], [3, 4], X).")
print(f"append([1,2], [3,4], X): {results}")

# Test clause
prolog.consult_string("test(foo). test(bar) :- true.")
print("\nTesting clause...")
results = prolog.query("clause(test(X), Body).")
print(f"clause(test(X), Body): {results}")

# Test call
print("\nTesting call...")
prolog.consult_string("foo(1).")
results = prolog.query("call(foo(1)).")
print(f"call(foo(1)): {results}")

# Test writeln
print("\nTesting writeln...")
results = prolog.query('writeln("Hello, World!").')
print(f"writeln result: {results}")

# Test maplist
print("\nTesting maplist...")
results = prolog.query("maplist(writeln, [a, b, c]).")
print(f"maplist result: {results}")
