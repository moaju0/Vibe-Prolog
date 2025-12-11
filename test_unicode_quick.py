#!/usr/bin/env python3
"""Quick test of Unicode atom support."""

from vibeprolog import PrologInterpreter

# Test 1: Simple Greek letter atom
print("Test 1: Simple Greek letter atom (δ)")
try:
    prolog = PrologInterpreter()
    result = prolog.query_once("X = δ")
    if result:
        print(f"  SUCCESS: X = {result['X']}")
    else:
        print("  FAILED: No result")
except Exception as e:
    print(f"  ERROR: {e}")

# Test 2: Unicode atom with underscore
print("\nTest 2: Unicode atom with underscore (δ_test)")
try:
    prolog = PrologInterpreter()
    result = prolog.query_once("X = δ_test")
    if result:
        print(f"  SUCCESS: X = {result['X']}")
    else:
        print("  FAILED: No result")
except Exception as e:
    print(f"  ERROR: {e}")

# Test 3: Unicode in predicate name
print("\nTest 3: Unicode in predicate name")
try:
    prolog = PrologInterpreter()
    prolog.consult_string("δ_test(a). δ_test(b).")
    results = list(prolog.query("δ_test(X)"))
    if len(results) == 2:
        print(f"  SUCCESS: Found 2 results: {results[0]['X']}, {results[1]['X']}")
    else:
        print(f"  FAILED: Expected 2 results, got {len(results)}")
except Exception as e:
    print(f"  ERROR: {e}")

# Test 4: Cyrillic
print("\nTest 4: Cyrillic atom (тест)")
try:
    prolog = PrologInterpreter()
    result = prolog.query_once("X = тест")
    if result:
        print(f"  SUCCESS: X = {result['X']}")
    else:
        print("  FAILED: No result")
except Exception as e:
    print(f"  ERROR: {e}")

# Test 5: ASCII atoms still work
print("\nTest 5: ASCII atoms still work (test)")
try:
    prolog = PrologInterpreter()
    result = prolog.query_once("X = test")
    if result:
        print(f"  SUCCESS: X = {result['X']}")
    else:
        print("  FAILED: No result")
except Exception as e:
    print(f"  ERROR: {e}")

print("\nDone!")
