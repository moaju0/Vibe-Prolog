#!/usr/bin/env python3
"""Quick test of Unicode atom support."""

from vibeprolog import PrologInterpreter

def run_query_once_test(test_name, query):
    print(f'\n{test_name}')
    try:
        prolog = PrologInterpreter()
        result = prolog.query_once(query)
        if result:
            print(f"  SUCCESS: X = {result['X']}")
        else:
            print("  FAILED: No result")
    except Exception as e:
        print(f"  ERROR: {e}")

def test_unicode_quick():
    run_query_once_test("Test 1: Simple Greek letter atom (δ)", "X = δ")
    run_query_once_test("Test 2: Unicode atom with underscore (δ_test)", "X = δ_test")

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

if __name__ == "__main__":
    test_unicode_quick()
