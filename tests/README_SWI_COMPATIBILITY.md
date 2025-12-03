# SWI-Prolog Compatibility Tests

This directory contains comprehensive compatibility tests comparing Vibe-Prolog behavior against SWI-Prolog 9.0.4.

## Quick Reference

**File:** `test_swi_compatibility.py`  
**Total Tests:** 89  
**Pass Rate:** 100%

## Test Categories

| Category | Tests |
|----------|-------|
| Control Flow | 10 |
| Arithmetic | 15 |
| List Operations | 13 |
| Term Manipulation | 10 |
| Type Testing | 13 |
| Comparison | 5 |
| Atom Processing | 7 |
| Meta-Predicates | 5 |
| Higher-Order | 3 |
| Prolog Semantics | 9 |

## Running Tests

### Run all compatibility tests
```bash
uv run pytest tests/test_swi_compatibility.py -v
```

### Run a specific test category
```bash
# Run arithmetic tests only
uv run pytest tests/test_swi_compatibility.py::TestSWIArithmetic -v

# Run list operation tests
uv run pytest tests/test_swi_compatibility.py::TestSWIListOperations -v
```

### Run a single test
```bash
uv run pytest tests/test_swi_compatibility.py::TestSWIArithmetic::test_operator_precedence -v
```

### Run with detailed output
```bash
uv run pytest tests/test_swi_compatibility.py -vv -s
```

## Test Examples

### Example 1: Arithmetic
```python
def test_operator_precedence(self):
    """
    SWI-Prolog: ?- X is 2 + 3 * 4.
    Expected: X = 14 (multiplication before addition)
    """
    prolog = PrologInterpreter()
    result = prolog.query_once("X is 2 + 3 * 4")
    assert result is not None
    assert result['X'] == 14
```

### Example 2: List Operations
```python
def test_append_decompose(self):
    """
    SWI-Prolog: ?- append(X, Y, [1,2,3]).
    Expected: Multiple solutions (X/Y pairs)
    """
    prolog = PrologInterpreter()
    results = list(prolog.query("append(X, Y, [1,2,3])"))
    assert len(results) == 4
    assert results[0]['X'] == []
    assert results[0]['Y'] == [1, 2, 3]
```

### Example 3: Control Flow
```python
def test_negation_as_failure_true(self):
    """
    SWI-Prolog: ?- \\+ member(5, [1,2,3]).
    Expected: true (5 is not in list)
    """
    prolog = PrologInterpreter()
    result = prolog.query_once(r"\+ member(5, [1,2,3])")
    assert result is not None
```

## Test Documentation

Each test includes:
1. **Docstring** - Explains what is being tested
2. **SWI-Prolog example** - Shows the query that was tested in SWI-Prolog
3. **Expected output** - Describes what result should be
4. **Assertions** - Validates the actual result

Example docstring format:
```python
"""
SWI-Prolog: ?- member(X, [1,2,3]).
Expected: X = 1 ; X = 2 ; X = 3 (all solutions via backtracking)
"""
```

## Verification Methodology

All test queries were:
1. **Verified in SWI-Prolog 9.0.4** - Run interactively to confirm expected output
2. **Converted to Python tests** - Using `PrologInterpreter` API
3. **Tested against Vibe-Prolog** - Confirmed behavior matches
4. **Documented thoroughly** - Clear docstrings for maintenance

## Integration

These tests are part of the standard test suite:
```bash
# Run all tests (including SWI compatibility tests)
uv run pytest tests/ -v
```

## Coverage

### ✅ Fully Covered Features
- All basic control flow (conjunction, disjunction, cut, negation)
- Complete arithmetic (operations, comparisons, precedence)
- Standard list operations (append, member, reverse, sort, etc.)
- Term manipulation (functor, arg, univ, copy_term)
- Type testing (var, atom, number, compound, etc.)
- Term comparison (==, \==, @<, @>, compare)
- Atom/string processing
- Meta-predicates (findall, bagof, setof)
- Dynamic predicates (assert, retract)

### No Gaps Found
All tested features behave identically to SWI-Prolog.

## Troubleshooting

### Test fails with assertion error
Check the test docstring for the expected behavior in SWI-Prolog, then compare with Vibe-Prolog output using:
```bash
uv run pytest tests/test_swi_compatibility.py::FailingTest -vv
```

### Understanding the test output
Look for:
- ✅ PASSED - Test passed, behavior matches SWI-Prolog
- ❌ FAILED - Behavior differs, needs investigation
- ⊘ SKIPPED - Test marked to skip with reason

## Maintenance

When adding new features:
1. Create corresponding test in this file
2. Verify behavior in SWI-Prolog 9.0.4
3. Document with clear docstring
4. Run `pytest tests/test_swi_compatibility.py` to verify

## Related Files

- `COMPATIBILITY_TEST_REPORT.md` - Detailed test results and analysis
- `test_arithmetic.py` - Additional arithmetic tests
- `test_builtins.py` - Built-in predicate tests
- `test_iso_core.py` - ISO Prolog standard tests

## References

- [SWI-Prolog Manual](https://www.swi-prolog.org/pldoc/man?section=builtin)
- [ISO/IEC 13211-1:1995](https://en.wikipedia.org/wiki/Prolog#ISO_Prolog)
- `docs/FEATURES.md` - Implementation status matrix
- `AGENTS.md` - Development guidelines
