# Dynamic Operator Implementation Status

## Summary

Partial implementation of dynamic operator support has been completed. The operator table and directive handling are in place, but **parser integration for custom operator syntax is NOT yet implemented**. Currently, custom operators must be used in canonical functor notation.

## What's Been Implemented ✅

1. **Operator Table (`vibeprolog/operators.py`)**
   - Full `OperatorTable` class with validation
   - Support for all ISO operator types (fx, fy, xfx, xfy, yfx, xf, yf)
   - Proper error handling with ISO-compliant error terms
   - Protection of critical operators (`,`, `;`, `->`, `:-`, `|`, `{}`)

2. **op/3 Directive Handling**
   - Parser recognizes `:- op(Prec, Type, Name).` syntax
   - Handles single operators and lists of operators
   - Validates precedence (0-1200)
   - Validates associativity type
   - Validates operator names
   - Operator removal via `op(0, _, Name)`
   - Operator redefinition

3. **current_op/3 Predicate**
   - Query all defined operators
   - Pattern matching on precedence, type, and name
   - Returns both built-in and custom operators

4. **write_term Options**
   - `ignore_ops(true)` - outputs canonical form
   - Respects custom operators in output formatting

5. **Test Coverage**
   - 39 tests passing out of 44
   - Comprehensive error handling tests
   - Integration tests

## What's NOT Implemented ❌

### 1. **Parser Integration for Custom Operator Syntax** (BLOCKING)

Custom operators are **defined and stored** but **not recognized during parsing**. This means:

```prolog
% This works:
:- op(500, xfx, loves).
fact(loves(alice, bob)).

% This does NOT work (parser error):
:- op(500, xfx, loves).
fact(alice loves bob).
```

**Why**: The Lark parser has a fixed grammar and cannot be dynamically modified during source file parsing.

### 2. **Two-Pass Parsing NOT Implemented**

To support operator syntax parsing, we would need to:
1. First pass: Extract op/3 directives using simple scanning
2. Rebuild Lark parser with new grammar
3. Second pass: Parse full program with updated grammar

This has not been attempted yet.

## Test Failure Analysis

### 5 Failing Tests

#### 1. `test_infix_associativity_types` ❌
**Issue**: `yfy` operator type is not recognized
**Root Cause**: `operators.py` line 121 is missing `"yfy"` from `valid_specs`
**Fix**: Add `"yfy"` to the set
**Priority**: HIGH (quick fix)

```python
valid_specs = {"xfx", "xfy", "yfx", "yfy", "fx", "fy", "xf", "yf"}  # Add yfy
```

#### 2. `test_write_term_respects_ignore_ops` ❌
**Issue**: Output is `custom(a,b)` but test expects `custom(a, b)` (with space)
**Root Cause**: Minor formatting difference (no spaces after commas)
**Fix**: Update test assertion to be more flexible or fix formatter
**Priority**: LOW (cosmetic)

**Option 1** (quick fix - update test):
```python
assert 'custom(a' in output and 'b)' in output
```

**Option 2** (better - fix formatter):
Find the write_term implementation and add spaces after commas

#### 3. `test_unbound_operator_raises_instantiation_error` ❌
**Issue**: Test expects `op(500, xfx, '_')` to raise instantiation_error, but it doesn't
**Root Cause**: The anonymous variable `_` is being treated as a valid atom
**Issue**: According to ISO, `_` is the "anonymous variable" and using it as operator name should fail
**Status**: DEBATE - Is this a real requirement?

According to ISO Prolog, `_` is anonymous and shouldn't be used as operator name.

**Fix**: In `operators.py` `_parse_operator_names`, add check:
```python
if element.name == '_':
    error_term = PrologError.instantiation_error(context)
    raise PrologThrow(error_term)
```

**Priority**: LOW (edge case, may conflict with meta-programming)

#### 4. `test_custom_and_builtin_operators_coexist` ❌
**Issue**: Parser rejects `a custom b` syntax
**Root Cause**: This requires dynamic parser - not implemented yet
**Status**: Expected - this is a LIMITATION of the current implementation
**Priority**: BLOCKING (for full operator support)

#### 5. `test_all_iso_operator_types` ❌
**Issue**: Same as #1 - `yfy` not recognized
**Root Cause**: Same as #1
**Fix**: Same as #1
**Priority**: HIGH (depends on #1)

### 1 Unexpectedly Passing Test (XPASS)

#### `test_operator_precedence_affects_grouping` ✅ (marked as xfail)
**Status**: This is marked `@pytest.mark.xfail` but is passing
**Reason**: The test doesn't actually test operator parsing - it just verifies that the op/3 directives succeed
**Action**: Update test to mark as xpass or move to a different category

## Architecture Gaps

### Gap 1: No Dynamic Grammar Generation
The `PrologParser` class doesn't support:
- Rebuilding Lark grammar with custom operators
- Two-pass parsing strategy

### Gap 2: No Operator Precedence Integration
Even if we could generate custom grammar, we'd need to:
- Map ISO precedence (1-1200) to Lark precedence
- Handle associativity (fx, fy, xfx, xfy, yfx, yfy, xf, yf)
- Properly integrate with existing operator precedence

### Gap 3: Parser Limitations
Lark LALR parser has constraints:
- May have shift/reduce conflicts with some operator combinations
- Dynamic grammar changes not well-supported
- Would need significant refactoring

## Recommendation

### Short Term (Fixes for failing tests)
1. **Fix `yfy` validation** (5 minutes)
   - Add `"yfy"` to `valid_specs` in `operators.py` line 121
   - Fixes tests #1 and #5

2. **Fix whitespace in write_term** (2 minutes)
   - Update test to be more flexible OR
   - Fix formatter to add spaces after commas

3. **Address anonymous variable issue** (10 minutes)
   - Decide if `_` should be allowed as operator name
   - Add validation if needed

4. **Mark XPASS test appropriately** (2 minutes)
   - Change `@pytest.mark.xfail` to `@pytest.mark.skip` or remove marker

### Medium Term (Better Documentation)
1. Update `FEATURES.md` to clarify:
   - ✅ op/3 directive works (tables, validation)
   - ❌ Custom operator syntax NOT parsed
   - ✅ Works in canonical form only

2. Add clear comments in test file explaining limitations

### Long Term (Full Implementation)
To implement custom operator parsing:
1. Implement two-pass parsing strategy
2. Generate dynamic Lark grammar with custom operators
3. Map ISO precedence/associativity to Lark
4. Handle parser conflicts
5. Full test coverage

**Estimated effort**: 40-80 hours

## Files Affected

- `vibeprolog/operators.py` - Main operator table (mostly done)
- `vibeprolog/parser.py` - Needs dynamic grammar support
- `vibeprolog/interpreter.py` - Needs two-pass parsing
- `tests/test_dynamic_operators.py` - Test fixes needed
- `FEATURES.md` - Documentation update needed
- `ARCHITECTURE.md` - Documentation update needed

## Test Results Summary

```
Total: 44 tests
Passed: 39 ✅
Failed: 5 ❌
XFail (expected): 3
XPass (unexpected): 1 ⚠️

Success Rate: 39/44 = 88.6%
```

## Next Steps

1. Apply quick fixes (yfy, whitespace, anonymous variable)
2. Run full test suite to ensure no regressions
3. Update FEATURES.md with current limitations
4. Create separate issue for full operator parsing implementation
