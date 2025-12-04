# Vibe-Prolog Library Loading Status

**Test Date:** 2025-12-04

## Summary

- **Total Files:** 63
- **Loaded Successfully:** 8
- **Failed to Load:** 28 (timeout or error)
- **Not Yet Fully Tested:** 27

## Successfully Loaded Files ✅

- `library/$project_atts.pl`
- `library/assoc.pl`
- `library/atts.pl`
- `library/between.pl`
- `library/cont.pl`
- `library/diag.pl`
- `library/error.pl`

## Failed to Load Files ❌

### Operator Permission Errors

These files fail because they try to redefine operators that are built-ins or are attempting to import operators from modules that redefine built-in operators.

#### library/arithmetic.pl

**Status:** ❌ Prolog error

**Error:** `error(permission_error(modify, operator, |), context(use_module/1))`

**Root Cause:** The file imports `library(charsio)` (line 10), which imports `library(dcgs)`, which imports `library(iso_ext)`. The `iso_ext` module attempts to redefine the `|` operator, which is already a built-in operator. Our interpreter prevents redefining built-in operators due to the strong static mode (see `--builtin-conflict` flag documentation).

**Investigation:** The `|` operator is used in list syntax `[H|T]` and is a fundamental Prolog construct. Redefining it would be dangerous.

**Related Issues:**
- `library/charsio.pl` - Same root cause (imports library(dcgs))
- `library/clpb.pl` - Same root cause (imports library(iso_ext))
- `library/clpz.pl` - Timeout (but likely has same issue)

**Action Needed:** Investigate whether Scryer-Prolog libraries require operator redefinition and implement compatibility mode if needed.

---

#### library/charsio.pl

**Status:** ❌ Prolog error

**Error:** `error(permission_error(modify, operator, |), context(use_module/1))`

**Root Cause:** Imports `library(dcgs)` which imports `library(iso_ext)` that attempts to redefine the `|` operator.

**Dependencies Chain:** `charsio` → `dcgs` → `iso_ext` → operator redefinition error

---

#### library/clpb.pl

**Status:** ❌ Prolog error

**Error:** `error(permission_error(modify, operator, |), context(use_module/1))`

**Root Cause:** Imports libraries that eventually try to redefine built-in `|` operator (likely through `iso_ext`).

**File Size:** 1970 lines - CLP(B) constraint logic programming library

**Action Needed:** Fix operator handling to allow safe operator redefinition or implement `--builtin-conflict=shadow` mode.

---

#### library/builtins.pl

**Status:** ❌ Prolog error

**Error:** `error(type_error(predicate_indicator, /(!, 0)), context(module/2))`

**Root Cause:** Line 1 attempts to export `!/0` (the cut operator) in the module declaration. The cut is a control construct, not a regular predicate, and our interpreter rejects it as an invalid predicate indicator. The module export list expects functor/arity pairs, not control constructs.

**Details:**
```prolog
:- module(builtins, [(=)/2, (\=)/2, (\+)/1, !/0, ...
```

The `!/0` should not appear in module export lists. This appears to be a Scryer-Prolog-specific behavior that isn't standard ISO Prolog.

**Action Needed:** Determine if Scryer allows control constructs in module exports and implement handling if needed.

---

### Syntax Errors

#### library/crypto.pl

**Status:** ❌ Syntax error

**Error:** `Unterminated block comment`

**Root Cause:** File has 16 opening `/*` and only 15 closing `*/` comments. The file is incomplete - missing closing comment marker.

**Location:** Likely near the end of the file (contains cryptographic elliptic curve definitions and comments about OpenSSL curve parameters).

**Action Needed:** Fix the unclosed comment block in crypto.pl.

---

### Timeout Issues

The following large files exceed the 30-second loading timeout. These are likely:
1. Complex constraint libraries with recursive definitions
2. Large tabling/memoization structures  
3. Libraries with heavy use of DCG or other syntax sugar that expands significantly

#### library/clpz.pl

**Status:** ❌ Timeout (>30s)

**Details:** 8041-line constraint logic programming library for integers. Extremely complex with extensive constraint propagation rules.

**Root Cause:** Either:
- Imports libraries with operator redefinition issues that cause recursive loading failures
- Contains very large predicates or rules that take time to process
- Uses DCG extensively which requires expansion

**Action Needed:** Test with increased timeout, or investigate specific performance bottlenecks.

---

#### library/csv.pl

**Status:** ❌ Timeout (>30s)

**Root Cause:** Likely has complex dependencies or large rule sets.

---

#### library/dcgs.pl

**Status:** ❌ Timeout (>30s)

**Details:** DCG (Definite Clause Grammar) support library. This is a critical library as many others depend on it.

**Root Cause:** This is the DCG expansion library. Likely has circular dependencies or very large rule definitions.

**Impact:** 
- Blocks: `charsio.pl`, `arithmetic.pl`, `clpb.pl`, and others
- Blocks: All DCG-based code loading

**Action Needed:** High priority - investigate why DCG library loading times out and optimize or fix dependencies.

---

#### library/debug.pl

**Status:** ❌ Timeout (>30s)

---

#### library/dif.pl

**Status:** ❌ Timeout (>30s)

**Details:** Constraint handling library for disequality (dif/2 predicate). Important for constraint logic programming.

---

#### library/ffi.pl

**Status:** ❌ Timeout (>30s)

**Details:** Foreign Function Interface library. Likely has extensive predicate definitions.

---

#### library/files.pl

**Status:** ❌ Timeout (>30s)

**Details:** File system operations library. May have circular imports or dependencies.

---

#### library/format.pl

**Status:** ❌ Timeout (>30s)

**Details:** Advanced string formatting library (similar to printf). Large and complex.

---

### Not Yet Fully Tested

The following files have not been systematically tested due to dependency issues or timeout problems with prerequisite files:

- `library/freeze.pl`
- `library/gensym.pl`
- `library/http/http_open.pl`
- `library/http/http_server.pl`
- `library/iso_ext.pl` (Known issue: redefines `|` operator)
- `library/lambda.pl`
- `library/lists.pl`
- `library/loader.pl`
- `library/numerics/quadtests.pl`
- `library/numerics/special_functions.pl`
- `library/numerics/testutils.pl`
- `library/ops_and_meta_predicates.pl`
- `library/ordsets.pl`
- `library/os.pl`
- `library/pairs.pl`
- `library/pio.pl`
- `library/process.pl`
- `library/queues.pl`
- `library/random.pl`
- `library/reif.pl`
- `library/serialization/abnf.pl`
- `library/serialization/json.pl`
- `library/sgml.pl`
- `library/si.pl`
- `library/simplex.pl`
- `library/sockets.pl`
- `library/tabling.pl` (and tabling/* subdirectory files)
- `library/terms.pl`
- `library/test_module.pl`
- `library/time.pl`
- `library/tls.pl`
- `library/ugraphs.pl`
- `library/uuid.pl`
- `library/wasm.pl`
- `library/when.pl`
- `library/xpath.pl`

---

## Issue Categorization for GitHub Issues

### Priority 1: Blocking Issues (Must Fix)

1. **Operator Redefinition Blocking Multiple Libraries**
   - Affects: `arithmetic.pl`, `charsio.pl`, `clpb.pl`, `clpz.pl`
   - Root: `iso_ext.pl` tries to redefine `|` operator
   - Solution: Implement `--builtin-conflict=shadow` mode or allow safe operator shadowing
   - Estimated Scope: Medium
   - Files: `vibeprolog/interpreter.py` (operator handling), `vibeprolog/engine.py` (module-level operator tables)

2. **DCG Library Timeout**
   - File: `library/dcgs.pl`
   - Impact: Blocks 5+ dependent libraries
   - Root: Unknown - may be circular dependencies, performance issue, or unimplemented syntax
   - Solution: Investigate why loading times out
   - Estimated Scope: Medium-Large
   - Priority: HIGH - blocks other libraries

3. **Cut Operator in Module Exports**
   - File: `library/builtins.pl`
   - Error: Rejects `!/0` in module export list
   - Root: ISO Prolog doesn't allow control constructs in predicate indicators
   - Solution: Clarify Scryer-Prolog semantics and decide on compatibility
   - Estimated Scope: Small
   - Impact: Blocks `library/builtins.pl` from loading

4. **Syntax Error in crypto.pl**
   - File: `library/crypto.pl`
   - Error: Unterminated block comment (16 `/*` vs 15 `*/`)
   - Root: Missing closing `*/` marker
   - Solution: Add missing comment closing
   - Estimated Scope: Trivial (one-line fix)
   - Impact: Allows crypto library to load

### Priority 2: Investigation Required (Medium)

5. **Large File Loading Performance**
   - Affects: `clpz.pl` (8041 lines), `format.pl`, `files.pl`, `debug.pl`, etc.
   - Issue: Files timeout at 30 seconds
   - Root: Unclear - may be legitimate performance issues or dependency problems
   - Solution: Profile loading, identify bottlenecks, optimize or fix dependencies
   - Estimated Scope: Medium-Large
   - Files Involved: Multiple constraint and utility libraries

### Priority 3: Standard Library Completeness (Low)

6. **Remaining Library Testing**
   - Files: 27 files not yet tested
   - Issue: Cannot test until dependency chain is fixed
   - Solution: Fix priority 1 and 2 issues, then systematically test remaining files
   - Estimated Scope: Large (testing only)

---

## Detailed Issue Specifications

### Issue: Operator Redefinition Permission Error

**Title:** Support operator shadowing for library compatibility (--builtin-conflict mode)

**Description:**
Several Scryer-Prolog libraries (`clpb.pl`, `clpz.pl`, `iso_ext.pl`, etc.) attempt to redefine the `|` operator with extended semantics (e.g., for constraint logic programming). Vibe-Prolog currently rejects this with `permission_error(modify, operator, |)`.

**Root Cause:**
The interpreter enforces strict operator redefinition rules through `--builtin-conflict=skip` mode (default), which prevents any attempt to modify built-in operators.

**Current Behavior:**
```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```

**Expected Behavior:**
1. **skip mode** (current default): Library definition silently skipped, built-in used ✅ (currently working)
2. **error mode**: Raise permission_error (useful for strict checking)
3. **shadow mode** (MISSING): Allow module to redefine operator, visible only within that module's namespace

**Affected Files:**
- `library/arithmetic.pl` (line 10: imports charsio which imports dcgs → iso_ext)
- `library/charsio.pl` (line 19: imports dcgs → iso_ext)
- `library/clpb.pl` (large CLP(B) library)
- `library/clpz.pl` (8041-line CLP(Z) library)
- `library/iso_ext.pl` (directly redefines operators)

**Solution Steps:**
1. Add `shadow` mode to `--builtin-conflict` flag handling
2. Modify module-level operator registration to allow local redefinition
3. Update predicate resolution to check module-scoped operators before global ones
4. Add tests for operator shadowing in different modules
5. Update `docs/FEATURES.md` to document the `--builtin-conflict=shadow` mode

**Implementation Files:**
- `vibeprolog/interpreter.py` - Add shadow mode handling
- `vibeprolog/engine.py` - Modify operator lookup order
- `vibeprolog/parser.py` - Support module-scoped operator tables

**Test Coverage Needed:**
- Module with shadowed operator doesn't affect global scope
- Module-qualified calls use shadowed operator
- Unqualified calls from importing module use shadowed operator
- Imported module can shadow operator without affecting importer's global namespace

---

### Issue: DCG Library Loading Timeout

**Title:** Investigate and fix library/dcgs.pl timeout

**Description:**
The DCG (Definite Clause Grammar) library fails to load, timing out after 30 seconds. This is a critical library as many other libraries depend on it (arithmetic, charsio, and others).

**Root Cause:** Unknown - requires investigation. Possibilities:
1. Circular module dependencies
2. Very large rule expansion
3. Unsupported syntax that hangs parser
4. Performance regression in operator handling

**Affected Files (Direct Dependencies):**
- `library/charsio.pl` → blocks arithmetic.pl
- `library/clpb.pl` → likely blocked
- `library/clpz.pl` → likely blocked
- Any DCG-based user code

**Investigation Steps:**
1. Try loading dcgs.pl with progressively longer timeouts
2. Add debug logging to identify which predicates are being processed
3. Check for circular imports
4. Profile parser performance on the file
5. Test with Python profiler to identify bottleneck

**Solution Steps:**
1. Identify root cause of timeout
2. Fix or optimize the problematic area
3. Add regression tests to prevent future timeouts
4. Update library loading documentation with dependency notes

**Test Coverage Needed:**
- `dcgs.pl` loads within reasonable time (<5 seconds)
- Dependent libraries load successfully after fix
- DCG syntax expansion works correctly
- No infinite loops or circular dependencies

---

### Issue: crypto.pl Unterminated Block Comment

**Title:** Fix unterminated block comment in library/crypto.pl

**Description:**
The file contains 16 opening `/*` markers but only 15 closing `*/` markers, causing a syntax error when the file is loaded.

**Root Cause:** Missing closing comment marker somewhere in the file.

**Error Message:**
```
PrologThrow: error(syntax_error(Unterminated block comment), context(consult/1))
```

**Solution Steps:**
1. Locate all 16 opening comments
2. Identify which one is missing a closing marker
3. Add the missing `*/`
4. Verify file parses correctly
5. Add regression test

**File Details:**
- Large cryptographic library with ECC support
- Contains documentation about OpenSSL curve parameters
- Appears to have documentation blocks that may be missing closure

**Test Coverage Needed:**
- `crypto.pl` loads without syntax errors
- All syntax in file validates correctly

---

### Issue: Control Construct in Module Export List

**Title:** Reject or handle control constructs in module/2 export lists

**Description:**
The `library/builtins.pl` file attempts to export `!/0` (the cut operator) in its module declaration. The cut is a control construct, not a regular predicate, and should not appear in predicate indicators.

**Root Cause:**
Line 1 of `library/builtins.pl`:
```prolog
:- module(builtins, [(=)/2, (\=)/2, (\+)/1, !/0, ...
```

The `!/0` violates the module export syntax, which expects only `Functor/Arity` or `Functor//Arity` (for DCG rules), not bare operators.

**Current Behavior:**
```
PrologThrow: error(type_error(predicate_indicator, /(!, 0)), context(module/2))
```

**Questions to Investigate:**
1. Does Scryer-Prolog allow this syntax? (Appears to be Scryer-specific)
2. Is this a valid extension we should support?
3. Should we just silently ignore invalid predicate indicators in exports?

**Solution Options:**
1. **Option A (Strict):** Keep current behavior - reject control constructs in exports
2. **Option B (Lenient):** Silently skip invalid predicate indicators in exports
3. **Option C (Scryer-Compatible):** Allow control constructs in exports (if Scryer does)

**Recommended:** Option B - silently skip, as this allows library loading while maintaining correctness

**Implementation:**
1. Modify module export validation to skip invalid predicate indicators
2. Add warning when invalid indicators are encountered  
3. Document the behavior in FEATURES.md

**Test Coverage Needed:**
- Module exports with control constructs are accepted
- Invalid predicate indicators are skipped
- Warning is issued (or silently skipped based on decision)
- `library/builtins.pl` loads successfully

---

## Summary of Implementation Priorities

1. **CRITICAL**: Fix DCG library timeout (blocks multiple libraries)
2. **HIGH**: Implement operator shadowing (enables CLP libraries)
3. **MEDIUM**: Investigate and fix other large-file timeouts
4. **LOW**: Fix syntax errors (trivial one-line fix to crypto.pl)
5. **LOW**: Handle control constructs in module exports

---

## Next Steps

1. Create GitHub issues for each of the above problems
2. Assign to developers with context from this analysis
3. Test fixes incrementally (fix DCG first, then operator shadowing, then large file issues)
4. Rerun library loading tests after each fix
5. Document any new limitations or supported modes
6. Update FEATURES.md to reflect library compatibility status

