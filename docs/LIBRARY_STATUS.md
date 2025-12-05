# Vibe-Prolog Library Loading Status

**Test Date:** 2025-12-05
**Last Investigation:** 2025-12-05

## Executive Summary

- **Total Files:** 63
- **Confirmed Working:** ~29
- **Known Issues:** 34 (categorized below)

The library loading failures fall into **four distinct root causes**:

1. **Parser Performance (Earley Parser Hangs)** - The most common issue
2. **Circular Dependencies via library(lists)** - Many libraries depend on lists.pl which hangs
3. **Syntax Errors** - Unicode characters, inline comments after `;`
4. **Missing Features** - Nested module paths, DCG variable goals

---

## Root Cause Analysis

### Issue #1: Earley Parser Performance Regression

**Affected Files:** library/lists.pl (and ALL libraries that depend on it)

**Symptom:** Parser hangs indefinitely when parsing certain Prolog files

**Root Cause:** The Lark Earley parser is experiencing exponential time complexity on certain grammar patterns in the Prolog source. This was confirmed by testing incremental parsing of library/lists.pl:
- Lines 0-110: Parses successfully (23 terms)
- Lines 0-111+: Parser hangs

**Line 111 of lists.pl:**
```prolog
length_addendum([], N, N).
```

This appears to trigger parser ambiguity issues, possibly related to:
- The comma operator precedence
- Variable-only clause bodies
- Interaction with meta_predicate directives

**Impact:** This is the **critical blocker** because lists.pl is imported by:
- library/iso_ext.pl → library/dcgs.pl → library/freeze.pl → many others
- library/error.pl
- library/format.pl
- library/clpz.pl
- Most other Scryer-Prolog libraries

**Cascade Effect:**
```
library/lists.pl [HANGS]
  └── library/iso_ext.pl [HANGS - imports lists]
        └── library/dcgs.pl [HANGS - imports iso_ext]
              └── library/freeze.pl [HANGS - imports dcgs]
                    └── library/pio.pl [HANGS - imports freeze]
                          └── library/format.pl [HANGS - imports pio]
                                └── library/debug.pl [HANGS - imports format]
                                      └── library/clpz.pl [HANGS - imports debug]
```

**Recommended Fix:** 
- Profile the Lark Earley parser to identify the ambiguity source
- Consider switching to LALR parsing mode for better performance
- May need grammar refactoring to reduce ambiguity

---

### Issue #2: Syntax Error - Inline Comments After Semicolon

**Affected Files:** library/ordsets.pl, library/ugraphs.pl

**Symptom:** 
```
error(syntax_error(No terminal matches 't' in the current parser context, at line 22 col 39
        ;/* R2 = (=),   Item == X2 */ true
```

**Root Cause:** The parser doesn't correctly handle `/* ... */` block comments immediately following a `;` (disjunction operator).

**Code Pattern:**
```prolog
;/* R2 = (=) */ true    % Parser fails here
```

**Recommended Fix:** Update parser to allow block comments anywhere whitespace is permitted, including after operators.

---

### Issue #3: Syntax Error - Unicode Characters in Identifiers

**Affected Files:** library/numerics/testutils.pl

**Symptom:**
```
error(syntax_error(No terminal matches 'δ' in the current parser context
              ,δ_inverses_t/5
```

**Root Cause:** The lexer doesn't recognize Unicode Greek letters (δ, π, etc.) as valid atom characters.

**Code Pattern:**
```prolog
:- module(testutils, [
    δ_inverses_t/5  % Greek delta - fails to parse
]).
```

**Recommended Fix:** Extend ATOM terminal regex to include Unicode letter categories (\p{L} or explicit Greek ranges).

---

### Issue #4: Missing Feature - Nested Module Paths

**Affected Files:** 
- library/numerics/special_functions.pl
- library/tabling.pl
- library/tabling/batched_worklist.pl
- library/tabling/table_data_structure.pl
- library/tabling/table_link_manager.pl

**Symptom:**
```
error(type_error(atom, /(tabling, double_linked_list)), context(use_module/1,2))
error(type_error(atom, /(numerics, testutils)), context(use_module/1,2))
```

**Root Cause:** The `use_module/1` implementation doesn't support nested library paths like `library(tabling/double_linked_list)` or `library(numerics/testutils)`.

**Code Pattern:**
```prolog
:- use_module(library(tabling/double_linked_list)).  % Fails
```

**Recommended Fix:** Extend module path resolution to handle `/` as path separator in library terms.

---

### Issue #5: Missing Feature - DCG Variable Goals

**Affected Files:** library/serialization/json.pl

**Symptom:**
```
ValueError: Unsupported DCG goal: NumberChars
```

**Root Cause:** The DCG expander doesn't handle variable goals in DCG bodies.

**Code Pattern (lines 207-208):**
```prolog
;   { number_chars(Number, NumberChars) },
    NumberChars   % Variable used as DCG goal - fails
```

**Recommended Fix:** Add support for DCG bodies where a variable is used directly as a terminal sequence goal.

---

### Issue #6: Permission Error - goal_expansion/2

**Affected Files:** library/clpz.pl

**Symptom:**
```
error(permission_error(modify, static_procedure, /(goal_expansion, 2)))
```

**Root Cause:** clpz.pl attempts to define clauses for `goal_expansion/2`, but this is treated as a static built-in predicate.

**Recommended Fix:** Either:
- Make goal_expansion/2 dynamic/multifile by default
- Or implement proper term expansion hooks

---

## Files By Status

### ✅ Successfully Loading (29 files)

These files load without errors:

| File | Notes |
|------|-------|
| library/$project_atts.pl | Residual goal projection |
| library/assoc.pl | Association lists |
| library/atts.pl | Attributed variables |
| library/between.pl | Integer generation |
| library/cont.pl | Delimited continuations |
| library/diag.pl | Diagnostics |
| library/dif.pl | Disequality constraint |
| library/error.pl | Error handling |
| library/gensym.pl | Symbol generation |
| library/http/http_open.pl | HTTP client |
| library/lambda.pl | Lambda expressions |
| library/loader.pl | Module loader |
| library/pairs.pl | Key-value pairs |
| library/queues.pl | Queue operations |
| library/random.pl | Random numbers |
| library/reif.pl | Reified predicates |
| library/si.pl | Safety infrastructure |
| library/sockets.pl | Network sockets |
| library/tabling/double_linked_list.pl | Tabling internals |
| library/tabling/global_worklist.pl | Tabling internals |
| library/tabling/wrapper.pl | Tabling internals |
| library/terms.pl | Term utilities |
| library/test_module.pl | Test module |
| library/tls.pl | TLS support |
| library/wasm.pl | WebAssembly |

### ❌ Parser Hang - Depends on library/lists.pl (26 files)

These files hang because they directly or transitively import library/lists.pl:

| File | Import Chain |
|------|--------------|
| library/lists.pl | **ROOT CAUSE** - Parser hangs at line 111 |
| library/iso_ext.pl | imports lists |
| library/dcgs.pl | imports iso_ext |
| library/freeze.pl | imports dcgs |
| library/pio.pl | imports freeze, gensym |
| library/format.pl | imports pio, charsio |
| library/debug.pl | imports format |
| library/charsio.pl | imports dcgs, iso_ext, lists |
| library/clpz.pl | imports debug, format + goal_expansion issue |
| library/clpb.pl | imports lists, format |
| library/crypto.pl | imports clpz |
| library/arithmetic.pl | imports lists |
| library/builtins.pl | imports lists, format |
| library/csv.pl | imports dcgs |
| library/ffi.pl | imports dcgs, lists |
| library/files.pl | imports dcgs, lists |
| library/ops_and_meta_predicates.pl | imports lists |
| library/os.pl | imports lists |
| library/process.pl | imports dcgs |
| library/sgml.pl | imports dcgs |
| library/simplex.pl | imports lists |
| library/time.pl | imports lists |
| library/uuid.pl | imports lists |
| library/when.pl | imports dcgs |
| library/xpath.pl | imports lists, dcgs |
| library/serialization/abnf.pl | imports dcgs |

### ❌ Syntax Errors (2 files)

| File | Issue |
|------|-------|
| library/ordsets.pl | Inline comments after `;` |
| library/ugraphs.pl | Inline comments after `;` (imports ordsets) |

### ❌ Unicode Support (1 file)

| File | Issue |
|------|-------|
| library/numerics/testutils.pl | Greek letter δ in identifiers |

### ❌ Nested Module Paths (5 files)

| File | Issue |
|------|-------|
| library/numerics/special_functions.pl | `library(numerics/testutils)` |
| library/tabling.pl | `library(tabling/double_linked_list)` |
| library/tabling/batched_worklist.pl | `library(tabling/global_worklist)` |
| library/tabling/table_data_structure.pl | `library(tabling/table_link_manager)` |
| library/tabling/table_link_manager.pl | `library(tabling/trie)` |

### ❌ DCG Variable Goals (1 file)

| File | Issue |
|------|-------|
| library/serialization/json.pl | Variable `NumberChars` used as DCG goal |

### ❌ Timeout - Unknown (2 files)

| File | Issue |
|------|-------|
| library/numerics/quadtests.pl | Needs investigation |
| library/tabling/trie.pl | Needs investigation |

### ❌ HTTP Server (1 file)

| File | Issue |
|------|-------|
| library/http/http_server.pl | Timeout - likely dependency issue |

---

## Priority for Fixes

### High Priority (Unblocks Many Libraries)

1. **Fix Earley parser performance on library/lists.pl**
   - This single fix would unblock 26+ libraries
   - Critical for Scryer-Prolog compatibility
   
2. **Support nested library paths in use_module/1**
   - Quick fix to module resolution
   - Unblocks 5 files

### Medium Priority

3. **Fix inline comments after `;` operator**
   - Parser grammar fix
   - Unblocks ordsets.pl, ugraphs.pl

4. **Support Unicode in atom identifiers**
   - Lexer extension
   - Unblocks numerics library

### Lower Priority

5. **DCG variable goal support**
   - DCG expander enhancement
   - Affects json.pl

6. **goal_expansion/2 handling**
   - Meta-predicate system
   - Affects clpz.pl

---

## Recommended GitHub Issues

Based on this analysis, the following issues should be created:

1. **Parser: Earley parser hangs on library/lists.pl** (Critical)
   - Labels: bug, parser, performance
   - Blocks: 26+ library files
   
2. **Module: Support nested library paths (library(a/b))** 
   - Labels: enhancement, module-system
   - Blocks: 5 files

3. **Parser: Support block comments after semicolon operator**
   - Labels: bug, parser
   - Blocks: ordsets.pl, ugraphs.pl

4. **Parser: Support Unicode letters in atom identifiers**
   - Labels: enhancement, parser, iso-compliance
   - Blocks: numerics library

5. **DCG: Support variable goals in DCG bodies**
   - Labels: enhancement, dcg
   - Blocks: json.pl

6. **Meta: Implement goal_expansion/2 hook**
   - Labels: enhancement, meta-predicates
   - Affects: clpz.pl optimization
