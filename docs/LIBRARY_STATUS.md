# Vibe-Prolog Library Loading Status

**Test Date:** 2025-12-04 20:03:41 UTC

## Summary

- **Total Files:** 63
- **Loaded Successfully:** 23
- **Failed to Load:** 40
- **Success Rate:** 23/63 (36%)

## Successfully Loaded Files ✅

- `library/$project_atts.pl`
- `library/atts.pl`
- `library/cont.pl`
- `library/dcgs.pl`
- `library/diag.pl`
- `library/error.pl`
- `library/gensym.pl`
- `library/http/http_open.pl`
- `library/iso_ext.pl`
- `library/lambda.pl`
- `library/lists.pl`
- `library/loader.pl`
- `library/pairs.pl`
- `library/queues.pl`
- `library/random.pl`
- `library/si.pl`
- `library/sockets.pl`
- `library/terms.pl`
- `library/test_module.pl`
- `library/tls.pl`
- `library/wasm.pl`

## Failed to Load Files ❌

### library/arithmetic.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/builtins.pl


**Details:**

```
PrologThrow: error(type_error(predicate_indicator, /(!, 0)), context(module/2))
```


### library/charsio.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/clpb.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/clpz.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/crypto.pl

**Details:**

```
File loading exceeded 30 seconds
```


### library/builtins.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/charsio.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/clpb.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```

**Details:**

```
PrologThrow: error(syntax_error(Unterminated block comment), context(consult/1))
```


### library/csv.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/debug.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/dif.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/ffi.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/files.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/format.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/freeze.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/http/http_server.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/numerics/quadtests.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/numerics/special_functions.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(type_error(atom, /(numerics, testutils)), context(use_module/1,2))
```


### library/numerics/testutils.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(No terminal matches 'δ' in the current parser context, at line 13 col 16

              ,δ_inverses_t/5
               ^
Expected one of: 
	* CONTROL_OP_FUNCTOR
	* PREFIX_FX_1200_45
	* PREFIX_FX_700_35
	* OPERATOR_ATOM
	* PREFIX_FY_200_3
	* STRING
	* VARIABLE
	* PREFIX_FY_900_36
	* OP_SYMBOL
	* COMPARISON_OP_FUNCTOR
	* ATOM
	* INFIX_YFX_500_15
	* PREFIX_FY_200_4
	* NUMBER
	* LPAR
	* BANG
	* PREFIX_FX_1200_44
	* INFIX_OP_FUNCTOR
	* SPECIAL_ATOM
	* LBRACE
	* PREFIX_FX_1150_41
	* LSQB
	* ARITH_OP_FUNCTOR
	* SPECIAL_ATOM_OPS
	* CHAR_CODE
), context(consult/1))
```


### library/ops_and_meta_predicates.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/ordsets.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(No terminal matches 't' in the current parser context, at line 22 col 39

        ;/* R2 = (=),   Item == X2 */ true
                                      ^
Expected one of: 
	* INFIX_XFY_600_18
	* INFIX_XFY_1100_39
	* INFIX_XFY_1000_37
	* INFIX_XFX_1200_41
	* INFIX_XFY_1050_38
	* INFIX_YFX_400_9
	* INFIX_YFX_400_5
	* INFIX_YFX_500_15
	* RPAR
	* LPAR
	* INFIX_YFX_400_7
	* INFIX_YFX_400_8
	* INFIX_YFX_400_12
	* INFIX_YFX_400_10
	* INFIX_YFX_500_17
	* INFIX_XFY_200_2
	* INFIX_YFX_500_16
	* INFIX_XFY_200_1
	* INFIX_YFX_400_6
	* INFIX_YFX_400_11
	* INFIX_YFX_400_13
	* INFIX_YFX_500_14
	* INFIX_XFX_1200_42
), context(consult/1))
```


### library/os.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/pio.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/process.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/reif.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/serialization/abnf.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/serialization/json.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(type_error(atom, /(tabling, global_worklist)), context(use_module/1,2))
```


### library/sgml.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/simplex.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/tabling.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(type_error(atom, /(tabling, double_linked_list)), context(use_module/1,2))
```


### library/tabling/batched_worklist.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(type_error(atom, /(tabling, global_worklist)), context(use_module/1,2))
```


### library/tabling/double_linked_list.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/tabling/global_worklist.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/tabling/table_data_structure.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(type_error(atom, /(tabling, table_link_manager)), context(use_module/1,2))
```


### library/tabling/table_link_manager.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/tabling/trie.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/tabling/wrapper.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/time.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/ugraphs.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(No terminal matches 't' in the current parser context, at line 22 col 39

        ;/* R2 = (=),   Item == X2 */ true
                                      ^
Expected one of: 
	* INFIX_XFY_600_18
	* INFIX_XFY_1100_39
	* INFIX_XFY_1000_37
	* INFIX_XFX_1200_41
	* INFIX_XFY_1050_38
	* INFIX_YFX_400_9
	* INFIX_YFX_400_5
	* INFIX_YFX_500_15
	* RPAR
	* LPAR
	* INFIX_YFX_400_7
	* INFIX_YFX_400_8
	* INFIX_YFX_400_12
	* INFIX_YFX_400_10
	* INFIX_YFX_500_17
	* INFIX_XFY_200_2
	* INFIX_YFX_500_16
	* INFIX_XFY_200_1
	* INFIX_YFX_400_6
	* INFIX_YFX_400_11
	* INFIX_YFX_400_13
	* INFIX_YFX_500_14
	* INFIX_XFX_1200_42
), context(consult/1))
```


### library/uuid.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(Unterminated block comment), context(consult/1))
```


### library/when.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


### library/xpath.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, |), context(use_module/1))
```


## Analysis by Error Type

### Issue Category 1: Operator/Predicate Redefinition Conflicts
**Root Cause:** Libraries try to use_module dependencies that define operators or predicates that are already built-in or defined elsewhere, and the interpreter rejects redefining them

**Affected Files (26 total):**
- library/arithmetic.pl - Fails when loading dependency chain (charsio → iso_ext → ...)
- library/charsio.pl - Fails when loading iso_ext which tries to use $project_atts
- library/csv.pl
- library/debug.pl
- library/dif.pl
- library/ffi.pl
- library/files.pl
- library/format.pl
- library/freeze.pl
- library/http/http_server.pl
- library/numerics/quadtests.pl
- library/os.pl
- library/pio.pl
- library/process.pl
- library/reif.pl
- library/serialization/abnf.pl
- library/serialization/json.pl
- library/sgml.pl
- library/simplex.pl
- library/tabling/double_linked_list.pl
- library/tabling/global_worklist.pl
- library/tabling/table_link_manager.pl
- library/tabling/trie.pl
- library/tabling/wrapper.pl
- library/time.pl
- library/when.pl
- library/xpath.pl

**Key Finding:** The error message reports `permission_error(modify, operator, |)` but the actual conflict appears to be with operators or predicates being redefined. This often happens when:
1. A library's use_module declarations try to redefine operators
2. A library depends on another that exports conflicting operators
3. The `--builtin-conflict` mode is set to `error` (default seems to be)

**Solution Path:** 
1. Examine each failing library's use_module calls to identify what operators/predicates are being re-exported
2. Implement `--builtin-conflict=skip` mode that silently ignores redefinitions of built-in operators
3. Or implement `--builtin-conflict=shadow` mode that allows module-scoped redefinitions
4. Document which standard library files cannot be loaded together due to conflicts

### Issue Category 2: Comment Parsing Issues
**Root Cause:** Unterminated block comments in source files

**Affected Files (2):**
- library/crypto.pl - `error(syntax_error(Unterminated block comment)`
- library/uuid.pl - `error(syntax_error(Unterminated block comment)`

**Solution Path:**
1. Check these files for malformed block comments (`/* ... */`)
2. Fix the comment syntax in the source files

### Issue Category 3: Parser Issues (Comments in Code)
**Root Cause:** Parser fails on `true` keyword after inline comments within expressions

**Affected Files (2):**
- library/ordsets.pl - Line 284: `;/* R2 = (=),   Item == X2 */ true`
- library/ugraphs.pl - Same pattern at line 22

**Pattern:** Comments followed by keywords like `true` in clause bodies are not parsed correctly. The parser expects an operator or term after the comment but gets the keyword.

**Root Cause Analysis:**
- In `library/ordsets.pl:284`, the code is: `(   R2 = (>) -> Item == X3 ; R2 = (<) -> Item == X1 ;/* R2 = (=),   Item == X2 */ true )`
- The parser sees `;/*comment*/` and after skipping the comment, expects a term but encounters `true` which the parser context doesn't recognize as a valid continuation
- This is a known Prolog pattern for inline documentation in disjunctions

**Solution Path:**
1. Update Lark grammar to skip comments in all contexts before tokenizing
2. Or pre-process Prolog source to remove inline comments before parsing
3. Test fix against both library/ordsets.pl and library/ugraphs.pl

### Issue Category 4: Unicode Character Issues
**Root Cause:** Parser doesn't support Unicode characters like `δ` in identifiers

**Affected Files (1):**
- library/numerics/testutils.pl - `No terminal matches 'δ' ... δ_inverses_t/5`

**Solution Path:**
1. Extend parser to support Unicode letters in identifiers
2. Or strip Unicode characters from predicate names in library files

### Issue Category 5: Module Not Found
**Root Cause:** `:- use_module(library(Name, Submodule))` with invalid module syntax

**Affected Files (5):**
- library/builtins.pl - `type_error(predicate_indicator, /(!, 0))` - Cut (!/0) in export list
- library/numerics/special_functions.pl - `type_error(atom, /(numerics, testutils))` - Invalid module reference
- library/tabling.pl - `type_error(atom, /(tabling, double_linked_list))` - Module path issue
- library/tabling/batched_worklist.pl - `type_error(atom, /(tabling, global_worklist))`
- library/tabling/table_data_structure.pl - `type_error(atom, /(tabling, table_link_manager))`

**Solution Path:**
1. library/builtins.pl: Remove `!` from export list (it's a built-in)
2. Other files: Verify module paths in use_module/2 calls are correctly formatted

### Issue Category 6: Performance Issues (Timeouts)
**Root Cause:** Library loading takes > 30 seconds, likely due to heavy computation or infinite loops

**Affected Files (3):**
- library/clpb.pl - 30+ second timeout
- library/clpz.pl - 30+ second timeout  
- library/ops_and_meta_predicates.pl - 30+ second timeout

**Solution Path:**
1. Investigate CLP libraries for expensive operations during load
2. Consider deferring constraint setup to query time
3. Or increase timeout threshold for these known slow libraries

## Priority Order for Fixes

**High Priority (affects many files):**
1. Issue Category 1: Pipe operator redefinition (25 files affected)
2. Issue Category 3: Comment parsing in expressions (2 files affected)

**Medium Priority (affects fewer files but critical functionality):**
1. Issue Category 4: Unicode support in identifiers (1 file)
2. Issue Category 5: Module reference issues (5 files)

**Low Priority (specialized/advanced libraries):**
1. Issue Category 6: Performance optimization (3 constraint files)
2. Issue Category 2: Comment syntax fixes (2 files)

## Next Steps

These categorized issues should be converted into GitHub issues with the details provided above.
