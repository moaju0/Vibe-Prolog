# Vibe-Prolog Library Loading Status

**Test Date:** 2025-12-06 18:10:56 UTC

## Summary

- **Total Files:** 63
- **Loaded Successfully:** 30
- **Failed to Load:** 33
- **Success Rate:** 30/63 (48%)

## Failure Type Breakdown

| Failure Type | Count | Representative Files | Notes |
| --- | --- | --- | --- |
| Timeout (30s limit hit) | 29 | `library/arithmetic.pl`, `library/builtins.pl`, `library/tabling.pl` | Likely waiting on unsupported predicates or cyclic `use_module/1` chains; affects most I/O, numeric, and tabling libraries. |
| Prolog error (syntax) | 4 | `library/numerics/special_functions.pl`, `library/ordsets.pl` | Parser currently rejects Unicode atoms such as `δ_*` and specific layouts where block comments touch code, blocking numerics and data-structure libs. |

## Priority Recommendations

1. **Unblock Unicode-heavy numerics modules.** Extend the tokenizer/parser so atoms like `δ_inverses_t/5` and `δ_successors_t/5` are accepted (ideally by treating all valid UTF-8 letters as atom constituents). This immediately enables both `library/numerics/special_functions.pl` and `library/numerics/testutils.pl` and removes an entire failure category.
2. **Fix comment-adjacent syntax handling.** Files such as `library/ordsets.pl` and `library/ugraphs.pl` place `true` or operators directly after `/* ... */` comments; today that raises `No terminal matches 't'`. Adjust the lexer to automatically insert whitespace after closing block comments (or forbid comment/term concatenation during tokenization) so these widely used data structure libraries load.
3. **Instrument 30s timeouts on foundational libs first.** Start with `library/builtins.pl`, `library/arithmetic.pl`, and `library/tabling.pl` because their failure cascades to many others. Add tracing around `consult/1` (e.g., predicate-level logging or a max-depth watchdog) to spot the missing predicate or infinite recursion that causes the stall before tackling lower-priority timeout files.
4. **Implement the `NumberChars/4` DCG helper.** `library/serialization/json.pl` only fails because `NumberChars` is unimplemented. Adding this DCG primitive (or a compatibility wrapper) restores JSON parsing and is self-contained compared with the systemic timeout issues.

## Successfully Loaded Files ✅

- `library/$project_atts.pl`
- `library/assoc.pl`
- `library/atts.pl`
- `library/between.pl`
- `library/cont.pl`
- `library/dcgs.pl`
- `library/diag.pl`
- `library/dif.pl`
- `library/error.pl`
- `library/freeze.pl`
- `library/gensym.pl`
- `library/http/http_open.pl`
- `library/iso_ext.pl`
- `library/lambda.pl`
- `library/lists.pl`
- `library/loader.pl`
- `library/pairs.pl`
- `library/queues.pl`
- `library/random.pl`
- `library/reif.pl`
- `library/serialization/json.pl`
- `library/si.pl`
- `library/sockets.pl`
- `library/tabling/double_linked_list.pl`
- `library/tabling/global_worklist.pl`
- `library/tabling/wrapper.pl`
- `library/terms.pl`
- `library/test_module.pl`
- `library/tls.pl`
- `library/wasm.pl`

## Failed to Load Files ❌

### library/arithmetic.pl

**Status:** ❌ Timeout

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


### library/clpz.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/crypto.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/csv.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/debug.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/ffi.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/files.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/format.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/http/http_server.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/numerics/quadtests.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/numerics/special_functions.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(No terminal matches 'δ' in the current parser context, at line 11 col 16

              ,δ_inverses_t/5
               ^
Expected one of: 
	* SPECIAL_ATOM
	* OP_SYMBOL
	* PREFIX_FY_900_45
	* STRING
	* CHAR_CODE
	* NUMBER
	* VARIABLE
	* PREFIX_FX_700_44
	* ARITH_OP_FUNCTOR
	* INFIX_OP_FUNCTOR
	* LBRACE
	* INFIX_YFX_500_16
	* PREFIX_FY_200_4
	* PREFIX_FX_1150_50
	* LPAR
	* SPECIAL_ATOM_OPS
	* PREFIX_FX_1200_53
	* CONTROL_OP_FUNCTOR
	* OPERATOR_ATOM
	* PREFIX_FX_1200_54
	* ATOM
	* BANG
	* LSQB
	* PREFIX_FY_200_3
	* COMPARISON_OP_FUNCTOR
), context(consult/1))
```


### library/numerics/testutils.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(No terminal matches 'δ' in the current parser context, at line 11 col 16

              ,δ_inverses_t/5
               ^
Expected one of: 
	* SPECIAL_ATOM
	* OP_SYMBOL
	* PREFIX_FY_900_45
	* STRING
	* CHAR_CODE
	* NUMBER
	* VARIABLE
	* PREFIX_FX_700_44
	* ARITH_OP_FUNCTOR
	* INFIX_OP_FUNCTOR
	* LBRACE
	* INFIX_YFX_500_16
	* PREFIX_FY_200_4
	* PREFIX_FX_1150_50
	* LPAR
	* SPECIAL_ATOM_OPS
	* PREFIX_FX_1200_53
	* CONTROL_OP_FUNCTOR
	* OPERATOR_ATOM
	* PREFIX_FX_1200_54
	* ATOM
	* BANG
	* LSQB
	* PREFIX_FY_200_3
	* COMPARISON_OP_FUNCTOR
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
	* INFIX_YFX_400_6
	* INFIX_XFY_1000_46
	* INFIX_YFX_400_9
	* INFIX_YFX_400_12
	* INFIX_YFX_500_18
	* INFIX_YFX_500_15
	* INFIX_XFX_450_14
	* INFIX_XFX_1200_50
	* INFIX_YFX_500_17
	* INFIX_XFY_200_1
	* INFIX_XFY_200_2
	* INFIX_YFX_500_16
	* INFIX_YFX_400_8
	* INFIX_XFX_1200_51
	* INFIX_YFX_400_13
	* INFIX_XFY_1100_48
	* LPAR
	* INFIX_XFY_600_19
	* INFIX_XFY_1050_47
	* INFIX_YFX_400_10
	* INFIX_YFX_400_11
	* INFIX_YFX_400_7
	* RPAR
	* INFIX_YFX_400_5
), context(consult/1))
```


### library/os.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/pio.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/process.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/serialization/abnf.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/sgml.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/simplex.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/tabling.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/tabling/batched_worklist.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/tabling/table_data_structure.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/tabling/table_link_manager.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/tabling/trie.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/time.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/ugraphs.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(No terminal matches 't' in the current parser context, at line 22 col 39

        ;/* R2 = (=),   Item == X2 */ true
                                      ^
Expected one of: 
	* INFIX_YFX_400_6
	* INFIX_XFY_1000_46
	* INFIX_YFX_400_9
	* INFIX_YFX_400_12
	* INFIX_YFX_500_18
	* INFIX_YFX_500_15
	* INFIX_XFX_450_14
	* INFIX_XFX_1200_50
	* INFIX_YFX_500_17
	* INFIX_XFY_200_1
	* INFIX_XFY_200_2
	* INFIX_YFX_500_16
	* INFIX_YFX_400_8
	* INFIX_XFX_1200_51
	* INFIX_YFX_400_13
	* INFIX_XFY_1100_48
	* LPAR
	* INFIX_XFY_600_19
	* INFIX_XFY_1050_47
	* INFIX_YFX_400_10
	* INFIX_YFX_400_11
	* INFIX_YFX_400_7
	* RPAR
	* INFIX_YFX_400_5
), context(consult/1))
```


### library/uuid.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/when.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/xpath.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


## Failures

### Timeout (29 files)

- **library/arithmetic.pl** - ❌ Timeout
- **library/builtins.pl** - ❌ Timeout
- **library/charsio.pl** - ❌ Timeout
- **library/clpb.pl** - ❌ Timeout
- **library/clpz.pl** - ❌ Timeout
- **library/crypto.pl** - ❌ Timeout
- **library/csv.pl** - ❌ Timeout
- **library/debug.pl** - ❌ Timeout
- **library/ffi.pl** - ❌ Timeout
- **library/files.pl** - ❌ Timeout
- **library/format.pl** - ❌ Timeout
- **library/http/http_server.pl** - ❌ Timeout
- **library/numerics/quadtests.pl** - ❌ Timeout
- **library/ops_and_meta_predicates.pl** - ❌ Timeout
- **library/os.pl** - ❌ Timeout
- **library/pio.pl** - ❌ Timeout
- **library/process.pl** - ❌ Timeout
- **library/serialization/abnf.pl** - ❌ Timeout
- **library/sgml.pl** - ❌ Timeout
- **library/simplex.pl** - ❌ Timeout
- **library/tabling.pl** - ❌ Timeout
- **library/tabling/batched_worklist.pl** - ❌ Timeout
- **library/tabling/table_data_structure.pl** - ❌ Timeout
- **library/tabling/table_link_manager.pl** - ❌ Timeout
- **library/tabling/trie.pl** - ❌ Timeout
- **library/time.pl** - ❌ Timeout
- **library/uuid.pl** - ❌ Timeout
- **library/when.pl** - ❌ Timeout
- **library/xpath.pl** - ❌ Timeout

### Prolog Errors (4 files)

- **library/numerics/special_functions.pl** - ❌ Prolog error
- **library/numerics/testutils.pl** - ❌ Prolog error
- **library/ordsets.pl** - ❌ Prolog error
- **library/ugraphs.pl** - ❌ Prolog error

### Parse Errors (0 files)
