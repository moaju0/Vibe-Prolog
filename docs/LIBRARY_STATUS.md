# Vibe-Prolog Library Loading Status

**Test Date:** 2025-12-13 12:12:24 UTC

## Summary

- **Total Files:** 63
- **Loaded Successfully:** 40
- **Failed to Load:** 23
- **Success Rate:** 40/63 (63%)

## Successfully Loaded Files ✅

- `library/$project_atts.pl`
- `library/arithmetic.pl`
- `library/assoc.pl`
- `library/atts.pl`
- `library/between.pl`
- `library/charsio.pl`
- `library/cont.pl`
- `library/dcgs.pl`
- `library/diag.pl`
- `library/dif.pl`
- `library/error.pl`
- `library/files.pl`
- `library/freeze.pl`
- `library/gensym.pl`
- `library/http/http_open.pl`
- `library/iso_ext.pl`
- `library/lambda.pl`
- `library/lists.pl`
- `library/loader.pl`
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
- `library/sockets.pl`
- `library/tabling/batched_worklist.pl`
- `library/tabling/double_linked_list.pl`
- `library/tabling/global_worklist.pl`
- `library/tabling/wrapper.pl`
- `library/terms.pl`
- `library/test_module.pl`
- `library/tls.pl`
- `library/wasm.pl`
- `library/xpath.pl`

## Failed to Load Files ❌

### library/builtins.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, static_procedure, /(:-, 1)), context(consult/1))
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
PrologThrow: error(syntax_error(No terminal matches 'P' in the current parser context, at line 10 col 22

              ,gamma_P_Q/4
                     ^
Expected one of: 
	* INFIX_XFY_1105_49
	* INFIX_XFY_1000_46
	* INFIX_XFX_700_27
	* INFIX_XFX_700_31
	* INFIX_YFX_400_13
	* _RBRA
	* INFIX_YFX_500_15
	* INFIX_XFX_700_20
	* INFIX_XFX_700_39
	* INFIX_YFX_400_12
	* INFIX_XFX_700_30
	* INFIX_XFX_700_34
	* INFIX_XFX_700_23
	* INFIX_XFX_700_32
	* INFIX_XFX_700_33
	* INFIX_XFX_700_25
	* INFIX_XFX_700_42
	* INFIX_YFX_400_9
	* INFIX_XFX_700_21
	* INFIX_XFX_700_22
	* INFIX_XFX_700_26
	* INFIX_XFY_200_1
	* INFIX_XFX_1200_51
	* INFIX_XFX_700_35
	* INFIX_YFX_400_11
	* INFIX_XFX_700_43
	* INFIX_YFX_400_6
	* INFIX_XFX_700_36
	* INFIX_XFY_1100_48
	* INFIX_XFX_1200_52
	* INFIX_XFX_700_38
	* INFIX_XFX_700_24
	* INFIX_XFY_200_2
	* INFIX_XFY_600_19
	* INFIX_YFX_500_17
	* INFIX_XFX_700_37
	* INFIX_XFX_700_28
	* INFIX_YFX_400_8
	* _LPAR
	* INFIX_YFX_400_10
	* INFIX_YFX_500_18
	* INFIX_XFX_700_40
	* INFIX_YFX_400_7
	* INFIX_YFX_400_5
	* INFIX_YFX_500_16
	* INFIX_XFX_700_41
	* INFIX_XFX_450_14
	* INFIX_XFY_1050_47
	* INFIX_XFX_700_29
), context(consult/1))
```


### library/numerics/testutils.pl

**Status:** ❌ Timeout

**Details:**

```
File loading exceeded 30 seconds
```


### library/ops_and_meta_predicates.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(permission_error(modify, operator, :), context(op/3))
```


### library/ordsets.pl

**Status:** ❌ Prolog error

**Details:**

```
PrologThrow: error(syntax_error(No terminal matches 'R' in the current parser context, at line 22 col 13

        ;/* R2 = (=),   Item == X2 */ true
            ^
Expected one of: 
	* INFIX_XFY_1000_46
	* INFIX_XFX_700_27
	* INFIX_XFX_700_31
	* INFIX_XFX_1200_50
	* INFIX_YFX_400_13
	* INFIX_YFX_500_15
	* INFIX_XFX_700_20
	* INFIX_XFX_700_39
	* INFIX_YFX_400_12
	* INFIX_XFX_700_30
	* INFIX_XFX_700_34
	* INFIX_XFX_700_23
	* INFIX_XFX_700_32
	* INFIX_XFX_700_33
	* INFIX_XFX_700_25
	* INFIX_XFX_700_42
	* INFIX_YFX_400_9
	* INFIX_XFX_700_21
	* INFIX_XFX_700_22
	* INFIX_XFX_700_26
	* INFIX_XFY_200_1
	* INFIX_XFX_1200_51
	* INFIX_XFX_700_35
	* INFIX_YFX_400_11
	* INFIX_XFX_700_43
	* INFIX_YFX_400_6
	* INFIX_XFX_700_36
	* INFIX_XFY_1100_48
	* INFIX_XFX_700_38
	* INFIX_XFX_700_24
	* INFIX_XFY_200_2
	* INFIX_XFY_600_19
	* INFIX_YFX_500_17
	* INFIX_XFX_700_37
	* INFIX_XFX_700_28
	* INFIX_YFX_400_8
	* _LPAR
	* INFIX_YFX_400_10
	* INFIX_YFX_500_18
	* INFIX_XFX_700_40
	* INFIX_YFX_400_7
	* INFIX_YFX_400_5
	* INFIX_YFX_500_16
	* INFIX_XFX_700_41
	* INFIX_XFX_450_14
	* INFIX_XFY_1050_47
	* INFIX_XFX_700_29
	* _RPAR
), context(consult/1))
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
PrologThrow: error(syntax_error(No terminal matches 'R' in the current parser context, at line 22 col 13

        ;/* R2 = (=),   Item == X2 */ true
            ^
Expected one of: 
	* INFIX_XFY_1000_46
	* INFIX_XFX_700_27
	* INFIX_XFX_700_31
	* INFIX_XFX_1200_50
	* INFIX_YFX_400_13
	* INFIX_YFX_500_15
	* INFIX_XFX_700_20
	* INFIX_XFX_700_39
	* INFIX_YFX_400_12
	* INFIX_XFX_700_30
	* INFIX_XFX_700_34
	* INFIX_XFX_700_23
	* INFIX_XFX_700_32
	* INFIX_XFX_700_33
	* INFIX_XFX_700_25
	* INFIX_XFX_700_42
	* INFIX_YFX_400_9
	* INFIX_XFX_700_21
	* INFIX_XFX_700_22
	* INFIX_XFX_700_26
	* INFIX_XFY_200_1
	* INFIX_XFX_1200_51
	* INFIX_XFX_700_35
	* INFIX_YFX_400_11
	* INFIX_XFX_700_43
	* INFIX_YFX_400_6
	* INFIX_XFX_700_36
	* INFIX_XFY_1100_48
	* INFIX_XFX_700_38
	* INFIX_XFX_700_24
	* INFIX_XFY_200_2
	* INFIX_XFY_600_19
	* INFIX_YFX_500_17
	* INFIX_XFX_700_37
	* INFIX_XFX_700_28
	* INFIX_YFX_400_8
	* _LPAR
	* INFIX_YFX_400_10
	* INFIX_YFX_500_18
	* INFIX_XFX_700_40
	* INFIX_YFX_400_7
	* INFIX_YFX_400_5
	* INFIX_YFX_500_16
	* INFIX_XFX_700_41
	* INFIX_XFX_450_14
	* INFIX_XFY_1050_47
	* INFIX_XFX_700_29
	* _RPAR
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


## Next Steps

The following library files have issues that should be investigated and addressed:

1. **library/builtins.pl** - ❌ Prolog error
1. **library/clpb.pl** - ❌ Timeout
1. **library/clpz.pl** - ❌ Timeout
1. **library/crypto.pl** - ❌ Timeout
1. **library/csv.pl** - ❌ Timeout
1. **library/debug.pl** - ❌ Timeout
1. **library/ffi.pl** - ❌ Timeout
1. **library/format.pl** - ❌ Timeout
1. **library/http/http_server.pl** - ❌ Timeout
1. **library/numerics/quadtests.pl** - ❌ Timeout
1. **library/numerics/special_functions.pl** - ❌ Prolog error
1. **library/numerics/testutils.pl** - ❌ Timeout
1. **library/ops_and_meta_predicates.pl** - ❌ Prolog error
1. **library/ordsets.pl** - ❌ Prolog error
1. **library/simplex.pl** - ❌ Timeout
1. **library/tabling.pl** - ❌ Timeout
1. **library/tabling/table_data_structure.pl** - ❌ Timeout
1. **library/tabling/table_link_manager.pl** - ❌ Timeout
1. **library/tabling/trie.pl** - ❌ Timeout
1. **library/time.pl** - ❌ Timeout
1. **library/ugraphs.pl** - ❌ Prolog error
1. **library/uuid.pl** - ❌ Timeout
1. **library/when.pl** - ❌ Timeout

These issues should be converted into GitHub issues with the details provided above.
