# Vibe-Prolog Library Loading Status

**Test Date:** 2025-12-13 11:47:58 UTC

## Summary

- **Total Files:** 8
- **Loaded Successfully:** 6
- **Failed to Load:** 2
- **Success Rate:** 6/8 (75%)

## Successfully Loaded Files ✅

- `library/$project_atts.pl`
- `library/arithmetic.pl`
- `library/assoc.pl`
- `library/atts.pl`
- `library/between.pl`
- `library/charsio.pl`

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


## Next Steps

The following library files have issues that should be investigated and addressed:

1. **library/builtins.pl** - ❌ Prolog error
1. **library/clpb.pl** - ❌ Timeout

These issues should be converted into GitHub issues with the details provided above.
