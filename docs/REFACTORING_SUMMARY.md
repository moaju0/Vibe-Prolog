# Refactoring Summary

## What Changed

- Reduced `engine.py` from 1668 lines to roughly 200 lines by moving built-ins and helpers into dedicated modules.
- Introduced modular structure under `prolog/builtins/` and `prolog/utils/`.
- Added explicit registration via `register_builtin` to decouple built-ins from the engine.
- Documented architecture and utility responsibilities for future contributors.

## Benefits

- Improved testability through smaller, focused modules.
- Clear separation of concerns between the engine core, built-ins, and utilities.
- Easier to extend with new predicates without touching the engine.
- Lower maintenance cost with reduced duplication and consistent typing.

## Migration Approach

1. Incremental extraction of built-ins into module-per-category files.
2. Continuous test runs to validate behavior at each step.
3. Adoption of shared helpers and typing aliases to keep handler signatures consistent.
4. Final polish with documentation and linting passes to stabilize the refactor.

## Before vs After

```
Before: engine.py ~1668 lines, built-ins embedded in engine methods.
After : engine.py ~200 lines, built-ins organized by category in prolog/builtins/.
```

## Notes for Contributors

- New built-ins should follow the `(args, subst, engine)` signature and register via the module `register` method.
- Reuse helpers from `prolog/utils/` and `prolog/builtins/common.py` to avoid duplication.
- Update `FEATURES.md` and relevant tests when adding predicates.
