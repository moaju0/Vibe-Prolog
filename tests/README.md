# Prolog Interpreter Test Suite

This directory holds the pytest suite for Vibe-Prolog. It exercises ISO core behavior, built-ins (control flow, arithmetic, lists, term utilities, modules, DCGs, I/O), parser edge cases, fixtures, and basic performance checks.

## Running Tests

Run everything:
```bash
uv run pytest
```

Performance tests are skipped by default; enable them explicitly:
```bash
uv run pytest --run-performance
```

Verbose or focused runs:
```bash
uv run pytest -v                           # verbose
uv run pytest tests/test_builtins.py       # specific file
uv run pytest tests/test_builtins.py::TestUnification  # specific class
uv run pytest tests/test_builtins.py::TestUnification::test_unify_atoms  # specific test
```

## Suite Highlights

- **Built-ins**: coverage for unification/comparison, arithmetic, control flow, meta-predicates (call/1, once/1, forall/2, ignore/1, apply/2), lists, term utilities (functor/3, arg/3, =../2, copy_term/2, term_variables/2, numbervars/3, subsumes_term/2), reflection, database, and modules.
- **Parser**: tokens, operators, numbers, lists, directives, comments, and complex examples.
- **Fixtures**: reusable Prolog programs in `tests/fixtures/` for recursion, lists, meta-predicates, arithmetic, DCGs, and scalability.
- **Performance**: smoke checks under `tests/performance/`.

Use `-s` to see predicate output, and keep `FEATURES.md` in sync when adding or changing behavior.
