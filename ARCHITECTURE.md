# Simple Prolog Interpreter Architecture & Usage

A minimal yet capable Prolog interpreter in Python. This document combines usage
guides, architecture notes, and contributor guidance.

## Project Structure

```
prolog/
├── interpreter.py          # High-level PrologInterpreter interface
├── engine.py               # Core execution engine (~200 lines)
├── parser.py               # Prolog syntax parser
├── unification.py          # Unification algorithm
├── builtins/               # Built-in predicate implementations
│   ├── arithmetic.py
│   ├── control.py
│   ├── ...
└── utils/                  # Utility functions
    ├── term_utils.py
    ├── list_utils.py
    └── variable_utils.py
```

## Features

See `FEATURES.md` for the full checklist. Core capabilities include ISO-style
parsing, unification, backtracking execution, and a broad set of built-ins.

## Usage

### Running the Demo

```bash
uv run main.py
```

### Running Tests

```bash
uv run pytest        # all tests
uv run pytest -v     # verbose
uv run pytest tests/test_builtins.py
uv run pytest tests/test_parser.py
```

### Using as a Library

```python
from prolog import PrologInterpreter

prolog = PrologInterpreter()
prolog.consult_string("""
    parent(tom, bob).
    parent(tom, liz).
    grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
""")

results = prolog.query("grandparent(tom, Who)")
for result in results:
    print(f"Tom's grandchild: {result['Who']}")

result = prolog.query_once("parent(tom, X)")
if result:
    print(f"Tom's child: {result['X']}")

if prolog.has_solution("parent(tom, bob)"):
    print("Tom is bob's parent")
```

### Loading from Files

```python
prolog = PrologInterpreter()
prolog.consult("my_rules.pl")
results = prolog.query("my_query(X)")
```

## Architecture Overview

The interpreter consists of four main components:

1. **Parser** (`prolog/parser.py`) - Uses Lark to parse Prolog syntax with full
   operator precedence and modern atom/number handling.
2. **Unification** (`prolog/unification.py`) - Robinson-style unification.
3. **Engine** (`prolog/engine.py`) - Backtracking search with built-in
   predicates, proper cut semantics, and dynamic predicate support.
4. **Interpreter** (`prolog/interpreter.py`) - Public API for loading and
   querying programs.

## Built-in Registry

Built-ins live under `prolog/builtins/` and register themselves via
`prolog.builtins.register_builtin`. Each module exposes a static `register`
method so the engine can populate its registry at startup. Handlers use the
signature `(args, subst, engine)` and can yield multiple substitutions for
non-deterministic predicates.

### Adding a New Built-in Predicate

1. **Choose the appropriate module** in `prolog/builtins/`:
   - Arithmetic operations → `arithmetic.py`
   - List operations → `list_ops.py`
   - Type tests → `type_tests.py`
   - Control flow → `control.py`
   - Database → `database.py`
   - I/O → `io.py`
   - Term manipulation → `term_manipulation.py`
   - All-solutions → `all_solutions.py`
   - Exceptions → `exceptions.py`
   - Reflection → `reflection.py`
   - Higher-order → `higher_order.py`

2. **Add a static method** to the module's class:

   ```python
   @staticmethod
   def my_builtin(args: tuple, subst: Substitution, engine) -> BuiltinResult:
       """my_builtin/N - Description."""
       # Implementation
   ```

3. **Register in the module's register() method**:

   ```python
   @staticmethod
   def register(registry: BuiltinRegistry, engine_ref=None):
       registry[("my_builtin", N)] = ModuleClass.my_builtin
   ```

4. **Add tests** in `tests/test_*.py`.
5. **Update FEATURES.md** to mark the predicate as implemented.

## Utility Modules

Shared helpers for the AST live in `prolog/utils/`:

- `term_utils.py`: Formatting, comparison, and sorting helpers for terms.
- `list_utils.py`: Convert between Python and Prolog list forms; match/shape
  lists.
- `variable_utils.py`: Variable collection, copying, and existential stripping.

These modules are imported by `prolog/engine.py` and can be tested independently
via the unit tests in `tests/utils/`.

## Examples

### Family Relationships

```prolog
parent(tom, bob).
parent(bob, ann).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

### List Operations

```prolog
% member/2 is built-in
?- member(2, [1,2,3]).  % True
?- member(X, [a,b,c]).  % X = a ; X = b ; X = c
```

### Logic Puzzles

```prolog
person(alice).
person(bob).

job(alice, doctor).
job(bob, engineer).

% Who has which job?
?- job(Person, doctor).  % Person = alice
```

## Limitations

- File I/O predicates (`see/1`, `seen/0`, `read/1` from files) are not
  supported.
- No DCG, modules/namespaces, CLP, or tail-call optimization.
- Some obscure parser edge cases (e.g., certain hex escapes, base-notation in
  arithmetic) may not parse.
- Occurs check is not performed by default (standard Prolog behavior).
- Very deep recursion can hit Python's stack limit.

## Test Coverage Snapshot

ISO core suite currently reports ~91% (69/76) passing; remaining failures are
parser edge cases. Core functionality (unification, type checks, arithmetic,
lists, control flow, meta-predicates) passes.
