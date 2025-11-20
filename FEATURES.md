## Built-in Predicates

### Core Unification and Comparison
- `=/2` – Robinson-style unification with full term handling (atoms, numbers, lists, compounds).
- `\=/2` – Explicit failure when two terms cannot unify.
- Arithmetic comparison operators `=:=/2`, `<`, `>`, `=<`, `>=` predicate-based evaluation with operator precedence.

### Arithmetic
- `is/2` – Arithmetic evaluation that combines integers, floats, scientific notation, unary minus, and computed expressions.
- Binary (`0b`), octal (`0o`), and hex (`0x`) literals along with division `/`, integer division `//`, modulo `mod`, and support for the `**` operator in expressions.

### Type Checking
- `atom/1`, `number/1`, `integer/1`, `var/1`, `nonvar/1`, `compound/1` – Predicate checks for term types and variable instantiation status.

### Term Manipulation
- `functor/3`, `arg/3`, `=../2` – Term decomposition and construction that work with atoms, numbers, compounds, and lists, including nested arguments and bidirectional univ conversions.

### List Operations
- `member/2`, `append/3`, `length/2`, `reverse/2`, `sort/2` – Fully bidirectional list support that enumerates through tails, handles open and nested tails, fails on improper lists, enumerates all solutions via backtracking, and produces sorted/deduplicated lists.

### Solution Collection
- `findall/3`, `bagof/3`, `setof/3` – Collection predicates that cooperate with dynamic goals, arithmetic filters, compound templates, and the usual success/failure semantics (`findall` yields `[]` when no solutions, `bagof`/`setof` fail without answers, `setof` deduplicates and sorts).

### Database Modification
- `assert/1`, `retract/1`, `clause/2` – Dynamic database updates for facts and rule-like structures plus clause inspection even for built-in goals, with backtracking-aware retrieval and if-then usage.

### Inspection & Error Handling
- `predicate_property/2` – Query predicate metadata, including whether a goal is a built-in, for dynamic reflection.
- `catch/3` – Simplified exception handling that attempts a goal and provides a recovery branch, mirroring ISO semantics in structure.

### Meta-Predicates
- `call/1`, `once/1` – Dynamic goal invocation and commitment to the first solution.

### Control Flow
- `true/0`, `fail/0`, `;/2`, `->/2`, `,/2`, `\+/1`, `!/0` – Standard Prolog control constructs, including disjunction, conjunction, if-then(-else), negation-as-failure, and the cut operator to prune choice points.

### I/O
- `write/1`, `writeln/1`, `nl/0`, `format/2`, `format/3` – Output helpers that format atoms, numbers, compound terms, and unbound variables.

### Higher-Order
- `maplist/2` – Apply goals over lists (including lists with explicit tails).

## Execution Model
- Robinson-style unification with occurs-check defenses, full backtracking search, and dynamic clause enumeration deliver a robust proof search engine that handles recursion, limit-aware queries (e.g., `member/2` backtracking), and compound predicate evaluation.

## Parser & Syntax
- Binary, octal, and hex number literals plus scientific notation and `**` arithmetic.
- Character codes (`0'a`, `0'\n`, `0'''`) and the usual mix of unary minus and spaced prefixes.
- Block (`/* ... */`) and line (`% ...`) comments, curly braces (`{Term}` sugar for `{}(Term)`), and special atoms such as `[]`, `{}`, `:-`, and `\+`.
- Parser recognizes `[]` and `'[]'` as equivalent terms, list tail syntax with variables or atoms, and complex operator atoms (e.g., `:-` as an atom in lists and terms).

## Data Types
- Atoms, integers, floats, variables, proper and improper lists, and compound terms with nested arguments, open tails, or quoted atoms.

-## Tooling & Tests
- Use the interpreter as a Python library (`from prolog import PrologInterpreter`) and exercise it via a comprehensive test suite (69+ tests covering ISO core predicates, parser edge cases, and built-in behavior).
-`PrologInterpreter` exposes `consult/consult_string`, `query`, `query_once`, and `has_solution` helpers, plus optional stdout capture for capturing `write`/`format` output while still returning solution bindings.
