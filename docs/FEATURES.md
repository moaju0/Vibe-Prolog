# ISO/IEC 13211-1 Prolog  — Conformance Tables

This documents a **minimal ISO Prolog conformance checklist**, organised by **ISO/IEC 13211-1 section**.

Status legend:

* ✅ Implemented
* ⚠️ Partial / Deviates from ISO
* ❌ Missing
* 📘 Extension (non-ISO, but compatible)

---

## §5–§6 — Prolog Text, Tokens, Syntax

| Feature                          | Status | Notes                                     |
| -------------------------------- | ------ | ----------------------------------------- |
| Atoms (quoted, unquoted)         | ✅      | Fully implemented                         |
| Variables                        | ✅      | ISO semantics                             |
| Numbers (int, float, scientific) | ✅      | Includes base-qualified (`16'ff`)         |
| Lists (proper, improper)         | ✅      |                                           |
| Compound terms                   | ✅      |                                           |
| Strings (quoted)                 | ✅      | Consistent representation                 |
| `%` line comments                | ✅      |                                           |
| `/* … */` block comments         | ✅      | Nested supported                          |
| Character code syntax (`0'X`)    | ✅      | Minor ISO edge gaps                       |
| Built-in operator syntax         | ✅      |                                           |
| `:- op/3` declaration            | ✅      | Full support - defines operators dynamically |
| Directive prefix operator `:-` (1200, fx) | ❌ | **ISO-required** - Prefix form for directives |
| Custom operator syntax in source | ✅      | Parser dynamically generates grammar for custom operators (infix/prefix/postfix) |
| Query prefix operator `?-` (1200, fx) | ✅ | **ISO-required** - Parsed via operator table |
| DCG rule operator `-->` (1200, xfx) | ✅    | **ISO-required** - DCG syntax |
| `:- char_conversion/2`           | ❌      | **ISO-mandatory**                         |

---

## §6 — Program Structure & Execution

| Feature                         | Status | Notes                              |
| ------------------------------- | ------ | ---------------------------------- |
| Facts and rules                 | ✅      |                                    |
| Clause ordering                 | ✅      | Preserved                          |
| Predicate identity (Name/Arity) | ✅      |                                    |
| Depth-first SLD resolution      | ✅      |                                    |
| Backtracking                    | ✅      |                                    |
| Cut (`!/0`)                     | ✅      | Correct semantics                  |
| If-then-else (`->/2`)           | ✅      | Lazy condition                     |
| Negation as failure (`\\+/1`)   | ✅      |                                    |
| First-argument indexing         | ✅      | O(1) clause lookup for large DBs   |
| Occurs check                    | ⚠️     | Always enabled (stricter than ISO) |
| Recursion handling with depth limits | ✅      | Configurable recursion depth limit (default: 10,000); supports deep tail recursion |
| Python recursion limit management | ✅      | Automatically increases Python's recursion limit to support Prolog depth |

---

## §7.4 — Directives

| Directive              | Status | Notes                                    |
| ---------------------- | ------ | ---------------------------------------- |
| `:- dynamic/1`         | ✅      |                                          |
| `:- multifile/1`       | ✅      |                                          |
| `:- discontiguous/1`   | ✅      |                                          |
| `:- initialization/1`  | ✅      |                                          |
| `:- op/3`              | ✅      | Full support - defines operators with validation |
| `:- char_conversion/2` | ❌      | **ISO-required**                         |

---

## §7.8 — Control Constructs

| Predicate              | Status | Notes     |
| ---------------------- | ------ | --------- |
| `true/0`               | ✅      |           |
| `fail/0`               | ✅      |           |
| `,/2`                  | ✅      |           |
| `;/2`                  | ✅      |           |
| `->/2`                 | ✅      |           |
| `\\+/1`                | ✅      |           |
| `!/0`                  | ✅      |           |
| `call/1`               | ✅      |           |
| `once/1`               | ✅      |           |
| `setup_call_cleanup/3` | ✅📘    | Extension |
| `call_cleanup/2`       | ✅📘    | Extension |

---

## §7.3 / §8.4 — Unification & Term Comparison

| Predicate / Rule                 | Status | Notes                                                                                                    |
| -------------------------------- | ------ | -------------------------------------------------------------------------------------------------------- |
| `=/2`                            | ✅      |                                                                                                          |
| `\\=/2`                          | ✅      |                                                                                                          |
| `==/2`                           | ✅      |                                                                                                          |
| `\\==/2`                         | ✅      |                                                                                                          |
| `@</2`, `@=</2`, `@>/2`, `@>=/2` | ✅      |                                                                                                          |
| `compare/3`                      | ✅      | **ISO-required** - Three-way term comparison                                                             |
| `unify_with_occurs_check/2`      | ✅      | **ISO-required** - Logically sound unification                                                           |
| ISO standard term order          | ✅      | Lists now correctly ordered as compounds                                                                 |

---

## §8.3 — Type Testing

| Predicate    | Status | Notes                                |
| ------------ | ------ | ------------------------------------ |
| `var/1`      | ✅      |                                      |
| `nonvar/1`   | ✅      |                                      |
| `atom/1`     | ✅      |                                      |
| `number/1`   | ✅      |                                      |
| `integer/1`  | ✅      |                                      |
| `float/1`    | ✅      |                                      |
| `atomic/1`   | ✅      |                                      |
| `compound/1` | ✅      |                                      |
| `callable/1` | ✅      |                                      |
| `ground/1`   | ✅      |                                      |
| `is_list/1`  | ✅      | Common extension (de facto standard); supports Atom('[]') as list terminator |

---

## §8.5 — Term Creation and Decomposition

| Predicate           | Status | Notes                   |
| ------------------- | ------ | ----------------------- |
| `functor/3`         | ✅      |                         |
| `arg/3`             | ✅      |                         |
| `=../2` (univ)      | ✅      |                         |
| `copy_term/2`       | ✅      |                         |
| `term_variables/2`  | ✅      | **ISO-required**        |
| `numbervars/3`      | ✅      | Common extension        |
| `subsumes_term/2`   | ✅      | ISO extension (Part 2)  |

---

## §8.16 — Atom Processing

| Predicate        | Status | Notes                                    |
| ---------------- | ------ | ---------------------------------------- |
| `atom_length/2`  | ✅      | **ISO-required**                         |
| `atom_concat/3`  | ✅      | **ISO-required**                         |
| `sub_atom/5`     | ✅      | **ISO-required**                         |
| `atom_chars/2`   | ✅      | **ISO-required** (needed for DCG examples) |
| `atom_codes/2`   | ✅      | **ISO-required**                         |
| `char_code/2`    | ✅      | **ISO-required**                         |
| `number_chars/2` | ✅      | **ISO-required**                         |
| `number_codes/2` | ✅      | **ISO-required**                         |
| `name/2`         | 📘      | Classic Prolog extension (pre-ISO)       |

---

## §9 — Arithmetic

| Feature                           | Status | Notes                                 |
| --------------------------------- | ------ | ------------------------------------- |
| `is/2`                            | ✅      |                                       |
| Arithmetic comparison operators   | ✅      |                                       |
| Arithmetic evaluation: `+/2`, `-/2`, `*/2`, `//2`, `///2`, `mod/2`, `div/2` | ✅      | **ISO-required** - Used within `is/2` |
| Integer division: `div/2`         | ✅      | **ISO-required** - Operator available   |
| Unary operators: `-/1`, `+/1`     | ✅      | **ISO-required** - Negation and plus  |
| Power operators: `^/2`, `**/2`    | ✅      | **ISO-required** - Operators available     |
| `abs/1`                           | ✅      |                                       |
| `min/2`, `max/2`                  | ✅      |                                       |
| `sqrt/1`                          | ✅      |                                       |
| Trig / exp / log                  | ✅      |                                       |
| `floor/1`, `ceiling/1`, `round/1` | ✅      |                                       |
| `rem/2`                           | ✅      | **ISO-required** - Integer remainder  |
| Bitwise AND: `/\`                 | ✅      | **ISO-required** - Operator available   |
| Bitwise OR: `\/`                  | ✅      | **ISO-required** - Operator available   |
| Bitwise complement: `\`           | ✅      | **ISO-required** - Operator available   |
| Bitwise shift: `<<`, `>>`         | ✅      | **ISO-required** - Operators available  |
| `between/3`                       | ✅      | **ISO-required** - Integer generation |
| `succ/2`                          | ✅      | **ISO-required** - Successor relation |
| `plus/3`                          | ✅      | **ISO-required** - Addition relation  |
| `divmod/4`                        | ✅      | Common extension                      |

---

## List Operations (ISO §8.5.3 + Common Extensions)

| Predicate               | Status | Notes                                         |
| ----------------------- | ------ | --------------------------------------------- |
| `append/3`              | ✅      |                                               |
| `member/2`              | ✅      |                                               |
| `length/2`              | ✅      |                                               |
| `reverse/2`             | ✅      |                                               |
| `sort/2`                | ✅      |                                               |
| `msort/2`               | ✅      | **ISO-required** - Sort keeping duplicates    |
| `keysort/2`             | ✅      | **ISO-required** - Sort Key-Value pairs       |
| `nth0/3`, `nth1/3`      | ✅      | Common extension - Access by index            |
| `last/2`                | ✅      | Common extension                              |
| `select/3`              | ✅      | Common extension - Select element             |
| `memberchk/2`           | ✅      | Common extension - Deterministic member       |
| `sumlist/2`             | ✅      | Common extension                              |
| `max_list/2`            | ✅      | Common extension                              |
| `min_list/2`            | ✅      | Common extension                              |
| `is_set/1`              | ✅      | Common extension - Test if list has no duplicates |
| `list_to_set/2`         | ✅      | Common extension - Remove duplicates          |
| `list_to_ord_set/2`     | ✅      | Common extension - Convert to ordered set     |
| `ord_subtract/3`        | ✅      | Common extension - Ordered set difference     |
| `numlist/3`             | ✅      | Common extension - Generate list of integers  |
| `permutation/2`         | ✅      | Common extension - Generate permutations      |
| `maplist/3-5`           | ✅      | Higher-order                                  |
| `include/3`             | ✅      | Higher-order - Filter list                    |
| `exclude/3`             | ✅      | Higher-order - Filter list (negated)          |
| `partition/4`           | ✅      | Higher-order - Split list by condition        |
| `foldl/4-6`             | ✅      | Higher-order - Fold left                      |

---

## §8.10 — All-Solutions Predicates

| Predicate   | Status | Notes                  |
| ----------- | ------ | ---------------------- |
| `findall/3` | ✅      |                        |
| `bagof/3`   | ✅      | Correct quantification |
| `setof/3`   | ✅      | ISO semantics          |

---

## Meta-Predicates & Control Flow Extensions

| Predicate   | Status | Notes                               |
| ----------- | ------ | ----------------------------------- |
| `forall/2`  | ✅      | Common extension - Universal quantification |
| `ignore/1`  | ✅      | Common extension - Always succeed   |
| `apply/2`   | ✅      | Common extension - Call with args   |

---

## §8.9 — Dynamic Program Modification

| Predicate / Rule                       | Status | Notes                                                               |
| -------------------------------------- | ------ | ------------------------------------------------------------------- |
| Static by default                      | ⚠️     | Semantics implemented; additional cross-module test coverage needed |
| `asserta/1`                            | ✅      |                                                                     |
| `assertz/1`                            | ✅      |                                                                     |
| `assert/1`                             | ✅      |                                                                     |
| `retract/1`                            | ✅      |                                                                     |
| `retractall/1`                         | ✅      | **ISO-required** - Retracts all clauses whose heads unify with Head |
| `abolish/1`                            | ✅      |                                                                     |
| `clause/2`                             | ✅      |                                                                     |
| Permission errors on static predicates | ⚠️     | Enforced, but requires further validation across modules            |

---

## §8.11 — Character Input/Output

| Predicate       | Status | Notes                            |
| --------------- | ------ | -------------------------------- |
| `get_char/1-2`  | ✅      |                                  |
| `put_char/1-2`  | ✅      |                                  |
| `get_code/1-2`  | ✅      | **ISO-required**                 |
| `put_code/1-2`  | ✅      | **ISO-required**                 |
| `get/1-2`       | ✅      | **ISO-required** - Skip whitespace |
| `put/1-2`       | ✅📘    | **ISO-required** - Write character code or single-character atom (SWI-Prolog compatible) |
| `peek_char/1-2` | ✅      | **ISO-required** - Look ahead    |
| `peek_code/1-2` | ✅      | **ISO-required** - Look ahead    |
| `peek_byte/1-2` | ✅      | **ISO-required**                 |
| `get_byte/1-2`  | ✅      | **ISO-required**                 |
| `put_byte/1-2`  | ✅      | **ISO-required**                 |
| `nl/0-1`        | ✅      |                                  |

---

## §8.12 — Term Input/Output

| Predicate               | Status | Notes                            |
| ----------------------- | ------ | -------------------------------- |
| `read/1-2`              | ✅      |                                  |
| `read_term/2-3`         | ✅      | **ISO-required** - With options  |
| `write/1-2`             | ✅      |                                  |
| `writeq/1-2`            | ✅      | **ISO-required** - With quotes   |
| `write_canonical/1-2`   | ✅      | **ISO-required** - Canonical form|
| `write_term/2-3`        | ✅      | **ISO-required** - With options  |
| `print/1-2`             | ✅      | Common extension                 |
| `writeln/1-2`           | ✅      | Extension                        |
| `format/2-3`            | ✅📘    | Extension                        |
| `write_term_to_chars/3` | ⚠️     | Minor formatting differences     |

---

## §8.13 — Stream Selection and Control

| Predicate                | Status | Notes                        |
| ------------------------ | ------ | ---------------------------- |
| `open/3-4`               | ✅      |                              |
| `close/1-2`              | ✅      |                              |
| `current_input/1`        | ✅      |                              |
| `current_output/1`       | ✅      |                              |
| `set_input/1`            | ✅      | **ISO-required**             |
| `set_output/1`           | ✅      | **ISO-required**             |
| `flush_output/0-1`       | ✅      | **ISO-required**             |
| `at_end_of_stream/0-1`   | ✅      | **ISO-required** - EOF test  |
| `stream_property/2`      | ✅      | **ISO-required**             |
| `set_stream_position/2`  | ✅      | **ISO-required** - Seek      |

## Classic I/O Predicates (Edinburgh Style)

| Predicate | Status | Notes                                      |
| --------- | ------ | ------------------------------------------ |
| `see/1`   | ✅      | Classic input redirection                  |
| `seen/0`  | ✅      | Close classic input                        |
| `tell/1`  | ✅      | Classic output redirection                 |
| `told/0`  | ✅      | Close classic output                       |

---

## §7.12 — Errors & Exceptions

| Feature                                                           | Status | Notes                                        |
| ----------------------------------------------------------------- | ------ | -------------------------------------------- |
| `throw/1`                                                         | ✅      |                                              |
| `catch/3`                                                         | ✅      |                                              |
| `instantiation_error`                                             | ✅      |                                              |
| `type_error/2`                                                    | ✅      |                                              |
| `domain_error/2`                                                  | ✅      |                                              |
| `permission_error/3`                                              | ✅      | Enforced consistently for most predicates    |
| `existence_error/2`                                               | ✅      |                                              |
| `syntax_error/1`                                                  | ✅      |                                              |
| `resource_error/2`                                                | ✅      | Recursion depth exceeded                      |
| Arithmetic errors (`zero_divisor`, `undefined`, `float_overflow`) | ✅      | Full ISO-compliant arithmetic error handling |

---

## §8.8 — Reflection

| Predicate                             | Status | Notes                   |
| ------------------------------------- | ------ | ----------------------- |
| `current_predicate/1`                 | ✅      |                         |
| `predicate_property/2`                | ⚠️     | Built-in detection only |
| `dynamic/static/multifile` properties | ❌      | Needed for ISO tooling  |

---

## SWI-Prolog Specific Extensions (Non-ISO)

These predicates are specific to SWI-Prolog and not part of the ISO standard.

| Category | Predicates | Status | Notes |
| -------- | ---------- | ------ | ----- |
| **Networking** | `tcp_socket/1`, `tcp_connect/4` | 🚫 Won't Implement | SWI-specific socket operations - out of scope |
| **DDE (Windows)** | `open_dde_conversation/3`, `close_dde_conversation/1`, `dde_request/3`, `dde_execute/2`, `dde_poke/3`, `dde_register_service/2` | 🚫 Won't Implement | Windows Dynamic Data Exchange - obsolete technology |
| **CLP(FD)** | `#=/2`, `#</2`, `#>/2`, `#=</2`, `#>=/2`, `ins/2`, `in/2` | ❌ | Constraint Logic Programming over Finite Domains |
| **Tabling** | `:- table/1` directive | 🔽 Low Priority | Tabled execution (memoization) - advanced optimization |
| **CHR** | `:- chr_constraint/1` | ❌ | Constraint Handling Rules |
| **RDF** | `:- rdf_meta/1` | 🔽 Low Priority | RDF (Resource Description Framework) support - specialized use case |
| **Random** | `random/1` | ❌ | Random number generation (non-ISO) |

---

## §10 — Modules (ISO Part 1)

| Feature                           | Status | Notes                                                          |
| --------------------------------- | ------ | -------------------------------------------------------------- |
| `:- module/2`                     | ✅      |                                                                |
| Module-qualified calls (`M:Goal`) | ✅      |                                                                |
| Export enforcement                | ✅      |                                                                |
| Built-ins visible in all modules  | ✅      |                                                                |
| Clause body module resolution     | ✅      | Unqualified goals resolve via imports → defining module → user |
| `use_module/1,2`                  | ✅      | Supports full and selective imports, including `library(Name)`; library/ preferred over examples/modules/ for module resolution |
| **Operator exports in module/2**  | ✅      | **SWI-Prolog compatible - operators can be exported**          |
| Imported operator discovery       | ✅      | Operators declared in imported modules are collected before parsing dependents |
| **Module-scoped predicate namespaces** | ✅  | Each module can define predicates with the same name/arity without conflict. `library(a):foo/1` and `library(b):foo/1` are distinct |
| Cross-module dynamic semantics    | ⚠️     | Implemented; dynamic predicate isolation per module needs further work |

---

## ISO Conformance Snapshot

| Category                  | Status                                                     |
| ------------------------- | ---------------------------------------------------------- |
| Core execution model      | ✅ Strong                                                   |
| Control constructs        | ✅ Strong                                                   |
| Unification & comparison  | ✅ Strong                                                   |
| Type testing              | ✅ Strong                                                   |
| Term manipulation         | ✅ Strong                                                   |
| Atom processing (§8.16)   | ✅ Strong                                                   |
| Arithmetic                | ✅ Strong                                                   |
| List operations           | ⚠️ Core ops ✅, missing higher-order predicates (maplist, foldl, etc.) |
| All-solutions             | ✅ Strong                                                   |
| Meta-predicates           | ⚠️ Core meta-preds implemented; higher-order list ops missing |
| Database operations       | ⚠️ Strong, missing `retractall/1`                           |
| Character I/O (§8.11)     | ✅ Strong - All ISO-required predicates implemented         |
| Term I/O (§8.12)          | ✅ Strong - All ISO-required predicates implemented        |
| Stream control (§8.13)    | ✅ Strong - All ISO-required predicates implemented        |
| Errors & exceptions       | ✅ Strong                                                   |
| Parsing & syntax          | ⚠️ op/3 ✅, custom operator syntax parsing ✅, missing ISO operators (div, ^, /\, etc.), char_conversion ❌ |
| Modules                   | ✅ Largely ISO-consistent (Part 1)                          |
| Reflection                | ⚠️ Partial                                                 |

---

## ISO Blocking Issues

1. **Missing operators** - Several ISO-required operators are not defined in the operator table:
   - Arithmetic: `div`, `rem`, `^`, `**`
   - Bitwise: `/\`, `\/`, `\`, `<<`, `>>`
   - Directives: `:-` (prefix), `?-`, `-->`
2. `op/3` affects parsing (§6.3) - Operators declared dynamically update the parser grammar
3. `char_conversion/2` missing (§6.4, §7.4)

## Common Extensions Worth Implementing

Based on analysis of real-world Prolog programs, these commonly-used predicates would improve compatibility:

1. **List utilities** - `is_set/1`, `list_to_set/2`, `list_to_ord_set/2`, `ord_subtract/3`, `numlist/3`, `permutation/2`
2. **Higher-order** - `maplist/3-5` (currently only `/2` implemented), `include/3`, `exclude/3`

---

## Command-Line Options

### `--builtin-conflict` Flag

Controls how the interpreter handles library definitions that conflict with built-in predicates.

**Syntax:**
```
--builtin-conflict=MODE
```

**Modes:**

| Mode | Behavior |
|------|----------|
| `skip` | **(Default)** Silently skip the library definition and use the existing built-in. Allows libraries like `clpz` to load without errors. |
| `error` | Raise a `permission_error` when a library tries to redefine a built-in predicate. Useful for strict checking and debugging library compatibility. |
| `shadow` | Allow a module to define a predicate that shadows a built-in within that module's namespace. Module-qualified calls use the module's definition; unqualified calls from user context use the built-in. |

**Examples:**

Skip mode (default):
```prolog
% Program tries to define length/2
:- module(my_lib, [length/2]).
length([], custom_zero).
length([_|T], s(N)) :- length(T, N).

?- length([a, b], L).  % Uses built-in: L = 2
```

Shadow mode:
```prolog
% Same program with --builtin-conflict=shadow
:- module(my_lib, [length/2]).
length([], custom_zero).
length([_|T], s(N)) :- length(T, N).

?- my_lib:length([a, b], L).  % Uses module version: L = s(s(custom_zero))
?- length([a, b], L).         % Uses built-in: L = 2
?- user:length([a, b], L).    % Uses built-in: L = 2
```

When importing a shadowed predicate:
```prolog
:- use_module(my_lib, [length/2]).
?- length([a, b], L).  % Uses imported shadow: L = s(s(custom_zero))
```
