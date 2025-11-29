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
| `:- op/3` declaration            | ⚠️     | Operator table updated, parser ignores it |
| Dynamic operator parsing         | ❌      | **ISO-blocking**                          |
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

---

## §7.4 — Directives

| Directive              | Status | Notes                                |
| ---------------------- | ------ | ------------------------------------ |
| `:- dynamic/1`         | ✅      |                                      |
| `:- multifile/1`       | ✅      |                                      |
| `:- discontiguous/1`   | ✅      |                                      |
| `:- initialization/1`  | ✅      |                                      |
| `:- op/3`              | ⚠️     | Declaration only; parsing unaffected |
| `:- char_conversion/2` | ❌      | **ISO-required**                     |

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
| ISO standard term order          | ❌      | Lists treated separately (explicitly defined for deterministic ordering; ISO requires lists ⊂ compounds) |

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
| `is_list/1`  | ❌      | Common extension (de facto standard) |

---

## §8.5 — Term Creation and Decomposition

| Predicate           | Status | Notes                   |
| ------------------- | ------ | ----------------------- |
| `functor/3`         | ✅      |                         |
| `arg/3`             | ✅      |                         |
| `=../2` (univ)      | ✅      |                         |
| `copy_term/2`       | ✅      |                         |
| `term_variables/2`  | ❌      | **ISO-required**        |
| `numbervars/3`      | ❌      | Common extension        |
| `subsumes_term/2`   | ❌      | ISO extension (Part 2)  |

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

---

## §9 — Arithmetic

| Feature                           | Status | Notes                                 |
| --------------------------------- | ------ | ------------------------------------- |
| `is/2`                            | ✅      |                                       |
| Arithmetic comparison operators   | ✅      |                                       |
| `+ - * / // mod`                  | ✅      |                                       |
| `abs/1`                           | ✅      |                                       |
| `min/2`, `max/2`                  | ✅      |                                       |
| `sqrt/1`                          | ✅      |                                       |
| Trig / exp / log                  | ✅      |                                       |
| `floor/1`, `ceiling/1`, `round/1` | ✅      |                                       |
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
| `msort/2`               | ❌      | **ISO-required** - Sort keeping duplicates    |
| `keysort/2`             | ❌      | **ISO-required** - Sort Key-Value pairs       |
| `nth0/3`, `nth1/3`      | ❌      | Common extension - Access by index            |
| `last/2`                | ❌      | Common extension                              |
| `select/3`              | ❌      | Common extension - Select element             |
| `memberchk/2`           | ❌      | Common extension - Deterministic member       |
| `sumlist/2`             | ❌      | Common extension                              |
| `max_list/2`            | ❌      | Common extension                              |
| `min_list/2`            | ❌      | Common extension                              |
| `maplist/3-5`           | ❌      | Higher-order (only `/2` implemented)          |
| `include/3`             | ❌      | Higher-order - Filter list                    |
| `exclude/3`             | ❌      | Higher-order - Filter list (negated)          |
| `partition/4`           | ❌      | Higher-order - Split list by condition        |
| `foldl/4-6`             | ❌      | Higher-order - Fold left                      |

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
| `forall/2`  | ❌      | Common extension - Universal quantification |
| `ignore/1`  | ❌      | Common extension - Always succeed   |
| `apply/2`   | ❌      | Common extension - Call with args   |

---

## §8.9 — Dynamic Program Modification

| Predicate / Rule                       | Status | Notes                                                               |
| -------------------------------------- | ------ | ------------------------------------------------------------------- |
| Static by default                      | ⚠️     | Semantics implemented; additional cross-module test coverage needed |
| `asserta/1`                            | ✅      |                                                                     |
| `assertz/1`                            | ✅      |                                                                     |
| `assert/1`                             | ✅      |                                                                     |
| `retract/1`                            | ✅      |                                                                     |
| `abolish/1`                            | ✅      |                                                                     |
| `clause/2`                             | ✅      |                                                                     |
| Permission errors on static predicates | ⚠️     | Enforced, but requires further validation across modules            |

---

## §8.11 — Character Input/Output

| Predicate       | Status | Notes                            |
| --------------- | ------ | -------------------------------- |
| `get_char/1-2`  | ⚠️     | Only `/1` implemented            |
| `put_char/1-2`  | ⚠️     | Only `/1` implemented            |
| `get_code/1-2`  | ❌      | **ISO-required**                 |
| `put_code/1-2`  | ❌      | **ISO-required**                 |
| `peek_char/1-2` | ❌      | **ISO-required** - Look ahead    |
| `peek_code/1-2` | ❌      | **ISO-required** - Look ahead    |
| `peek_byte/1-2` | ❌      | **ISO-required**                 |
| `get_byte/1-2`  | ❌      | **ISO-required**                 |
| `put_byte/1-2`  | ❌      | **ISO-required**                 |
| `nl/0-1`        | ⚠️     | Only `/0` implemented            |

---

## §8.12 — Term Input/Output

| Predicate               | Status | Notes                            |
| ----------------------- | ------ | -------------------------------- |
| `read/1-2`              | ✅      |                                  |
| `read_term/2-3`         | ❌      | **ISO-required** - With options  |
| `write/1-2`             | ⚠️     | Only `/1` implemented            |
| `writeq/1-2`            | ❌      | **ISO-required** - With quotes   |
| `write_canonical/1-2`   | ❌      | **ISO-required** - Canonical form|
| `write_term/2-3`        | ❌      | **ISO-required** - With options  |
| `print/1-2`             | ❌      | Common extension                 |
| `writeln/1-2`           | ⚠️     | Only `/1` implemented (extension)|
| `format/2-3`            | ✅📘    | Extension                        |
| `write_term_to_chars/3` | ⚠️     | Minor formatting differences     |

---

## §8.13 — Stream Selection and Control

| Predicate                | Status | Notes                        |
| ------------------------ | ------ | ---------------------------- |
| `open/3-4`               | ⚠️     | Only `/3` implemented        |
| `close/1-2`              | ⚠️     | Only `/1` implemented        |
| `current_input/1`        | ✅      |                              |
| `current_output/1`       | ✅      |                              |
| `set_input/1`            | ❌      | **ISO-required**             |
| `set_output/1`           | ❌      | **ISO-required**             |
| `flush_output/0-1`       | ❌      | **ISO-required**             |
| `at_end_of_stream/0-1`   | ❌      | **ISO-required** - EOF test  |
| `stream_property/2`      | ❌      | **ISO-required**             |
| `set_stream_position/2`  | ❌      | **ISO-required** - Seek      |

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
| Arithmetic errors (`zero_divisor`, `undefined`, `float_overflow`) | ✅      | Full ISO-compliant arithmetic error handling |

---

## §8.8 — Reflection

| Predicate                             | Status | Notes                   |
| ------------------------------------- | ------ | ----------------------- |
| `current_predicate/1`                 | ✅      |                         |
| `predicate_property/2`                | ⚠️     | Built-in detection only |
| `dynamic/static/multifile` properties | ❌      | Needed for ISO tooling  |

---

## §10 — Modules (ISO Part 1)

| Feature                           | Status | Notes                                                          |
| --------------------------------- | ------ | -------------------------------------------------------------- |
| `:- module/2`                     | ✅      |                                                                |
| Module-qualified calls (`M:Goal`) | ✅      |                                                                |
| Export enforcement                | ✅      |                                                                |
| Built-ins visible in all modules  | ✅      |                                                                |
| Clause body module resolution     | ✅      | Unqualified goals resolve via imports → defining module → user |
| `use_module/1,2`                  | ✅      | Supports full and selective imports, including `library(Name)` |
| Cross-module dynamic semantics    | ⚠️     | Implemented; further test coverage recommended                 |

---

## ISO Conformance Snapshot

| Category                  | Status                                                     |
| ------------------------- | ---------------------------------------------------------- |
| Core execution model      | ✅ Strong                                                   |
| Control constructs        | ✅ Strong                                                   |
| Unification & comparison  | ✅ Strong                                                   |
| Type testing              | ✅ Strong (missing `is_list/1` extension)                   |
| Term manipulation         | ⚠️ Missing `term_variables/2`, `numbervars/3`              |
| Atom processing (§8.16)   | ✅ Strong                                                   |
| Arithmetic                | ✅ Strong                                                   |
| List operations           | ⚠️ Basic ops ✅, missing `msort/2`, `keysort/2`, nth, etc. |
| All-solutions             | ✅ Strong                                                   |
| Meta-predicates           | ⚠️ Missing `forall/2`, higher-order list ops               |
| Database operations       | ✅ Strong                                                   |
| Character I/O (§8.11)     | ⚠️ Basic ✅, missing code/peek predicates                  |
| Term I/O (§8.12)          | ⚠️ Basic read/write ✅, missing options & variants         |
| Stream control (§8.13)    | ⚠️ Basic ✅, missing flush, seek, properties               |
| Errors & exceptions       | ✅ Strong                                                   |
| Parsing & syntax          | ❌ Blocking gaps (op/3, char_conversion)                   |
| Modules                   | ✅ Largely ISO-consistent (Part 1)                          |
| Reflection                | ⚠️ Partial                                                 |

---

## ISO Blocking Issues

1. `op/3` must affect parsing (§6.3)
2. `char_conversion/2` missing (§6.4, §7.4)
3. Incorrect term ordering (§6.4.10)
4. Module-local clause resolution (§10)
5. **List sorting missing** - `msort/2`, `keysort/2` unimplemented (ISO-required)
6. **Character I/O incomplete (§8.11)** - Missing `get_code`, `put_code`, `peek_*` predicates
7. **Term I/O incomplete (§8.12)** - Missing `read_term`, `write_term`, `writeq`, `write_canonical`
8. **Stream operations incomplete (§8.13)** - Missing `flush_output`, `at_end_of_stream`, `stream_property`
