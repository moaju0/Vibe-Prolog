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
| ISO standard term order          | ❌      | Lists treated separately (explicitly defined for deterministic ordering; ISO requires lists ⊂ compounds) |

---

## §8.3 — Type Testing

| Predicate    | Status | Notes |
| ------------ | ------ | ----- |
| `var/1`      | ✅      |       |
| `nonvar/1`   | ✅      |       |
| `atom/1`     | ✅      |       |
| `number/1`   | ✅      |       |
| `integer/1`  | ✅      |       |
| `float/1`    | ✅      |       |
| `atomic/1`   | ✅      |       |
| `compound/1` | ✅      |       |
| `callable/1` | ✅      |       |
| `ground/1`   | ✅      |       |

---

## §9 — Arithmetic

| Feature                           | Status | Notes |
| --------------------------------- | ------ | ----- |
| `is/2`                            | ✅      |       |
| Arithmetic comparison operators   | ✅      |       |
| `+ - * / // mod`                  | ✅      |       |
| `abs/1`                           | ✅      |       |
| `min/2`, `max/2`                  | ✅      |       |
| `sqrt/1`                          | ✅      |       |
| Trig / exp / log                  | ✅      |       |
| `floor/1`, `ceiling/1`, `round/1` | ✅      |       |

---

## §8.10 — All-Solutions Predicates

| Predicate   | Status | Notes                  |
| ----------- | ------ | ---------------------- |
| `findall/3` | ✅      |                        |
| `bagof/3`   | ✅      | Correct quantification |
| `setof/3`   | ✅      | ISO semantics          |

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

## §8.11–§8.12 — Input / Output

| Predicate               | Status | Notes                        |
| ----------------------- | ------ | ---------------------------- |
| `read/1`, `read/2`      | ✅      |                              |
| `write/1`               | ✅      |                              |
| `writeln/1`             | ✅      |                              |
| `nl/0`                  | ✅      |                              |
| `format/2,3`            | ✅📘    | Extension                    |
| `get_char/1`            | ✅      |                              |
| `put_char/1`            | ✅      |                              |
| `open/3`                | ✅      |                              |
| `close/1`               | ✅      |                              |
| `current_input/1`       | ✅      |                              |
| `current_output/1`      | ✅      |                              |
| `write_term_to_chars/3` | ⚠️     | Minor formatting differences |

---

## §8.12 — Errors & Exceptions

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

| Category               | Status                            |
| ---------------------- | --------------------------------- |
| Core execution model   | ✅ Strong                          |
| Built-ins & arithmetic | ✅ Strong                          |
| Errors & exceptions    | ✅ Strong                          |
| Parsing & syntax       | ❌ Blocking gaps                   |
| Modules                | ✅ Largely ISO-consistent (Part 1) |
| Reflection             | ⚠️ Partial                        |

---

## ISO Blocking Issues

1. `op/3` must affect parsing (§6.3)
2. `char_conversion/2` missing (§6.4, §7.4)
3. Incorrect term ordering (§6.4.10)
4. Module-local clause resolution (§10)
