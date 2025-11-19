# Prolog Interpreter Test Suite

This directory contains comprehensive tests for the Prolog interpreter.

## Running Tests

Run all tests:
```bash
uv run pytest
```

Run with verbose output:
```bash
uv run pytest -v
```

Run specific test file:
```bash
uv run pytest tests/test_builtins.py
```

Run specific test class:
```bash
uv run pytest tests/test_builtins.py::TestUnification
```

Run specific test:
```bash
uv run pytest tests/test_builtins.py::TestUnification::test_unify_atoms
```

## Test Coverage

### Built-in Predicates (`test_builtins.py`)

**Unification (8 tests)**
- `=/2` - Unification of atoms, numbers, variables, compounds, and lists
- `\=/2` - Not unifiable predicate

**Arithmetic (10 tests)**
- `is/2` - Arithmetic evaluation
- Basic operations: `+`, `-`, `*`, `/`, `//`, `mod`
- Complex expressions with precedence
- Arithmetic with variables

**Comparisons (6 tests)**
- `=:=/2` - Arithmetic equality
- `</2` - Less than
- `>/2` - Greater than
- `=</2` - Less than or equal
- `>=/2` - Greater than or equal
- Comparisons with variables and expressions

**List Operations (6 tests)**
- `member/2` - List membership checking and element enumeration
- Empty lists, single elements, duplicates
- Lists with variables

**String Formatting (9 tests)**
- `format/3` - String formatting with placeholders
- `~w` - Write any term
- `~d` - Integer formatting
- `~f` - Float formatting
- `~Nf` - Float with precision
- `~n` - Newline
- `~~` - Literal tilde

**Integration Tests (5 tests)**
- Combining arithmetic with rules
- List processing with member
- Unification with arithmetic
- Multiple solutions
- Format with computed values

### Parser (`test_parser.py`)

**Basic Parsing (6 tests)**
- Atoms, variables, numbers (integers, floats, negative)
- Simple facts and compounds

**Rules (3 tests)**
- Simple rules
- Rules with multiple goals
- Rules with arithmetic

**Lists (4 tests)**
- Empty lists
- Lists with elements
- Head|Tail syntax
- Mixed types

**Arithmetic (4 tests)**
- Addition, multiplication, division
- Complex expressions with parentheses

**Comparisons (4 tests)**
- `=`, `=:=`, `<`, `>`

**Control Structures (6 tests)**
- Cut (`!`)
- Conjunction (`,`)
- Parenthesized conjunctions
- If-then (`->`)
- If-then-else (`-> ; `)
- Disjunction (`;`)

**Multiple Clauses (2 tests)**
- Multiple facts
- Mix of facts and rules

**Comments (2 tests)**
- Line comments
- Inline comments

**Strings (2 tests)**
- Regular strings
- Empty strings

**Complex Examples (2 tests)**
- Real-world examples (cans.pl)
- Meta-interpreter patterns

## Test Statistics

- **Total Tests**: 79
- **Test Files**: 2
- **Test Classes**: 15
- **All Tests Passing**: ✓

## Implemented Built-ins Tested

1. `=/2` - Unification
2. `\=/2` - Not unifiable
3. `is/2` - Arithmetic evaluation
4. `=:=/2`, `</2`, `>/2`, `=</2`, `>=/2` - Arithmetic comparisons
5. `member/2` - List membership
6. `format/3` - String formatting

## Not Yet Implemented (Not Tested)

Built-ins that parse but don't execute yet:
- `!/0` - Cut
- `true/0` - Always succeeds
- `clause/2` - Database introspection
- `call/1` - Meta-call
- `append/3` - List concatenation
- `maplist/2` - List mapping
- `writeln/1` - I/O
- `->/2`, `;/2` - If-then-else (parsed, not executed)
- `,/2` - Conjunction as callable (parsed, not fully handled)

And many standard Prolog built-ins like `atom/1`, `var/1`, `functor/3`, `findall/3`, etc.
