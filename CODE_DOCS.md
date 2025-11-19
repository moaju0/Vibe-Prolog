# Simple Prolog Interpreter

A minimal Prolog interpreter implementation in Python, capable of solving logic puzzles and demonstrating logical inference.

## Features

- **Unification**: Robinson's unification algorithm with occurs check
- **Backtracking**: Full backtracking search for finding all solutions
- **Built-in Predicates**:
  - **Unification**: `=/2`, `\=/2`
  - **Type Checking**: `var/1`, `nonvar/1`, `atom/1`, `number/1`, `integer/1`, `compound/1`
  - **Arithmetic**: `is/2` with operators `+`, `-`, `*`, `/`, `//`, `mod`, `**` (power)
  - **Comparisons**: `=:=/2`, `</2`, `>/2`, `=</2`, `>=/2`
  - **Lists**: `member/2`, `append/3`, `length/2`, `reverse/2`, `sort/2`
  - **Term Manipulation**: `functor/3`, `arg/3`, `=../2` (univ)
  - **Control Flow**: `!/0` (cut), `true/0`, `fail/0`, `;/2` (or), `->/2` (if-then), `,/2` (and)
  - **Meta-predicates**: `call/1`, `once/1`, `clause/2`
  - **Solution Collection**: `findall/3`, `bagof/3`, `setof/3`
  - **Database**: `assert/1`, `retract/1`
  - **I/O**: `write/1`, `writeln/1`, `nl/0`, `format/2`, `format/3`
  - **Higher-Order**: `maplist/2`
- **Advanced Parser Features**:
  - Binary (`0b1011`), octal (`0o755`), and hex (`0xFF`) number literals
  - Scientific notation (`1.0e-10`)
  - Character codes (`0'a`, `0'\n`, `0'''`)
  - Block comments (`/* ... */`) and line comments (`% ...`)
  - Curly braces syntax sugar (`{X}` → `{}(X)`)
  - Special atoms (`[]`, `{}`, `:-`, operators as atoms)
- **Data Types**: Atoms, integers, floats, variables, lists, and compound terms
- **Python Integration**: Use as a library from Python code
- **Comprehensive Test Suite**: 69+ passing tests covering ISO Prolog core functionality


## Usage

### Running the Demo

```bash
uv run main.py
```

### Running Tests

Run the comprehensive test suite:

```bash
# Run all tests
uv run pytest

# Run with verbose output
uv run pytest -v

# Run specific test file
uv run pytest tests/test_builtins.py
uv run pytest tests/test_parser.py
```

See [tests/README.md](tests/README.md) for detailed test documentation.

### Using as a Library

```python
from prolog import PrologInterpreter

# Create interpreter instance
prolog = PrologInterpreter()

# Load Prolog code from string
prolog.consult_string("""
    parent(tom, bob).
    parent(tom, liz).
    grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
""")

# Query for all solutions
results = prolog.query("grandparent(tom, Who)")
for result in results:
    print(f"Tom's grandchild: {result['Who']}")

# Query for first solution only
result = prolog.query_once("parent(tom, X)")
if result:
    print(f"Tom's child: {result['X']}")

# Check if query has a solution
if prolog.has_solution("parent(tom, bob)"):
    print("Tom is bob's parent")
```

### Loading from Files

```python
prolog = PrologInterpreter()
prolog.consult("my_rules.pl")
results = prolog.query("my_query(X)")
```


**ISO Core Test Suite:**
- 69 out of 76 tests passing (**90.8% success rate**)
- Covers unification, type checking, arithmetic, lists, control flow, meta-predicates, and more
- Only 6 failing tests, all related to obscure parser edge cases
- All core Prolog functionality tests pass

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

## Architecture

The interpreter consists of four main components:

1. **Parser** ([prolog/parser.py](prolog/parser.py)) - Uses Lark to parse Prolog syntax
   - Supports ISO Prolog syntax including advanced number formats
   - Handles character codes, escape sequences, and special atoms
   - Full operator precedence support (including `**` power operator)
   - Block and line comment parsing
2. **Unification** ([prolog/unification.py](prolog/unification.py)) - Robinson's unification algorithm
3. **Engine** ([prolog/engine.py](prolog/engine.py)) - Backtracking search with built-in predicates
   - 40+ built-in predicates covering ISO Prolog core
   - Proper cut semantics and control flow
   - Dynamic predicate assertion/retraction
4. **Interpreter** ([prolog/interpreter.py](prolog/interpreter.py)) - Main API for loading and querying

## Limitations

This is a Prolog interpreter implementing most of the ISO Prolog core standard, designed for educational purposes and logic programming.

**Not supported:**
- File I/O predicates (`see/1`, `seen/0`, `read/1` from files)
- Definite clause grammars (DCG)
- Modules or namespaces
- Some advanced meta-predicates (`copy_term/2`, `term_variables/2`)
- Constraint logic programming (CLP)
- Tail call optimization (may hit Python recursion limits on deep recursion)
- Some obscure parser edge cases:
  - Hex character codes with specific escaping patterns
  - Base notation in arithmetic expressions (`16'mod'2`)
  - Complex operator precedence in certain contexts

**Known Issues:**
- Occurs check is not performed by default in unification (standard Prolog behavior)
- For very deep recursion, the interpreter may encounter Python's stack depth limit
