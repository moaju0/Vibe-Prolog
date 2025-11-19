# Prolog Interpreter - Development Guide

This document provides an overview of the project structure and guidelines for development and testing.

## Project Structure

```
prolog/
├── prolog/                  # Main source code
│   ├── __init__.py         # Package initialization, exports PrologInterpreter
│   ├── interpreter.py      # Main interpreter interface (PrologInterpreter class)
│   ├── engine.py          # Query engine with backtracking and built-ins
│   ├── parser.py          # Prolog parser using Lark
│   └── unification.py     # Unification algorithm
│
├── tests/                  # All test files go here
│   ├── __init__.py
│   └── test_*.py           #  test files
│   └── *.pl                # Prolog test fixtures
│
```

## Built-in Predicates

### Core Unification and Comparison
- `=/2` - Unification
- `\=/2` - Not unifiable
- `=:=/2, </2, >/2, =</2, >=/2` - Arithmetic comparisons

### Arithmetic
- `is/2` - Arithmetic evaluation
- Operators: `+, -, *, /, //, mod`

### Type Checking
- `fail/0` - Always fails
- `atom/1` - Check if term is an atom
- `number/1` - Check if term is a number
- `var/1` - Check if term is an unbound variable
- `nonvar/1` - Check if term is not an unbound variable

### Term Manipulation
- `functor/3` - Extract or construct functor (e.g., `functor(foo(a,b), F, N)` gives F=foo, N=2)
- `arg/3` - Access Nth argument of compound term (1-based indexing)
- `=../2` (univ) - Convert between term and list (e.g., `foo(a,b) =.. [foo, a, b]`)

### List Operations
- `member/2` - List membership
- `append/3` - List concatenation
- `length/2` - List length (bidirectional: compute or generate)
- `reverse/2` - Reverse a list
- `sort/2` - Sort list and remove duplicates

### Solution Collection
- `findall/3` - Collect all solutions to a goal (returns empty list if none)
- `bagof/3` - Collect solutions with duplicates (fails if no solutions)
- `setof/3` - Collect unique sorted solutions (fails if no solutions)

### Database Modification
- `assert/1` - Add a fact to the database dynamically
- `retract/1` - Remove a clause from the database (backtracks over matching clauses)

### Meta-Predicates
- `clause/2` - Retrieve clause bodies from the knowledge base
- `call/1` - Call a goal dynamically
- `once/1` - Call a goal and commit to first solution (prevents backtracking into the goal)
- `true/0` - Always succeeds
- `!/0` - Cut operator (prevents backtracking)

### Control Flow
- `;/2` - Disjunction (logical OR)
- `->/2` - If-then
- `(Cond -> Then ; Else)` - If-then-else pattern
- `,/2` - Conjunction (logical AND) - handles explicit conjunction in goals

### I/O
- `format/2` - Print formatted string to stdout (e.g., `format("Value: ~w", [X])`)
- `format/3` - String formatting with format specifiers (~w, ~d, ~f, ~n, ~~) to atom
- `write/1` - Print term without newline
- `writeln/1` - Print term with newline
- `nl/0` - Print newline

### Higher-Order
- `maplist/2` - Apply a goal to each element of a list

## Testing Guidelines

### Where to Put Tests

**All test files must go in the `tests/` directory.**

- **Unit tests**: `test_<module>.py` - Tests for specific modules
- **Integration tests**: `test_<feature>.py` - Tests for features spanning multiple modules
- **Fixtures**: `*.pl` files - Prolog code used by tests

### Test File Naming

- Python test files: `test_*.py`
- Prolog fixtures: `test_*.pl` or descriptive names like `minimal_mi_test.pl`
- Follow pytest conventions

### Writing Tests

1. **Import the interpreter**:
   ```python
   from prolog import PrologInterpreter
   ```

2. **Create a new interpreter for each test**:
   ```python
   def test_something():
       prolog = PrologInterpreter()
       # Your test code
   ```

3. **Use pytest test classes to organize related tests**:
   ```python
   class TestAppend:
       """Tests for append/3 predicate."""

       def test_append_two_lists(self):
           prolog = PrologInterpreter()
           result = prolog.query_once("append([1, 2], [3, 4], X).")
           assert result['X'] == [1, 2, 3, 4]
   ```

4. **Test both success and failure cases**:
   ```python
   def test_unification_success(self):
       prolog = PrologInterpreter()
       assert prolog.has_solution("X = 5")

   def test_unification_failure(self):
       prolog = PrologInterpreter()
       assert not prolog.has_solution("1 = 2")
   ```

### Running Tests

```bash
# Run all tests
uv run pytest

# Run specific test file
uv run pytest tests/test_builtins.py

# Run specific test class
uv run pytest tests/test_builtins.py::TestAppend

# Run specific test
uv run pytest tests/test_builtins.py::TestAppend::test_append_two_lists

# Run with verbose output
uv run pytest -v

# Run with output capture disabled (see print statements)
uv run pytest -s
```


## Development Workflow

This is standard ISO prolog implementation. The parser should parse standard prolog syntax.

### Adding a New Built-in

1. **Implement in `prolog/engine.py`**:
   - Add handler in `_try_builtin()` method
   - Implement the built-in as a private method (e.g., `_builtin_append()`)

2. **Write tests in `tests/test_new_builtins.py`** (or create new test file):
   - Test basic functionality
   - Test edge cases
   - Test integration with other predicates

3. **Document in this file** (CLAUDE.md):
   - Add to the "Built-in Predicates" section
   - Include description and arity

4. **Run tests**:
   ```bash
   uv run pytest tests/test_new_builtins.py -v
   ```

### Debugging Tips

1. **Use print statements in built-ins**:
   ```python
   def _builtin_something(self, arg, subst):
       print(f"Debug: arg={arg}, subst={subst}")
       # ...
   ```

2. **Create minimal test cases**:
   - Start with the simplest possible query
   - Gradually add complexity
   - Put test files in `tests/` directory

3. **Check parser output**:
   ```python
   from prolog.parser import PrologParser
   parser = PrologParser()
   result = parser.parse("your_query.")
   print(result)
   ```

## Known Issues


## Code Style

- Follow PEP 8 for Python code
- Use type hints where helpful
- Add docstrings to all public methods
- Keep functions focused and testable
- Use descriptive variable names

## Resources

- [SWI-Prolog Documentation](https://www.swi-prolog.org/pldoc/man?section=builtin) - Reference for built-in predicates
- [pytest Documentation](https://docs.pytest.org/) - Testing framework
- [Lark Parser](https://lark-parser.readthedocs.io/) - Parser generator used for Prolog syntax

## Contributing

1. Write tests first (TDD approach recommended)
2. Ensure all tests pass before committing
3. Update this documentation when adding features
4. Keep the codebase clean and well-organized
