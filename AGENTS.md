# Goals

These goals are what we are working towards in this project:
- A complete, widely compatible Prolog implementation
- Robust: It should handle all kinds of error conditions gracefully
- Fast: It should run prolog programs fast
- Scalable: It should be able to run large prolog programs

# Coding rules
- Follow PEP8
- Run all tests before pushing a PR

# Prolog Interpreter - Development Guide

This document provides an overview of the project structure and guidelines for development and testing.

## Project Structure

See @ARCHITECTURE.md

## Features

See @FEATURES.md

You must keep ./FEATURES.md up to date when you add or change these


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

1. **Choose a module in `prolog/builtins/`** that fits (e.g., `arithmetic.py`, `list_ops.py`, `type_tests.py`).
2. **Implement a static handler** with the signature `(args, subst, engine)` and annotate return types.
3. **Register the predicate** in the module's `register(registry, engine_ref=None)` using `register_builtin`.
4. **Write tests in `tests/`** covering success and failure cases.
5. **Update documentation** (`CLAUDE.md`/`FEATURES.md`) to reflect the new predicate.
6. **Run tests**:
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
