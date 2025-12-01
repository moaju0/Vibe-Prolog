# AI Tools - Development Guide

Development and testing guide for the AI automation tools in this directory.

## Goals

- Provide robust automation helpers that integrate git, GitHub, and AI coding assistants
- Ensure all integrations handle errors gracefully
- Maintain compatibility with Python stdlib and CLI tools only
- Keep tools testable without requiring live API calls

## Coding Rules

- Follow PEP 8 for Python code
  - **All imports must be at the top of the file** (after module docstring, before other code)
  - Never use `import` statements inside functions or methods
  - Group imports: standard library, third-party, local (separated by blank lines)
- Run all tests before pushing a PR
- Use type hints where helpful
- Add docstrings to all public functions
- Raise exceptions instead of calling `SystemExit()` - let the entry point handle exit codes
- Keep functions focused and testable
- Use descriptive variable names

## Testing Guidelines

### Where to Put Tests

**All test files must go in the `tests/` directory.**

- **Unit tests**: `test_<module>.py` - Tests for specific modules
- Tests should use mocking to avoid live API calls to GitHub

### Test File Naming

- Python test files: `test_*.py`
- Follow pytest conventions

### Writing Tests

1. **Use mocking for external dependencies**:
   ```python
   from unittest.mock import patch, MagicMock

   @patch("autocoder_utils.gh_pr_helper.subprocess.run")
   def test_fetch_api_success(self, mock_run):
       mock_result = MagicMock()
       mock_result.stdout = json.dumps({"id": 1})
       mock_run.return_value = mock_result
       # Test code
   ```

2. **Test error handling**:
   ```python
   @patch("autocoder_utils.gh_pr_helper.subprocess.run")
   def test_fetch_api_error(self, mock_run):
       import subprocess
       mock_run.side_effect = subprocess.CalledProcessError(1, "gh")
       with pytest.raises(GitHubAPICallError):
           fetch_api("/path")
   ```

3. **Organize tests with pytest classes**:
   ```python
   class TestFetchAPI:
       """Tests for fetch_api function."""

       def test_success(self):
           # Test code

       def test_error(self):
           # Test code
   ```

4. **Test both success and failure cases**:
   ```python
   def test_valid_pr_path(self):
       owner, repo, pr = parse_pr_path("nlothian/Vibe-Prolog/pull/10")
       assert owner == "nlothian"

   def test_invalid_pr_path(self):
       with pytest.raises(ValueError):
           parse_pr_path("invalid/path")
   ```

### Running Tests

```bash
# Run all tests
uv run pytest

# Run all tests with verbose output
uv run pytest -v

# Run specific test file
uv run pytest tests/test_gh_pr_helper.py

# Run specific test class
uv run pytest tests/test_gh_pr_helper.py::TestFetchAPI

# Run specific test
uv run pytest tests/test_gh_pr_helper.py::TestFetchAPI::test_fetch_api_success

# Run with output capture disabled (see print statements)
uv run pytest -s

# Run with coverage
uv run pytest --cov=autocoder_utils tests/
```

## Module-Specific Guidelines

### gh_pr_helper Module

The `gh_pr_helper` module provides GitHub PR comment fetching and formatting.

**Key functions:**
- `parse_pr_path(pr_path)` - Parse PR path strings
- `fetch_api(api_path)` - REST API calls via `gh cli`
- `fetch_review_comments_graphql(owner, repo, pr_number)` - GraphQL review comments with pagination
- `fetch_pr_comments(owner, repo, pr_number)` - Fetch both review and issue comments
- `format_comments_as_markdown(...)` - Format comments for display

**Error Handling:**
- Raises `GitHubAPIError` and subclasses instead of calling `SystemExit()`
- Custom exception types: `GitHubAPICallError`, `GitHubJSONError`, `GitHubGraphQLError`, `GitHubResponseError`
- Entry point (`gh_pr_helper` function) catches exceptions and handles exit codes

**Testing:**
- All subprocess calls are mocked - tests don't require GitHub authentication
- Pagination logic is tested with mock data
- Response parsing is tested with realistic GraphQL responses

## Development Workflow

### Adding a New Function

1. **Implement the function** in the appropriate module (e.g., `autocoder_utils/gh_pr_helper.py`)
2. **Add type hints and docstring** with Args, Returns, and Raises sections
3. **Raise exceptions** for error cases (don't call `SystemExit()`)
4. **Write tests** in `tests/test_<module>.py` covering:
   - Success cases
   - Failure/error cases
   - Edge cases
5. **Mock all external calls** (subprocess, API, file I/O)
6. **Run tests**:
   ```bash
   uv run pytest tests/test_<module>.py -v
   ```
7. **Update documentation** in `README.md` if adding a new public API

### Debugging Tips

1. **Run tests with output**:
   ```bash
   uv run pytest -s tests/test_something.py
   ```

2. **Use pytest's pdb integration**:
   ```bash
   uv run pytest --pdb tests/test_something.py
   ```

3. **Print mock call arguments**:
   ```python
   mock_run.assert_called_once()
   print(mock_run.call_args)
   ```

4. **Create minimal test cases**:
   - Start with the simplest scenario
   - Gradually add complexity
   - Use fixtures for reusable mock data

## Error Handling Pattern

All modules should raise exceptions rather than exiting directly:

```python
try:
    result = subprocess.run(cmd, check=True)
    return json.loads(result.stdout)
except subprocess.CalledProcessError as exc:
    raise GitHubAPICallError(f"Command failed: {exc.stderr}") from exc
except json.JSONDecodeError as exc:
    raise GitHubJSONError(f"JSON parse failed: {exc}") from exc
```

The entry point catches exceptions:

```python
def gh_pr_helper(argv=None):
    try:
        # Do work
        pass
    except GitHubAPIError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        raise SystemExit(1)
```

## Resources

- [pytest Documentation](https://docs.pytest.org/) - Testing framework
- [unittest.mock Documentation](https://docs.python.org/3/library/unittest.mock.html) - Mocking tools
- [GitHub CLI (gh)](https://cli.github.com/) - Command-line tool used by these utilities
- [GitHub GraphQL API](https://docs.github.com/en/graphql) - API reference for gh-pr-helper

## Contributing

1. Write tests first (TDD approach recommended)
2. Ensure all tests pass before committing: `uv run pytest`
3. Use mocks to avoid external dependencies
4. Update `README.md` when adding new functionality
5. Keep the codebase clean and well-organized
