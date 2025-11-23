# Autocoder Utils

This package provides shared utilities for AI-powered coding automation tools in the ai-tools directory.

## Overview

The `autocoder_utils` module contains common helper functions used by various AI coding automation scripts, including:

- Command availability checking
- Environment setup (API keys, LLM configuration)
- Git operations (staging, checking for changes, repository metadata)
- Subprocess execution with error handling

## Functions

### Command and Environment

- `check_commands_available(required)`: Verify that required CLI tools are installed
- `ensure_env()`: Ensure required environment variables are set

### Git Operations

- `stage_changes()`: Stage all changes with `git add -A`
- `has_staged_changes()`: Check if there are staged changes
- `get_repo_root()`: Get the root directory of the git repository
- `get_owner_repo()`: Extract owner and repository name from git remote URL

### Utilities

- `run(cmd, input_text=None, capture_output=True)`: Execute a command with proper error handling

## Usage

```python
from autocoder_utils import check_commands_available, ensure_env, run

# Check for required tools
check_commands_available(["gh", "llm", "kilocode"])

# Ensure environment is set up
ensure_env()

# Run a command
output = run(["git", "status"])
```

## Dependencies

This module has no external dependencies beyond Python's standard library.

## Testing

Tests are located in `tests/test_autocoder_utils.py` and can be run with:

```bash
uv run pytest tests/test_autocoder_utils.py