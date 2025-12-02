import pytest
from vibeprolog import PrologInterpreter


@pytest.fixture
def prolog():
    """Provides a PrologInterpreter instance for tests."""
    return PrologInterpreter()