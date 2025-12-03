import pytest
from vibeprolog import PrologInterpreter


@pytest.fixture
def prolog():
    """Provides a PrologInterpreter instance for tests."""
    return PrologInterpreter()


def pytest_addoption(parser: pytest.Parser) -> None:
    """Add custom command line options."""
    parser.addoption(
        "--run-performance",
        action="store_true",
        default=False,
        help="Run performance test suite (disabled by default).",
    )


def pytest_configure(config: pytest.Config) -> None:
    """Register custom markers."""
    config.addinivalue_line(
        "markers",
        "performance: marks tests as performance heavy; skipped unless --run-performance is provided",
    )


def pytest_collection_modifyitems(config: pytest.Config, items: list[pytest.Item]) -> None:
    """Skip performance tests unless explicitly requested."""
    if config.getoption("--run-performance"):
        return

    skip_perf = pytest.mark.skip(reason="use --run-performance to run performance tests")
    for item in items:
        if "performance" in item.keywords:
            item.add_marker(skip_perf)
