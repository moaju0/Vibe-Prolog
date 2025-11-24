"""Tests for stream selection built-ins (current_input/1, current_output/1)."""

import pytest
from vibeprolog import PrologInterpreter


@pytest.fixture
def prolog() -> PrologInterpreter:
    return PrologInterpreter()


class TestStreams:
    """Tests for current_input/1 and current_output/1 predicates."""

    @pytest.mark.parametrize(
        "predicate, expected_atom",
        [
            ("current_input", "user_input"),
            ("current_output", "user_output"),
        ],
    )
    def test_bind_variable(self, prolog: PrologInterpreter, predicate: str, expected_atom: str):
        f"""Test {predicate}(X) binds X to {expected_atom}."""
        result = prolog.query_once(f"{predicate}(X)")
        assert result is not None
        assert result["X"] == expected_atom

    @pytest.mark.parametrize(
        "predicate, expected_atom",
        [
            ("current_input", "user_input"),
            ("current_output", "user_output"),
        ],
    )
    def test_ground_success(self, prolog: PrologInterpreter, predicate: str, expected_atom: str):
        f"""Test {predicate}({expected_atom}) succeeds."""
        assert prolog.has_solution(f"{predicate}({expected_atom})")

    @pytest.mark.parametrize(
        "predicate", ["current_input", "current_output"]
    )
    def test_ground_failure(self, prolog: PrologInterpreter, predicate: str):
        f"""Test {predicate}(foo) fails."""
        assert not prolog.has_solution(f"{predicate}(foo)")

    @pytest.mark.parametrize(
        "predicate, expected_atom",
        [
            ("current_input", "user_input"),
            ("current_output", "user_output"),
        ],
    )
    def test_deterministic(self, prolog: PrologInterpreter, predicate: str, expected_atom: str):
        f"""Test {predicate}/1 yields exactly one solution."""
        results = prolog.query(f"{predicate}(X)")
        assert len(results) == 1
        assert results[0]["X"] == expected_atom
