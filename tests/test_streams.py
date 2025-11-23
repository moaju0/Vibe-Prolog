"""Tests for stream selection built-ins (current_input/1, current_output/1)."""

import pytest
from prolog import PrologInterpreter


class TestStreams:
    """Tests for current_input/1 and current_output/1 predicates."""

    def test_current_input_bind_variable(self):
        """Test current_input(X) binds X to user_input."""
        prolog = PrologInterpreter()
        result = prolog.query_once("current_input(X)")
        assert result is not None
        assert result['X'] == 'user_input'

    def test_current_output_bind_variable(self):
        """Test current_output(X) binds X to user_output."""
        prolog = PrologInterpreter()
        result = prolog.query_once("current_output(X)")
        assert result is not None
        assert result['X'] == 'user_output'

    def test_current_input_ground_success(self):
        """Test current_input(user_input) succeeds."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("current_input(user_input)")

    def test_current_output_ground_success(self):
        """Test current_output(user_output) succeeds."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("current_output(user_output)")

    def test_current_input_ground_failure(self):
        """Test current_input(foo) fails."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("current_input(foo)")

    def test_current_output_ground_failure(self):
        """Test current_output(foo) fails."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("current_output(foo)")

    def test_current_input_deterministic(self):
        """Test current_input/1 yields exactly one solution."""
        prolog = PrologInterpreter()
        results = prolog.query("current_input(X)")
        assert len(results) == 1
        assert results[0]['X'] == 'user_input'

    def test_current_output_deterministic(self):
        """Test current_output/1 yields exactly one solution."""
        prolog = PrologInterpreter()
        results = prolog.query("current_output(X)")
        assert len(results) == 1
        assert results[0]['X'] == 'user_output'