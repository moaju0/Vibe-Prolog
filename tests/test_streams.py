"""Tests for stream selection built-ins (current_input/1, current_output/1) and stream I/O (open/3, close/1)."""

import os
import tempfile
import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


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


class TestStreamIO:
    """Tests for stream I/O built-ins (open/3, close/1)."""

    @pytest.fixture
    def temp_file(self, tmp_path):
        """Create a temporary file for testing using pytest's tmp_path fixture."""
        file_path = tmp_path / "test.txt"
        file_path.write_text('test content\n')
        return str(file_path)

    @pytest.mark.parametrize("mode", ["read", "write", "append"])
    def test_open_file_modes(self, prolog: PrologInterpreter, temp_file: str, mode: str):
        f"""Test opening file in {mode} mode."""
        result = prolog.query_once(f"open('{temp_file}', {mode}, Stream)")
        assert result is not None
        assert "Stream" in result
        assert isinstance(result["Stream"], str)
        assert result["Stream"].startswith("$stream_")

        # Close the stream
        close_result = prolog.query_once(f"close({result['Stream']})")
        assert close_result == {}

    def test_open_nonexistent_file_read(self, prolog: PrologInterpreter):
        """Test opening non-existent file for reading raises existence_error."""
        result = prolog.query_once("catch(open('nonexistent_file.txt', read, Stream), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error
        args = existence_error["existence_error"]
        assert args[0] == "source_sink"
        assert args[1] == "nonexistent_file.txt"

    def test_open_invalid_mode(self, prolog: PrologInterpreter, temp_file: str):
        """Test opening file with invalid mode raises domain_error."""
        result = prolog.query_once(f"catch(open('{temp_file}', invalid_mode, Stream), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        domain_error = error_term["error"][0]
        assert "domain_error" in domain_error
        args = domain_error["domain_error"]
        assert args[0] == "io_mode"
        assert args[1] == "invalid_mode"

    def test_close_invalid_stream(self, prolog: PrologInterpreter):
        """Test closing invalid stream raises existence_error."""
        result = prolog.query_once("catch(close(invalid_stream), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error
        args = existence_error["existence_error"]
        assert args[0] == "stream"
        assert args[1] == "invalid_stream"

    @pytest.mark.parametrize("stream_name", ["user_input", "user_output", "user_error"])
    def test_close_standard_streams_fails(self, prolog: PrologInterpreter, stream_name: str):
        """Test that closing standard streams raises existence_error."""
        with pytest.raises(PrologThrow) as exc_info:
            prolog.query_once(f"close({stream_name})")

        error_term = exc_info.value.term
        assert error_term.functor == "error"
        assert error_term.args[0].functor == "existence_error"
        assert error_term.args[0].args[0].name == "stream"

    def test_stream_lifecycle(self, prolog: PrologInterpreter, temp_file: str):
        """Test complete stream lifecycle: open, verify, close, then ensure closed."""
        open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
        assert open_result is not None
        stream_handle = open_result["Stream"]
        close_result = prolog.query_once(f"close({stream_handle})")
        assert close_result == {}
        # Verify closing twice errors (existence_error on second close)
        result = prolog.query_once(f"catch(close({stream_handle}), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert error_term["error"][0]["existence_error"][0] == "stream"

    def test_multiple_streams(self, prolog: PrologInterpreter, temp_file: str):
        """Test opening multiple streams simultaneously."""
        # Open first stream
        result1 = prolog.query_once(f"open('{temp_file}', read, Stream1)")
        assert result1 is not None
        stream1 = result1["Stream1"]

        # Open second stream to same file
        result2 = prolog.query_once(f"open('{temp_file}', read, Stream2)")
        assert result2 is not None
        stream2 = result2["Stream2"]

        # Verify they have different handles
        assert stream1 != stream2
        assert stream1.startswith("$stream_")
        assert stream2.startswith("$stream_")

        # Close both
        assert prolog.query_once(f"close({stream1})") == {}
        assert prolog.query_once(f"close({stream2})") == {}

    def test_current_streams_updated(self, prolog: PrologInterpreter):
        """Test that current_input/1 and current_output/1 work with stream infrastructure."""
        # These should return the standard stream handles
        input_result = prolog.query_once("current_input(Stream)")
        assert input_result is not None
        assert input_result["Stream"] == "user_input"

        output_result = prolog.query_once("current_output(Stream)")
        assert output_result is not None
        assert output_result["Stream"] == "user_output"
