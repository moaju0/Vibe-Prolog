"""Tests for stream control built-ins (set_input/1, set_output/1, flush_output/0-1, at_end_of_stream/0-1, stream_property/2, set_stream_position/2)."""

import tempfile
import pytest
from vibeprolog import PrologInterpreter


@pytest.fixture
def prolog() -> PrologInterpreter:
    return PrologInterpreter()


class TestStreamSwitching:
    """Tests for set_input/1 and set_output/1 predicates."""

    def test_set_input_invalid_stream(self, prolog: PrologInterpreter):
        """Test set_input/1 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(set_input(invalid_stream), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error

    def test_set_output_invalid_stream(self, prolog: PrologInterpreter):
        """Test set_output/1 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(set_output(invalid_stream), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error


class TestFlushOutput:
    """Tests for flush_output/0-1 predicates."""

    def test_flush_output_current(self, prolog: PrologInterpreter):
        """Test flush_output/0 flushes current output stream."""
        # This should succeed without error
        result = prolog.query_once("flush_output")
        assert result == {}

    def test_flush_output_invalid_stream(self, prolog: PrologInterpreter):
        """Test flush_output/1 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(flush_output(invalid_stream), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error


class TestAtEndOfStream:
    """Tests for at_end_of_stream/0-1 predicates."""

    def test_at_end_of_stream_current_false(self, prolog: PrologInterpreter):
        """Test at_end_of_stream/0 on current input (should be false for stdin)."""
        # Current input is stdin, which is not at EOF
        assert not prolog.has_solution("at_end_of_stream")


class TestStreamProperty:
    """Tests for stream_property/2 predicate."""

    def test_stream_property_user_input(self, prolog: PrologInterpreter):
        """Test stream_property/2 on user_input stream."""
        results = list(prolog.query("stream_property(user_input, Property)"))
        assert len(results) > 0

        # Check that we get some properties
        properties = [result["Property"] for result in results]
        property_names = [list(p.keys())[0] for p in properties]

        # Should have at least one property
        assert len(property_names) > 0
        assert "type" in property_names

    def test_stream_property_all_streams(self, prolog: PrologInterpreter):
        """Test stream_property/2 enumerates all streams."""
        results = list(prolog.query("stream_property(Stream, Property)"))
        assert len(results) > 0

        # Should find at least the standard streams
        streams_found = set()
        for result in results:
            streams_found.add(result["Stream"])

        assert "user_input" in streams_found
        # Note: user_output may not be found if no output streams are open

    def test_stream_property_specific_property(self, prolog: PrologInterpreter):
        """Test stream_property/2 with specific property."""
        results = list(prolog.query("stream_property(user_input, mode(Mode))"))
        assert len(results) == 1
        assert results[0]["Mode"] == "read"


class TestSetStreamPosition:
    """Tests for set_stream_position/2 predicate."""

    def test_set_stream_position_invalid_stream(self, prolog: PrologInterpreter):
        """Test set_stream_position/2 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(set_stream_position(invalid_stream, 0), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error

    def test_set_stream_position_invalid_position(self, prolog: PrologInterpreter):
        """Test set_stream_position/2 with invalid position raises domain_error."""
        result = prolog.query_once("catch(set_stream_position(user_input, -1), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        domain_error = error_term["error"][0]
        assert "domain_error" in domain_error


class TestOpenCloseOptions:
    """Tests for open/4 and close/2 with options."""

    def test_open_with_options(self, prolog: PrologInterpreter, tmp_path):
        """Test open/4 with options (currently ignored but should not error)."""
        file_path = tmp_path / "test.txt"
        file_path.write_text('test content\n')

        result = prolog.query_once(f"open('{file_path}', read, Stream, [])")
        assert result is not None
        assert "Stream" in result

        # Close the stream
        close_result = prolog.query_once(f"close({result['Stream']}, [])")
        assert close_result == {}

    def test_close_with_options(self, prolog: PrologInterpreter, tmp_path):
        """Test close/2 with options (currently ignored but should not error)."""
        file_path = tmp_path / "test.txt"
        file_path.write_text('test content\n')

        open_result = prolog.query_once(f"open('{file_path}', read, Stream)")
        assert open_result is not None

        # Close with options
        close_result = prolog.query_once(f"close({open_result['Stream']}, [])")
        assert close_result == {}