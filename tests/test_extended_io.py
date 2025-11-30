"""Tests for extended I/O predicates."""

import pytest
import os
import tempfile
from vibeprolog import PrologInterpreter


class TestClassicIO:
    """Test see/seen and tell/told."""

    def test_see_and_read(self):
        """see/1 opens file for input."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.pl') as f:
            f.write("fact(hello).\n")
            f.write("fact(world).\n")
            filename = f.name

        try:
            prolog = PrologInterpreter()
            prolog.query_once(f"see('{filename}')")

            # Read from file
            result = prolog.query_once("read(X)")
            assert result == {'X': {'fact': ['hello']}}

            prolog.query_once("seen")
        finally:
            os.unlink(filename)

    def test_tell_and_write(self):
        """tell/1 opens file for output."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            filename = f.name

        try:
            prolog = PrologInterpreter()
            prolog.query_once(f"tell('{filename}')")
            prolog.query_once("write(hello)")
            prolog.query_once("nl")
            prolog.query_once("told")

            # Read back
            with open(filename, 'r') as f:
                content = f.read()
            assert 'hello' in content
        finally:
            os.unlink(filename)

    def test_see_nonexistent_file(self):
        """see/1 with nonexistent file raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("see('/nonexistent/file.txt')")
        assert "existence_error" in str(exc_info.value)


class TestCharacterCodeIO:
    """Test get/put for character codes."""

    def test_put_writes_code(self, capsys):
        """put/1 writes character code."""
        prolog = PrologInterpreter()
        result = prolog.query_once("put(65)")  # 'A'
        assert result is not None
        captured = capsys.readouterr()
        assert captured.out == 'A'

    def test_put_invalid_code(self):
        """put/1 with invalid code raises error."""
        prolog = PrologInterpreter()
        with pytest.raises(Exception) as exc_info:
            prolog.query_once("put(-1)")
        assert "representation_error" in str(exc_info.value)


class TestStreamProperties:
    """Test stream_property/2."""

    def test_query_stream_properties(self):
        """stream_property/2 enumerates streams."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            filename = f.name

        try:
            prolog = PrologInterpreter()
            # Open and query in one session
            results = list(prolog.query(f"open('{filename}', write, S), stream_property(S, P), close(S)"))
            assert len(results) > 0
        finally:
            os.unlink(filename)


class TestEOFDetection:
    """Test at_end_of_stream."""

    def test_at_end_of_stream_empty_file(self):
        """at_end_of_stream/0 succeeds on empty file."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            filename = f.name

        try:
            prolog = PrologInterpreter()
            prolog.query_once(f"see('{filename}')")

            assert prolog.has_solution("at_end_of_stream")

            prolog.query_once("seen")
        finally:
            os.unlink(filename)


class TestFlushOutput:
    """Test flush_output."""

    def test_flush_output_succeeds(self):
        """flush_output/0 succeeds."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("flush_output")