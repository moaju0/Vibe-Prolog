"""Tests for character I/O predicates (get_char/1-2, put_char/1-2)."""

import io
import tempfile
import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


@pytest.fixture
def prolog() -> PrologInterpreter:
    return PrologInterpreter()


class TestGetChar:
    """Tests for get_char/1 and get_char/2 predicates."""

    def test_get_char_from_string_stream(self, prolog: PrologInterpreter):
        """Test reading characters from a string-based stream."""
        # Create a temporary file with test content
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("abc")
            temp_file = f.name

        try:
            # Open the file for reading
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Read first character
            result1 = prolog.query_once(f"get_char({stream_handle}, Char1)")
            assert result1 is not None
            assert result1["Char1"] == "a"

            # Read second character
            result2 = prolog.query_once(f"get_char({stream_handle}, Char2)")
            assert result2 is not None
            assert result2["Char2"] == "b"

            # Read third character
            result3 = prolog.query_once(f"get_char({stream_handle}, Char3)")
            assert result3 is not None
            assert result3["Char3"] == "c"

            # Read EOF
            result4 = prolog.query_once(f"get_char({stream_handle}, Char4)")
            assert result4 is not None
            assert result4["Char4"] == "end_of_file"

            # Close stream
            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_get_char_current_input_fails(self, prolog: PrologInterpreter):
        """Test that get_char/1 fails on current input (stdin) with permission_error."""
        # This should fail because stdin is captured by pytest
        result = prolog.query_once("catch(get_char(Char), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        permission_error = error_term["error"][0]
        assert "permission_error" in permission_error
        args = permission_error["permission_error"]
        assert args[0] == "input"
        assert args[1] == "stream"

    def test_get_char_invalid_stream(self, prolog: PrologInterpreter):
        """Test get_char/2 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(get_char(invalid_stream, Char), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error
        args = existence_error["existence_error"]
        assert args[0] == "stream"

    def test_get_char_write_only_stream(self, prolog: PrologInterpreter):
        """Test get_char on write-only stream raises permission_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            # Open for writing
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Try to read from write-only stream
            result = prolog.query_once(f"catch(get_char({stream_handle}, Char), Error, true)")
            assert result is not None
            error_term = result["Error"]
            assert "error" in error_term
            permission_error = error_term["error"][0]
            assert "permission_error" in permission_error
            args = permission_error["permission_error"]
            assert args[0] == "input"
            assert args[1] == "stream"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)


class TestPutChar:
    """Tests for put_char/1 and put_char/2 predicates."""

    def test_put_char_to_file(self, prolog: PrologInterpreter):
        """Test writing characters to a file stream."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            # Open the file for writing
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Write characters
            assert prolog.query_once(f"put_char({stream_handle}, 'H')") == {}
            assert prolog.query_once(f"put_char({stream_handle}, 'e')") == {}
            assert prolog.query_once(f"put_char({stream_handle}, 'l')") == {}
            assert prolog.query_once(f"put_char({stream_handle}, 'l')") == {}
            assert prolog.query_once(f"put_char({stream_handle}, 'o')") == {}

            # Close stream
            prolog.query_once(f"close({stream_handle})")

            # Verify content
            with open(temp_file, 'r') as f:
                content = f.read()
                assert content == "Hello"
        finally:
            import os
            os.unlink(temp_file)

    def test_put_char_invalid_character(self, prolog: PrologInterpreter):
        """Test put_char with invalid character raises type_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Try to write multi-character atom
            result = prolog.query_once(f"catch(put_char({stream_handle}, 'ab'), Error, true)")
            assert result is not None
            error_term = result["Error"]
            assert "error" in error_term
            type_error = error_term["error"][0]
            assert "type_error" in type_error
            args = type_error["type_error"]
            assert args[0] == "in_character"

            # Try to write non-atom
            result2 = prolog.query_once(f"catch(put_char({stream_handle}, 123), Error2, true)")
            assert result2 is not None
            error_term2 = result2["Error2"]
            assert "error" in error_term2
            type_error2 = error_term2["error"][0]
            assert "type_error" in type_error2

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_put_char_invalid_stream(self, prolog: PrologInterpreter):
        """Test put_char/2 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(put_char(invalid_stream, 'a'), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error
        args = existence_error["existence_error"]
        assert args[0] == "stream"

    def test_put_char_read_only_stream(self, prolog: PrologInterpreter):
        """Test put_char on read-only stream raises permission_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("test")
            temp_file = f.name

        try:
            # Open for reading
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Try to write to read-only stream
            result = prolog.query_once(f"catch(put_char({stream_handle}, 'a'), Error, true)")
            assert result is not None
            error_term = result["Error"]
            assert "error" in error_term
            permission_error = error_term["error"][0]
            assert "permission_error" in permission_error
            args = permission_error["permission_error"]
            assert args[0] == "output"
            assert args[1] == "stream"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_put_char_uninstantiated_args(self, prolog: PrologInterpreter):
        """Test put_char with uninstantiated arguments raises instantiation_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Uninstantiated character
            result = prolog.query_once(f"catch(put_char({stream_handle}, Char), Error, true)")
            assert result is not None
            error_term = result["Error"]
            assert "error" in error_term
            instantiation_error = error_term["error"][0]
            assert "instantiation_error" in instantiation_error

            # Uninstantiated stream
            result2 = prolog.query_once("catch(put_char(Stream, 'a'), Error2, true)")
            assert result2 is not None
            error_term2 = result2["Error2"]
            assert "error" in error_term2
            instantiation_error2 = error_term2["error"][0]
            assert "instantiation_error" in instantiation_error2

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)


class TestCharIOEdgeCases:
    """Tests for edge cases in character I/O."""

    def test_get_char_newline(self, prolog: PrologInterpreter):
        """Test reading newline character."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("a\nb")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            result1 = prolog.query_once(f"get_char({stream_handle}, Char1)")
            assert result1["Char1"] == "a"

            result2 = prolog.query_once(f"get_char({stream_handle}, Char2)")
            assert result2["Char2"] == "\n"

            result3 = prolog.query_once(f"get_char({stream_handle}, Char3)")
            assert result3["Char3"] == "b"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_put_char_special_chars(self, prolog: PrologInterpreter):
        """Test writing special characters."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Write various special characters
            assert prolog.query_once(f"put_char({stream_handle}, '\\n')") == {}
            assert prolog.query_once(f"put_char({stream_handle}, '\\t')") == {}
            assert prolog.query_once(f"put_char({stream_handle}, ' ')") == {}

            prolog.query_once(f"close({stream_handle})")

            with open(temp_file, 'r') as f:
                content = f.read()
                assert content == "\n\t "
        finally:
            import os
            os.unlink(temp_file)