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


class TestCharacterCodeIO:
    """Tests for get_code/1-2 and put_code/1-2 predicates."""

    def test_get_code_from_stream(self, prolog: PrologInterpreter):
        """Test reading character codes from a stream."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("ABC")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Read 'A' (code 65)
            result1 = prolog.query_once(f"get_code({stream_handle}, Code1)")
            assert result1 is not None
            assert result1["Code1"] == 65

            # Read 'B' (code 66)
            result2 = prolog.query_once(f"get_code({stream_handle}, Code2)")
            assert result2 is not None
            assert result2["Code2"] == 66

            # Read 'C' (code 67)
            result3 = prolog.query_once(f"get_code({stream_handle}, Code3)")
            assert result3 is not None
            assert result3["Code3"] == 67

            # Read EOF (returns -1)
            result4 = prolog.query_once(f"get_code({stream_handle}, Code4)")
            assert result4 is not None
            assert result4["Code4"] == -1

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_put_code_to_stream(self, prolog: PrologInterpreter):
        """Test writing character codes to a stream."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Write 'H' (72), 'i' (105), '!' (33)
            assert prolog.query_once(f"put_code({stream_handle}, 72)") == {}
            assert prolog.query_once(f"put_code({stream_handle}, 105)") == {}
            assert prolog.query_once(f"put_code({stream_handle}, 33)") == {}

            prolog.query_once(f"close({stream_handle})")

            with open(temp_file, 'r') as f:
                content = f.read()
                assert content == "Hi!"
        finally:
            import os
            os.unlink(temp_file)

    def test_put_code_invalid_code(self, prolog: PrologInterpreter):
        """Test put_code with invalid character code raises domain_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Negative code
            result = prolog.query_once(f"catch(put_code({stream_handle}, -1), Error, true)")
            assert result is not None
            error_term = result["Error"]
            assert "error" in error_term
            domain_error = error_term["error"][0]
            assert "domain_error" in domain_error
            args = domain_error["domain_error"]
            assert args[0] == "character_code"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_put_code_non_integer(self, prolog: PrologInterpreter):
        """Test put_code with non-integer raises type_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Atom instead of integer
            result = prolog.query_once(f"catch(put_code({stream_handle}, abc), Error, true)")
            assert result is not None
            error_term = result["Error"]
            assert "error" in error_term
            type_error = error_term["error"][0]
            assert "type_error" in type_error

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_get_code_invalid_stream(self, prolog: PrologInterpreter):
        """Test get_code/2 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(get_code(invalid_stream, Code), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error
        args = existence_error["existence_error"]
        assert args[0] == "stream"


class TestPeekOperations:
    """Tests for peek_char/1-2, peek_code/1-2, and peek_byte/1-2 predicates."""

    def test_peek_char_from_stream(self, prolog: PrologInterpreter):
        """Test peeking at characters without consuming them."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("XYZ")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Peek at 'X' - should not consume
            result1 = prolog.query_once(f"peek_char({stream_handle}, Char1)")
            assert result1 is not None
            assert result1["Char1"] == "X"

            # Peek again - should still be 'X'
            result2 = prolog.query_once(f"peek_char({stream_handle}, Char2)")
            assert result2 is not None
            assert result2["Char2"] == "X"

            # Now actually read 'X'
            result3 = prolog.query_once(f"get_char({stream_handle}, Char3)")
            assert result3 is not None
            assert result3["Char3"] == "X"

            # Peek at 'Y'
            result4 = prolog.query_once(f"peek_char({stream_handle}, Char4)")
            assert result4 is not None
            assert result4["Char4"] == "Y"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_peek_code_from_stream(self, prolog: PrologInterpreter):
        """Test peeking at character codes without consuming them."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("AB")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Peek at 'A' (code 65) - should not consume
            result1 = prolog.query_once(f"peek_code({stream_handle}, Code1)")
            assert result1 is not None
            assert result1["Code1"] == 65

            # Peek again - should still be 65
            result2 = prolog.query_once(f"peek_code({stream_handle}, Code2)")
            assert result2 is not None
            assert result2["Code2"] == 65

            # Now actually read 'A'
            result3 = prolog.query_once(f"get_code({stream_handle}, Code3)")
            assert result3 is not None
            assert result3["Code3"] == 65

            # Peek at 'B' (code 66)
            result4 = prolog.query_once(f"peek_code({stream_handle}, Code4)")
            assert result4 is not None
            assert result4["Code4"] == 66

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_peek_byte_from_stream(self, prolog: PrologInterpreter):
        """Test peeking at bytes without consuming them."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("12")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Peek at '1' (code 49) - should not consume
            result1 = prolog.query_once(f"peek_byte({stream_handle}, Byte1)")
            assert result1 is not None
            assert result1["Byte1"] == 49

            # Peek again - should still be 49
            result2 = prolog.query_once(f"peek_byte({stream_handle}, Byte2)")
            assert result2 is not None
            assert result2["Byte2"] == 49

            # Now actually read '1'
            result3 = prolog.query_once(f"get_byte({stream_handle}, Byte3)")
            assert result3 is not None
            assert result3["Byte3"] == 49

            # Peek at '2' (code 50)
            result4 = prolog.query_once(f"peek_byte({stream_handle}, Byte4)")
            assert result4 is not None
            assert result4["Byte4"] == 50

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_peek_char_eof(self, prolog: PrologInterpreter):
        """Test peek_char at EOF returns end_of_file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            result = prolog.query_once(f"peek_char({stream_handle}, Char)")
            assert result is not None
            assert result["Char"] == "end_of_file"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_peek_code_eof(self, prolog: PrologInterpreter):
        """Test peek_code at EOF returns -1."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            result = prolog.query_once(f"peek_code({stream_handle}, Code)")
            assert result is not None
            assert result["Code"] == -1

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_peek_byte_eof(self, prolog: PrologInterpreter):
        """Test peek_byte at EOF returns -1."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            result = prolog.query_once(f"peek_byte({stream_handle}, Byte)")
            assert result is not None
            assert result["Byte"] == -1

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_peek_invalid_stream(self, prolog: PrologInterpreter):
        """Test peek operations with invalid stream raise existence_error."""
        result1 = prolog.query_once("catch(peek_char(invalid, Char), Error, true)")
        assert result1 is not None
        assert "existence_error" in result1["Error"]["error"][0]

        result2 = prolog.query_once("catch(peek_code(invalid, Code), Error, true)")
        assert result2 is not None
        assert "existence_error" in result2["Error"]["error"][0]

        result3 = prolog.query_once("catch(peek_byte(invalid, Byte), Error, true)")
        assert result3 is not None
        assert "existence_error" in result3["Error"]["error"][0]


class TestByteIO:
    """Tests for get_byte/1-2 and put_byte/1-2 predicates."""

    def test_get_byte_from_stream(self, prolog: PrologInterpreter):
        """Test reading bytes from a stream."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("abc")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Read 'a' (byte 97)
            result1 = prolog.query_once(f"get_byte({stream_handle}, Byte1)")
            assert result1 is not None
            assert result1["Byte1"] == 97

            # Read 'b' (byte 98)
            result2 = prolog.query_once(f"get_byte({stream_handle}, Byte2)")
            assert result2 is not None
            assert result2["Byte2"] == 98

            # Read 'c' (byte 99)
            result3 = prolog.query_once(f"get_byte({stream_handle}, Byte3)")
            assert result3 is not None
            assert result3["Byte3"] == 99

            # Read EOF (returns -1)
            result4 = prolog.query_once(f"get_byte({stream_handle}, Byte4)")
            assert result4 is not None
            assert result4["Byte4"] == -1

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_put_byte_to_stream(self, prolog: PrologInterpreter):
        """Test writing bytes to a stream."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Write bytes for 'OK!'
            assert prolog.query_once(f"put_byte({stream_handle}, 79)") == {}  # 'O'
            assert prolog.query_once(f"put_byte({stream_handle}, 75)") == {}  # 'K'
            assert prolog.query_once(f"put_byte({stream_handle}, 33)") == {}  # '!'

            prolog.query_once(f"close({stream_handle})")

            with open(temp_file, 'r') as f:
                content = f.read()
                assert content == "OK!"
        finally:
            import os
            os.unlink(temp_file)

    def test_put_byte_out_of_range(self, prolog: PrologInterpreter):
        """Test put_byte with out-of-range value raises domain_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Byte > 255
            result1 = prolog.query_once(f"catch(put_byte({stream_handle}, 256), Error, true)")
            assert result1 is not None
            error_term1 = result1["Error"]
            assert "error" in error_term1
            domain_error1 = error_term1["error"][0]
            assert "domain_error" in domain_error1
            args1 = domain_error1["domain_error"]
            assert args1[0] == "byte"

            # Byte < 0
            result2 = prolog.query_once(f"catch(put_byte({stream_handle}, -1), Error, true)")
            assert result2 is not None
            error_term2 = result2["Error"]
            assert "error" in error_term2
            domain_error2 = error_term2["error"][0]
            assert "domain_error" in domain_error2
            args2 = domain_error2["domain_error"]
            assert args2[0] == "byte"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_put_byte_non_integer(self, prolog: PrologInterpreter):
        """Test put_byte with non-integer raises type_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            result = prolog.query_once(f"catch(put_byte({stream_handle}, 3.14), Error, true)")
            assert result is not None
            error_term = result["Error"]
            assert "error" in error_term
            type_error = error_term["error"][0]
            assert "type_error" in type_error
            assert args[0] == "integer"

            prolog.query_once(f"close({stream_handle})")
        finally:
            import os
            os.unlink(temp_file)

    def test_get_byte_invalid_stream(self, prolog: PrologInterpreter):
        """Test get_byte/2 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(get_byte(invalid_stream, Byte), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error
        args = existence_error["existence_error"]
        assert args[0] == "stream"


class TestNlStreamVariant:
    """Tests for nl/1 predicate."""

    def test_nl_to_stream(self, prolog: PrologInterpreter):
        """Test writing newline to a specified stream."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', write, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            # Write some text and newlines
            assert prolog.query_once(f"put_char({stream_handle}, 'A')") == {}
            assert prolog.query_once(f"nl({stream_handle})") == {}
            assert prolog.query_once(f"put_char({stream_handle}, 'B')") == {}
            assert prolog.query_once(f"nl({stream_handle})") == {}

            prolog.query_once(f"close({stream_handle})")

            with open(temp_file, 'r') as f:
                content = f.read()
                assert content == "A\nB\n"
        finally:
            import os
            os.unlink(temp_file)

    def test_nl_invalid_stream(self, prolog: PrologInterpreter):
        """Test nl/1 with invalid stream raises existence_error."""
        result = prolog.query_once("catch(nl(invalid_stream), Error, true)")
        assert result is not None
        error_term = result["Error"]
        assert "error" in error_term
        existence_error = error_term["error"][0]
        assert "existence_error" in existence_error
        args = existence_error["existence_error"]
        assert args[0] == "stream"

    def test_nl_read_only_stream(self, prolog: PrologInterpreter):
        """Test nl/1 on read-only stream raises permission_error."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("test")
            temp_file = f.name

        try:
            open_result = prolog.query_once(f"open('{temp_file}', read, Stream)")
            assert open_result is not None
            stream_handle = open_result["Stream"]

            result = prolog.query_once(f"catch(nl({stream_handle}), Error, true)")
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