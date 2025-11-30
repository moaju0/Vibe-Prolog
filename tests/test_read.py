import io
from pathlib import Path

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.streams import Stream
from vibeprolog.terms import Atom


@pytest.fixture
def prolog() -> PrologInterpreter:
    return PrologInterpreter()


class SingleLineTTY(io.StringIO):
    """TTY-like stream that should only be read once."""

    def __init__(self, content: str):
        super().__init__(content)
        self.readline_calls = 0

    def readline(self, *args, **kwargs):
        self.readline_calls += 1
        if self.readline_calls > 1:
            raise AssertionError("readline called more than once")
        return super().readline(*args, **kwargs)

    def isatty(self) -> bool:
        return True


def test_read_terms_from_stream(prolog: PrologInterpreter):
    fixture_path = Path(__file__).parent / "read_terms.pl"
    open_result = prolog.query_once(f"open('{fixture_path}', read, StreamHandle)")
    assert open_result is not None
    stream_handle = open_result["StreamHandle"]

    expected_terms = [
        "hello",
        42,
        3.14,
        [1, 2, 3],
        {"parent": ["john", "doe"]},
        {"value": ["e1"]},
    ]

    for expected in expected_terms:
        result = prolog.query_once(f"read({stream_handle}, Term)")
        assert result is not None
        assert result["Term"] == expected

    eof_result = prolog.query_once(f"read({stream_handle}, Term)")
    assert eof_result is not None
    assert eof_result["Term"] == "end_of_file"

    close_result = prolog.query_once(f"close({stream_handle})")
    assert close_result == {}


def test_read_uses_current_input_stream(prolog: PrologInterpreter):
    # Initialize engine to populate the stream registry
    prolog.query_once("true.")

    input_data = io.StringIO("foo.\n[].\n")
    user_stream = Stream(handle=Atom("user_input"), file_obj=input_data, mode="read")
    prolog.engine._streams["user_input"] = user_stream

    first = prolog.query_once("read(Term)")
    assert first is not None
    assert first["Term"] == "foo"

    second = prolog.query_once("read(Term)")
    assert second is not None
    assert second["Term"] == []

    eof_result = prolog.query_once("read(Term)")
    assert eof_result is not None
    assert eof_result["Term"] == "end_of_file"


def test_read_from_tty_line_does_not_block(prolog: PrologInterpreter):
    # Initialize engine to populate the stream registry
    prolog.query_once("true.")

    tty_input = SingleLineTTY("yes.\n")
    prolog.engine._streams["user_input"] = Stream(
        handle=Atom("user_input"), file_obj=tty_input, mode="read"
    )

    result = prolog.query_once("read(Term)")
    assert result is not None
    assert result["Term"] == "yes"


def test_read_reports_syntax_error_on_incomplete_term(
    prolog: PrologInterpreter, tmp_path
):
    bad_file = tmp_path / "incomplete.pl"
    bad_file.write_text("foo(")

    open_result = prolog.query_once(f"open('{bad_file}', read, StreamHandle)")
    assert open_result is not None
    stream_handle = open_result["StreamHandle"]

    result = prolog.query_once(f"catch(read({stream_handle}, Term), Error, true)")
    assert result is not None
    error_term = result["Error"]
    assert "error" in error_term
    syntax_error = error_term["error"][0]
    assert "syntax_error" in syntax_error

    prolog.query_once(f"close({stream_handle})")


def test_read_raises_on_missing_stream(prolog: PrologInterpreter):
    result = prolog.query_once("catch(read(no_stream, Term), Error, true)")
    assert result is not None
    error_term = result["Error"]
    assert error_term["error"][0]["existence_error"][0] == "stream"
