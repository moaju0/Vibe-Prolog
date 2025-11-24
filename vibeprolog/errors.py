"""Helpers for constructing ISO-style error terms."""

from __future__ import annotations

from lark.exceptions import (
    UnexpectedCharacters,
    UnexpectedEOF,
    UnexpectedInput,
    UnexpectedToken,
)


def raise_syntax_error(context: str, exc: Exception | None = None) -> None:
    """Raise a PrologThrow carrying error(syntax_error(_), Context)."""
    from vibeprolog.exceptions import PrologThrow
    from vibeprolog.terms import Atom, Compound

    reason_atom = _syntax_error_reason(exc)
    syntax_error_term = Compound("syntax_error", (reason_atom,))
    context_atom = Atom(context)
    raise PrologThrow(Compound("error", (syntax_error_term, context_atom)))


def _syntax_error_reason(exc: Exception | None) -> "Atom":
    """Map parser exceptions to ISO syntax_error/1 reasons."""
    from vibeprolog.terms import Atom

    if exc is None:
        return Atom("syntax_error")
    if isinstance(exc, UnexpectedEOF):
        return Atom("incomplete_reduction")
    if isinstance(exc, UnexpectedCharacters):
        return Atom("unexpected_char")
    if isinstance(exc, UnexpectedToken):
        return Atom("unexpected_token")
    if isinstance(exc, UnexpectedInput):
        return Atom("parse_error")
    if isinstance(exc, ValueError):
        return Atom("parse_error")

    message = str(exc).strip()
    if not message:
        return Atom("syntax_error")
    sanitized = message.replace(" ", "_")
    return Atom(sanitized)


__all__ = ["raise_syntax_error"]
