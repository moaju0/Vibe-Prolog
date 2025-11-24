# prolog/terms.py (new file)
from __future__ import annotations
from typing import Any
from abc import ABC
from dataclasses import dataclass

class Term(ABC):
    """Base class for Prolog terms."""
    pass

@dataclass(frozen=True)
class Variable(Term):
    """A variable."""

    name: str

    def __repr__(self):
        return self.name

@dataclass(frozen=True)
class Atom(Term):
    """An atom (constant)."""

    name: str

    def __repr__(self):
        return self.name

@dataclass(frozen=True)
class Number(Term):
    """A number."""

    value: int | float

    def __repr__(self):
        return str(self.value)

@dataclass(frozen=True)
class Compound(Term):
    """A compound term (functor with arguments)."""

    functor: str
    args: tuple[Any, ...]

    def __repr__(self):
        if not self.args:
            return self.functor
        args_str = ", ".join(str(arg) for arg in self.args)
        return f"{self.functor}({args_str})"

# Export all for convenience
__all__ = ['Term', 'Variable', 'Atom', 'Number', 'Compound']