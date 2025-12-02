"""Operator table and helpers for dynamic operator directives."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Tuple

from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.operator_defaults import DEFAULT_OPERATORS
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.utils.list_utils import list_to_python


@dataclass(frozen=True)
class OperatorInfo:
    """Operator metadata."""

    precedence: int
    spec: str  # One of: xfx, xfy, yfx, fx, fy, xf, yf

    @property
    def is_prefix(self) -> bool:
        return self.spec in ("fx", "fy")

    @property
    def is_postfix(self) -> bool:
        return self.spec in ("xf", "yf")

    @property
    def is_infix(self) -> bool:
        return self.spec in ("xfx", "xfy", "yfx", "yfy")


class OperatorTable:
    """Holds operator definitions and validates op/3 directives."""

    def __init__(self) -> None:
        self._table: dict[tuple[str, str], OperatorInfo] = {}
        self._protected_ops: set[str] = {",", ";", "->", ":-", ":", "|", "{}"}
        self._seed_defaults()

    def _seed_defaults(self) -> None:
        """Populate ISO-ish default operators."""
        for precedence, spec, name in DEFAULT_OPERATORS:
            self._table[(name, spec)] = OperatorInfo(precedence, spec)

    def clone(self) -> "OperatorTable":
        clone = OperatorTable()
        clone._table = dict(self._table)
        return clone

    def get_matching(self, name: str) -> list[OperatorInfo]:
        """Return all OperatorInfo entries for a given operator name."""
        return [info for (op, _), info in self._table.items() if op == name]

    def iter_current_ops(self) -> Iterable[Tuple[str, OperatorInfo]]:
        for (name, _), info in sorted(self._table.items(), key=lambda item: (item[1].precedence, item[0])):
            yield name, info

    def lookup(self, name: str, spec: str) -> OperatorInfo | None:
        return self._table.get((name, spec))

    def define(self, precedence_term, spec_term, name_term, context: str) -> None:
        """Apply op/3 directive semantics."""
        precedence_value = self._parse_precedence(precedence_term, context)
        spec = self._parse_specifier(spec_term, context)
        names = self._parse_operator_names(name_term, context)

        for name in names:
            self._define_single(precedence_value, spec, name, context)

    def _parse_precedence(self, precedence_term, context: str) -> int:
        if isinstance(precedence_term, Variable):
            error_term = PrologError.instantiation_error(context)
            raise PrologThrow(error_term)
        if not isinstance(precedence_term, Number):
            error_term = PrologError.type_error("integer", precedence_term, context)
            raise PrologThrow(error_term)
        if not isinstance(precedence_term.value, int):
            error_term = PrologError.type_error("integer", precedence_term, context)
            raise PrologThrow(error_term)
        precedence = precedence_term.value
        if precedence < 0 or precedence > 1200:
            error_term = PrologError.domain_error("operator_priority", precedence_term, context)
            raise PrologThrow(error_term)
        return precedence

    def _parse_specifier(self, spec_term, context: str) -> str:
        valid_specs = {"xfx", "xfy", "yfx", "yfy", "fx", "fy", "xf", "yf"}
        if isinstance(spec_term, Variable):
            error_term = PrologError.instantiation_error(context)
            raise PrologThrow(error_term)
        if not isinstance(spec_term, Atom):
            error_term = PrologError.type_error("atom", spec_term, context)
            raise PrologThrow(error_term)
        spec = spec_term.name
        if spec not in valid_specs:
            error_term = PrologError.domain_error("operator_specifier", spec_term, context)
            raise PrologThrow(error_term)
        return spec

    def _parse_operator_names(self, name_term, context: str) -> list[str]:
        def _flatten_symbol_term(term):
            """Flatten nested unary operator terms into a symbol name."""
            if isinstance(term, Atom):
                return term.name
            if isinstance(term, Compound) and len(term.args) == 1:
                tail = _flatten_symbol_term(term.args[0])
                if tail is None:
                    return None
                return f"{term.functor}{tail}"
            return None

        if isinstance(name_term, Variable):
            error_term = PrologError.instantiation_error(context)
            raise PrologThrow(error_term)
        if isinstance(name_term, Atom):
            return [name_term.name]
        if isinstance(name_term, List):
            try:
                elements = list_to_python(name_term)
            except TypeError:
                error_term = PrologError.type_error("list", name_term, context)
                raise PrologThrow(error_term)

            names: list[str] = []
            for element in elements:
                if isinstance(element, Variable):
                    error_term = PrologError.instantiation_error(context)
                    raise PrologThrow(error_term)
                symbol_name = _flatten_symbol_term(element)
                if symbol_name is None:
                    error_term = PrologError.type_error("atom", element, context)
                    raise PrologThrow(error_term)
                names.append(symbol_name)
            return names

        flattened = _flatten_symbol_term(name_term)
        if flattened is not None:
            return [flattened]

        error_term = PrologError.type_error("atom", name_term, context)
        raise PrologThrow(error_term)

    def _define_single(self, precedence_value: int, spec: str, name: str, context: str) -> None:
        key = (name, spec)
        if name in self._protected_ops:
            error_term = PrologError.permission_error("modify", "operator", Atom(name), context)
            raise PrologThrow(error_term)

        if precedence_value == 0:
            self._table.pop(key, None)
            return

        self._table[key] = OperatorInfo(precedence_value, spec)


__all__ = ["OperatorInfo", "OperatorTable"]
