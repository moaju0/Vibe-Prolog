"""Operator built-ins: op/3 and current_op/3."""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.terms import Atom, Number, Variable
from vibeprolog.unification import Substitution, deref, unify


class OperatorBuiltins:
    """Expose operator directives as callable predicates."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        register_builtin(registry, "op", 3, OperatorBuiltins._builtin_op)
        register_builtin(registry, "current_op", 3, OperatorBuiltins._builtin_current_op)

    @staticmethod
    def _builtin_op(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        precedence_term, spec_term, name_term = args
        precedence_term = deref(precedence_term, subst)
        spec_term = deref(spec_term, subst)
        name_term = deref(name_term, subst)

        module_name = None
        if hasattr(engine, "interpreter") and engine.interpreter:
            module_name = engine.interpreter.current_module

        engine.operator_table.define(
            precedence_term, spec_term, name_term, "op/3", module_name=module_name
        )

        return subst

    @staticmethod
    def _builtin_current_op(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        precedence_term, spec_term, name_term = args
        precedence_term = deref(precedence_term, subst)
        spec_term = deref(spec_term, subst)
        name_term = deref(name_term, subst)

        # Validate non-var types when instantiated
        if isinstance(precedence_term, Number):
            if not isinstance(precedence_term.value, int):
                error_term = PrologError.type_error("integer", precedence_term, "current_op/3")
                raise PrologThrow(error_term)
        elif not isinstance(precedence_term, Variable):
            error_term = PrologError.type_error("integer", precedence_term, "current_op/3")
            raise PrologThrow(error_term)

        if not isinstance(spec_term, (Atom, Variable)):
            error_term = PrologError.type_error("atom", spec_term, "current_op/3")
            raise PrologThrow(error_term)
        if not isinstance(name_term, (Atom, Variable)):
            error_term = PrologError.type_error("atom", name_term, "current_op/3")
            raise PrologThrow(error_term)

        for name, info in engine.operator_table.iter_current_ops():
            prec_term = Number(info.precedence)
            spec_atom = Atom(info.spec)
            name_atom = Atom(name)

            s1 = unify(precedence_term, prec_term, subst)
            if s1 is None:
                continue
            s2 = unify(spec_term, spec_atom, s1)
            if s2 is None:
                continue
            s3 = unify(name_term, name_atom, s2)
            if s3 is not None:
                yield s3


__all__ = ["OperatorBuiltins"]
