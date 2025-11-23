"""Type test built-ins (atom/1, number/1, var/1, etc.)."""

from __future__ import annotations

from typing import Any

from prolog.builtins import register_builtin
from prolog.parser import Atom, Compound, List, Number, Variable
from prolog.unification import Substitution, deref, unify


class TypeTestBuiltins:
    """Built-ins for inspecting term types."""

    @staticmethod
    def register(registry, _engine) -> None:
        register_builtin(registry, "atom", 1, TypeTestBuiltins._builtin_atom)
        register_builtin(registry, "number", 1, TypeTestBuiltins._builtin_number)
        register_builtin(registry, "var", 1, TypeTestBuiltins._builtin_var)
        register_builtin(registry, "nonvar", 1, TypeTestBuiltins._builtin_nonvar)
        register_builtin(registry, "compound", 1, TypeTestBuiltins._builtin_compound)
        register_builtin(registry, "integer", 1, TypeTestBuiltins._builtin_integer)
        register_builtin(registry, "float", 1, TypeTestBuiltins._builtin_float)
        register_builtin(registry, "atomic", 1, TypeTestBuiltins._builtin_atomic)
        register_builtin(registry, "callable", 1, TypeTestBuiltins._builtin_callable)
        register_builtin(registry, "ground", 1, TypeTestBuiltins._builtin_ground)

    @staticmethod
    def _builtin_atom(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, Atom):
            return subst
        return None

    @staticmethod
    def _builtin_number(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, Number):
            return subst
        return None

    @staticmethod
    def _builtin_var(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, Variable):
            return subst
        return None

    @staticmethod
    def _builtin_nonvar(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if not isinstance(term, Variable):
            return subst
        return None

    @staticmethod
    def _builtin_compound(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, (Compound, List)):
            return subst
        return None

    @staticmethod
    def _builtin_integer(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, Number) and isinstance(term.value, int):
            return subst
        return None

    @staticmethod
    def _builtin_float(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, Number) and isinstance(term.value, float):
            return subst
        return None

    @staticmethod
    def _builtin_atomic(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, (Atom, Number)) or (isinstance(term, List) and not term.elements and term.tail is None):
            return subst
        return None

    @staticmethod
    def _builtin_callable(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if isinstance(term, (Atom, Compound, List)):
            return subst
        return None

    @staticmethod
    def _builtin_ground(args: tuple[Any, ...], subst: Substitution, _engine) -> Substitution | None:
        term = deref(args[0], subst)
        if TypeTestBuiltins._is_ground(term, subst):
            return subst
        return None

    @staticmethod
    def _is_ground(term: Any, subst: Substitution) -> bool:
        term = deref(term, subst)
        if isinstance(term, Variable):
            return False
        if isinstance(term, (Atom, Number)):
            return True
        if isinstance(term, Compound):
            return all(TypeTestBuiltins._is_ground(arg, subst) for arg in term.args)
        if isinstance(term, List):
            for elem in term.elements:
                if not TypeTestBuiltins._is_ground(elem, subst):
                    return False
            if term.tail is not None:
                return TypeTestBuiltins._is_ground(term.tail, subst)
            return True
        return True


__all__ = ["TypeTestBuiltins"]
