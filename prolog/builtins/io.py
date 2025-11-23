"""I/O built-ins (write/1, writeln/1, format/N, nl/0).

Implements basic output predicates including formatted printing.
"""

from __future__ import annotations

from typing import Any

from prolog.builtins import BuiltinRegistry, register_builtin
from prolog.builtins.common import BuiltinArgs, EngineContext
from prolog.parser import Atom, Compound, List, Number, Variable
from prolog.unification import Substitution, deref, unify
from prolog.utils.list_utils import list_to_python
from prolog.utils.term_utils import term_to_string


class IOBuiltins:
    """Built-ins for standard output and formatting."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register I/O predicate handlers."""
        register_builtin(registry, "write", 1, IOBuiltins._builtin_write)
        register_builtin(registry, "writeln", 1, IOBuiltins._builtin_writeln)
        register_builtin(registry, "format", 3, IOBuiltins._builtin_format)
        register_builtin(registry, "format", 2, IOBuiltins._builtin_format_stdout)
        register_builtin(
            registry,
            "format",
            1,
            lambda args, subst, engine: IOBuiltins._builtin_format_stdout(
                (args[0], List(())), subst, engine
            ),
        )
        register_builtin(registry, "nl", 0, IOBuiltins._builtin_newline)

    @staticmethod
    def _builtin_write(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        term = deref(args[0], subst)
        output = term_to_string(term)
        print(output, end="")
        return subst

    @staticmethod
    def _builtin_writeln(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        term = deref(args[0], subst)
        output = term_to_string(term)
        print(output)
        return subst

    @staticmethod
    def _builtin_format(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        output, format_str, args_list = args
        output = deref(output, subst)
        if (
            not isinstance(output, Compound)
            or output.functor != "atom"
            or len(output.args) != 1
        ):
            return None

        rendered = IOBuiltins._format_to_string(format_str, args_list, subst)
        if rendered is None:
            return None

        return unify(output.args[0], Atom(rendered), subst)

    @staticmethod
    def _builtin_format_stdout(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        format_str, args_list = args
        rendered = IOBuiltins._format_to_string(format_str, args_list, subst)
        if rendered is None:
            return None

        print(rendered, end="")
        return subst

    @staticmethod
    def _builtin_newline(
        _args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution:
        print()
        return subst

    @staticmethod
    def _format_to_string(
        format_term: Any, args_term: Any, subst: Substitution
    ) -> str | None:
        format_term = deref(format_term, subst)
        args_term = deref(args_term, subst)

        if not isinstance(format_term, Atom):
            return None

        try:
            format_args = list_to_python(args_term, subst)
        except TypeError:
            return None

        coerced_args: list[str | int | float] = []
        for elem in format_args:
            elem = deref(elem, subst)
            if isinstance(elem, Number):
                coerced_args.append(elem.value)
            elif isinstance(elem, Atom):
                coerced_args.append(elem.name)
            elif isinstance(elem, Variable):
                coerced_args.append(f"_{elem.name}")
            else:
                coerced_args.append(str(elem))

        fmt = format_term.name
        result = ""
        i = 0
        arg_index = 0

        while i < len(fmt):
            if fmt[i] == "~":
                if i + 1 < len(fmt):
                    next_char = fmt[i + 1]

                    if next_char == "~":
                        result += "~"
                        i += 2
                    elif next_char == "w":
                        if arg_index < len(coerced_args):
                            result += str(coerced_args[arg_index])
                            arg_index += 1
                        i += 2
                    elif next_char == "d":
                        if arg_index < len(coerced_args):
                            result += str(int(coerced_args[arg_index]))
                            arg_index += 1
                        i += 2
                    elif next_char.isdigit():
                        j = i + 1
                        while j < len(fmt) and fmt[j].isdigit():
                            j += 1
                        if j < len(fmt) and fmt[j] == "f":
                            precision = int(fmt[i + 1 : j])
                            if arg_index < len(coerced_args):
                                result += (
                                    f"{float(coerced_args[arg_index]):.{precision}f}"
                                )
                                arg_index += 1
                            i = j + 1
                        else:
                            result += fmt[i]
                            i += 1
                    elif next_char == "f":
                        if arg_index < len(coerced_args):
                            result += str(float(coerced_args[arg_index]))
                            arg_index += 1
                        i += 2
                    elif next_char == "n":
                        result += "\n"
                        i += 2
                    else:
                        result += fmt[i]
                        i += 1
                else:
                    result += fmt[i]
                    i += 1
            else:
                result += fmt[i]
                i += 1

        return result


__all__ = ["IOBuiltins"]
