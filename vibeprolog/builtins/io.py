"""I/O built-ins (write/1, writeln/1, format/N, nl/0).

Implements basic output predicates including formatted printing.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from lark.exceptions import LarkError

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import List, PrologParser
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution, deref, unify
from vibeprolog.utils.list_utils import list_to_python, python_to_list
from vibeprolog.utils.term_utils import term_to_string

USER_INPUT_STREAM = Atom("user_input")
USER_OUTPUT_STREAM = Atom("user_output")


@dataclass(frozen=True)
class OperatorInfo:
    """Operator metadata for formatting terms with ignore_ops(false)."""

    precedence: int
    spec: str  # e.g., yfx, xfy, xfx, fy, fx

    @property
    def is_prefix(self) -> bool:
        return len(self.spec) == 2


# Default ISO-ish operator table for rendering terms when ignore_ops(false).
OPERATOR_TABLE: dict[tuple[str, int], OperatorInfo] = {
    (";", 2): OperatorInfo(1100, "xfy"),
    ("->", 2): OperatorInfo(1050, "xfy"),
    (",", 2): OperatorInfo(1000, "xfy"),
    ("\\+", 1): OperatorInfo(900, "fy"),
    ("=..", 2): OperatorInfo(700, "xfx"),
    ("is", 2): OperatorInfo(700, "xfx"),
    ("=", 2): OperatorInfo(700, "xfx"),
    ("\\=", 2): OperatorInfo(700, "xfx"),
    ("=:=", 2): OperatorInfo(700, "xfx"),
    ("=\\=", 2): OperatorInfo(700, "xfx"),
    ("<", 2): OperatorInfo(700, "xfx"),
    (">", 2): OperatorInfo(700, "xfx"),
    ("=<", 2): OperatorInfo(700, "xfx"),
    (">=", 2): OperatorInfo(700, "xfx"),
    ("==", 2): OperatorInfo(700, "xfx"),
    ("\\==", 2): OperatorInfo(700, "xfx"),
    ("@<", 2): OperatorInfo(700, "xfx"),
    ("@=<", 2): OperatorInfo(700, "xfx"),
    ("@>", 2): OperatorInfo(700, "xfx"),
    ("@>=", 2): OperatorInfo(700, "xfx"),
    ("+", 2): OperatorInfo(500, "yfx"),
    ("-", 2): OperatorInfo(500, "yfx"),
    ("*", 2): OperatorInfo(400, "yfx"),
    ("/", 2): OperatorInfo(400, "yfx"),
    ("//", 2): OperatorInfo(400, "yfx"),
    ("mod", 2): OperatorInfo(400, "yfx"),
    ("**", 2): OperatorInfo(200, "xfx"),
    ("+", 1): OperatorInfo(200, "fy"),
    ("-", 1): OperatorInfo(200, "fy"),
    (":-", 2): OperatorInfo(1200, "xfx"),
}

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
        register_builtin(
            registry, "read_from_chars", 2, IOBuiltins._builtin_read_from_chars
        )
        register_builtin(
            registry, "write_term_to_chars", 3, IOBuiltins._builtin_write_term_to_chars
        )
        register_builtin(registry, "current_input", 1, IOBuiltins._builtin_current_input)
        register_builtin(registry, "current_output", 1, IOBuiltins._builtin_current_output)

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
    def _builtin_current_input(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        arg = deref(args[0], subst)
        return unify(arg, USER_INPUT_STREAM, subst)

    @staticmethod
    def _builtin_current_output(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        arg = deref(args[0], subst)
        return unify(arg, USER_OUTPUT_STREAM, subst)

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

    @staticmethod
    def _builtin_read_from_chars(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        """read_from_chars/2 - Parse a Prolog term from a string.

        read_from_chars(+Chars, -Term)
        Parses Chars (atom or list of chars) as a Prolog term and unifies with Term.
        """
        chars_term, term_var = args
        chars_term = deref(chars_term, subst)

        # Convert input to string
        if isinstance(chars_term, Atom):
            input_str = chars_term.name
        elif isinstance(chars_term, List):
            try:
                char_list = list_to_python(chars_term, subst)
                # Convert list of character atoms to string
                input_str = ''.join(
                    c.name if isinstance(c, Atom) else str(c)
                    for c in char_list
                )
            except (TypeError, AttributeError):
                return None
        else:
            return None

        # Parse the string as a Prolog term
        try:
            parser = PrologParser()
            # Remove trailing period if present (parse expects no period)
            input_str = input_str.strip()
            if input_str.endswith('.'):
                input_str = input_str[:-1].strip()

            parsed_term = parser.parse_term(input_str, "read_from_chars/2")
            return unify(term_var, parsed_term, subst)
        except (ValueError, LarkError, PrologThrow) as exc:
            if isinstance(exc, PrologThrow):
                raise exc
            else:
                error_term = PrologError.syntax_error(str(exc), "read_from_chars/2")
                raise PrologThrow(error_term)

    @staticmethod
    def _builtin_write_term_to_chars(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Substitution | None:
        """write_term_to_chars/3 - Convert a Prolog term to a character list.

        write_term_to_chars(+Term, +Options, -Chars)
        Converts Term to a string representation according to Options and unifies with Chars.

        Supported options:
        - ignore_ops(true/false): Write in canonical form
        - numbervars(true/false): Convert $VAR(N) to variable names (A, B, C, ...)
        - quoted(true/false): Quote atoms that need quoting
        - variable_names([]): Variable name mappings (not yet implemented)
        """
        term, options_term, chars_var = args
        term = deref(term, subst)
        options_term = deref(options_term, subst)

        # Parse options
        try:
            options_list = list_to_python(options_term, subst)
        except TypeError:
            return None

        # Extract option values
        ignore_ops = False
        numbervars = False
        quoted = False

        for opt in options_list:
            opt = deref(opt, subst)
            if isinstance(opt, Compound):
                if opt.functor == "ignore_ops" and len(opt.args) == 1:
                    val = deref(opt.args[0], subst)
                    ignore_ops = isinstance(val, Atom) and val.name == "true"
                elif opt.functor == "numbervars" and len(opt.args) == 1:
                    val = deref(opt.args[0], subst)
                    numbervars = isinstance(val, Atom) and val.name == "true"
                elif opt.functor == "quoted" and len(opt.args) == 1:
                    val = deref(opt.args[0], subst)
                    quoted = isinstance(val, Atom) and val.name == "true"

        # Convert term to string
        output_str = IOBuiltins._term_to_chars_string(
            term, subst, ignore_ops, numbervars, quoted
        )

        # Convert string to list of character atoms
        char_list = [Atom(c) for c in output_str]
        chars_list = python_to_list(char_list)

        return unify(chars_var, chars_list, subst)

    @staticmethod
    def _term_to_chars_string(
        term: Any,
        subst: Substitution,
        ignore_ops: bool,
        numbervars: bool,
        quoted: bool,
        parent_prec: int = 1200,
    ) -> str:
        """Convert a term to string with specified options."""
        term = deref(term, subst)

        # Handle $VAR(N) for numbervars
        if numbervars and isinstance(term, Compound) and term.functor == "$VAR" and len(term.args) == 1:
            arg = deref(term.args[0], subst)
            if isinstance(arg, Number) and isinstance(arg.value, int) and arg.value >= 0:
                # Convert to A, B, C, ..., Z, A1, B1, ...
                n = arg.value
                var_name = chr(ord('A') + (n % 26))
                if n >= 26:
                    var_name += str(n // 26)
                return var_name

        if isinstance(term, Atom):
            if quoted and IOBuiltins._needs_quoting(term.name):
                # Escape special characters
                escaped = term.name.replace('\\', '\\\\').replace("'", "\\'")
                return f"'{escaped}'"
            return term.name

        if isinstance(term, Number):
            val = term.value
            if isinstance(val, float):
                # Handle scientific notation
                s = str(val)
                if 'e' in s:
                    return s
                # Check if we need scientific notation
                if abs(val) >= 1e10 or (abs(val) < 1e-4 and val != 0):
                    return f"{val:e}"
                return s
            return str(val)

        if isinstance(term, Variable):
            return f"_{term.name}"

        if isinstance(term, List):
            # Handle empty list
            if not term.elements and term.tail is None:
                return "[]"

            # Handle list elements
            elements_str = [
                IOBuiltins._term_to_chars_string(
                    e, subst, ignore_ops, numbervars, quoted, 1200
                )
                for e in term.elements
            ]

            # Handle tail
            if term.tail is not None and not (
                isinstance(term.tail, List) and not term.tail.elements and term.tail.tail is None
            ):
                tail_str = IOBuiltins._term_to_chars_string(
                    term.tail, subst, ignore_ops, numbervars, quoted, 1200
                )
                return f"[{','.join(elements_str)}|{tail_str}]"

            return f"[{','.join(elements_str)}]"

        if isinstance(term, Compound):
            if not ignore_ops:
                operator_rendered = IOBuiltins._format_operator_term(
                    term, subst, ignore_ops, numbervars, quoted, parent_prec
                )
                if operator_rendered is not None:
                    return operator_rendered
            if not term.args:
                if quoted and IOBuiltins._needs_quoting(term.functor):
                    escaped = term.functor.replace('\\', '\\\\').replace("'", "\\'")
                    return f"'{escaped}'"
                return term.functor
            args_str = ','.join(
                IOBuiltins._term_to_chars_string(
                    arg, subst, ignore_ops, numbervars, quoted, 1200
                )
                for arg in term.args
            )
            functor = term.functor
            if quoted and IOBuiltins._needs_quoting(functor):
                escaped = functor.replace('\\', '\\\\').replace("'", "\\'")
                functor = f"'{escaped}'"
            return f"{functor}({args_str})"

        return str(term)

    @staticmethod
    def _format_operator_term(
        term: Compound,
        subst: Substitution,
        ignore_ops: bool,
        numbervars: bool,
        quoted: bool,
        parent_prec: int,
    ) -> str | None:
        info = OPERATOR_TABLE.get((term.functor, len(term.args)))
        if info is None:
            return None

        if info.is_prefix:
            arg_limit = IOBuiltins._child_precedence_limit(info, "right")
            arg_str = IOBuiltins._term_to_chars_string(
                term.args[0], subst, ignore_ops, numbervars, quoted, arg_limit
            )
            rendered = IOBuiltins._render_prefix(term.functor, arg_str)
        else:
            left_limit = IOBuiltins._child_precedence_limit(info, "left")
            right_limit = IOBuiltins._child_precedence_limit(info, "right")
            left_str = IOBuiltins._term_to_chars_string(
                term.args[0], subst, ignore_ops, numbervars, quoted, left_limit
            )
            right_str = IOBuiltins._term_to_chars_string(
                term.args[1], subst, ignore_ops, numbervars, quoted, right_limit
            )
            rendered = IOBuiltins._render_infix(term.functor, left_str, right_str)

        if info.precedence > parent_prec:
            return f"({rendered})"
        return rendered

    @staticmethod
    def _child_precedence_limit(info: OperatorInfo, position: str) -> int:
        if info.is_prefix:
            char = info.spec[1]
        else:
            char = info.spec[0] if position == "left" else info.spec[2]
        return info.precedence - 1 if char == "x" else info.precedence

    @staticmethod
    def _render_prefix(functor: str, arg_str: str) -> str:
        if functor and functor[0].isalpha():
            return f"{functor} {arg_str}"
        return f"{functor}{arg_str}"

    @staticmethod
    def _render_infix(functor: str, left: str, right: str) -> str:
        if functor in {":-", "?-"}:
            return f"{left} {functor} {right}"
        if functor and functor[0].isalpha():
            return f"{left} {functor} {right}"
        if functor == ",":
            return f"{left},{right}"
        if functor == ";":
            return f"{left};{right}"
        return f"{left}{functor}{right}"

    @staticmethod
    def _needs_quoting(atom: str) -> bool:
        """Check if an atom needs quoting."""
        if not atom:
            return True

        # An atom starting with a lowercase letter followed by alphanumerics or underscores does not need quoting.
        if atom[0].islower() and all(c.isalnum() or c == '_' for c in atom):
            return False

        # A set of common graphic/symbolic atoms that do not need quoting.
        # This set is not exhaustive but covers many common cases.
        unquoted_graphic_atoms = {
            '!', ';', ',', '+', '-', '*', '/', '//', '**', '=', ':-', '->', '\=',
            '<', '>', '=<', '>=', '=:=', '=\\=', '==', '\\==',
            '@<', '@>', '@=<', '@>=', 'is', '=..', '\\+'
        }
        if atom in unquoted_graphic_atoms:
            return False

        # Atoms that are just `[]` or `{}`
        if atom in ('[]', '{}'):
            return False

        # Everything else needs quoting (e.g., starts with uppercase, contains spaces, looks like a number).
        return True


__all__ = ["IOBuiltins"]
