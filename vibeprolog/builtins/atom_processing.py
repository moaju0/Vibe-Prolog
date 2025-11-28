"""Atom processing built-ins (ISO §8.16).

Implements ISO-standard predicates for atom manipulation:
- atom_chars/2: Convert between atoms and character lists
- atom_codes/2: Convert between atoms and character code lists
- char_code/2: Convert between single characters and character codes
- atom_length/2: Get or check atom length
- atom_concat/3: Concatenate atoms or decompose atoms into parts
- sub_atom/5: Extract or check subatoms with position information
- number_chars/2: Convert between numbers and character lists
- number_codes/2: Convert between numbers and character code lists
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Number, Variable
from vibeprolog.unification import Substitution, deref, unify


class AtomProcessingBuiltins:
    """Built-ins for atom/character processing (ISO §8.16)."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register atom processing predicate handlers."""
        register_builtin(registry, "atom_chars", 2, AtomProcessingBuiltins._builtin_atom_chars)
        register_builtin(registry, "atom_codes", 2, AtomProcessingBuiltins._builtin_atom_codes)
        register_builtin(registry, "char_code", 2, AtomProcessingBuiltins._builtin_char_code)
        register_builtin(registry, "atom_length", 2, AtomProcessingBuiltins._builtin_atom_length)
        register_builtin(registry, "atom_concat", 3, AtomProcessingBuiltins._builtin_atom_concat)
        register_builtin(registry, "sub_atom", 5, AtomProcessingBuiltins._builtin_sub_atom)
        register_builtin(registry, "number_chars", 2, AtomProcessingBuiltins._builtin_number_chars)
        register_builtin(registry, "number_codes", 2, AtomProcessingBuiltins._builtin_number_codes)

    @staticmethod
    def _builtin_atom_chars(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """atom_chars(+Atom, ?Chars) - Convert between atom and character list."""
        atom_term, chars_term = args
        atom_term = deref(atom_term, subst)
        chars_term = deref(chars_term, subst)

        # Check type errors
        if not isinstance(atom_term, Variable) and not isinstance(atom_term, Atom):
            error_term = PrologError.type_error("atom", atom_term, "atom_chars/2")
            raise PrologThrow(error_term)
        if not isinstance(chars_term, Variable) and not isinstance(chars_term, List):
            error_term = PrologError.type_error("list", chars_term, "atom_chars/2")
            raise PrologThrow(error_term)

        # Mode 1: atom_chars(+Atom, ?Chars) - decompose atom to chars
        if isinstance(atom_term, Atom):
            chars = [Atom(c) for c in atom_term.name]
            char_list = List(tuple(chars), None)
            new_subst = unify(chars_term, char_list, subst)
            if new_subst is not None:
                yield new_subst

        # Mode 2: atom_chars(?Atom, +Chars) - construct atom from chars
        elif isinstance(chars_term, List):
            # Collect all elements from the list (handle nested representation)
            elements = AtomProcessingBuiltins._flatten_prolog_list(chars_term)
            if elements is None:
                return  # Not a proper list

            char_str = ""
            for elem in elements:
                if not isinstance(elem, Atom) or len(elem.name) != 1:
                    return  # Invalid character
                char_str += elem.name
            result_atom = Atom(char_str)
            new_subst = unify(atom_term, result_atom, subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _flatten_prolog_list(term) -> list | None:
        """Flatten a Prolog list into a Python list of elements.

        Returns None if the term is not a proper list.
        """
        if not isinstance(term, List):
            return None

        elements = []
        current = term
        while isinstance(current, List):
            elements.extend(current.elements)
            if current.tail is None:
                break
            current = current.tail
        else:
            # Not a proper list
            return None

        return elements

    @staticmethod
    def _is_char_list(term) -> bool:
        """Check if term is a proper list of single-character atoms."""
        if not isinstance(term, List):
            return False

        # Check that it's a proper list (no tail)
        if term.tail is not None:
            return False

        # Check that all elements are single-character atoms
        for elem in term.elements:
            if not isinstance(elem, Atom) or len(elem.name) != 1:
                return False

        return True

    @staticmethod
    def _builtin_atom_codes(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """atom_codes(+Atom, ?Codes) - Convert between atom and character code list."""
        atom_term, codes_term = args
        atom_term = deref(atom_term, subst)
        codes_term = deref(codes_term, subst)

        # Check type errors
        if not isinstance(atom_term, Variable) and not isinstance(atom_term, Atom):
            error_term = PrologError.type_error("atom", atom_term, "atom_codes/2")
            raise PrologThrow(error_term)
        if not isinstance(codes_term, Variable) and not isinstance(codes_term, List):
            error_term = PrologError.type_error("list", codes_term, "atom_codes/2")
            raise PrologThrow(error_term)

        # Mode 1: atom_codes(+Atom, ?Codes) - decompose atom to codes
        if isinstance(atom_term, Atom):
            codes = [Number(ord(c)) for c in atom_term.name]
            code_list = List(tuple(codes), None)
            new_subst = unify(codes_term, code_list, subst)
            if new_subst is not None:
                yield new_subst

        # Mode 2: atom_codes(?Atom, +Codes) - construct atom from codes
        elif isinstance(codes_term, List):
            # Collect all elements from the list (handle nested representation)
            elements = AtomProcessingBuiltins._flatten_prolog_list(codes_term)
            if elements is None:
                return  # Not a proper list

            char_str = ""
            for elem in elements:
                if not isinstance(elem, Number) or not isinstance(elem.value, int):
                    return  # Invalid integer
                char_str += chr(int(elem.value))
            result_atom = Atom(char_str)
            new_subst = unify(atom_term, result_atom, subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _is_code_list(term) -> bool:
        """Check if term is a proper list of integers."""
        if not isinstance(term, List):
            return False

        # Check that it's a proper list (no tail)
        if term.tail is not None:
            return False

        # Check that all elements are integers
        for elem in term.elements:
            if not isinstance(elem, Number) or not isinstance(elem.value, int):
                return False

        return True

    @staticmethod
    def _builtin_char_code(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """char_code(+Char, ?Code) - Convert between character atom and character code."""
        char_term, code_term = args
        char_term = deref(char_term, subst)
        code_term = deref(code_term, subst)

        # Check type errors for instantiated arguments
        if isinstance(char_term, Atom) and len(char_term.name) != 1:
            error_term = PrologError.type_error("character", char_term, "char_code/2")
            raise PrologThrow(error_term)
        if isinstance(code_term, Number) and not isinstance(code_term.value, int):
            error_term = PrologError.type_error("integer", code_term, "char_code/2")
            raise PrologThrow(error_term)
        if not isinstance(char_term, Variable) and not isinstance(char_term, Atom):
            error_term = PrologError.type_error("character", char_term, "char_code/2")
            raise PrologThrow(error_term)
        if not isinstance(code_term, Variable) and not isinstance(code_term, Number):
            error_term = PrologError.type_error("integer", code_term, "char_code/2")
            raise PrologThrow(error_term)

        # Mode 1: char_code(+Char, ?Code) - get code from char
        if isinstance(char_term, Atom):
            char_code = ord(char_term.name)
            code_number = Number(char_code)
            new_subst = unify(code_term, code_number, subst)
            if new_subst is not None:
                yield new_subst

        # Mode 2: char_code(?Char, +Code) - get char from code
        elif isinstance(code_term, Number) and isinstance(code_term.value, int):
            code_val = int(code_term.value)
            try:
                char = chr(code_val)
                char_atom = Atom(char)
                new_subst = unify(char_term, char_atom, subst)
                if new_subst is not None:
                    yield new_subst
            except ValueError:
                # Invalid character code
                pass

    @staticmethod
    def _builtin_atom_length(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """atom_length(+Atom, ?Length) - Get or check atom length."""
        atom_term, length_term = args
        atom_term = deref(atom_term, subst)
        length_term = deref(length_term, subst)

        if isinstance(atom_term, Variable):
            error_term = PrologError.instantiation_error("atom_length/2")
            raise PrologThrow(error_term)

        if not isinstance(atom_term, Atom):
            error_term = PrologError.type_error("atom", atom_term, "atom_length/2")
            raise PrologThrow(error_term)

        atom_len = len(atom_term.name)
        length_number = Number(atom_len)

        new_subst = unify(length_term, length_number, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _builtin_atom_concat(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """atom_concat(+Atom1, +Atom2, ?Atom3) or atom_concat(?Atom1, ?Atom2, +Atom3)."""
        atom1_term, atom2_term, atom3_term = args
        atom1_term = deref(atom1_term, subst)
        atom2_term = deref(atom2_term, subst)
        atom3_term = deref(atom3_term, subst)

        # Mode 1: atom_concat(+Atom1, +Atom2, ?Atom3) - concatenate
        if isinstance(atom1_term, Atom) and isinstance(atom2_term, Atom):
            result = Atom(atom1_term.name + atom2_term.name)
            new_subst = unify(atom3_term, result, subst)
            if new_subst is not None:
                yield new_subst

        # Mode 2: atom_concat(?Atom1, ?Atom2, +Atom3) - decompose
        elif isinstance(atom3_term, Atom):
            atom3_str = atom3_term.name
            # Generate all possible splits
            for i in range(len(atom3_str) + 1):
                part1 = atom3_str[:i]
                part2 = atom3_str[i:]
                atom1 = Atom(part1)
                atom2 = Atom(part2)

                new_subst = unify(atom1_term, atom1, subst)
                if new_subst is not None:
                    new_subst = unify(atom2_term, atom2, new_subst)
                    if new_subst is not None:
                        yield new_subst

        # Error cases
        if not isinstance(atom1_term, Variable) and not isinstance(atom1_term, Atom):
            error_term = PrologError.type_error("atom", atom1_term, "atom_concat/3")
            raise PrologThrow(error_term)
        if not isinstance(atom2_term, Variable) and not isinstance(atom2_term, Atom):
            error_term = PrologError.type_error("atom", atom2_term, "atom_concat/3")
            raise PrologThrow(error_term)
        if not isinstance(atom3_term, Variable) and not isinstance(atom3_term, Atom):
            error_term = PrologError.type_error("atom", atom3_term, "atom_concat/3")
            raise PrologThrow(error_term)

    @staticmethod
    def _builtin_sub_atom(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """sub_atom(+Atom, ?Before, ?Length, ?After, ?SubAtom)."""
        atom_term, before_term, length_term, after_term, sub_term = args
        atom_term = deref(atom_term, subst)
        before_term = deref(before_term, subst)
        length_term = deref(length_term, subst)
        after_term = deref(after_term, subst)
        sub_term = deref(sub_term, subst)

        if isinstance(atom_term, Variable):
            error_term = PrologError.instantiation_error("sub_atom/5")
            raise PrologThrow(error_term)

        if not isinstance(atom_term, Atom):
            error_term = PrologError.type_error("atom", atom_term, "sub_atom/5")
            raise PrologThrow(error_term)

        atom_str = atom_term.name
        atom_len = len(atom_str)

        # Collect constraints
        before_val = None
        length_val = None
        after_val = None
        sub_val = None

        if isinstance(before_term, Number) and isinstance(before_term.value, int):
            before_val = int(before_term.value)
            if before_val < 0:
                error_term = PrologError.domain_error("not_less_than_zero", before_term, "sub_atom/5")
                raise PrologThrow(error_term)
        elif not isinstance(before_term, Variable):
            error_term = PrologError.type_error("integer", before_term, "sub_atom/5")
            raise PrologThrow(error_term)

        if isinstance(length_term, Number) and isinstance(length_term.value, int):
            length_val = int(length_term.value)
            if length_val < 0:
                error_term = PrologError.domain_error("not_less_than_zero", length_term, "sub_atom/5")
                raise PrologThrow(error_term)
        elif not isinstance(length_term, Variable):
            error_term = PrologError.type_error("integer", length_term, "sub_atom/5")
            raise PrologThrow(error_term)

        if isinstance(after_term, Number) and isinstance(after_term.value, int):
            after_val = int(after_term.value)
            if after_val < 0:
                error_term = PrologError.domain_error("not_less_than_zero", after_term, "sub_atom/5")
                raise PrologThrow(error_term)
        elif not isinstance(after_term, Variable):
            error_term = PrologError.type_error("integer", after_term, "sub_atom/5")
            raise PrologThrow(error_term)

        if isinstance(sub_term, Atom):
            sub_val = sub_term.name
        elif not isinstance(sub_term, Variable):
            error_term = PrologError.type_error("atom", sub_term, "sub_atom/5")
            raise PrologThrow(error_term)

        # Generate all valid combinations
        for before in range(atom_len + 1) if before_val is None else [before_val]:
            for length in range(atom_len - before + 1) if length_val is None else [length_val]:
                after = atom_len - before - length
                if after_val is not None and after != after_val:
                    continue
                if after < 0:
                    continue

                sub_str = atom_str[before:before + length]
                if sub_val is not None and sub_str != sub_val:
                    continue

                # Create substitution
                new_subst = subst
                if isinstance(before_term, Variable):
                    new_subst = unify(before_term, Number(before), new_subst)
                    if new_subst is None:
                        continue
                if isinstance(length_term, Variable):
                    new_subst = unify(length_term, Number(length), new_subst)
                    if new_subst is None:
                        continue
                if isinstance(after_term, Variable):
                    new_subst = unify(after_term, Number(after), new_subst)
                    if new_subst is None:
                        continue
                if isinstance(sub_term, Variable):
                    new_subst = unify(sub_term, Atom(sub_str), new_subst)
                    if new_subst is None:
                        continue

                yield new_subst

    @staticmethod
    def _builtin_number_chars(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """number_chars(+Number, ?Chars) - Convert between number and character list."""
        number_term, chars_term = args
        number_term = deref(number_term, subst)
        chars_term = deref(chars_term, subst)

        # Check type errors
        if not isinstance(number_term, Variable) and not isinstance(number_term, Number):
            error_term = PrologError.type_error("number", number_term, "number_chars/2")
            raise PrologThrow(error_term)
        if not isinstance(chars_term, Variable) and not isinstance(chars_term, List):
            error_term = PrologError.type_error("list", chars_term, "number_chars/2")
            raise PrologThrow(error_term)

        # Mode 1: number_chars(+Number, ?Chars) - decompose number to chars
        if isinstance(number_term, Number):
            num_str = str(number_term.value)
            chars = [Atom(c) for c in num_str]
            char_list = List(tuple(chars), None)
            new_subst = unify(chars_term, char_list, subst)
            if new_subst is not None:
                yield new_subst

        # Mode 2: number_chars(?Number, +Chars) - construct number from chars
        elif isinstance(chars_term, List):
            # Collect all elements from the list (handle nested representation)
            elements = AtomProcessingBuiltins._flatten_prolog_list(chars_term)
            if elements is None:
                return  # Not a proper list

            char_str = ""
            for elem in elements:
                elem = deref(elem, subst)
                if not isinstance(elem, Atom) or len(elem.name) != 1:
                    error_term = PrologError.type_error("character", elem, "number_chars/2")
                    raise PrologThrow(error_term)
                char_str += elem.name

            # Parse the number
            try:
                if '.' in char_str or 'e' in char_str.lower():
                    result = float(char_str)
                else:
                    result = int(char_str)
                result_number = Number(result)
                new_subst = unify(number_term, result_number, subst)
                if new_subst is not None:
                    yield new_subst
            except ValueError:
                # Syntax error for invalid number format
                error_term = PrologError.syntax_error(f"Invalid number format: {char_str}", "number_chars/2")
                raise PrologThrow(error_term)

    @staticmethod
    def _builtin_number_codes(
        args: BuiltinArgs, subst: Substitution, _engine: EngineContext | None
    ) -> Iterator[Substitution]:
        """number_codes(+Number, ?Codes) - Convert between number and character code list."""
        number_term, codes_term = args
        number_term = deref(number_term, subst)
        codes_term = deref(codes_term, subst)

        # Check type errors
        if not isinstance(number_term, Variable) and not isinstance(number_term, Number):
            error_term = PrologError.type_error("number", number_term, "number_codes/2")
            raise PrologThrow(error_term)
        if not isinstance(codes_term, Variable) and not isinstance(codes_term, List):
            error_term = PrologError.type_error("list", codes_term, "number_codes/2")
            raise PrologThrow(error_term)

        # Mode 1: number_codes(+Number, ?Codes) - decompose number to codes
        if isinstance(number_term, Number):
            num_str = str(number_term.value)
            codes = [Number(ord(c)) for c in num_str]
            code_list = List(tuple(codes), None)
            new_subst = unify(codes_term, code_list, subst)
            if new_subst is not None:
                yield new_subst

        # Mode 2: number_codes(?Number, +Codes) - construct number from codes
        elif isinstance(codes_term, List):
            # Collect all elements from the list (handle nested representation)
            elements = AtomProcessingBuiltins._flatten_prolog_list(codes_term)
            if elements is None:
                return  # Not a proper list

            char_str = ""
            for elem in elements:
                elem = deref(elem, subst)
                if not isinstance(elem, Number) or not isinstance(elem.value, int):
                    error_term = PrologError.type_error("integer", elem, "number_codes/2")
                    raise PrologThrow(error_term)
                try:
                    char_str += chr(elem.value)
                except ValueError:
                    error_term = PrologError.type_error("character_code", elem, "number_codes/2")
                    raise PrologThrow(error_term)

            # Parse the number
            try:
                if '.' in char_str or 'e' in char_str.lower():
                    result = float(char_str)
                else:
                    result = int(char_str)
                result_number = Number(result)
                new_subst = unify(number_term, result_number, subst)
                if new_subst is not None:
                    yield new_subst
            except ValueError:
                # Syntax error for invalid number format
                error_term = PrologError.syntax_error(f"Invalid number format: {char_str}", "number_codes/2")
                raise PrologThrow(error_term)


__all__ = ["AtomProcessingBuiltins"]