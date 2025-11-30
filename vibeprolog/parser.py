"""Prolog parser using Lark."""

import re

from lark import Lark, Transformer, v_args
from lark.exceptions import LarkError, UnexpectedCharacters, UnexpectedToken
from dataclasses import dataclass
from typing import Any

from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.terms import Atom, Variable, Number, Compound


@dataclass(frozen=True)
class List:
    """A list."""

    elements: tuple[Any, ...]
    tail: Any = None  # For [H|T] syntax

    def __repr__(self):
        if not self.elements and self.tail is None:
            return "[]"
        if self.tail is not None:
            elements_str = ", ".join(str(e) for e in self.elements)
            return f"[{elements_str}|{self.tail}]"
        elements_str = ", ".join(str(e) for e in self.elements)
        return f"[{elements_str}]"


@dataclass(frozen=True)
class Cut:
    """The cut operator (!)."""

    def __repr__(self):
        return "!"


@dataclass
class Clause:
    """A Prolog clause (fact or rule)."""

    head: Compound
    body: list[Compound] | None = None  # None for facts
    doc: str | None = None  # PlDoc documentation
    meta: Any = None  # Lark meta information
    dcg: bool = False  # True for DCG rules

    def is_fact(self):
        return self.body is None

    def is_rule(self):
        return self.body is not None


@dataclass
class Directive:
    """A Prolog directive (e.g., :- initialization(goal).)."""

    goal: Any  # The directive term, e.g., initialization(goal)
    doc: str | None = None  # PlDoc documentation
    meta: Any = None  # Lark meta information


@dataclass
class PredicateIndicator:
    """A predicate indicator Name/Arity."""

    name: Any
    arity: Any


@dataclass
class PredicatePropertyDirective:
    """Directive declaring predicate properties such as dynamic/1."""

    property: str
    indicators: tuple[PredicateIndicator, ...]


# Lark grammar for Prolog
PROLOG_GRAMMAR = r"""
    start: (clause | directive)+

    clause: rule | dcg_rule | fact
    fact: atom_or_compound "."
    rule: term ":-" goals "."
    dcg_rule: term "-->" goals "."
    atom_or_compound: atom
        | atom "(" args ")"
    
    directive: ":-" (op_directive | prefix_directive | property_directive | term) "."

    prefix_directive: "dynamic" predicate_indicators -> dynamic_directive
        | "multifile" predicate_indicators -> multifile_directive
        | "discontiguous" predicate_indicators -> discontiguous_directive

    property_directive: "dynamic" "(" predicate_indicators ")"    -> dynamic_directive
        | "multifile" "(" predicate_indicators ")"   -> multifile_directive
        | "discontiguous" "(" predicate_indicators ")" -> discontiguous_directive
    
    op_directive: "op" "(" number "," ATOM "," op_arg_list ")" -> op_directive
    
    op_arg_list: op_symbol_or_atom
        | "[" op_symbol_or_atom ("," op_symbol_or_atom)* "]" -> op_arg_list_items
    
    // operator symbol or atom for op/3 - avoid parsing as expressions
    op_symbol_or_atom: OP_SYMBOL | ATOM | SPECIAL_ATOM | OPERATOR_ATOM
    
    // Operator symbols - must match complete sequences before breaking into component operators
    // Priority set to ensure special atoms like -$ are recognized, but still below SPECIAL_ATOM_OPS
    OP_SYMBOL: /[+\-*\/<>=\\@#$&!~:?^.]+/

    predicate_indicators: comparison_term ("," comparison_term)*

    goals: term ("," term)*

    // Operator precedence (lowest to highest):
    // ; (or/semicolon) < -> (if-then) < , (and/comma) < comparisons < arithmetic

    term: or_term

    or_term: if_then_term (";" if_then_term)*

    if_then_term: and_term ("->" and_term)?

    and_term: comparison_term ("," comparison_term)*

    comparison_term: module_term (COMP_OP module_term)?

    prefix_term: expr

    // Module qualification operator (right-associative, xfy)
    module_term: prefix_term (":" module_term)?

    expr: add_expr

    add_expr: mul_expr (ADD_OP mul_expr)*

    mul_expr: pow_expr (MUL_OP pow_expr)*

    pow_expr: unary_expr (POW_OP unary_expr)*

    unary_expr: PREFIX_OP unary_expr  -> prefix_op
        | "-" unary_expr  -> prefix_op
        | "+" unary_expr  -> prefix_op
        | primary

    primary: SPECIAL_ATOM_OPS -> special_atom_token
        | operator_atom
        | compound
        | curly_braces
        | string
        | cut
        | char_code
        | number
        | "-" number -> negative_number
        | atom
        | variable
        | list
        | "(" term ")"

    compound: atom "(" args ")"
    operator_atom: OPERATOR_ATOM
    args: comparison_term ("," comparison_term)*

    curly_braces: "{" term "}"

    COMP_OP: "=.." | "is" | "=" | "\\=" | "=:=" | "=\=" | "<" | ">" | "=<" | ">=" | "==" | "\\==" | "@<" | "@=<" | "@>" | "@>="
    OPERATOR_ATOM: ":-" | "-->"
    PREFIX_OP.3: "\\+" | /\+(?!\$)/ | /\-(?!\$)/
    ADD_OP: /\+(?!\$)/ | /\-(?!\$)/
    POW_OP.2: "**"
    MUL_OP: "*" | "/" | "//" | "mod"

    list: "[" "]"                          -> empty_list
        | "[" list_items "]"               -> list_items_only
        | "[" list_items "|" comparison_term "]"      -> list_with_tail

    list_items: comparison_term ("," comparison_term)*

    string: STRING
    atom: ATOM | SPECIAL_ATOM | SPECIAL_ATOM_OPS | OP_SYMBOL
    variable: VARIABLE
    char_code: CHAR_CODE
    number: NUMBER
    cut: "!"

    // Character codes: 0'X where X is any character (must come before NUMBER)
    // Patterns: 0'a (simple char), 0'\\ (backslash), 0'\' (quote), 0''' (doubled quote), 0'\xHH (hex)
    // Allow broader alphanumerics after \\x so lexer does not reject malformed hex sequences that should become syntax errors
    CHAR_CODE.5: /0'(\\x[0-9a-zA-Z]+\\?|\\\\\\\\|\\\\['tnr]|''|[^'\\])/ | /\d+'.'/

    STRING: /"([^"\\]|\\.)*"/ | /'(\\.|''|[^'\\])*'/
    SPECIAL_ATOM: /'([^'\\]|\\.)+'/
    
    // Special atom operators must have HIGHEST priority to prevent being parsed as prefix operators
    SPECIAL_ATOM_OPS.12: /-\$/ | /\$-/

    // Scientific notation, hex, octal, binary, base'digits
    NUMBER.4: /-?0x[0-9a-fA-F]+/i
            | /-?0o[0-7]+/i
            | /-?0b[01]+/i
            | /-?\d+\.?\d*[eE][+-]?\d+/
            | /-?\d+\.\d+/
            | /-?\.\d+/
            | /-?\d+'[a-zA-Z0-9_]+/
            | /-?\d+/

    ATOM: /[a-z][a-zA-Z0-9_]*/ | /\{\}/ | /\$[a-zA-Z0-9_-]*/ | /[+\-*\/]/

    VARIABLE: /[A-Z_][a-zA-Z0-9_]*/

    %import common.WS
    %ignore WS
    %ignore /%.*/  // Line comments
"""


class PrologTransformer(Transformer):
    """Transform parse tree into AST."""

    def __init__(self):
        super().__init__()
        self._anon_counter = 0

    def start(self, items):
        return items

    def clause(self, items):
        return items[0]

    def atom_or_compound(self, items):
        if len(items) == 1:
            return items[0]  # Just an atom
        else:
            # atom "(" args ")"
            atom = items[0]
            args = items[1]
            return Compound(atom.name, tuple(args))

    @v_args(meta=True)
    def directive(self, meta, items):
        return Directive(goal=items[0], meta=meta)

    def predicate_indicators(self, items):
        return items

    def dynamic_directive(self, items):
        indicators = self._flatten_comma_separated(items[0])
        return PredicatePropertyDirective("dynamic", tuple(indicators))

    def multifile_directive(self, items):
        indicators = self._flatten_comma_separated(items[0])
        return PredicatePropertyDirective("multifile", tuple(indicators))

    def discontiguous_directive(self, items):
        indicators = self._flatten_comma_separated(items[0])
        return PredicatePropertyDirective("discontiguous", tuple(indicators))

    def op_directive(self, items):
        """Handle op/3 directive: op(Precedence, Type, Name).
        
        Creates a Compound term op(Prec, Type, Name) that the engine will process.
        """
        # items[0] = number (precedence)
        # items[1] = atom (type: xfx, yfx, etc.) - raw token
        # items[2] = op_arg_list (either single atom or list of atoms)
        precedence = items[0]
        op_type_token = items[1]
        op_names = items[2]
        
        # Convert operator type token to Atom
        op_type = Atom(str(op_type_token))
        
        # op_names is either a single Atom or a list of Atoms
        if isinstance(op_names, list):
            # op_arg_list_items case - convert to list representation
            return Compound("op", (precedence, op_type, List(elements=tuple(op_names))))
        else:
            # Single op_symbol_or_atom case
            return Compound("op", (precedence, op_type, op_names))

    def op_arg_list(self, items):
        """Handle operator argument list - either single or bracketed list."""
        return items[0]

    def op_arg_list_items(self, items):
        """Handle list of operators: [op1, op2, ...]"""
        return items

    def op_symbol_or_atom(self, items):
        """Operator symbol or atom - convert raw token to Atom."""
        token = items[0]
        if isinstance(token, Atom):
            return token
        else:
            # Raw token from OP_SYMBOL, ATOM, SPECIAL_ATOM, or OPERATOR_ATOM
            s = str(token)
            # Remove quotes if present (for SPECIAL_ATOM)
            if (s.startswith("'") and s.endswith("'")):
                s = s[1:-1]
                # In Prolog, single quotes are escaped by doubling them
                s = s.replace("''", "'")
            return Atom(s)

    def _flatten_comma_separated(self, items):
        """Flatten comma-separated predicates into a list.
        
        If items is a list with a single Compound with functor ',',
        unwrap it into a flat list of indicators.
        """
        if isinstance(items, list) and len(items) == 1:
            item = items[0]
            if isinstance(item, Compound) and item.functor == ',':
                # Flatten comma compound
                return self._collect_comma_terms(item)
        return items

    def _collect_comma_terms(self, compound):
        """Recursively collect terms from a comma compound."""
        if isinstance(compound, Compound) and compound.functor == ',':
            left = self._collect_comma_terms(compound.args[0])
            right = self._collect_comma_terms(compound.args[1])
            return left + right
        else:
            return [compound]

    @v_args(meta=True)
    def fact(self, meta, items):
        return Clause(head=items[0], body=None, meta=meta)

    @v_args(meta=True)
    def rule(self, meta, items):
        head, body = items
        return Clause(head=head, body=body, meta=meta)

    @v_args(meta=True)
    def dcg_rule(self, meta, items):
        head, body = items
        return Clause(head=head, body=body, dcg=True, meta=meta)

    def goals(self, items):
        # Flatten any comma compounds in the goal list
        # This handles cases where ambiguous parsing creates comma compounds
        result = []
        for item in items:
            if isinstance(item, Compound) and item.functor == ',':
                result.extend(self._collect_comma_terms(item))
            else:
                result.append(item)
        return result

    def term(self, items):
        return items[0]

    def or_term(self, items):
        if len(items) == 1:
            return items[0]
        # Build right-associative tree for disjunction
        result = items[-1]
        for i in range(len(items) - 2, -1, -1):
            result = Compound(";", (items[i], result))
        return result

    def if_then_term(self, items):
        if len(items) == 1:
            return items[0]
        # items[0] -> items[1]
        return Compound("->", (items[0], items[1]))

    def and_term(self, items):
        if len(items) == 1:
            return items[0]
        # Build right-associative tree for conjunction
        result = items[-1]
        for i in range(len(items) - 2, -1, -1):
            result = Compound(",", (items[i], result))
        return result

    def comparison_term(self, items):
        if len(items) == 1:
            return items[0]
        left, op, right = items
        return Compound(str(op), (left, right))

    def prefix_term(self, items):
        return items[0]

    def prefix_op(self, items):
        # items can be [op, term] or just [term] depending on the rule
        if len(items) == 2:
            op, term = items
            op_str = str(op)
            # Special case: convert unary minus applied to a number into a negative number
            if op_str == "-" and isinstance(term, Number):
                if isinstance(term.value, int):
                    return Number(-term.value)
                else:
                    return Number(-term.value)
            return Compound(op_str, (term,))
        else:
            # For "-" prefix_term rule, items is just [term]
            # The "-" is implicit in the rule
            term = items[0]
            # Special case: convert unary minus applied to a number into a negative number
            if isinstance(term, Number):
                if isinstance(term.value, int):
                    return Number(-term.value)
                else:
                    return Number(-term.value)
            return Compound("-", (term,))

    def module_term(self, items):
        # Module qualification: left:right (right-associative)
        if len(items) == 1:
            return items[0]
        left = items[0]
        right = items[1]
        return Compound(":", (left, right))

    def expr(self, items):
        return items[0]

    def add_expr(self, items):
        if len(items) == 1:
            return items[0]
        # Build left-associative tree
        result = items[0]
        for i in range(1, len(items), 2):
            op = items[i]
            right = items[i + 1]
            result = Compound(str(op), (result, right))
        return result

    def mul_expr(self, items):
        if len(items) == 1:
            return items[0]
        # Build left-associative tree
        result = items[0]
        for i in range(1, len(items), 2):
            op = items[i]
            right = items[i + 1]
            result = Compound(str(op), (result, right))
        return result

    def pow_expr(self, items):
       if len(items) == 1:
           return items[0]
       # Build right-associative tree for exponentiation
       result = items[-1]
       for i in range(len(items) - 2, -1, -2):
           if i > 0:
               result = Compound(str(items[i]), (items[i - 1], result))
       if len(items) % 2 == 0:
           # If even number of items, first item hasn't been added yet
           result = Compound(str(items[1]), (items[0], result))
       return result

    def unary_expr(self, items):
       return items[0]

    def primary(self, items):
       return items[0]
    
    def special_atom_token(self, items):
       """Convert SPECIAL_ATOM_OPS token to an Atom."""
       token_str = str(items[0])
       return Atom(token_str)

    def compound(self, items):
        functor = items[0]
        args = items[1] if len(items) > 1 else []
        return Compound(functor.name, tuple(args))

    def args(self, items):
        return items

    def empty_list(self, items):
        return List(elements=())

    def list_items_only(self, items):
        return List(elements=tuple(items[0]))

    def list_with_tail(self, items):
        elements = items[0]
        tail = items[1]
        return List(elements=tuple(elements), tail=tail)

    def list_items(self, items):
        return items

    def string(self, items):
        # Remove quotes from the string (both double and single quotes)
        s = str(items[0])
        if s.startswith('"') and s.endswith('"'):
            s = s[1:-1]
            # Handle escape sequences in double-quoted strings
            s = self._unescape_string(s)
        elif s.startswith("'") and s.endswith("'"):
            s = s[1:-1]
            # In Prolog, single quotes are escaped by doubling them
            s = s.replace("''", "'")
            # Also handle backslash escapes
            s = self._unescape_string(s)
        return Atom(s)

    def _unescape_string(self, s):
        """Handle backslash escape sequences."""
        # Process escape sequences in the correct order
        # Double backslash must be processed LAST to avoid interfering with other escapes
        # We use a placeholder to preserve \\

        # First, temporarily replace \\ with a placeholder
        placeholder = "\x00BACKSLASH\x00"
        s = s.replace(r"\\", placeholder)

        # Now handle other escape sequences
        replacements = {
            r"\'": "'",
            r"\"": '"',
            r"\n": "\n",
            r"\t": "\t",
            r"\r": "\r",
        }
        for escaped, unescaped in replacements.items():
            s = s.replace(escaped, unescaped)

        # Finally, replace placeholder with single backslash
        s = s.replace(placeholder, "\\")

        return s

    def atom(self, items):
        atom_str = str(items[0])
        # Reject a bare dot as it's a special terminator token
        if atom_str == ".":
            raise PrologThrow(PrologError.syntax_error(
                "Unexpected '.' - dot is a clause terminator and cannot be used as an atom",
                "atom/1"
            ))
        return Atom(atom_str)

    def operator_atom(self, items):
        return Atom(str(items[0]))

    def variable(self, items):
        var_name = str(items[0])
        # Each anonymous variable _ should be unique
        if var_name == "_":
            unique_name = f"_G{self._anon_counter}"
            self._anon_counter += 1
            return Variable(unique_name)
        return Variable(var_name)

    def number(self, items):
        value = str(items[0])
        # Handle hex, octal, binary
        if value.startswith(("-0x", "0x")) or value.startswith(("-0X", "0X")):
            # Hex number
            return Number(int(value, 16))
        elif value.startswith(("-0o", "0o")) or value.startswith(("-0O", "0O")):
            # Octal number
            return Number(int(value, 8))
        elif value.startswith(("-0b", "0b")) or value.startswith(("-0B", "0B")):
            # Binary number
            return Number(int(value, 2))
        elif "'" in value:
            # Base'digits syntax
            return self._parse_base_number(value)
        elif "e" in value.lower() or "." in value:
            # Scientific notation or float
            return Number(float(value))
        else:
            # Regular integer
            return Number(int(value))

    def negative_number(self, items):
        # items[0] is the dash, items[1] is the number
        num = items[1]
        # Negate the number
        if isinstance(num, Number):
            if isinstance(num.value, int):
                return Number(-num.value)
            else:
                return Number(-num.value)
        return num

    def _parse_base_number(self, value):
        """Parse base'digits syntax like 16'ff or -2'abcd."""
        # Handle negative sign
        negative = value.startswith('-')
        if negative:
            value = value[1:]

        # Split by '
        parts = value.split("'", 1)
        base_str, digits_str = parts
        # Base must be a valid integer; token regex guarantees digits for base
        base = int(base_str)
        if not (2 <= base <= 36):
            raise ValueError(f"Base must be between 2 and 36, got {base}")

        # Remove underscores from digits
        digits_clean = digits_str.replace('_', '')

        if not digits_clean:
            raise ValueError(f"Empty digits in {value}")

        # Validate and accumulate value
        result = 0
        for digit in digits_clean.lower():
            # Convert single digit in base up to 36
            digit_val = int(digit, 36)
            if digit_val >= base:
                raise ValueError(f"Invalid digit '{digit}' for base {base} in {value}")
            result = result * base + digit_val

        if negative:
            result = -result

        return Number(result)

    def char_code(self, items):
        """Handle character code notation like 0'X or base'char."""
        code_str = str(items[0])

        # Check for base'char format (e.g., 16'mod'2)
        if "'" in code_str and not code_str.startswith("0'"):
            # This is base'char format - extract just the character
            parts = code_str.split("'")
            if len(parts) >= 2:
                if not parts[1]:
                    # This case, e.g., from `16''`, would cause `ord('')` to crash.
                    # Treating as an invalid char code for now to prevent a crash.
                    return Number(0)
                char = parts[1][0]
                return Number(ord(char))

        # Standard 0'X format
        if code_str.startswith("0'"):
            char_part = code_str[2:]

            # Handle doubled quote escape ''
            if char_part == "''":
                return Number(ord("'"))

            # Handle backslash escape sequences
            if char_part.startswith("\\"):
                if char_part.startswith("\\x"):
                    match = re.fullmatch(r"\\x([0-9a-fA-F]*)\\?", char_part)
                    if not match:
                        raise ValueError("unexpected_char")

                    hex_digits = match.group(1)
                    if len(hex_digits) < 2:
                        raise ValueError("incomplete_reduction")

                    try:
                        return Number(int(hex_digits, 16))
                    except ValueError as exc:
                        raise ValueError("unexpected_char") from exc

                if char_part == "\\\\":
                    # Escaped backslash
                    return Number(ord("\\"))
                if char_part == "\\'":
                    # Escaped single quote
                    return Number(ord("'"))

                if len(char_part) >= 2:
                    # Other escape like \t, \n, etc - just use the second char
                    return Number(ord(char_part[1]))

                raise ValueError("unexpected_char")

            # Regular character
            if char_part:
                return Number(ord(char_part[0]))

        raise ValueError("unexpected_char")

    def curly_braces(self, items):
        """Handle curly braces {X} which is syntax sugar for {}(X)"""
        return Compound("{}", (items[0],))

    def cut(self, items):
        return Cut()


def extract_op_directives(source: str) -> list[tuple[int, str, str]]:
    """Extract op/3 directives from source code.
    
    Args:
        source: Prolog source code string
        
    Returns:
        List of (precedence, type, operator) tuples, in order of appearance
        
    Examples:
        >>> extract_op_directives(":- op(500, xfx, @).")
        [(500, 'xfx', '@')]
    """
    operators = []
    
    # Match :- op(Precedence, Type, Operator).
    # Handles atom, single-quoted atoms, and lists
    pattern = r':-\s*op\s*\(\s*(\d+)\s*,\s*(\w+)\s*,\s*([^)]+)\s*\)\s*\.'
    
    for match in re.finditer(pattern, source):
        precedence_str = match.group(1)
        op_type = match.group(2)
        operator_part = match.group(3).strip()
        
        try:
            precedence = int(precedence_str)
            
            # Handle single operator or list
            # For now, simple cases: atoms or quoted atoms
            operator_names = []
            
            # Try to parse as a list [op1, op2, ...]
            if operator_part.startswith('[') and operator_part.endswith(']'):
                # Simple list parsing
                list_content = operator_part[1:-1]
                for item in re.split(r',', list_content):
                    item = item.strip()
                    # Remove quotes if present
                    if (item.startswith("'") and item.endswith("'")) or \
                       (item.startswith('"') and item.endswith('"')):
                        operator_names.append(item[1:-1])
                    else:
                        operator_names.append(item)
            else:
                # Single operator (quoted or unquoted)
                if (operator_part.startswith("'") and operator_part.endswith("'")) or \
                   (operator_part.startswith('"') and operator_part.endswith('"')):
                    operator_names.append(operator_part[1:-1])
                else:
                    operator_names.append(operator_part)
            
            for op_name in operator_names:
                operators.append((precedence, op_type, op_name))
        except (ValueError, AttributeError):
            # Skip malformed directives - they'll be caught by parser
            pass
    
    return operators


def escape_for_lark(s: str) -> str:
    """Escape special characters for use in Lark grammar.
    
    Args:
        s: String to escape
        
    Returns:
        Escaped string safe for Lark grammar
    """
    # Escape backslashes and quotes
    s = s.replace("\\", "\\\\")
    s = s.replace('"', '\\"')
    return s


def generate_operator_rules(operators: list[tuple[int, str, str]]) -> str:
    """Generate grammar extension rules for custom operators.
    
    Args:
        operators: List of (precedence, type, operator) tuples
        
    Returns:
        String containing additional grammar rules for custom operators
    """
    if not operators:
        return ""
    
    # Group operators by their precedence and type
    # Build rules to integrate custom operators into the expression hierarchy
    infix_ops = {}  # precedence -> list of operators
    prefix_ops = {}  # precedence -> list of operators
    postfix_ops = {}  # precedence -> list of operators
    
    for prec, op_type, op_name in operators:
        if op_type in ('xfx', 'xfy', 'yfx', 'yfy'):
            if prec not in infix_ops:
                infix_ops[prec] = []
            infix_ops[prec].append((op_type, op_name))
        elif op_type in ('fx', 'fy'):
            if prec not in prefix_ops:
                prefix_ops[prec] = []
            prefix_ops[prec].append((op_type, op_name))
        elif op_type in ('xf', 'yf'):
            if prec not in postfix_ops:
                postfix_ops[prec] = []
            postfix_ops[prec].append((op_type, op_name))
    
    # For now, return empty - full implementation would need careful precedence handling
    # This is a placeholder for the complex grammar generation logic
    return ""


class PrologParser:
    """Parse Prolog source code with support for dynamic operators."""

    def __init__(self, operator_table=None):
        self.operator_table = operator_table
        self._grammar_cache = {}  # Cache compiled grammars by operator set
        self.parser = self._create_parser(None)

    def _create_parser(self, operators: list[tuple[int, str, str]] | None):
        """Create a Lark parser, optionally with custom operators.
        
        Args:
            operators: List of (precedence, type, operator) tuples, or None for base grammar
            
        Returns:
            Lark parser instance
        """
        grammar = PROLOG_GRAMMAR
        
        # TODO: Future enhancement - generate grammar with custom operators
        # For now, we use the base grammar but operators are available for future use
        
        return Lark(
            grammar, parser="earley", propagate_positions=True, ambiguity='resolve'
        )

    def _strip_block_comments(self, text: str) -> tuple[str, list[tuple[int, str]]]:
        """Strip block comments from text, handling nesting and quoted strings.
        Returns cleaned text and list of (position, pldoc_comment) tuples."""
        result = []
        pldoc_comments = []
        i = 0
        in_single_quote = False
        in_double_quote = False
        escape_next = False
        while i < len(text):
            if not in_single_quote and not in_double_quote and (text.startswith('/*', i) or text.startswith('/**', i) or text.startswith('/*!', i)):
                comment_start = i
                depth = 1
                if text.startswith('/**', i) or text.startswith('/*!', i):
                    i += 3  # Skip /** or /*!
                    is_pldoc = True
                else:
                    i += 2  # Skip /*
                    is_pldoc = False
                comment_content = []
                while i < len(text) and depth > 0:
                    if text.startswith('/*', i):
                        depth += 1
                        i += 2
                    elif text.startswith('*/', i):
                        depth -= 1
                        if depth == 0:
                            # End of comment
                            if is_pldoc:
                                pldoc_comments.append((comment_start, ''.join(comment_content)))
                            i += 2
                            break
                        else:
                            i += 2
                    else:
                        if is_pldoc:
                            comment_content.append(text[i])
                        i += 1
                if depth > 0:
                    raise ValueError("Unterminated block comment")
                continue  # Skip the comment

            char = text[i]
            result.append(char)

            if escape_next:
                escape_next = False
            elif char == '\\':
                escape_next = True
            elif char == "'" and not in_double_quote:
                in_single_quote = not in_single_quote
            elif char == '"' and not in_single_quote:
                in_double_quote = not in_double_quote

            i += 1
        return ''.join(result), pldoc_comments

    def _collect_pldoc_comments(self, text: str) -> tuple[str, list[tuple[int, str]]]:
        """Collect PlDoc comments and return cleaned text with positions in cleaned text."""
        pldoc_comments = []
        cleaned = []
        i = 0
        in_single_quote = False
        in_double_quote = False
        escape_next = False
        while i < len(text):
            if not in_single_quote and not in_double_quote:
                if text.startswith('%%', i):
                    # Line comment
                    pos = len(''.join(cleaned))  # position in cleaned text
                    # Find end of line
                    end = text.find('\n', i)
                    if end == -1:
                        end = len(text)
                    comment = text[i+2:end].rstrip('\n')
                    pldoc_comments.append((pos, comment))
                    i = end + 1 if end < len(text) else len(text)
                    continue
                elif text.startswith('/*', i):
                    # Block comment
                    pos = len(''.join(cleaned))
                    is_pldoc = text.startswith('/**', i) or text.startswith('/*!', i)
                    i += 3 if is_pldoc else 2
                    comment_content = []
                    depth = 1
                    while i < len(text) and depth > 0:
                        if text.startswith('/*', i):
                            depth += 1
                            i += 2
                        elif text.startswith('*/', i):
                            depth -= 1
                            if depth == 0:
                                if is_pldoc:
                                    pldoc_comments.append((pos, ''.join(comment_content)))
                                i += 2
                                break
                            else:
                                i += 2
                        else:
                            if is_pldoc:
                                comment_content.append(text[i])
                            i += 1
                    if depth > 0:
                        raise ValueError("Unterminated block comment")
                    continue
            # Not in comment, add char
            cleaned.append(text[i])
            if escape_next:
                escape_next = False
            elif text[i] == '\\':
                escape_next = True
            elif text[i] == "'" and not in_double_quote:
                in_single_quote = not in_single_quote
            elif text[i] == '"' and not in_single_quote:
                in_double_quote = not in_double_quote
            i += 1
        cleaned_text = ''.join(cleaned)
        pldoc_comments.sort(key=lambda x: x[0])
        return cleaned_text, pldoc_comments

    def _associate_pldoc_comments(self, items: list, comments: list[tuple[int, str]]):
        """Associate PlDoc comments with clauses/directives."""
        if not comments:
            return

        # Robust association: attach each comment to the next item by source position.
        # We rely on Lark's propagate_positions to furnish item.meta.start_pos where available.
        entities: list[tuple[int, str, object]] = []
        for pos, text in comments:
            entities.append((pos, 'comment', text))
        for item in items:
            start_pos = getattr(item.meta, 'start_pos', None) if hasattr(item, 'meta') else None
            if start_pos is not None:
                entities.append((start_pos, 'item', item))

        entities.sort(key=lambda x: x[0])
        last_comment_text: str | None = None
        for _, entity_type, payload in entities:
            if entity_type == 'comment':
                last_comment_text = payload
            elif entity_type == 'item':
                if last_comment_text is not None:
                    payload.doc = last_comment_text
                last_comment_text = None

    def parse(self, text: str, context: str = "parse/1") -> list[Clause | Directive]:
        """Parse Prolog source code and return list of clauses."""
        try:
            text, pldoc_comments = self._collect_pldoc_comments(text)
            tree = self.parser.parse(text)
            transformer = PrologTransformer()
            parsed_items = transformer.transform(tree)
            # Associate PlDoc comments with items
            self._associate_pldoc_comments(parsed_items, pldoc_comments)
            return parsed_items
        except (UnexpectedToken, UnexpectedCharacters) as e:
            # If the lexer/parser choked inside a char code hex escape like 0'\x4G,
            # surface the ISO-style unexpected_char error rather than the raw Lark token message.
            last_token = getattr(e, "token_history", None)
            if last_token:
                last_token = last_token[-1]
                if getattr(last_token, "type", None) == "CHAR_CODE" and str(last_token).startswith(
                    "0'\\x"
                ):
                    error_term = PrologError.syntax_error("unexpected_char", context)
                    raise PrologThrow(error_term)
            error_term = PrologError.syntax_error(str(e), context)
            # We handle these specific Lark errors here so they are normalized to
            # `PrologThrow` before control leaves the try; otherwise the outer
            # `LarkError` handler never runs and tests break.
            raise PrologThrow(error_term)
        except LarkError as e:
            # Convert Lark parse error to Prolog syntax_error
            error_term = PrologError.syntax_error(str(e), context)
            raise PrologThrow(error_term)
        except ValueError as e:
            # Unterminated comment
            error_term = PrologError.syntax_error(str(e), context)
            raise PrologThrow(error_term)

    def parse_term(self, text: str, context: str = "parse_term/1") -> Any:
        """Parse a single Prolog term."""
        try:
            # Add a period to make it a valid clause
            text, _ = self._collect_pldoc_comments(f"dummy({text}).")
            tree = self.parser.parse(text)
            transformer = PrologTransformer()
            result = transformer.transform(tree)
            if result and isinstance(result[0], Clause):
                compound = result[0].head
                if isinstance(compound, Compound) and compound.args:
                    return compound.args[0]
            raise ValueError(f"Failed to parse term: {text}")
        except LarkError as e:
            # Convert Lark parse error to Prolog syntax_error
            error_term = PrologError.syntax_error(str(e), context)
            raise PrologThrow(error_term)
        except ValueError as e:
            # Unterminated comment or parse error
            error_term = PrologError.syntax_error(str(e), context)
            raise PrologThrow(error_term)
