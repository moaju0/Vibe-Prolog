#!/usr/bin/env python3
"""
Check which operators are used in a Prolog file and compare with what's supported.

Analyzes a Prolog library file to find:
- Which operators are used in the code
- Which are supported by Vibe-Prolog
- Which are missing but required

Outputs a markdown report.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import Set

from vibeprolog.operator_defaults import DEFAULT_OPERATORS

# ISO Prolog standard operators (from ISO/IEC 13211-1)
ISO_OPERATORS = [
    (1200, "xfx", ":-"),
    (1200, "xfx", "-->"),
    (1200, "fx", ":-"),
    (1200, "fx", "?-"),
    (1100, "xfy", ";"),
    (1050, "xfy", "->"),
    (1000, "xfy", ","),
    (900, "fy", "\\+"),
    (700, "xfx", "="),
    (700, "xfx", "\\="),
    (700, "xfx", "=="),
    (700, "xfx", "\\=="),
    (700, "xfx", "@<"),
    (700, "xfx", "@=<"),
    (700, "xfx", "@>"),
    (700, "xfx", "@>="),
    (700, "xfx", "=.."),
    (700, "xfx", "is"),
    (700, "xfx", "=:="),
    (700, "xfx", "=\\="),
    (700, "xfx", "<"),
    (700, "xfx", "=<"),
    (700, "xfx", ">"),
    (700, "xfx", ">="),
    (600, "xfy", ":"),
    (500, "yfx", "+"),
    (500, "yfx", "-"),
    (500, "yfx", "/\\"),  # bitwise and
    (500, "yfx", "\\/"),  # bitwise or
    (400, "yfx", "*"),
    (400, "yfx", "/"),
    (400, "yfx", "//"),
    (400, "yfx", "rem"),
    (400, "yfx", "mod"),
    (400, "yfx", "div"),
    (400, "yfx", "<<"),  # bitwise shift left
    (400, "yfx", ">>"),  # bitwise shift right
    (200, "xfx", "**"),
    (200, "xfy", "^"),
    (200, "fy", "-"),
    (200, "fy", "+"),
    (200, "fy", "\\"),  # bitwise complement
]


def strip_comments_and_strings(code: str) -> str:
    """Remove comments and strings from Prolog code to avoid false matches."""
    # Remove line comments
    code = re.sub(r'%.*$', '', code, flags=re.MULTILINE)

    # Remove block comments (handle nesting)
    depth = 0
    result = []
    i = 0
    while i < len(code):
        if i < len(code) - 1:
            if code[i:i+2] == '/*':
                depth += 1
                i += 2
                continue
            elif code[i:i+2] == '*/':
                depth -= 1
                i += 2
                continue

        if depth == 0:
            result.append(code[i])
        i += 1

    code = ''.join(result)

    # Remove quoted strings and atoms
    code = re.sub(r'"([^"\\]|\\.)*"', '""', code)
    code = re.sub(r"'([^'\\]|\\.)*'", "''", code)

    return code


def find_operators_in_code(filepath: Path) -> Set[str]:
    """
    Find operators used in a Prolog file.

    Returns a set of operator symbols found in the code.
    """
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            code = f.read()
    except Exception as e:
        print(f"Error: Could not read {filepath}: {e}", file=sys.stderr)
        sys.exit(1)

    # Strip comments and strings
    code = strip_comments_and_strings(code)

    # All possible operators to search for
    all_operators = set()
    for _, _, op in ISO_OPERATORS:
        all_operators.add(op)
    for _, _, op in DEFAULT_OPERATORS:
        all_operators.add(op)

    # Find which operators are actually used
    found_operators = set()

    for op in all_operators:
        # Escape special regex characters
        escaped_op = re.escape(op)

        # Look for the operator with word boundaries or spacing
        # This is a heuristic and might have false positives/negatives
        pattern = rf'(?:^|\s|\(|\[){escaped_op}(?:\s|\)|]|$)'

        if re.search(pattern, code, re.MULTILINE):
            found_operators.add(op)

    return found_operators


def find_declared_operators(filepath: Path) -> Set[tuple[int, str, str]]:
    """
    Find operators declared in :- op(...) directives or module exports.

    Returns a set of (precedence, associativity, operator) tuples.
    """
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            code = f.read()
    except Exception as e:
        return set()

    declared = set()

    # Find :- op(Prec, Type, Name) directives
    op_pattern = r':-\s*op\s*\(\s*(\d+)\s*,\s*([a-z]+)\s*,\s*([^\)]+)\)'
    for match in re.finditer(op_pattern, code):
        prec = int(match.group(1))
        assoc = match.group(2)
        name = match.group(3).strip()
        # Remove quotes if present
        name = name.strip("'\"")
        declared.add((prec, assoc, name))

    # Find operators in module exports
    module_pattern = r':-\s*module\s*\([^,]+,\s*\[(.*?)\]\s*\)'
    for match in re.finditer(module_pattern, code, re.DOTALL):
        exports = match.group(1)
        # Find op(...) in exports
        for op_match in re.finditer(r'op\s*\(\s*(\d+)\s*,\s*([a-z]+)\s*,\s*([^\)]+)\)', exports):
            prec = int(op_match.group(1))
            assoc = op_match.group(2)
            name = op_match.group(3).strip().strip("'\"")
            declared.add((prec, assoc, name))

    return declared


def main():
    """Main entry point."""
    if len(sys.argv) != 2:
        print("Usage: python check_operators.py <prolog_file>")
        print("\nExample:")
        print("  python check_operators.py library/clpb.pl")
        sys.exit(1)

    filepath = Path(sys.argv[1])

    if not filepath.exists():
        print(f"Error: File {filepath} does not exist", file=sys.stderr)
        sys.exit(1)

    if not filepath.is_file():
        print(f"Error: {filepath} is not a file", file=sys.stderr)
        sys.exit(1)

    print(f"# Operator Analysis: {filepath.name}\n", file=sys.stderr)
    print(f"Analyzing {filepath}...\n", file=sys.stderr)

    # Find operators used in code
    used_operators = find_operators_in_code(filepath)

    # Find operators declared in the file
    declared_operators = find_declared_operators(filepath)

    # Build sets for comparison
    supported_ops = {op for _, _, op in DEFAULT_OPERATORS}
    iso_ops = {op for _, _, op in ISO_OPERATORS}

    # Categorize operators
    used_and_supported = used_operators & supported_ops
    used_but_unsupported = used_operators - supported_ops

    # Further categorize unsupported
    unsupported_iso = used_but_unsupported & iso_ops
    unsupported_non_iso = used_but_unsupported - iso_ops

    # Categorize declared operators
    declared_op_names = {op for _, _, op in declared_operators}
    declared_supported = declared_op_names & supported_ops
    declared_unsupported = declared_op_names - supported_ops

    # Output markdown report
    print("# Operator Analysis Report")
    print()
    print(f"**File:** `{filepath}`")
    print()
    print(f"**Operators found in code:** {len(used_operators)}")
    print(f"**Operators declared:** {len(declared_operators)}")
    print()

    if declared_operators:
        print("## Declared Operators")
        print()
        print("Operators explicitly declared in `:- op(...)` directives or module exports:")
        print()
        for prec, assoc, op in sorted(declared_operators):
            status = "✅ Supported" if op in supported_ops else "❌ Not supported"
            print(f"- `{op}` ({prec}, {assoc}) - {status}")
        print()

    print("## Supported Operators Used")
    print()
    if used_and_supported:
        print("These operators are used in the file and supported by Vibe-Prolog:")
        print()
        for op in sorted(used_and_supported):
            print(f"- `{op}`")
    else:
        print("*None found*")
    print()

    print("## Unsupported ISO Operators Used")
    print()
    if unsupported_iso:
        print("These **ISO-required** operators are used but not currently supported:")
        print()
        for op in sorted(unsupported_iso):
            # Find the ISO spec for this operator
            specs = [f"({prec}, {assoc})" for prec, assoc, o in ISO_OPERATORS if o == op]
            spec_str = ", ".join(specs)
            print(f"- `{op}` - ISO spec: {spec_str}")
    else:
        print("*None found* ✅")
    print()

    print("## Unsupported Non-ISO Operators Used")
    print()
    if unsupported_non_iso:
        print("These non-ISO operators are used but not currently supported:")
        print()
        for op in sorted(unsupported_non_iso):
            print(f"- `{op}`")
    else:
        print("*None found*")
    print()

    # Summary
    print("## Summary")
    print()
    print(f"- ✅ Supported operators used: {len(used_and_supported)}")
    print(f"- ❌ Unsupported ISO operators used: {len(unsupported_iso)}")
    print(f"- ⚠️ Unsupported non-ISO operators used: {len(unsupported_non_iso)}")
    print()

    if unsupported_iso:
        print("**Action Required:** Add missing ISO operators to `vibeprolog/operator_defaults.py`")

    # Exit with error code if unsupported ISO operators found
    if unsupported_iso:
        sys.exit(1)


if __name__ == "__main__":
    main()
