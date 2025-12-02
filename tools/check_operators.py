#!/usr/bin/env python3
"""
Check which operators are used in a Prolog file and compare with what's supported.

Analyzes a Prolog library file to find:
- Which operators are used in the code
- Which are supported by Vibe-Prolog
- Which are missing but required

Outputs a markdown report by default, or JSON with --json flag.
"""

from __future__ import annotations

import argparse
import json
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


def output_json(filepath: Path, data: dict) -> None:
    """Output results in JSON format."""
    json.dump(data, sys.stdout, indent=2)
    print()  # Add newline at end


def output_markdown(filepath: Path, data: dict) -> None:
    """Output results in markdown format."""
    print("# Operator Analysis Report")
    print()
    print(f"**File:** `{filepath}`")
    print()
    print(f"**Operators found in code:** {data['operators_found_count']}")
    print(f"**Operators declared in file:** {data['operators_declared_count']}")
    print()

    if data['declared_operators']:
        print("## Operators Declared in File")
        print()
        print("These operators are explicitly declared via `:- op(...)` directives or module exports")
        print("and will be loaded automatically when this file is consulted:")
        print()
        for op_info in data['declared_operators']:
            print(f"- `{op_info['operator']}` ({op_info['precedence']}, {op_info['associativity']}) - {op_info['status']}")
        print()

    print("## Operators Available (Defaults + Declared)")
    print()
    if data['available_operators']:
        print("These operators are used and will be available (either in defaults or declared in file):")
        print()
        for op_info in data['available_operators']:
            print(f"- `{op_info['operator']}` ({op_info['source']})")
    else:
        print("*None found*")
    print()

    print("## Missing ISO Operators")
    print()
    if data['missing_iso_operators']:
        print("These **ISO-required** operators are used but NOT declared in file and NOT in defaults:")
        print()
        for op_info in data['missing_iso_operators']:
            print(f"- `{op_info['operator']}` - ISO spec: {op_info['iso_spec']}")
        print()
        print("**These should be added to `vibeprolog/operator_defaults.py`**")
    else:
        print("*None found* ✅")
    print()

    print("## Missing Non-ISO Operators")
    print()
    if data['missing_non_iso_operators']:
        print("These non-ISO operators are used but NOT declared in file and NOT in defaults:")
        print()
        for op in data['missing_non_iso_operators']:
            print(f"- `{op}`")
        print()
        print("**Note:** These may be typos or need to be declared with `:- op(...)` directives.")
    else:
        print("*None found*")
    print()

    # Summary
    print("## Summary")
    print()
    print(f"- ✅ Operators available (defaults + declared): {data['summary']['available_count']}")
    print(f"- ❌ Missing ISO operators: {data['summary']['missing_iso_count']}")
    print(f"- ⚠️ Missing non-ISO operators: {data['summary']['missing_non_iso_count']}")
    print()


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Check which operators are used in a Prolog file and compare with what's supported."
    )
    parser.add_argument("file", type=Path, help="Prolog file to analyze")
    parser.add_argument("--json", action="store_true", help="Output results in JSON format")

    args = parser.parse_args()
    filepath = args.file

    if not filepath.exists():
        print(f"Error: File {filepath} does not exist", file=sys.stderr)
        sys.exit(1)

    if not filepath.is_file():
        print(f"Error: {filepath} is not a file", file=sys.stderr)
        sys.exit(1)

    if not args.json:
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
    declared_op_names = {op for _, _, op in declared_operators}

    # Operators that will be available (either in defaults or declared in this file)
    available_ops = supported_ops | declared_op_names

    used_and_available = used_operators & available_ops
    used_but_missing = used_operators - available_ops

    # Further categorize missing operators (not in defaults AND not declared in file)
    missing_iso = used_but_missing & iso_ops
    missing_non_iso = used_but_missing - iso_ops

    # Build data structure for output
    data = {
        "file": str(filepath),
        "operators_found_count": len(used_operators),
        "operators_declared_count": len(declared_operators),
        "declared_operators": [
            {
                "operator": op,
                "precedence": prec,
                "associativity": assoc,
                "status": "in defaults" if op in supported_ops else "extension"
            }
            for prec, assoc, op in sorted(declared_operators)
        ],
        "available_operators": [
            {
                "operator": op,
                "source": "default" if op in supported_ops else "declared in file"
            }
            for op in sorted(used_and_available)
        ],
        "missing_iso_operators": [
            {
                "operator": op,
                "iso_spec": ", ".join(f"({prec}, {assoc})" for prec, assoc, o in ISO_OPERATORS if o == op)
            }
            for op in sorted(missing_iso)
        ],
        "missing_non_iso_operators": sorted(list(missing_non_iso)),
        "summary": {
            "available_count": len(used_and_available),
            "missing_iso_count": len(missing_iso),
            "missing_non_iso_count": len(missing_non_iso)
        }
    }

    # Output in requested format
    if args.json:
        output_json(filepath, data)
    else:
        output_markdown(filepath, data)

    # Exit with error code if missing ISO operators found
    if missing_iso:
        sys.exit(1)


if __name__ == "__main__":
    main()
