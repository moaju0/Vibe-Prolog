#!/usr/bin/env python3
"""
Find built-in predicates used in Prolog programs.

Scans a directory of .pl files and identifies which predicates are called
but not defined, likely indicating they're built-in predicates.
"""

from __future__ import annotations

import sys
from collections import defaultdict
from pathlib import Path
from typing import Dict, Set

# Use package-installed imports; do not modify sys.path
from vibeprolog.parser import PrologParser, Clause, Directive, PredicatePropertyDirective  # noqa: E402
from vibeprolog.terms import Compound, Atom, Number  # noqa: E402
from vibeprolog.engine import PrologEngine  # noqa: E402


def extract_calls(term, calls: Set[tuple[str, int]]) -> None:
    """Recursively extract predicate calls from a term."""
    if isinstance(term, Compound):
        # Add this compound as a call
        calls.add((term.functor, len(term.args)))

        # Recursively process arguments
        for arg in term.args:
            extract_calls(arg, calls)
    elif isinstance(term, list):
        for item in term:
            extract_calls(item, calls)


def get_known_builtins() -> Set[tuple[str, int]]:
    """Get the set of built-in predicates registered in Vibe-Prolog."""
    # Create a minimal engine just to get the built-in registry
    engine = PrologEngine([])
    return set(engine._builtin_registry.keys())


def analyze_file(
    filepath: Path,
) -> tuple[Set[tuple[str, int]], Set[tuple[str, int]], Set[tuple[str, int]]]:
    """
    Analyze a Prolog file to find defined predicates, called predicates, and dynamic declarations.

    Returns:
        (defined_predicates, called_predicates, dynamic_predicates)
    """
    parser = PrologParser()

    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        print(f"Warning: Could not read {filepath}: {e}", file=sys.stderr)
        return set(), set(), set()

    try:
        items = parser.parse(content, str(filepath))
    except Exception as e:
        print(f"Warning: Could not parse {filepath}: {e}", file=sys.stderr)
        return set(), set(), set()

    defined: set[tuple[str, int]] = set()
    called: set[tuple[str, int]] = set()
    dynamic: set[tuple[str, int]] = set()

    for item in items:
        if isinstance(item, Clause):
            # Head of the clause defines the predicate
            head = item.head
            if isinstance(head, Compound):
                defined.add((head.functor, len(head.args)))
            elif isinstance(head, Atom):
                defined.add((head.name, 0))

            # Body goals are potential calls
            if item.body:
                for goal in item.body:
                    extract_calls(goal, called)
        elif isinstance(item, Directive):
            # Dynamic directive: :- dynamic Foo/Arity, ...
            if isinstance(item.goal, PredicatePropertyDirective) and item.goal.property == 'dynamic':
                for indicator in item.goal.indicators:
                    if isinstance(indicator, Compound) and indicator.functor == '/':
                        name_term, arity_term = indicator.args
                        if isinstance(name_term, Atom) and isinstance(arity_term, Number):
                            dynamic.add((name_term.name, int(arity_term.value)))

            # Also extract calls inside directives (e.g., queries like :- ?- goal.)
            if hasattr(item, 'goal'):
                extract_calls(item.goal, called)

    return defined, called, dynamic


def find_builtins(
    directory: Path,
) -> tuple[
    Dict[tuple[str, int], Set[Path]],
    Dict[tuple[str, int], Set[Path]],
    Dict[tuple[str, int], Set[Path]],
]:
    """
    Find predicates used in Prolog files in a directory.

    Returns:
        (actual_builtins, dynamic_predicates, undefined_predicates)
    """
    all_defined = set()
    all_called = defaultdict(set)
    all_dynamic = set()

    # Get known built-ins from Vibe-Prolog
    print("Loading Vibe-Prolog built-ins registry...", file=sys.stderr)
    known_builtins = get_known_builtins()

    # Find all .pl files
    pl_files = list(directory.rglob("*.pl"))

    if not pl_files:
        print(f"No .pl files found in {directory}", file=sys.stderr)
        return {}, {}, {}

    print(f"Analyzing {len(pl_files)} Prolog files in {directory}...\n", file=sys.stderr)

    # Analyze each file
    for filepath in pl_files:
        defined, called, dynamic = analyze_file(filepath)
        all_defined.update(defined)
        all_dynamic.update(dynamic)

        for predicate in called:
            all_called[predicate].add(filepath)

    # Categorize predicates that are called but not defined
    actual_builtins = {}
    dynamic_predicates = {}
    undefined_predicates = {}

    # Exclude meta predicates and control structures
    excluded = {',', ';', '->', '!', 'true', 'false'}

    for predicate, files in all_called.items():
        if predicate not in all_defined:
            functor, _ = predicate
            if functor not in excluded:
                if predicate in known_builtins:
                    actual_builtins[predicate] = files
                elif predicate in all_dynamic:
                    dynamic_predicates[predicate] = files
                else:
                    undefined_predicates[predicate] = files

    return actual_builtins, dynamic_predicates, undefined_predicates


def main():
    """Main entry point."""
    if len(sys.argv) != 2:
        print("Usage: python find_builtins.py <directory>")
        print("\nExample:")
        print("  python find_builtins.py examples/")
        sys.exit(1)

    directory = Path(sys.argv[1])

    if not directory.exists():
        print(f"Error: Directory {directory} does not exist", file=sys.stderr)
        sys.exit(1)

    if not directory.is_dir():
        print(f"Error: {directory} is not a directory", file=sys.stderr)
        sys.exit(1)

    actual_builtins, dynamic_predicates, undefined_predicates = find_builtins(
        directory
    )

    def print_category(title: str, predicates: dict, color: str = ""):
        """Print a category of predicates."""
        if not predicates:
            return

        sorted_preds = sorted(predicates.items(), key=lambda x: (x[0][0], x[0][1]))

        print("=" * 80)
        print(f"{title}")
        print("=" * 80)
        print()

        current_functor = None
        for (functor, arity), files in sorted_preds:
            if functor != current_functor:
                if current_functor is not None:
                    print()
                current_functor = functor

            predicate_name = f"{functor}/{arity}"
            file_list = ", ".join(f.name for f in sorted(files))
            print(f"  {predicate_name:30s} used in: {file_list}")

        print()
        unique_files = len(set(f for files in predicates.values() for f in files))
        print(f"Total: {len(predicates)} predicates in {unique_files} files")
        print()

    # Print actual built-ins (registered in Vibe-Prolog)
    print_category("ACTUAL BUILT-IN PREDICATES (registered in Vibe-Prolog)", actual_builtins)

    # Print dynamic predicates (declared but asserted at runtime)
    print_category("DYNAMIC PREDICATES (declared :- dynamic, asserted at runtime)", dynamic_predicates)

    # Print undefined predicates (might be missing implementations or typos)
    print_category(
        "UNDEFINED PREDICATES (not built-in, not dynamic, not defined - might be missing)",
        undefined_predicates
    )

    # Summary
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"  Actual built-ins:      {len(actual_builtins)}")
    print(f"  Dynamic predicates:    {len(dynamic_predicates)}")
    print(f"  Undefined predicates:  {len(undefined_predicates)}")
    print("=" * 80)


if __name__ == "__main__":
    main()
