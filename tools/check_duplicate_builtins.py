"""Detect Prolog library files that redefine built-in predicates.

Scans every `.pl` file in `library/` and reports predicates whose clause
heads match the interpreter's built-in registry.  This helps prevent
permission errors when consulting modules that would otherwise redefine a
static built-in.
"""

from __future__ import annotations

import argparse
from collections import defaultdict
from pathlib import Path
import sys
from typing import Iterable

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:  # pragma: no cover - sys.path hygiene
    sys.path.insert(0, str(REPO_ROOT))

from vibeprolog import PrologInterpreter
from vibeprolog.parser import tokenize_prolog_statements, _split_top_level_commas


def collect_builtin_predicates() -> set[tuple[str, int]]:
    """Return all predicates marked as built-ins by the interpreter."""
    interpreter = PrologInterpreter()
    interpreter._ensure_builtin_properties()  # Seed built-in registry
    return {
        key for key, props in interpreter.predicate_properties.items() if "built_in" in props
    }


def iter_library_files(root: Path) -> Iterable[Path]:
    """Yield every .pl file under the given root directory."""
    yield from sorted(root.rglob("*.pl"))


def _normalize_functor(raw: str) -> str | None:
    """Strip module qualification and validate atom name."""
    functor = raw.split(":")[-1].strip()
    if not functor:
        return None
    if functor.startswith("'") and functor.endswith("'"):
        return functor  # quoted atom
    if not functor[0].islower():
        return None
    return functor


def _count_arguments(arg_src: str) -> int:
    if not arg_src.strip():
        return 0
    args = [part for part in _split_top_level_commas(arg_src) if part]
    return len(args)


def _extract_head_indicator(statement: str) -> tuple[str, int] | None:
    stripped = statement.strip()
    if not stripped:
        return None
    if stripped.startswith(":-") or stripped.startswith("?-"):
        return None

    clause = stripped[:-1] if stripped.endswith(".") else stripped
    if "-->" in clause:
        return None
    clause = clause.split(":-", 1)[0]

    clause = clause.strip()
    if not clause:
        return None

    if "(" not in clause:
        functor = _normalize_functor(clause)
        if functor is None:
            return None
        return functor, 0

    functor_part, rest = clause.split("(", 1)
    functor = _normalize_functor(functor_part)
    if functor is None:
        return None
    if ")" not in rest:
        return None
    args_src = rest.rsplit(")", 1)[0]
    arity = _count_arguments(args_src)
    return functor, arity


def _split_statements(text: str) -> list[str]:
    try:
        return tokenize_prolog_statements(text)
    except ValueError:
        # Fallback: naive split on periods to avoid aborting the check entirely.
        return [f"{chunk.strip()}." for chunk in text.split(".") if chunk.strip()]


def detect_duplicates(path: Path, builtins: set[tuple[str, int]]) -> set[str]:
    """Return built-in predicate indicators that are redefined in the file."""
    statements = _split_statements(path.read_text())

    duplicates: set[str] = set()
    for statement in statements:
        key = _extract_head_indicator(statement)
        if key is None:
            continue
        if key in builtins:
            duplicates.add(f"{key[0]}/{key[1]}")
    return duplicates


def main() -> None:
    arg_parser = argparse.ArgumentParser(description=__doc__)
    arg_parser.add_argument(
        "--library-dir",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "library",
        help="Directory containing Prolog library modules (default: %(default)s)",
    )
    args = arg_parser.parse_args()

    builtins = collect_builtin_predicates()

    duplicates_by_file: dict[Path, set[str]] = defaultdict(set)
    for file_path in iter_library_files(args.library_dir):
        file_duplicates = detect_duplicates(file_path, builtins)
        if file_duplicates:
            duplicates_by_file[file_path] = file_duplicates

    if not duplicates_by_file:
        print("No duplicate built-in predicate definitions found in", args.library_dir)
        return

    print("Duplicate built-in predicate definitions detected:")
    for path in sorted(duplicates_by_file.keys()):
        indicators = ", ".join(sorted(duplicates_by_file[path]))
        print(f"  {path}: {indicators}")


if __name__ == "__main__":
    main()
