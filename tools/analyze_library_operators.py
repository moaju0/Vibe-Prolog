#!/usr/bin/env python3
"""
Analyze all Prolog files in the library directory for operator usage.

Runs check_operators.py on each .pl file in library/ and consolidates
the results into a single status.json file showing:
- Which operators are missing across all files
- Which files use which operators
- Summary statistics
"""

from __future__ import annotations

import json
import subprocess
import sys
from collections import defaultdict
from pathlib import Path


def run_check_operators(filepath: Path) -> dict | None:
    """
    Run check_operators.py on a file and return the JSON result.

    Returns None if the check fails or the file cannot be analyzed.
    """
    try:
        result = subprocess.run(
            [sys.executable, "tools/check_operators.py", str(filepath), "--json"],
            capture_output=True,
            text=True,
            check=False  # Don't raise on non-zero exit (expected for files with missing ops)
        )

        # Parse JSON output (ignoring stderr which has progress messages)
        if result.stdout.strip():
            return json.loads(result.stdout)
        else:
            print(f"Warning: No output from {filepath}", file=sys.stderr)
            return None

    except Exception as e:
        print(f"Error analyzing {filepath}: {e}", file=sys.stderr)
        return None


def consolidate_results(results: dict[str, dict]) -> dict:
    """
    Consolidate results from multiple files into a summary.

    Args:
        results: Dictionary mapping file paths to their check_operators.py results

    Returns:
        Consolidated analysis with aggregated missing operators and statistics
    """
    # Aggregate missing ISO operators across all files
    missing_iso_by_operator = defaultdict(lambda: {"iso_spec": "", "used_in_files": []})

    # Aggregate missing non-ISO operators across all files
    missing_non_iso_by_operator = defaultdict(lambda: {"used_in_files": []})

    # Track all available operators
    all_available_operators = set()

    # Process each file's results
    for filepath, data in results.items():
        if data is None:
            continue

        # Collect available operators
        for op_info in data.get("available_operators", []):
            all_available_operators.add(op_info["operator"])

        # Aggregate missing ISO operators
        for op_info in data.get("missing_iso_operators", []):
            op = op_info["operator"]
            missing_iso_by_operator[op]["iso_spec"] = op_info["iso_spec"]
            missing_iso_by_operator[op]["used_in_files"].append(filepath)

        # Aggregate missing non-ISO operators
        for op in data.get("missing_non_iso_operators", []):
            missing_non_iso_by_operator[op]["used_in_files"].append(filepath)

    # Build consolidated output
    consolidated = {
        "analyzed_files": sorted(list(results.keys())),
        "missing_iso_operators": {
            op: {
                "iso_spec": info["iso_spec"],
                "used_in_files": sorted(info["used_in_files"]),
                "file_count": len(info["used_in_files"])
            }
            for op, info in sorted(missing_iso_by_operator.items())
        },
        "missing_non_iso_operators": {
            op: {
                "used_in_files": sorted(info["used_in_files"]),
                "file_count": len(info["used_in_files"])
            }
            for op, info in sorted(missing_non_iso_by_operator.items())
        },
        "available_operators": sorted(list(all_available_operators)),
        "per_file_results": results,
        "summary": {
            "total_files_analyzed": len([r for r in results.values() if r is not None]),
            "total_files_with_errors": len([r for r in results.values() if r is None]),
            "unique_missing_iso_operators": len(missing_iso_by_operator),
            "unique_missing_non_iso_operators": len(missing_non_iso_by_operator),
            "unique_available_operators": len(all_available_operators)
        }
    }

    return consolidated


def main():
    """Main entry point."""
    # Find library directory
    library_dir = Path("library")

    if not library_dir.exists():
        print(f"Error: Directory {library_dir} does not exist", file=sys.stderr)
        sys.exit(1)

    # Find all .pl files
    pl_files = sorted(library_dir.glob("*.pl"))

    if not pl_files:
        print(f"Error: No .pl files found in {library_dir}", file=sys.stderr)
        sys.exit(1)

    print(f"Analyzing {len(pl_files)} Prolog files in {library_dir}...", file=sys.stderr)

    # Analyze each file
    results = {}
    for i, filepath in enumerate(pl_files, 1):
        print(f"  [{i}/{len(pl_files)}] {filepath.name}...", file=sys.stderr)
        result = run_check_operators(filepath)
        results[str(filepath)] = result

    print("\nConsolidating results...", file=sys.stderr)

    # Consolidate results
    consolidated = consolidate_results(results)

    # Write to status.json
    output_file = Path("tools/status.json")
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(consolidated, f, indent=2)

    print(f"\nResults written to {output_file}", file=sys.stderr)

    # Print summary
    print("\n" + "=" * 70, file=sys.stderr)
    print("SUMMARY", file=sys.stderr)
    print("=" * 70, file=sys.stderr)
    print(f"Files analyzed:                {consolidated['summary']['total_files_analyzed']}", file=sys.stderr)
    print(f"Files with errors:             {consolidated['summary']['total_files_with_errors']}", file=sys.stderr)
    print(f"Unique available operators:    {consolidated['summary']['unique_available_operators']}", file=sys.stderr)
    print(f"Unique missing ISO operators:  {consolidated['summary']['unique_missing_iso_operators']}", file=sys.stderr)
    print(f"Unique missing non-ISO ops:    {consolidated['summary']['unique_missing_non_iso_operators']}", file=sys.stderr)
    print("=" * 70, file=sys.stderr)

    if consolidated['missing_iso_operators']:
        print("\nMissing ISO Operators (by frequency):", file=sys.stderr)
        for op, info in sorted(
            consolidated['missing_iso_operators'].items(),
            key=lambda x: x[1]['file_count'],
            reverse=True
        ):
            print(f"  {op:15s} used in {info['file_count']:2d} file(s) - {info['iso_spec']}", file=sys.stderr)

    # Exit with error if any ISO operators are missing
    if consolidated['missing_iso_operators']:
        sys.exit(1)


if __name__ == "__main__":
    main()
