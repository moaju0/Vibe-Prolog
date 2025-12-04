#!/usr/bin/env python3
"""Find problematic quoted infix operator usage in Prolog files.

Finds patterns like:
  X'|'Y  or  X '|' Y  - quoted operators used as infix
  
These should be rewritten as:
  '|'(X, Y)  - standard functor notation
"""

import re
import sys
from pathlib import Path


# Pattern to match: identifier/variable followed by quoted atom followed by identifier/variable
# This catches things like: _'|'_  or  GREither '|' GROr
QUOTED_INFIX_PATTERN = re.compile(
    r"([A-Z_][A-Za-z0-9_]*)\s*'([^']+)'\s*([A-Z_][A-Za-z0-9_]*)"
)

# Also catch when wrapped in parentheses like: ( _'|'_ )
PAREN_QUOTED_INFIX_PATTERN = re.compile(
    r"\(\s*([A-Z_][A-Za-z0-9_]*)\s*'([^']+)'\s*([A-Z_][A-Za-z0-9_]*)\s*\)"
)


def find_quoted_infix_in_file(filepath: Path) -> list[tuple[int, str, str]]:
    """Find problematic quoted infix patterns in a file.
    
    Returns list of (line_number, line_content, matched_pattern)
    """
    issues = []
    
    try:
        content = filepath.read_text()
    except Exception as e:
        print(f"Error reading {filepath}: {e}", file=sys.stderr)
        return issues
    
    for line_num, line in enumerate(content.splitlines(), 1):
        # Skip comments
        stripped = line.split('%')[0]  # Remove line comments
        
        for pattern in [QUOTED_INFIX_PATTERN, PAREN_QUOTED_INFIX_PATTERN]:
            for match in pattern.finditer(stripped):
                var1, op, var2 = match.groups()
                # Report the issue
                issues.append((line_num, line.rstrip(), f"{var1}'{op}'{var2}"))
    
    return issues


def main():
    if len(sys.argv) < 2:
        directory = Path("./library")
    else:
        directory = Path(sys.argv[1])
    
    if not directory.exists():
        print(f"Directory not found: {directory}", file=sys.stderr)
        sys.exit(1)
    
    total_issues = 0
    
    for pl_file in sorted(directory.glob("**/*.pl")):
        issues = find_quoted_infix_in_file(pl_file)
        
        if issues:
            print(f"\n{pl_file}:")
            for line_num, line, pattern in issues:
                print(f"  Line {line_num}: {pattern}")
                print(f"    {line}")
                total_issues += 1
    
    print(f"\n{'='*60}")
    print(f"Total issues found: {total_issues}")
    
    if total_issues > 0:
        print("\nTo fix, rewrite patterns like:")
        print("  X '|' Y  -->  '|'(X, Y)")
        print("  ( _'|'_ )  -->  '|'(_, _)")


if __name__ == "__main__":
    main()
