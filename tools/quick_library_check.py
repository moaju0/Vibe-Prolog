#!/usr/bin/env python3
"""
Quick check of library files to identify which ones have issues.
Tests files individually without dependencies.
"""

import sys
import os
from pathlib import Path
from typing import Dict, List, Tuple
import traceback
import json
from datetime import datetime

# Add the project root to the path
sys.path.insert(0, str(Path(__file__).parent.parent))

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow

def get_library_files() -> List[Path]:
    """Get all .pl files from the library directory."""
    library_dir = Path(__file__).parent.parent / "library"
    pl_files = sorted(library_dir.glob("**/*.pl"))
    return pl_files

def test_load_file(file_path: Path) -> Tuple[bool, str]:
    """
    Attempt to load a Prolog file.
    
    Returns: (success: bool, error_details: str)
    """
    try:
        prolog = PrologInterpreter()
        prolog.consult(str(file_path))
        return True, ""
    except Exception as e:
        error_type = type(e).__name__
        error_msg = str(e)
        return False, f"{error_type}: {error_msg}"

def main():
    """Main entry point."""
    library_files = get_library_files()
    print(f"Testing {len(library_files)} library files...\n")
    
    results = []
    for i, file_path in enumerate(library_files, 1):
        relative_path = str(file_path.relative_to(Path(__file__).parent.parent))
        sys.stdout.write(f"[{i:2d}/{len(library_files)}] {relative_path:<50} ... ")
        sys.stdout.flush()
        
        success, error_msg = test_load_file(file_path)
        
        if success:
            print("✅")
            results.append((relative_path, True, ""))
        else:
            print(f"❌")
            results.append((relative_path, False, error_msg))
            print(f"       Error: {error_msg[:100]}...")
    
    print(f"\n{'='*80}\n")
    
    # Summary
    passed = [r for r in results if r[1]]
    failed = [r for r in results if not r[1]]
    
    print(f"Summary: {len(passed)} passed, {len(failed)} failed\n")
    
    if failed:
        print("Failed libraries:")
        for path, _, error in failed:
            print(f"  - {path}")
            print(f"    {error[:120]}")
    
    return 0 if not failed else 1

if __name__ == "__main__":
    sys.exit(main())
