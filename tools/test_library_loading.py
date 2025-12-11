#!/usr/bin/env python3
"""
Test script to load all Prolog library files and document status.
Supports resuming from where it left off if interrupted.
"""

import sys
import os
from pathlib import Path
from typing import Dict, List, Tuple
import traceback
import json
from datetime import datetime
import signal

# Add the project root to the path
sys.path.insert(0, str(Path(__file__).parent.parent))

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow

PROGRESS_FILE = Path(__file__).parent.parent / ".paige" / "library_test_progress.json"
FILE_TIMEOUT = 30  # seconds per file

class TimeoutException(Exception):
    pass

def timeout_handler(signum, frame):
    raise TimeoutException("File loading timed out")

def get_library_files() -> List[Path]:
    """Get all .pl files from the library directory."""
    library_dir = Path(__file__).parent.parent / "library"
    pl_files = sorted(library_dir.glob("**/*.pl"))
    return pl_files

def test_load_file(file_path: Path) -> Tuple[bool, str, str]:
    """
    Attempt to load a Prolog file with timeout.
    
    Returns: (success: bool, short_message: str, detailed_message: str)
    """
    try:
        # Set timeout (Unix only)
        if hasattr(signal, 'SIGALRM'):
            signal.signal(signal.SIGALRM, timeout_handler)
            signal.alarm(FILE_TIMEOUT)
        
        try:
            prolog = PrologInterpreter(builtin_conflict="skip")
            prolog.consult(str(file_path))
            return True, f"✅ Loaded successfully", ""
        finally:
            # Cancel timeout
            if hasattr(signal, 'SIGALRM'):
                signal.alarm(0)
    
    except TimeoutException:
        return False, f"❌ Timeout", f"File loading exceeded {FILE_TIMEOUT} seconds"
    except PrologThrow as e:
        error_msg = str(e)
        return False, f"❌ Prolog error", f"PrologThrow: {error_msg}"
    except SyntaxError as e:
        return False, f"❌ Syntax error", f"SyntaxError at line {e.lineno}: {e.msg}"
    except ValueError as e:
        return False, f"❌ Parse error", f"ValueError: {str(e)}"
    except ImportError as e:
        return False, f"❌ Import error", f"ImportError: {str(e)}"
    except Exception as e:
        error_type = type(e).__name__
        error_msg = str(e)
        tb = traceback.format_exc()
        return False, f"❌ {error_type}", f"{error_type}: {error_msg}\n\nTraceback:\n{tb}"

def load_progress() -> Dict:
    """Load progress from previous run, if it exists."""
    if PROGRESS_FILE.exists():
        try:
            return json.loads(PROGRESS_FILE.read_text())
        except Exception:
            return {}
    return {}

def save_progress(results: Dict[str, Tuple[bool, str, str]]):
    """Save progress to file."""
    PROGRESS_FILE.parent.mkdir(parents=True, exist_ok=True)
    progress_data = {
        'timestamp': datetime.now().isoformat(),
        'results': {
            path: {
                'success': success,
                'short_msg': short_msg,
                'detailed_msg': detailed_msg
            }
            for path, (success, short_msg, detailed_msg) in results.items()
        }
    }
    PROGRESS_FILE.write_text(json.dumps(progress_data, indent=2))

def generate_report(results: Dict[str, Tuple[bool, str, str]]) -> str:
    """Generate markdown report from test results."""
    
    passed = [k for k, (success, _, _) in results.items() if success]
    failed = [k for k, (success, _, _) in results.items() if not success]
    
    report = []
    report.append("# Vibe-Prolog Library Loading Status")
    report.append("")
    report.append(f"**Test Date:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S UTC')}")
    report.append("")
    report.append("## Summary")
    report.append("")
    report.append(f"- **Total Files:** {len(results)}")
    report.append(f"- **Loaded Successfully:** {len(passed)}")
    report.append(f"- **Failed to Load:** {len(failed)}")
    if len(results) > 0:
        report.append(f"- **Success Rate:** {len(passed)}/{len(results)} ({100*len(passed)//len(results)}%)")
    report.append("")
    
    # Successfully loaded files
    if passed:
        report.append("## Successfully Loaded Files ✅")
        report.append("")
        for file_path in sorted(passed):
            report.append(f"- `{file_path}`")
        report.append("")
    
    # Failed files with details
    if failed:
        report.append("## Failed to Load Files ❌")
        report.append("")
        for file_path in sorted(failed):
            success, short_msg, detailed_msg = results[file_path]
            report.append(f"### {file_path}")
            report.append("")
            report.append(f"**Status:** {short_msg}")
            report.append("")
            if detailed_msg:
                report.append("**Details:**")
                report.append("")
                # Format detailed message as code block if it's multi-line
                if '\n' in detailed_msg:
                    report.append("```")
                    report.append(detailed_msg)
                    report.append("```")
                else:
                    report.append(f"```\n{detailed_msg}\n```")
                report.append("")
            report.append("")
    
    # Recommendations
    if failed:
        report.append("## Next Steps")
        report.append("")
        report.append("The following library files have issues that should be investigated and addressed:")
        report.append("")
        for file_path in sorted(failed):
            success, short_msg, detailed_msg = results[file_path]
            report.append(f"1. **{file_path}** - {short_msg}")
        report.append("")
        report.append("These issues should be converted into GitHub issues with the details provided above.")
        report.append("")
    
    return "\n".join(report)

def write_report(results: Dict[str, Tuple[bool, str, str]]):
    """Write report to file."""
    report = generate_report(results)
    report_path = Path(__file__).parent.parent / "docs" / "LIBRARY_STATUS.md"
    report_path.write_text(report)
    return report_path

def main():
    """Main entry point."""
    print("Testing Vibe-Prolog library file loading...")
    print()
    
    library_files = get_library_files()
    print(f"Found {len(library_files)} .pl files in library directory")
    print()
    
    # Load existing progress
    progress_data = load_progress()
    existing_results = {}
    if progress_data and 'results' in progress_data:
        for path, result in progress_data['results'].items():
            existing_results[path] = (
                result['success'],
                result['short_msg'],
                result['detailed_msg']
            )
    
    results = existing_results.copy()
    report_path = None
    
    # Test files not yet processed
    total_to_test = 0
    for i, file_path in enumerate(library_files, 1):
        relative_path = str(file_path.relative_to(Path(__file__).parent.parent))
        
        if relative_path in results:
            print(f"[{i}/{len(library_files)}] {relative_path} ... (cached) {results[relative_path][1]}")
        else:
            total_to_test += 1
    
    if total_to_test > 0:
        print()
        print(f"Testing {total_to_test} new files...")
        print()
    else:
        print("All files already tested (cached).")
    
    # Process new files
    for i, file_path in enumerate(library_files, 1):
        relative_path = str(file_path.relative_to(Path(__file__).parent.parent))
        
        if relative_path in results:
            continue  # Already tested
        
        print(f"[{i}/{len(library_files)}] Testing {relative_path}...", end=" ", flush=True)
        
        success, short_msg, detailed_msg = test_load_file(file_path)
        results[relative_path] = (success, short_msg, detailed_msg)
        
        print(short_msg)
        
        # Save progress and write report after each file
        save_progress(results)
        report_path = write_report(results)
    
    if report_path is None:
        report_path = write_report(results)
    
    print()
    print("=" * 80)
    print()
    
    print(f"Report written to: {report_path}")
    print()
    
    # Summary
    passed = sum(1 for success, _, _ in results.values() if success)
    failed = sum(1 for success, _, _ in results.values() if not success)
    print(f"Results: {passed} passed, {failed} failed")
    
    return 0 if failed == 0 else 1

if __name__ == "__main__":
    sys.exit(main())
