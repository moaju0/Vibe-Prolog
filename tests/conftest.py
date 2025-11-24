from __future__ import annotations

import importlib.util
import subprocess
import sys
from pathlib import Path


def _install_ai_tools_package() -> None:
    repo_root = Path(__file__).parent.parent
    ai_tools_dir = repo_root / "ai-tools"
    if not ai_tools_dir.exists():
        return

    subprocess.run(
        [sys.executable, "-m", "pip", "install", "-e", str(ai_tools_dir)],
        check=True,
    )


if importlib.util.find_spec("autocoder_utils") is None:
    _install_ai_tools_package()
