from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Iterable


DEFAULT_REQUIRED_COMMANDS = ["gh", "llm"]


def check_commands_available(required: Iterable[str] | None = None) -> None:
    commands = list(required) if required is not None else DEFAULT_REQUIRED_COMMANDS
    missing = [cmd for cmd in commands if shutil.which(cmd) is None]
    if missing:
        missing_str = ", ".join(missing)
        print(
            f"Error: required command(s) {missing_str!r} are not available in PATH",
            file=sys.stderr,
        )
        raise SystemExit(1)


def ensure_env() -> None:
    os.environ.setdefault("LLM_MODEL", "gpt-5-nano")
    if "OPENAI_API_KEY" not in os.environ:
        print("OPENAI_API_KEY must be set", file=sys.stderr)
        raise SystemExit(1)


def run(cmd: list[str], *, input_text: str | None = None, capture_output: bool = True) -> str:
    result = subprocess.run(
        cmd,
        input=input_text.encode("utf-8") if input_text is not None else None,
        stdout=subprocess.PIPE if capture_output else None,
        stderr=subprocess.PIPE,
        check=False,
    )
    if result.returncode != 0:
        stderr = result.stderr.decode("utf-8", errors="replace")
        raise SystemExit(f"Command {' '.join(cmd)!r} failed with code {result.returncode}:\n{stderr}")
    if capture_output:
        return result.stdout.decode("utf-8", errors="replace")
    return ""


def stage_changes() -> None:
    run(["git", "add", "-A"], capture_output=False)


def has_staged_changes() -> bool:
    result = subprocess.run(
        ["git", "diff", "--cached", "--quiet"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    return result.returncode != 0


def get_repo_root() -> Path:
    output = run(["git", "rev-parse", "--show-toplevel"]).strip()
    return Path(output)


def get_owner_repo() -> tuple[str, str]:
    url = run(["git", "config", "--get", "remote.origin.url"]).strip()
    if not url:
        raise SystemExit("Could not determine remote.origin.url")

    url = url.rstrip("/")
    if url.endswith(".git"):
        url = url[:-4]

    if url.startswith("git@"):
        try:
            path = url.split(":", 1)[1]
        except IndexError:
            raise SystemExit(f"Unrecognised git URL format: {url!r}")
    elif "github.com/" in url:
        path = url.split("github.com/", 1)[1]
    else:
        raise SystemExit(f"Unrecognised git URL format: {url!r}")

    parts = path.split("/")
    if len(parts) != 2:
        raise SystemExit(f"Unrecognised owner/repo path in URL: {url!r}")

    owner, repo = parts[0], parts[1]
    if not owner or not repo:
        raise SystemExit(f"Unrecognised owner/repo in URL: {url!r}")

    return owner, repo