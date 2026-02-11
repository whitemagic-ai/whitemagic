"""``python -m whitemagic.seed`` — scaffold a WhiteMagic project directory.

This is a lightweight entry point that delegates to ``wm init``.  It exists
so that users who installed WhiteMagic from a wheel (without the ``[cli]``
extra) can still scaffold a project without needing Click installed:

    python -m whitemagic.seed              # current directory
    python -m whitemagic.seed my-project   # create + scaffold my-project/
    python -m whitemagic.seed --force      # overwrite existing files
    python -m whitemagic.seed --minimal    # only .mcp.json + README + run.sh
"""
from __future__ import annotations

import argparse
import stat
from pathlib import Path


def main(argv: list[str] | None = None) -> None:
    parser = argparse.ArgumentParser(
        prog="python -m whitemagic.seed",
        description="Scaffold a WhiteMagic project directory.",
    )
    parser.add_argument(
        "directory", nargs="?", default=".",
        help="Target directory (default: current directory)",
    )
    parser.add_argument(
        "-f", "--force", action="store_true",
        help="Overwrite existing files",
    )
    parser.add_argument(
        "-m", "--minimal", action="store_true",
        help="Only create .mcp.json, README.md, and run.sh",
    )
    args = parser.parse_args(argv)

    # Import templates from init_command (no Click dependency needed)
    from whitemagic.cli.init_command import (
        _ENV,
        _GITIGNORE,
        _MCP_JSON,
        _PLAYGROUND,
        _PLAYGROUND_VERSION_PLACEHOLDER,
        _README,
        _RUN_SH,
    )

    try:
        from whitemagic import __version__
    except Exception:
        __version__ = "unknown"

    target = Path(args.directory).resolve()
    target.mkdir(parents=True, exist_ok=True)

    files: dict[str, str] = {
        ".mcp.json": _MCP_JSON,
        "README.md": _README.format(version=__version__),
        "run.sh": _RUN_SH,
    }

    if not args.minimal:
        files["playground.py"] = _PLAYGROUND.replace(
            _PLAYGROUND_VERSION_PLACEHOLDER, __version__,
        )
        files[".env"] = _ENV
        files[".gitignore"] = _GITIGNORE

    created: list[str] = []
    skipped: list[str] = []

    for filename, content in files.items():
        filepath = target / filename
        if filepath.exists() and not args.force:
            skipped.append(filename)
            continue
        filepath.write_text(content, encoding="utf-8")
        created.append(filename)
        if filename.endswith(".sh") or filename.endswith(".py"):
            filepath.chmod(
                filepath.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH,
            )

    # Create runtime directories
    for dirname in ("data", "logs", "tmp"):
        (target / dirname).mkdir(exist_ok=True)

    print(f"\n  WhiteMagic v{__version__} — Project initialized at {target}\n")

    if created:
        print("  Created:")
        for f in created:
            print(f"    + {f}")

    if skipped:
        print("\n  Skipped (already exist, use --force to overwrite):")
        for f in skipped:
            print(f"    ~ {f}")

    print("\n  Next steps:")
    print("    1. python playground.py      # Interactive demo")
    print("    2. ./run.sh                  # Launch MCP server")
    print("    3. wm doctor                 # System health check")
    print()


if __name__ == "__main__":
    main()
