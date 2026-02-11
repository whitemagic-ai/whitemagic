#!/usr/bin/env python3
"""
Path Sanitizer (Open-Source Readiness Helper)
============================================

This helper scans the repo for hard-coded, machine-specific absolute paths
(for example `/home/<user>/...`) that should not be shipped in an OSS repo.

Default behavior is *check-only* (no writes). Use `--apply` to rewrite content
with conservative, documentation-friendly placeholders.

Notes:
- This tool is intentionally conservative for code: it will report issues, but
  only auto-rewrites Markdown/Text by default.
- Prefer fixing Python code properly (derive paths from env/config) instead of
  replacing strings with placeholders.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator, Sequence


@dataclass(frozen=True)
class Finding:
    path: Path
    lineno: int
    line: str


_UNIX_HOME_RE = re.compile(r"/home/[^/\s]+")
_REPO_ROOT_RE = re.compile(r"/home/[^/\s]+/Desktop/whitemagic")
_REPO_ROOT_ALT_RE = re.compile(r"/home/[^/\s]+/Desktop/WHITE MAGIC")
_WINDOWS_HOME_RE = re.compile(r"[A-Za-z]:\\\\Users\\\\[^\\\\/\\s]+")


def _iter_files(root: Path, globs: Sequence[str]) -> Iterator[Path]:
    for pat in globs:
        yield from (p for p in root.glob(pat) if p.is_file())


def _scan_file(path: Path) -> Iterator[Finding]:
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except Exception:
        return
    for i, line in enumerate(text.splitlines(), start=1):
        if (
            _REPO_ROOT_RE.search(line)
            or _REPO_ROOT_ALT_RE.search(line)
            or _UNIX_HOME_RE.search(line)
            or _WINDOWS_HOME_RE.search(line)
        ):
            yield Finding(path=path, lineno=i, line=line.rstrip("\n"))


def _rewrite_doc(text: str) -> str:
    # Keep it human-readable and shell-friendly.
    text = _REPO_ROOT_RE.sub("<REPO_ROOT>", text)
    text = _REPO_ROOT_ALT_RE.sub("<REPO_ROOT>", text)
    text = _WINDOWS_HOME_RE.sub("<HOME>", text)
    text = _UNIX_HOME_RE.sub("$HOME", text)
    return text


def _apply(root: Path, files: Iterable[Path]) -> int:
    changed = 0
    for path in files:
        if path.suffix.lower() not in {".md", ".txt"}:
            continue
        before = path.read_text(encoding="utf-8", errors="replace")
        after = _rewrite_doc(before)
        if after != before:
            path.write_text(after, encoding="utf-8")
            changed += 1
    return changed


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".", help="Repo root to scan (default: .)")
    parser.add_argument(
        "--apply",
        action="store_true",
        help="Rewrite Markdown/Text files with placeholders (default: report only).",
    )
    args = parser.parse_args(list(argv) if argv is not None else None)

    root = Path(args.root).expanduser().resolve()

    globs = [
        "audit/**/*.py",
        "scripts/**/*.py",
        "docs/**/*.md",
        "docs/**/*.txt",
        "elixir/**/*.ex",
        "whitemagic-mojo/**/*.mojo",
        "whitemagic-rust/**/*.rs",
        "whitemagic/**/*.py",
    ]

    files = sorted({p for p in _iter_files(root, globs)})
    findings: list[Finding] = []
    for p in files:
        findings.extend(list(_scan_file(p)))

    if args.apply:
        changed = _apply(root, files)
        print(f"Rewrote {changed} Markdown/Text file(s).", file=sys.stderr)

    if findings:
        print("Found hard-coded absolute paths:", file=sys.stderr)
        for f in findings[:200]:
            print(f"{f.path}:{f.lineno}: {f.line}", file=sys.stderr)
        if len(findings) > 200:
            print(f"... ({len(findings) - 200} more)", file=sys.stderr)
        return 2

    print("OK: no hard-coded absolute paths found in scanned files.", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

