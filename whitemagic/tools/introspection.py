"""Introspection Tools (AI-First).
==============================

These helpers back the contract-level introspection tools:
- capabilities
- manifest
- state.paths / state.summary
- repo.summary / ship.check

They are designed to be safe to run in automated contexts:
- read-only
- no content exfiltration of user memory by default
"""

from __future__ import annotations

import os
import platform
import re
import subprocess
import sys
from collections.abc import Iterator
from dataclasses import asdict
from importlib.util import find_spec
from pathlib import Path
from typing import Any

from whitemagic import __version__
from whitemagic.config.paths import (
    ARTIFACTS_DIR,
    CACHE_DIR,
    DATA_DIR,
    LOGS_DIR,
    MEMORY_DIR,
    PROJECT_ROOT,
    RESTORE_DIR,
    SESSIONS_DIR,
    WM_ROOT,
)
from whitemagic.tools.contract import ENV_VARS, ENVELOPE_VERSION, TOOL_CONTRACT_VERSION
from whitemagic.tools.registry import TOOL_REGISTRY


def _has_module(name: str) -> bool:
    return find_spec(name) is not None


def _dir_size_bytes(path: Path, *, max_files: int = 10000) -> int:
    total = 0
    count = 0
    if not path.exists():
        return 0
    for root, dirs, files in os.walk(path):
        for fn in files:
            count += 1
            if count > max_files:
                return total
            try:
                total += (Path(root) / fn).stat().st_size
            except OSError:
                pass
    return total


def state_paths() -> dict[str, Any]:
    return {
        "wm_state_root": str(WM_ROOT),
        "paths": {
            "data": str(DATA_DIR),
            "memory": str(MEMORY_DIR),
            "cache": str(CACHE_DIR),
            "sessions": str(SESSIONS_DIR),
            "logs": str(LOGS_DIR),
            "artifacts": str(ARTIFACTS_DIR),
            "restoration": str(RESTORE_DIR),
        },
    }


def state_summary(*, include_sizes: bool = True) -> dict[str, Any]:
    # Intentionally avoid reading memory contents; only count files/bytes.
    summary: dict[str, Any] = {
        "wm_state_root": str(WM_ROOT),
        "exists": WM_ROOT.exists(),
        "dirs": {
            "data": DATA_DIR.exists(),
            "memory": MEMORY_DIR.exists(),
            "cache": CACHE_DIR.exists(),
            "sessions": SESSIONS_DIR.exists(),
            "logs": LOGS_DIR.exists(),
            "artifacts": ARTIFACTS_DIR.exists(),
            "restoration": RESTORE_DIR.exists(),
        },
    }
    if include_sizes:
        summary["sizes_bytes"] = {
            "data": _dir_size_bytes(DATA_DIR, max_files=20_000),
            "memory": _dir_size_bytes(MEMORY_DIR, max_files=50_000),
            "cache": _dir_size_bytes(CACHE_DIR, max_files=50_000),
            "sessions": _dir_size_bytes(SESSIONS_DIR, max_files=50_000),
            "logs": _dir_size_bytes(LOGS_DIR, max_files=50_000),
            "artifacts": _dir_size_bytes(ARTIFACTS_DIR, max_files=50_000),
            "restoration": _dir_size_bytes(RESTORE_DIR, max_files=50_000),
        }
    return summary


def manifest(*, format: str = "summary", include_schemas: bool = False) -> dict[str, Any]:
    if format == "summary":
        return {
            "package_version": __version__,
            "tool_contract_version": TOOL_CONTRACT_VERSION,
            "envelope_version": ENVELOPE_VERSION,
            "tool_count": len(TOOL_REGISTRY),
            "categories": sorted({t.category.value for t in TOOL_REGISTRY}),
        }

    if format == "whitemagic":
        whitemagic_tools: list[dict[str, Any]] = []
        for tool_def in TOOL_REGISTRY:
            d = tool_def.to_dict()
            if not include_schemas:
                d.pop("input_schema", None)
            whitemagic_tools.append(d)
        return {"format": "whitemagic", "tools": whitemagic_tools}

    if format == "mcp":
        mcp_tools: list[dict[str, Any]] = [tool_def.to_mcp_tool() for tool_def in TOOL_REGISTRY]
        if not include_schemas:
            for tool_item in mcp_tools:
                tool_item.pop("inputSchema", None)
        return {"format": "mcp", "tools": mcp_tools}

    if format == "openai":
        openai_tools: list[dict[str, Any]] = [
            tool_def.to_openai_function() for tool_def in TOOL_REGISTRY
        ]
        if not include_schemas:
            for tool_item in openai_tools:
                try:
                    tool_item["function"].pop("parameters", None)
                except Exception:
                    pass
        return {"format": "openai", "tools": openai_tools}

    return {"error": f"unknown format: {format}"}


def capabilities(*, include_tools: bool = True, include_schemas: bool = False, include_env: bool = True) -> dict[str, Any]:
    caps: dict[str, Any] = {
        "package_version": __version__,
        "tool_contract_version": TOOL_CONTRACT_VERSION,
        "envelope_version": ENVELOPE_VERSION,
        "runtime": {
            "python": sys.version.split(" ")[0],
            "platform": platform.platform(),
        },
        "features": {
            "mcp_server": _has_module("fastmcp"),
            "rust_bridge": _has_module("whitemagic_rs"),
            "rich_cli": _has_module("rich"),
            "fastapi": _has_module("fastapi"),
        },
        "state": state_paths(),
        "limits": {
            "batch_read_memories.max_batch_size": 50,
        },
        "tool_groups": {
            # Stable group names for AI callers (prefer groups over gardens).
            "memory": ["create_memory", "search_memories", "read_memory", "list_memories", "update_memory", "delete_memory"],
            "session": ["create_session", "checkpoint_session", "resume_session", "session_bootstrap"],
            "scratchpad": ["scratchpad_create", "scratchpad_update", "scratchpad_finalize"],
            "introspection": ["capabilities", "manifest", "state.paths", "state.summary", "repo.summary", "ship.check"],
        },
    }

    if include_env:
        caps["env_vars"] = [asdict(e) for e in ENV_VARS]

    if include_tools:
        tools_out = []
        for t in TOOL_REGISTRY:
            d = t.to_dict()
            if not include_schemas:
                d.pop("input_schema", None)
            tools_out.append(d)
        caps["tools"] = tools_out

    return caps


def telemetry_summary() -> dict[str, Any]:
    """Get summarized performance and error metrics."""
    try:
        from whitemagic.core.monitoring.telemetry import get_telemetry
        return dict(get_telemetry().get_summary())
    except Exception as e:
        return {"error": str(e)}


_DEFAULT_SKIP_DIRS = {
    ".git",
    ".venv",
    "temp_venv",
    "__pycache__",
    ".mypy_cache",
    ".ruff_cache",
    ".pytest_cache",
    ".tox",
    "memory",
    "data",
    "logs",
    "reports",
    ".whitemagic",
    "_archived",
    "archive",
    "dist",
    "build",
    "node_modules",
    "target",
    "deps",
    "_build",
}

def _git_list_files() -> list[Path] | None:
    """Best-effort: list tracked + untracked (non-ignored) files via git."""
    if not (PROJECT_ROOT / ".git").exists():
        return None
    try:
        proc = subprocess.run(
            ["git", "-C", str(PROJECT_ROOT), "ls-files", "-co", "--exclude-standard"],
            check=True,
            capture_output=True,
            text=True,
        )
    except Exception:
        return None

    paths: list[Path] = []
    for line in proc.stdout.splitlines():
        line = line.strip()
        if not line:
            continue
        paths.append(PROJECT_ROOT / line)
    return paths


def _iter_repo_files(*, max_files: int, skip_dirs: set[str] | None = None) -> Iterator[Path]:
    git_paths = _git_list_files()
    if git_paths is not None:
        yield from git_paths[:max_files]
        return

    skip_dirs = skip_dirs or set(_DEFAULT_SKIP_DIRS)
    count = 0
    for root, dirs, files in os.walk(PROJECT_ROOT):
        # In-place prune for speed.
        dirs[:] = [d for d in dirs if d not in skip_dirs]
        for fn in files:
            count += 1
            if count > max_files:
                return
            yield Path(root) / fn


def repo_summary(*, max_files: int = 2500, max_matches: int = 25) -> dict[str, Any]:
    total_files = 0
    total_bytes = 0
    largest: list[tuple[str, int]] = []

    for path in _iter_repo_files(max_files=max_files):
        total_files += 1
        try:
            size = path.stat().st_size
        except OSError:
            continue
        total_bytes += size
        largest.append((str(path.relative_to(PROJECT_ROOT)), size))

    largest.sort(key=lambda p: p[1], reverse=True)
    return {
        "project_root": str(PROJECT_ROOT),
        "scanned_files": total_files,
        "scanned_bytes": total_bytes,
        "largest_files": largest[:10],
        "notes": [
            "This is a lightweight scan; use ship.check for deeper hygiene checks.",
        ],
    }


_ABS_PATH_PATTERNS = [
    # Likely user-specific absolute paths. Keep these strict so we don't flag
    # our own regex patterns like `/home/[^\s]+` or placeholders like
    # `/home/<user>`.
    re.compile(r"/home/[A-Za-z0-9._-]{1,64}(?:/[^\\s\"']+)?"),
    re.compile(r"/Users/[A-Za-z0-9._-]{1,64}(?:/[^\\s\"']+)?"),
    # Windows absolute paths that frequently contain user-specific leaks.
    re.compile(r"[A-Za-z]:\\\\Users\\\\[^\\s\"']+"),
]

_SECRET_PATTERNS = [
    re.compile(r"BEGIN (RSA|OPENSSH|EC) PRIVATE KEY"),
    re.compile(r"\\bAKIA[0-9A-Z]{16}\\b"),  # AWS access key id
    re.compile(r"\\bsk-[0-9a-zA-Z]{20,}\\b"),  # common API key prefix
]


def _is_text_file(path: Path) -> bool:
    # Best-effort heuristic by extension.
    return path.suffix.lower() in {
        ".py",
        ".md",
        ".txt",
        ".toml",
        ".json",
        ".yml",
        ".yaml",
        ".ini",
        ".cfg",
        ".sh",
        ".bash",
        ".zsh",
        ".rs",
        ".go",
        ".zig",
        ".mojo",
        ".ex",
        ".exs",
        ".hs",
        ".c",
        ".h",
        ".cpp",
        ".hpp",
    }


def ship_check(
    *,
    max_files: int = 4000,
    max_large_files: int = 25,
    large_file_mb: int = 10,
    max_matches: int = 50,
) -> dict[str, Any]:
    """Run quick open-source shipping hygiene checks.

    This intentionally avoids reading user state under WM_STATE_ROOT.
    """
    issues: list[dict[str, Any]] = []

    # 1) Ensure .gitignore includes runtime exclusions.
    gitignore = PROJECT_ROOT / ".gitignore"
    if gitignore.exists():
        gi = gitignore.read_text(encoding="utf-8", errors="replace")
        required = [
            "/memory/",
            "/data/",
            "/logs/",
            "/reports/",
            "/.whitemagic/",
            "/windsurf_conversations/",
            "resonance_state.json",
            ".env",
        ]
        missing = [p for p in required if p not in gi]
        if missing:
            issues.append({"kind": "gitignore_missing", "missing": missing})
    else:
        issues.append({"kind": "gitignore_missing", "missing": ["(file missing)"]})

    # 2) Large file scan.
    threshold = large_file_mb * 1024 * 1024
    large_files: list[tuple[str, int]] = []

    # 3) Absolute path + secrets scan (text files only, size-limited).
    abs_path_hits: list[tuple[str, str]] = []
    secret_hits: list[tuple[str, str]] = []

    for path in _iter_repo_files(max_files=max_files):
        rel = str(path.relative_to(PROJECT_ROOT))
        try:
            size = path.stat().st_size
        except OSError:
            continue

        if size >= threshold:
            large_files.append((rel, size))

        # Avoid reading huge blobs.
        if not _is_text_file(path) or size > 1_000_000:
            continue

        try:
            text = path.read_text(encoding="utf-8", errors="replace")
        except Exception:
            continue

        for pat in _ABS_PATH_PATTERNS:
            if len(abs_path_hits) >= max_matches:
                break
            m = pat.search(text)
            if m:
                abs_path_hits.append((rel, m.group(0)))
                break

        for pat in _SECRET_PATTERNS:
            if len(secret_hits) >= max_matches:
                break
            m = pat.search(text)
            if m:
                secret_hits.append((rel, pat.pattern))
                break

    large_files.sort(key=lambda p: p[1], reverse=True)
    large_files = large_files[:max_large_files]

    if large_files:
        issues.append({"kind": "large_files", "threshold_bytes": threshold, "files": large_files})

    if abs_path_hits:
        issues.append({"kind": "absolute_path_literals", "hits": abs_path_hits})

    if secret_hits:
        issues.append({"kind": "potential_secrets", "hits": secret_hits})

    return {
        "project_root": str(PROJECT_ROOT),
        "checks": 4,
        "issues": issues,
        "ok": len(issues) == 0,
        "notes": [
            "This is a heuristic scan; treat findings as prompts for manual review.",
            "No WM_STATE_ROOT content is scanned by ship.check.",
        ],
    }
