"""Centralized path configuration for Whitemagic.

This module provides two distinct path concepts:
1. PROJECT_ROOT - where the code/repository lives (for scripts, archive, etc.)
2. WM_ROOT - where user state/data lives (configurable via env vars, defaults to ~/.whitemagic)

Policy (AI-first / OSS-friendly):
- Whitemagic should not write runtime state into the repo by default.
- Callers can explicitly set `WM_STATE_ROOT` to place state wherever they want.
"""
from typing import Any
import os
import tempfile
from pathlib import Path


def get_project_root() -> Path:
    """Returns the project root directory (where pyproject.toml lives).

    This is the CODE location, not the state/data location.
    Use this for finding scripts, archive, and other repo-relative paths.
    """
    return Path(__file__).resolve().parent.parent.parent


# Project root (code location) - for finding scripts, archive, etc.
PROJECT_ROOT: Path = get_project_root()
SCRIPTS_DIR: Path = PROJECT_ROOT / "scripts"
ARCHIVE_DIR: Path = PROJECT_ROOT / "archive"


# Canonical Root for STATE/DATA
# 1. Check for WM_STATE_ROOT or WM_CONFIG_ROOT
# 2. Default to ~/.whitemagic
# 3. Fallback to /tmp if the default is not writable (CI/sandbox-safe)
# 4. As a last resort, fallback to ./.whitemagic
_intended_root = Path(
    os.getenv("WM_STATE_ROOT")
    or os.getenv("WM_CONFIG_ROOT")
    or str(Path.home() / ".whitemagic"),
).expanduser()

def _is_writable(path: Path) -> bool:
    try:
        # NOTE: os.access() can return true in sandboxed environments even when
        # writes are blocked. Do a real create/delete probe.
        if not path.exists():
            path.mkdir(parents=True, exist_ok=True)
        if not path.is_dir():
            return False
        with tempfile.NamedTemporaryFile(prefix=".wm_write_probe_", dir=path, delete=True) as fp:
            fp.write(b"probe")
            fp.flush()
        return True
    except Exception:
        return False

WM_ROOT: Path
if not _is_writable(_intended_root):
    _tmp_fallback = Path(tempfile.gettempdir()) / "whitemagic_state"
    if _is_writable(_tmp_fallback):
        WM_ROOT = _tmp_fallback
    else:
        # Last-resort fallback for extremely restricted environments.
        WM_ROOT = Path.cwd() / ".whitemagic"
else:
    WM_ROOT = _intended_root

# Subdirectories
DATA_DIR: Path = WM_ROOT / "data"
MEMORY_DIR: Path = WM_ROOT / "memory"
CACHE_DIR: Path = WM_ROOT / "cache"
SESSIONS_DIR: Path = WM_ROOT / "sessions"
LOGS_DIR: Path = WM_ROOT / "logs"
ARTIFACTS_DIR: Path = WM_ROOT / "artifacts"
RESTORE_DIR: Path = WM_ROOT / "restoration"

# Files
USER_PROFILE_PATH: Path = WM_ROOT / "user_profile.json"
CONFIG_PATH: Path = WM_ROOT / "config.json"

_db_env = (os.getenv("WM_DB_PATH") or "").strip()
DB_PATH: Path
if _db_env:
    _p = Path(_db_env).expanduser()
    if not _p.is_absolute():
        # Treat relative DB paths as relative to the state root.
        _p = (WM_ROOT / _p)
    DB_PATH = _p.resolve()
else:
    DB_PATH = MEMORY_DIR / "whitemagic.db"

# Cold storage DB (v13.3.2) — deep_archive + scavenged memories
COLD_DB_PATH: Path = MEMORY_DIR / "whitemagic_cold.db"

def ensure_paths() -> Any:
    """Ensure all core directories exist."""
    import logging
    log = logging.getLogger(__name__)
    if not os.getenv("WM_SILENT_INIT"):
        log.info(f"Ensuring paths exist at {WM_ROOT}...")
    for path in [WM_ROOT, DATA_DIR, MEMORY_DIR, CACHE_DIR, SESSIONS_DIR, LOGS_DIR, ARTIFACTS_DIR, RESTORE_DIR]:
        path.mkdir(parents=True, exist_ok=True)
    # DB path may be overridden (e.g., containers); ensure its parent exists.
    try:
        DB_PATH.parent.mkdir(parents=True, exist_ok=True)
    except Exception:
        pass
    if not os.getenv("WM_SILENT_INIT"):
        log.info("Paths verified.")
