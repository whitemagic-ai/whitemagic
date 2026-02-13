"""Non-blocking update checker — compares local version against PyPI.

Usage:
    from whitemagic.core.update_checker import check_for_update
    msg = check_for_update()  # returns str or None
    if msg:
        print(msg, file=sys.stderr)

Respects:
    WM_NO_UPDATE_CHECK=1  — disable entirely
    Caches result for 24 hours in $WM_STATE_ROOT/.update_check
"""

from __future__ import annotations

import json
import logging
import os
import time
from pathlib import Path
from typing import Optional

logger = logging.getLogger(__name__)

_PYPI_URL = "https://pypi.org/pypi/whitemagic/json"
_CACHE_TTL_SECONDS = 86400  # 24 hours
_TIMEOUT_SECONDS = 3  # don't slow down startup


def _cache_path() -> Path:
    """Return path to the update-check cache file."""
    state = os.environ.get("WM_STATE_ROOT") or os.path.expanduser("~/.whitemagic")
    return Path(state) / ".update_check"


def _read_cache() -> Optional[dict]:
    """Read cached check result if still valid."""
    try:
        p = _cache_path()
        if not p.exists():
            return None
        data = json.loads(p.read_text(encoding="utf-8"))
        if time.time() - data.get("ts", 0) < _CACHE_TTL_SECONDS:
            return data
    except Exception:
        pass
    return None


def _write_cache(latest: str, current: str) -> None:
    """Persist check result."""
    try:
        p = _cache_path()
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(
            json.dumps({"ts": time.time(), "latest": latest, "current": current}),
            encoding="utf-8",
        )
    except Exception:
        pass


def _fetch_latest_version() -> Optional[str]:
    """Fetch latest version from PyPI (blocking, with short timeout)."""
    try:
        from urllib.request import urlopen, Request

        req = Request(_PYPI_URL, headers={"Accept": "application/json"})
        with urlopen(req, timeout=_TIMEOUT_SECONDS) as resp:
            data = json.loads(resp.read())
            return data.get("info", {}).get("version")
    except Exception:
        return None


def _parse_version(v: str) -> tuple:
    """Parse 'X.Y.Z' into a comparable tuple."""
    try:
        return tuple(int(p) for p in v.split(".")[:3])
    except (ValueError, AttributeError):
        return (0, 0, 0)


def check_for_update() -> Optional[str]:
    """Check if a newer WhiteMagic version is available on PyPI.

    Returns a human-readable message if an update is available, else None.
    Safe to call on every startup — results are cached for 24h.
    """
    # Opt-out
    if os.environ.get("WM_NO_UPDATE_CHECK", "").strip() in ("1", "true", "yes"):
        return None

    try:
        from whitemagic.config import VERSION as current
    except ImportError:
        return None

    # Check cache first
    cached = _read_cache()
    if cached:
        latest = cached.get("latest", "")
        if _parse_version(latest) > _parse_version(current):
            return (
                f"⬆  WhiteMagic {latest} available (you have {current}). "
                f"Update: pip install -U whitemagic"
            )
        return None

    # Fetch from PyPI
    latest = _fetch_latest_version()
    if not latest:
        return None

    _write_cache(latest, current)

    if _parse_version(latest) > _parse_version(current):
        return (
            f"⬆  WhiteMagic {latest} available (you have {current}). "
            f"Update: pip install -U whitemagic"
        )
    return None
