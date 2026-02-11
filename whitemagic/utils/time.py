"""Deterministic Time Utilities
============================

For evaluation, replay, and idempotent behavior, Whitemagic supports an
optional "time override" per tool invocation.

This is intentionally lightweight: callers can pass `now` (ISO 8601 string) to
tools, and the dispatcher will set a context-local override so internal code
*that uses these helpers* becomes deterministic.
"""

from __future__ import annotations

from collections.abc import Generator
from contextlib import contextmanager
from contextvars import ContextVar
from datetime import datetime, timezone

_NOW_OVERRIDE: ContextVar[datetime | None] = ContextVar("_wm_now_override", default=None)


def _normalize(dt: datetime) -> datetime:
    # Convert aware -> UTC naive for compatibility with legacy code.
    if dt.tzinfo is not None:
        dt = dt.astimezone(timezone.utc).replace(tzinfo=None)
    return dt


def parse_now_iso(now_iso: str) -> datetime:
    s = now_iso.strip()
    # Allow common UTC "Z" suffix.
    if s.endswith("Z"):
        s = s[:-1] + "+00:00"
    dt = datetime.fromisoformat(s)
    return _normalize(dt)


def now() -> datetime:
    override = _NOW_OVERRIDE.get()
    if override is not None:
        return override
    return datetime.now(timezone.utc).replace(tzinfo=None)


def utcnow() -> datetime:
    override = _NOW_OVERRIDE.get()
    if override is not None:
        # Override is stored as naive UTC; treat it as UTC for "utcnow".
        return override
    return datetime.now(timezone.utc).replace(tzinfo=None)


def now_iso() -> str:
    return now().isoformat()


@contextmanager
def override_now(now_iso_str: str | None) -> Generator[None, None, None]:
    token = None
    try:
        if now_iso_str:
            token = _NOW_OVERRIDE.set(parse_now_iso(now_iso_str))
        yield
    finally:
        if token is not None:
            _NOW_OVERRIDE.reset(token)
