"""Compact Response Mode — Token-efficient tool output for AI agents.
==================================================================
When ``_compact=true`` is passed to any tool call, the response is
automatically trimmed to essential fields only, reducing token usage.

Rules:
  1. Keep: status, error, error_code (always)
  2. Keep: any scalar fields (str, int, float, bool)
  3. Truncate: lists to first 3 items + count
  4. Truncate: nested dicts to keys-only summary
  5. Truncate: strings > 200 chars to first 200 + "..."

This runs as a post-processor on the dispatch result, not as a
modification to handler logic.

Usage:
    from whitemagic.tools.compact_response import compact

    result = handler(**kwargs)
    if kwargs.get("_compact"):
        result = compact(result)
"""

from typing import Any

# Fields always preserved in full
_PRESERVE_FIELDS = {"status", "error", "error_code", "tool", "retry_after_seconds"}

# Max string length in compact mode
_MAX_STRING = 200

# Max list items shown in compact mode
_MAX_LIST_ITEMS = 3


def compact(result: Any) -> Any:
    """Apply compact transformation to a tool result."""
    if not isinstance(result, dict):
        return result
    return _compact_dict(result, depth=0)


def _compact_dict(d: dict[str, Any], depth: int) -> dict[str, Any]:
    """Recursively compact a dict."""
    out: dict[str, Any] = {}

    for k, v in d.items():
        # Always preserve essential fields
        if k in _PRESERVE_FIELDS:
            out[k] = v
            continue

        # Depth > 1: just summarize
        if depth > 1:
            out[k] = _summarize(v)
            continue

        out[k] = _compact_value(v, depth)

    return out


def _compact_value(v: Any, depth: int) -> Any:
    """Compact a single value."""
    if v is None or isinstance(v, (bool, int, float)):
        return v

    if isinstance(v, str):
        if len(v) > _MAX_STRING:
            return v[:_MAX_STRING] + "..."
        return v

    if isinstance(v, list):
        if len(v) == 0:
            return []
        shown = [_compact_value(item, depth + 1) for item in v[:_MAX_LIST_ITEMS]]
        if len(v) > _MAX_LIST_ITEMS:
            shown.append(f"...and {len(v) - _MAX_LIST_ITEMS} more")
        return shown

    if isinstance(v, dict):
        return _compact_dict(v, depth + 1)

    return str(v)


def _summarize(v: Any) -> Any:
    """Deep summarize: just type + size hints."""
    if v is None or isinstance(v, (bool, int, float)):
        return v
    if isinstance(v, str):
        if len(v) <= 50:
            return v
        return v[:50] + "..."
    if isinstance(v, list):
        return f"[{len(v)} items]"
    if isinstance(v, dict):
        return f"{{{len(v)} keys}}"
    return str(v)[:50]
