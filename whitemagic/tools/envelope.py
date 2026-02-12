"""Tool Envelope Helpers.
=====================

All tool responses returned via `whitemagic.tools.unified_api.call_tool()` must
conform to a stable envelope shape so AI callers can reason reliably.

Design goals:
- Always JSON-serializable
- Stable top-level keys
- Tool-specific payload lives under `details`
"""

from __future__ import annotations

import base64
import json
from collections.abc import Iterable, Mapping, MutableMapping
from dataclasses import asdict, is_dataclass
from datetime import date, datetime
from enum import Enum
from pathlib import Path
from typing import Any, cast

from whitemagic.tools.contract import ENVELOPE_VERSION, TOOL_CONTRACT_VERSION

_RESERVED_KEYS = {
    "status",
    "tool",
    "request_id",
    "idempotency_key",
    "message",
    "error_code",
    "details",
    "retryable",
    "writes",
    "artifacts",
    "metrics",
    "side_effects",
    "warnings",
    "timestamp",
    "envelope_version",
    "tool_contract_version",
}


def _stable_sorted(items: Iterable[Any]) -> list[Any]:
    try:
        return sorted(items)
    except Exception:
        return sorted(items, key=lambda x: str(x))


def coerce_jsonable(value: Any) -> Any:
    """Best-effort conversion to JSON-safe types."""
    if value is None:
        return None
    if isinstance(value, (str, int, float, bool)):
        return value
    if isinstance(value, (datetime, date)):
        return value.isoformat()
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, Enum):
        # Prefer the human-stable string, then fallback.
        v = getattr(value, "value", None)
        return v if isinstance(v, (str, int, float, bool)) else getattr(value, "name", str(value))
    if isinstance(value, bytes):
        return {"_type": "bytes", "b64": base64.b64encode(value).decode("ascii")}
    if is_dataclass(value) and not isinstance(value, type):
        return coerce_jsonable(asdict(value))
    if hasattr(value, "to_dict") and callable(getattr(value, "to_dict")):
        try:
            return coerce_jsonable(value.to_dict())
        except Exception:
            pass
    if isinstance(value, Mapping):
        out: dict[str, Any] = {}
        for k, v in value.items():
            out[str(k)] = coerce_jsonable(v)
        return out
    if isinstance(value, (list, tuple)):
        return [coerce_jsonable(v) for v in value]
    if isinstance(value, set):
        return [coerce_jsonable(v) for v in _stable_sorted(value)]
    # Last resort: string repr.
    return str(value)


def _ensure_jsonable_dict(d: MutableMapping[str, Any]) -> dict[str, Any]:
    return cast("dict[str, Any]", coerce_jsonable(d))


def ok(
    *,
    tool: str,
    request_id: str,
    message: str | None = None,
    details: Any = None,
    idempotency_key: str | None = None,
    timestamp: str | None = None,
    retryable: bool = False,
    writes: list[dict[str, Any]] | None = None,
    artifacts: list[dict[str, Any]] | None = None,
    metrics: dict[str, Any] | None = None,
    side_effects: dict[str, Any] | None = None,
    warnings: list[str] | None = None,
) -> dict[str, Any]:
    env: dict[str, Any] = {
        "status": "success",
        "tool": tool,
        "request_id": request_id,
        "idempotency_key": idempotency_key,
        "message": message or "ok",
        "error_code": None,
        "details": coerce_jsonable(details) if details is not None else {},
        "retryable": bool(retryable),
        "writes": coerce_jsonable(writes or []),
        "artifacts": coerce_jsonable(artifacts or []),
        "metrics": coerce_jsonable(metrics or {}),
        "side_effects": coerce_jsonable(side_effects or {}),
        "warnings": coerce_jsonable(warnings or []),
        "timestamp": timestamp,
        "envelope_version": ENVELOPE_VERSION,
        "tool_contract_version": TOOL_CONTRACT_VERSION,
    }
    return _ensure_jsonable_dict(env)


def err(
    *,
    tool: str,
    request_id: str,
    error_code: str,
    message: str,
    details: Any = None,
    idempotency_key: str | None = None,
    timestamp: str | None = None,
    retryable: bool = False,
    writes: list[dict[str, Any]] | None = None,
    artifacts: list[dict[str, Any]] | None = None,
    metrics: dict[str, Any] | None = None,
    side_effects: dict[str, Any] | None = None,
    warnings: list[str] | None = None,
) -> dict[str, Any]:
    env: dict[str, Any] = {
        "status": "error",
        "tool": tool,
        "request_id": request_id,
        "idempotency_key": idempotency_key,
        "message": message,
        "error_code": error_code,
        "details": coerce_jsonable(details) if details is not None else {},
        "retryable": bool(retryable),
        "writes": coerce_jsonable(writes or []),
        "artifacts": coerce_jsonable(artifacts or []),
        "metrics": coerce_jsonable(metrics or {}),
        "side_effects": coerce_jsonable(side_effects or {}),
        "warnings": coerce_jsonable(warnings or []),
        "timestamp": timestamp,
        "envelope_version": ENVELOPE_VERSION,
        "tool_contract_version": TOOL_CONTRACT_VERSION,
    }
    return _ensure_jsonable_dict(env)


def is_enveloped(result: Any) -> bool:
    return isinstance(result, dict) and "status" in result and "tool" in result and "details" in result


def normalize_raw(
    *,
    tool: str,
    request_id: str,
    raw: Any,
    idempotency_key: str | None = None,
    timestamp: str | None = None,
) -> dict[str, Any]:
    """Convert legacy tool return shapes into the v11 envelope.

    Legacy patterns seen in the codebase:
    - {"status": "success", ...}
    - {"status": "error", "message": "..."}
    - {"success": true/false, ...}
    - non-dict values (list/str/etc.)
    """
    if is_enveloped(raw):
        # Ensure required meta keys are present & JSONable.
        out = dict(raw)
        out.setdefault("request_id", request_id)
        out.setdefault("tool", tool)
        out.setdefault("idempotency_key", idempotency_key)
        out.setdefault("timestamp", timestamp)
        return _ensure_jsonable_dict(out)

    # Normalize to dict payload.
    payload: dict[str, Any]
    if isinstance(raw, dict):
        payload = dict(raw)
    else:
        payload = {"value": raw}

    status_val = payload.get("status")
    success_val = payload.get("success")

    has_error_signal = bool(payload.get("error_code") or payload.get("code") or payload.get("error"))

    if isinstance(status_val, str):
        normalized_status = "success" if status_val.lower() in {"success", "ok"} else "error"
    elif isinstance(success_val, bool):
        normalized_status = "success" if success_val else "error"
    elif has_error_signal:
        normalized_status = "error"
    else:
        # Default to success if not specified.
        normalized_status = "success"

    msg = payload.get("message")
    if not isinstance(msg, str) or not msg.strip():
        if normalized_status == "error":
            msg = payload.get("error") or "error"
        else:
            msg = "ok"

    # Remove envelope-ish keys from details to avoid duplication.
    details = {k: v for k, v in payload.items() if k not in _RESERVED_KEYS and k not in {"success"}}

    if normalized_status == "error":
        error_code = payload.get("error_code") or payload.get("code") or "internal_error"
        return err(
            tool=tool,
            request_id=request_id,
            idempotency_key=idempotency_key,
            timestamp=timestamp,
            error_code=str(error_code),
            message=str(msg),
            details=details,
            retryable=bool(payload.get("retryable", False)),
            writes=payload.get("writes") if isinstance(payload.get("writes"), list) else None,
            artifacts=payload.get("artifacts") if isinstance(payload.get("artifacts"), list) else None,
            metrics=payload.get("metrics") if isinstance(payload.get("metrics"), dict) else None,
            side_effects=payload.get("side_effects") if isinstance(payload.get("side_effects"), dict) else None,
            warnings=payload.get("warnings") if isinstance(payload.get("warnings"), list) else None,
        )

    return ok(
        tool=tool,
        request_id=request_id,
        idempotency_key=idempotency_key,
        timestamp=timestamp,
        message=str(msg),
        details=details,
        retryable=bool(payload.get("retryable", False)),
        writes=payload.get("writes") if isinstance(payload.get("writes"), list) else None,
        artifacts=payload.get("artifacts") if isinstance(payload.get("artifacts"), list) else None,
        metrics=payload.get("metrics") if isinstance(payload.get("metrics"), dict) else None,
        side_effects=payload.get("side_effects") if isinstance(payload.get("side_effects"), dict) else None,
        warnings=payload.get("warnings") if isinstance(payload.get("warnings"), list) else None,
    )


def dumps(result: dict[str, Any]) -> str:
    """Stable JSON dump for envelopes (useful for eval/regression)."""
    return json.dumps(result, indent=2, sort_keys=True, ensure_ascii=True)
