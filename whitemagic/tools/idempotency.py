"""Idempotency Ledger.
==================

For AI/tool callers, retries are normal. Write tools should support
`idempotency_key` so the caller can safely retry without duplicating writes.

Implementation:
- Store one JSON record per (tool, idempotency_key) under WM_STATE_ROOT.
- Read-before-write to replay prior successful result.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from whitemagic.config.paths import ARTIFACTS_DIR
from whitemagic.tools.envelope import coerce_jsonable
from whitemagic.utils.fileio import atomic_write, file_lock


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat()


def _safe_tool_filename(tool: str) -> str:
    return "".join(c if c.isalnum() or c in {"_", "-", "."} else "_" for c in tool)


def _record_path(tool: str, key: str) -> Path:
    h = hashlib.sha256(f"{tool}:{key}".encode("utf-8")).hexdigest()[:24]
    base = Path(ARTIFACTS_DIR) / "idempotency"
    base.mkdir(parents=True, exist_ok=True)
    return Path(base / f"{_safe_tool_filename(tool)}__{h}.json")


@dataclass(frozen=True)
class IdempotencyRecord:
    tool: str
    key: str
    stored_at: str
    response: dict[str, Any]

    def to_dict(self) -> dict[str, Any]:
        return {
            "tool": self.tool,
            "key": self.key,
            "stored_at": self.stored_at,
            "response": coerce_jsonable(self.response),
        }


def get_record(tool: str, key: str) -> IdempotencyRecord | None:
    path = _record_path(tool, key)
    if not path.exists():
        return None
    try:
        with file_lock(path):
            data = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(data, dict):
            return None
        resp = data.get("response")
        if not isinstance(resp, dict):
            return None
        return IdempotencyRecord(
            tool=str(data.get("tool", tool)),
            key=str(data.get("key", key)),
            stored_at=str(data.get("stored_at", "")),
            response=resp,
        )
    except Exception:
        return None


def put_record(tool: str, key: str, response: dict[str, Any]) -> None:
    path = _record_path(tool, key)
    record = IdempotencyRecord(tool=tool, key=key, stored_at=_utc_now_iso(), response=response)
    with file_lock(path):
        atomic_write(path, json.dumps(record.to_dict(), indent=2, sort_keys=True))
