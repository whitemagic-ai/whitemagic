"""MCP Integrity Checking — Edgerunner Violet Security Layer
============================================================
Computes and verifies SHA-256 fingerprints of MCP tool definitions
to detect tampering, drift, or unauthorized modifications.

Inspired by: AIBoMGen (Ghent, 2026), OpenSSF Model Signing, and
the MCP server vulnerability findings (7.2% of 1,899 servers
compromised — Hasan et al., 2025).

Usage:
    from whitemagic.security.mcp_integrity import get_mcp_integrity
    integrity = get_mcp_integrity()
    integrity.snapshot()          # capture current tool fingerprints
    report = integrity.verify()   # detect drift since last snapshot
"""

from __future__ import annotations

import hashlib
import json
import logging
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class ToolFingerprint:
    """Cryptographic fingerprint of a single tool definition."""

    tool_name: str
    schema_hash: str       # SHA-256 of the canonical JSON of input_schema
    description_hash: str  # SHA-256 of description string
    safety: str
    category: str
    composite_hash: str    # SHA-256 of all the above combined
    timestamp: str = ""


def _sha256(data: str) -> str:
    return hashlib.sha256(data.encode("utf-8")).hexdigest()


def _canonical_json(obj: Any) -> str:
    """Deterministic JSON serialization for hashing."""
    return json.dumps(obj, sort_keys=True, separators=(",", ":"), default=str)


class McpIntegrity:
    """Fingerprints all MCP tool definitions and detects drift.

    On first call to ``snapshot()``, records the SHA-256 of every tool's
    schema, description, safety level, and category.  On subsequent calls
    to ``verify()``, compares current state against the snapshot and
    reports any additions, removals, or modifications.
    """

    def __init__(self, storage_dir: Path | None = None):
        self._lock = threading.Lock()
        self._baseline: dict[str, ToolFingerprint] = {}
        self._snapshot_time: float = 0.0
        self._storage_dir = storage_dir
        self._verification_count: int = 0
        self._drift_events: list[dict[str, Any]] = []

        if self._storage_dir:
            self._storage_dir.mkdir(parents=True, exist_ok=True)
            self._load_baseline()

    def _fingerprint_tool(self, tool: Any) -> ToolFingerprint:
        """Compute fingerprint for a single ToolDefinition."""
        schema_hash = _sha256(_canonical_json(tool.input_schema or {}))
        desc_hash = _sha256(str(tool.description or ""))
        safety = str(getattr(tool, "safety", "read"))
        category = str(getattr(tool, "category", "unknown"))
        composite = _sha256(f"{tool.name}|{schema_hash}|{desc_hash}|{safety}|{category}")
        return ToolFingerprint(
            tool_name=tool.name,
            schema_hash=schema_hash,
            description_hash=desc_hash,
            safety=safety,
            category=category,
            composite_hash=composite,
        )

    def snapshot(self) -> dict[str, Any]:
        """Capture current tool registry as the integrity baseline."""
        from whitemagic.tools.registry import TOOL_REGISTRY

        fingerprints: dict[str, ToolFingerprint] = {}
        for tool in TOOL_REGISTRY:
            fp = self._fingerprint_tool(tool)
            fingerprints[tool.name] = fp

        with self._lock:
            self._baseline = fingerprints
            self._snapshot_time = time.time()

        self._persist_baseline()

        logger.info(
            "MCP integrity: snapshot captured (%d tools fingerprinted)", len(fingerprints),
        )
        return {
            "status": "success",
            "tools_fingerprinted": len(fingerprints),
            "snapshot_time": self._snapshot_time,
            "registry_hash": self._registry_hash(fingerprints),
        }

    def verify(self) -> dict[str, Any]:
        """Compare current registry against the baseline snapshot."""
        from whitemagic.tools.registry import TOOL_REGISTRY

        with self._lock:
            if not self._baseline:
                return {
                    "status": "warning",
                    "message": "No baseline snapshot exists. Call snapshot() first.",
                    "verified": False,
                }

        current: dict[str, ToolFingerprint] = {}
        for tool in TOOL_REGISTRY:
            fp = self._fingerprint_tool(tool)
            current[tool.name] = fp

        added: list[str] = []
        removed: list[str] = []
        modified: list[dict[str, str]] = []

        with self._lock:
            baseline_names = set(self._baseline.keys())
            current_names = set(current.keys())

            added = sorted(current_names - baseline_names)
            removed = sorted(baseline_names - current_names)

            for name in baseline_names & current_names:
                if self._baseline[name].composite_hash != current[name].composite_hash:
                    modified.append({
                        "tool": name,
                        "baseline_hash": self._baseline[name].composite_hash[:16],
                        "current_hash": current[name].composite_hash[:16],
                    })

            self._verification_count += 1

        is_clean = not added and not removed and not modified
        result = {
            "status": "success",
            "verified": is_clean,
            "integrity": "CLEAN" if is_clean else "DRIFT_DETECTED",
            "baseline_count": len(self._baseline),
            "current_count": len(current),
            "added": added,
            "removed": removed,
            "modified": modified,
            "verification_number": self._verification_count,
        }

        if not is_clean:
            event = {
                "type": "mcp_drift",
                "added": len(added),
                "removed": len(removed),
                "modified": len(modified),
                "timestamp": time.time(),
            }
            with self._lock:
                self._drift_events.append(event)
                if len(self._drift_events) > 1000:
                    self._drift_events = self._drift_events[-500:]
            logger.warning(
                "MCP integrity DRIFT: +%d added, -%d removed, ~%d modified",
                len(added), len(removed), len(modified),
            )

        return result

    def status(self) -> dict[str, Any]:
        """Return integrity subsystem status."""
        with self._lock:
            return {
                "has_baseline": bool(self._baseline),
                "baseline_tool_count": len(self._baseline),
                "snapshot_time": self._snapshot_time,
                "verification_count": self._verification_count,
                "drift_events": len(self._drift_events),
                "recent_drifts": self._drift_events[-5:],
            }

    def _registry_hash(self, fingerprints: dict[str, ToolFingerprint]) -> str:
        """Compute a single hash over the entire registry for quick comparison."""
        ordered = sorted(fingerprints.items())
        combined = "|".join(f"{name}:{fp.composite_hash}" for name, fp in ordered)
        return _sha256(combined)[:32]

    def _persist_baseline(self) -> None:
        if not self._storage_dir:
            return
        try:
            path = self._storage_dir / "mcp_integrity_baseline.json"
            data = {
                "snapshot_time": self._snapshot_time,
                "tools": {
                    name: {
                        "schema_hash": fp.schema_hash,
                        "description_hash": fp.description_hash,
                        "safety": fp.safety,
                        "category": fp.category,
                        "composite_hash": fp.composite_hash,
                    }
                    for name, fp in self._baseline.items()
                },
            }
            path.write_text(json.dumps(data, indent=2), encoding="utf-8")
        except Exception as e:
            logger.debug("MCP integrity persist failed: %s", e)

    def _load_baseline(self) -> None:
        if not self._storage_dir:
            return
        path = self._storage_dir / "mcp_integrity_baseline.json"
        if not path.exists():
            return
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
            self._snapshot_time = data.get("snapshot_time", 0.0)
            for name, fp_data in data.get("tools", {}).items():
                self._baseline[name] = ToolFingerprint(
                    tool_name=name,
                    schema_hash=fp_data["schema_hash"],
                    description_hash=fp_data["description_hash"],
                    safety=fp_data["safety"],
                    category=fp_data["category"],
                    composite_hash=fp_data["composite_hash"],
                )
            logger.info("MCP integrity: loaded baseline (%d tools)", len(self._baseline))
        except Exception as e:
            logger.debug("MCP integrity load failed: %s", e)


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_instance: McpIntegrity | None = None
_instance_lock = threading.Lock()


def get_mcp_integrity() -> McpIntegrity:
    """Get the global MCP Integrity instance."""
    global _instance
    if _instance is None:
        with _instance_lock:
            if _instance is None:
                try:
                    from whitemagic.config.paths import WM_ROOT
                    storage = WM_ROOT / "security"
                except Exception:
                    storage = None
                _instance = McpIntegrity(storage_dir=storage)
    return _instance
