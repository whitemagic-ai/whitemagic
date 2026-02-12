"""Unified Telemetry System - v12.3
Tracks tool latency, errors, and success rates.
"""

import json
import logging
from collections import deque
from datetime import datetime
from pathlib import Path
from typing import Any, cast

logger = logging.getLogger(__name__)

class Telemetry:
    """Unified telemetry for monitoring tool performance and reliability."""

    def __init__(self, log_path: Path | None = None) -> None:
        from whitemagic.config.paths import WM_ROOT
        self.log_path = log_path or (WM_ROOT / "logs" / "telemetry.jsonl")
        self.log_path.parent.mkdir(parents=True, exist_ok=True)

        # In-memory buffer for fast summaries (last 100 calls)
        self.recent_calls: deque[dict[str, Any]] = deque(maxlen=100)

        # Aggregated stats
        self.stats: dict[str, Any] = {
            "total_calls": 0,
            "success_count": 0,
            "error_count": 0,
            "total_latency": 0.0,
            "errors_by_code": {},
            "context_reuse_hits": 0,
            "context_reuse_misses": 0,
            "per_tool": {},
        }

    def record_context_reuse(self, hit: bool) -> None:
        """Record whether a tool call used recalled memory (hit) or fresh context (miss)."""
        if hit:
            self.stats["context_reuse_hits"] += 1
        else:
            self.stats["context_reuse_misses"] += 1

    def record_call(self, tool: str, duration: float, status: str, error_code: str | None = None) -> None:
        """Record a tool execution event."""
        event = {
            "timestamp": datetime.now().isoformat(),
            "tool": tool,
            "duration": round(duration, 4),
            "status": status,
            "error_code": error_code,
        }

        # 1. Update in-memory stats
        self.stats["total_calls"] += 1
        self.stats["total_latency"] += duration

        if status == "success":
            self.stats["success_count"] += 1
        else:
            self.stats["error_count"] += 1
            if error_code:
                errors_by_code = cast("dict[str, int]", self.stats["errors_by_code"])
                errors_by_code[error_code] = errors_by_code.get(error_code, 0) + 1

        # Per-tool stats
        per_tool = cast("dict[str, dict[str, Any]]", self.stats["per_tool"])
        if tool not in per_tool:
            per_tool[tool] = {"calls": 0, "total_latency": 0.0, "errors": 0}
        per_tool[tool]["calls"] += 1
        per_tool[tool]["total_latency"] += duration
        if status != "success":
            per_tool[tool]["errors"] += 1

        self.recent_calls.append(event)

        # 2. Persist to JSON-L
        try:
            with open(self.log_path, "a", encoding="utf-8") as f:
                f.write(json.dumps(event) + "\n")
        except Exception as e:
            logger.warning(f"Failed to persist telemetry: {e}")

    def get_summary(self) -> dict[str, Any]:
        """Get summarized performance metrics."""
        avg_latency = self.stats["total_latency"] / max(1, self.stats["total_calls"])
        success_rate = self.stats["success_count"] / max(1, self.stats["total_calls"])

        hits = self.stats["context_reuse_hits"]
        misses = self.stats["context_reuse_misses"]
        reuse_total = hits + misses
        reuse_rate = hits / max(1, reuse_total)

        # Top 5 most-called tools
        per_tool = cast("dict[str, dict[str, Any]]", self.stats["per_tool"])
        top_tools = sorted(per_tool.items(), key=lambda x: x[1]["calls"], reverse=True)[:5]

        return {
            "total_calls": self.stats["total_calls"],
            "avg_latency_ms": round(avg_latency * 1000, 2),
            "success_rate": round(success_rate, 4),
            "error_count": self.stats["error_count"],
            "errors_by_code": self.stats["errors_by_code"],
            "context_reuse": {
                "hits": hits,
                "misses": misses,
                "reuse_rate": round(reuse_rate, 4),
            },
            "top_tools": [
                {"tool": name, "calls": stats["calls"], "avg_ms": round(stats["total_latency"] / max(1, stats["calls"]) * 1000, 2)}
                for name, stats in top_tools
            ],
            "recent_events": list(self.recent_calls)[-10:],
        }

# Global instance
_telemetry = None

def get_telemetry() -> Telemetry:
    global _telemetry
    if _telemetry is None:
        _telemetry = Telemetry()
    return _telemetry
