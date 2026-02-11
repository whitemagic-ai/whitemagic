"""Dispatch-Level Rate Limiter — Per-Agent, Per-Tool Call Throttling.
=================================================================
Prevents runaway agents from consuming excessive resources. Plugs into
the dispatch pipeline at step 0.25 (after circuit breaker, before maturity).

Two levels of limiting:
  1. **Per-tool**: e.g. max 60 calls/minute to any single tool
  2. **Global per-agent**: e.g. max 300 calls/minute across all tools

Configurable via overrides for specific tools (e.g. gnosis gets a higher
limit since it's read-only introspection).

Usage:
    from whitemagic.tools.rate_limiter import get_rate_limiter

    limiter = get_rate_limiter()
    result = limiter.check("agent_123", "create_memory")
    if result is not None:
        return result  # blocked — contains retry_after_seconds
"""

import logging
import threading
import time
from collections import defaultdict
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)

# Cache Rust rate limiter availability at module level (v13.3.3)
_RUST_RATE_AVAILABLE = False
_rust_rate_check: Any | None = None
try:
    from whitemagic.optimization.rust_accelerators import (
        rate_check as _imported_rust_rate_check,
    )
    from whitemagic.optimization.rust_accelerators import (
        rust_rate_limiter_available,
    )
    _RUST_RATE_AVAILABLE = rust_rate_limiter_available()
    _rust_rate_check = _imported_rust_rate_check
    if _RUST_RATE_AVAILABLE:
        logger.debug("Rust atomic rate limiter cached as primary path")
except ImportError:
    pass


@dataclass
class RateLimitConfig:
    """Configuration for rate limiting."""

    per_tool_rpm: int = 60          # Max calls per minute per tool
    global_rpm: int = 300           # Max calls per minute across all tools
    burst_allowance: int = 10       # Extra burst above limit before hard block
    window_seconds: float = 60.0    # Sliding window size


# Per-tool overrides: tool_name -> max calls per minute
_TOOL_RATE_OVERRIDES: dict[str, int] = {
    # Introspection tools are cheap — higher limits
    "gnosis": 120,
    "capabilities": 120,
    "manifest": 120,
    "harmony_vector": 120,
    "salience.spotlight": 120,
    "homeostasis.status": 120,
    "maturity.assess": 120,
    "tool.graph": 120,
    # Write-heavy tools — lower limits
    "create_memory": 30,
    "broker.publish": 30,
    "pipeline.create": 20,
    "task.distribute": 20,
    # Expensive tools — tight limits
    "reasoning.bicameral": 10,
    "memory.retention_sweep": 10,
    "memory.consolidate": 5,
}


class _SlidingWindow:
    """Thread-safe sliding window counter."""

    def __init__(self, window_seconds: float = 60.0):
        self._window = window_seconds
        self._timestamps: list[float] = []
        self._lock = threading.Lock()

    def record(self) -> int:
        """Record a call and return current count within window."""
        now = time.monotonic()
        with self._lock:
            cutoff = now - self._window
            self._timestamps = [t for t in self._timestamps if t > cutoff]
            self._timestamps.append(now)
            return len(self._timestamps)

    def count(self) -> int:
        """Return current count without recording."""
        now = time.monotonic()
        with self._lock:
            cutoff = now - self._window
            self._timestamps = [t for t in self._timestamps if t > cutoff]
            return len(self._timestamps)

    def oldest_in_window(self) -> float | None:
        """Return the oldest timestamp still in the window."""
        with self._lock:
            return self._timestamps[0] if self._timestamps else None


class RateLimiter:
    """Per-agent, per-tool rate limiter with sliding windows."""

    def __init__(self, config: RateLimitConfig | None = None):
        self._config = config or RateLimitConfig()
        # agent_id -> tool_name -> SlidingWindow
        self._tool_windows: dict[str, dict[str, _SlidingWindow]] = defaultdict(
            lambda: defaultdict(lambda: _SlidingWindow(self._config.window_seconds)),
        )
        # agent_id -> SlidingWindow (global)
        self._global_windows: dict[str, _SlidingWindow] = defaultdict(
            lambda: _SlidingWindow(self._config.window_seconds),
        )
        self._total_blocked = 0
        self._total_checked = 0
        self._lock = threading.Lock()

    def check(self, agent_id: str, tool_name: str) -> dict[str, Any] | None:
        """Check if the call should be rate-limited.

        Trust-aware: agents with higher trust get more generous limits,
        while RESTRICTED agents get tighter throttling.

        Uses Rust atomic rate limiter as primary path (0.45μs, 2.21M ops/sec)
        with Python sliding window as fallback.

        Returns None if allowed, or a dict with error info if blocked.
        """
        self._total_checked += 1

        # Fast pre-check: Rust atomic rate limiter (lock-free, sub-μs)
        # Rust enforces its own global defaults; if it blocks, we block immediately.
        # If Rust allows, we still check Python's configurable per-tool/global limits.
        if _RUST_RATE_AVAILABLE and callable(_rust_rate_check):
            try:
                result = _rust_rate_check(f"{agent_id}:{tool_name}")
                if result and not result.get("allowed", True):
                    self._total_blocked += 1
                    return {
                        "status": "error",
                        "error": f"Rate limited: {tool_name} (Rust atomic)",
                        "error_code": "rate_limited",
                        "retry_after_seconds": 1.0,
                        "limit": result.get("limit", 60),
                        "current": result.get("current_rate", 0),
                    }
            except Exception:
                pass  # Rust call failed, fall through to Python

        # Trust-based multiplier (Gap A2 synthesis)
        trust_mult = self._trust_multiplier(agent_id)

        # Per-tool check
        base_limit = _TOOL_RATE_OVERRIDES.get(tool_name, self._config.per_tool_rpm)
        tool_limit = max(1, int(base_limit * trust_mult))
        tool_window = self._tool_windows[agent_id][tool_name]
        tool_count = tool_window.record()

        if tool_count > tool_limit + self._config.burst_allowance:
            self._total_blocked += 1
            retry_after = self._estimate_retry(tool_window)
            logger.warning(f"Rate limited: agent={agent_id} tool={tool_name} ({tool_count}/{tool_limit}/min)")
            return {
                "status": "error",
                "error": f"Rate limited: {tool_name} ({tool_count} calls in last minute, limit={tool_limit})",
                "error_code": "rate_limited",
                "retry_after_seconds": retry_after,
                "limit": tool_limit,
                "current": tool_count,
            }

        # Global check
        global_window = self._global_windows[agent_id]
        global_count = global_window.record()

        if global_count > self._config.global_rpm + self._config.burst_allowance:
            self._total_blocked += 1
            retry_after = self._estimate_retry(global_window)
            logger.warning(f"Global rate limited: agent={agent_id} ({global_count}/{self._config.global_rpm}/min)")
            return {
                "status": "error",
                "error": f"Global rate limit: {global_count} calls in last minute (limit={self._config.global_rpm})",
                "error_code": "rate_limited",
                "retry_after_seconds": retry_after,
                "limit": self._config.global_rpm,
                "current": global_count,
            }

        return None  # Allowed

    # ------------------------------------------------------------------
    # Trust-based dynamic throttling (Gap A2 synthesis)
    # ------------------------------------------------------------------

    # Tier → multiplier: EXEMPLARY gets 2× limits, RESTRICTED gets 0.25×
    _TRUST_MULTIPLIERS: dict[str, float] = {
        "EXEMPLARY": 2.0,
        "TRUSTED": 1.5,
        "STANDARD": 1.0,
        "PROBATIONARY": 0.5,
        "RESTRICTED": 0.25,
    }

    def _trust_multiplier(self, agent_id: str) -> float:
        """Query the agent's trust tier and return a rate-limit multiplier.

        Higher trust → higher multiplier → more generous limits.
        Falls back to 1.0 (STANDARD) if trust data is unavailable.
        """
        try:
            from whitemagic.tools.agent_trust import get_agent_trust_scores
            result = get_agent_trust_scores(agent_id=agent_id)
            agents = result.get("agents", {})
            agent_data = agents.get(agent_id)
            if agent_data:
                tier = agent_data.get("tier", "STANDARD")
                return self._TRUST_MULTIPLIERS.get(tier, 1.0)
        except Exception:
            pass
        return 1.0  # default: no adjustment

    def _estimate_retry(self, window: _SlidingWindow) -> float:
        """Estimate seconds until oldest call falls out of window."""
        oldest = window.oldest_in_window()
        if oldest is None:
            return 1.0
        elapsed = time.monotonic() - oldest
        return max(round(self._config.window_seconds - elapsed, 1), 1.0)

    def get_stats(self) -> dict[str, Any]:
        """Return rate limiter statistics."""
        return {
            "total_checked": self._total_checked,
            "total_blocked": self._total_blocked,
            "block_rate": round(self._total_blocked / max(self._total_checked, 1), 4),
            "tracked_agents": len(self._global_windows),
            "config": {
                "per_tool_rpm": self._config.per_tool_rpm,
                "global_rpm": self._config.global_rpm,
                "burst_allowance": self._config.burst_allowance,
            },
        }

    def get_agent_usage(self, agent_id: str) -> dict[str, Any]:
        """Return current rate usage for a specific agent."""
        global_count = self._global_windows[agent_id].count() if agent_id in self._global_windows else 0
        tool_counts = {}
        if agent_id in self._tool_windows:
            for tool_name, window in self._tool_windows[agent_id].items():
                c = window.count()
                if c > 0:
                    tool_counts[tool_name] = c
        return {
            "agent_id": agent_id,
            "global_calls_this_minute": global_count,
            "per_tool_calls": tool_counts,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_instance: RateLimiter | None = None
_instance_lock = threading.Lock()


def get_rate_limiter() -> RateLimiter:
    """Get or create the global RateLimiter singleton."""
    global _instance
    with _instance_lock:
        if _instance is None:
            _instance = RateLimiter()
        return _instance
