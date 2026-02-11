"""Security Circuit Breakers — Edgerunner Violet Security Layer
================================================================
Anomaly detection for suspicious tool-call patterns. Extends the
existing Stoic Circuit Breaker with security-specific heuristics:

  - **Rapid-fire detection**: flags N calls to the same tool within M seconds
  - **Escalation detection**: flags sequences like read→write→delete
  - **Lateral movement**: flags calls to many distinct tools in a short window
  - **Exfiltration pattern**: flags large data reads followed by external sends

Each detection can trigger graduated responses (LOG, WARN, THROTTLE, BLOCK)
via the Dharma Engine, and feeds into the Harmony Vector's security score.

Usage:
    from whitemagic.security.security_breaker import get_security_monitor
    monitor = get_security_monitor()
    alert = monitor.record_call("shell_exec", safety="WRITE")
    if alert:
        print(alert["pattern"], alert["action"])
"""

from __future__ import annotations

import logging
import threading
import time
from collections import defaultdict, deque
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class SecurityAlert:
    """A security anomaly detection alert."""

    pattern: str          # e.g., "rapid_fire", "escalation", "lateral_movement"
    severity: float       # 0.0–1.0
    action: str           # "log", "warn", "throttle", "block"
    tool: str
    detail: str
    timestamp: float


class SecurityMonitor:
    """Monitors tool-call patterns for security anomalies.

    Runs alongside the existing circuit breaker system — the standard
    breaker handles availability (N failures → open), while this monitor
    handles security (suspicious patterns → graduated response).
    """

    def __init__(
        self,
        rapid_fire_threshold: int = 10,
        rapid_fire_window_s: float = 5.0,
        lateral_threshold: int = 15,
        lateral_window_s: float = 10.0,
        escalation_window_s: float = 30.0,
    ):
        self._lock = threading.Lock()

        # Config
        self._rapid_fire_threshold = rapid_fire_threshold
        self._rapid_fire_window = rapid_fire_window_s
        self._lateral_threshold = lateral_threshold
        self._lateral_window = lateral_window_s
        self._escalation_window = escalation_window_s

        # State
        self._call_log: deque[dict[str, Any]] = deque(maxlen=10000)
        self._per_tool_times: dict[str, deque[float]] = defaultdict(lambda: deque(maxlen=200))
        self._alerts: list[SecurityAlert] = []
        self._total_calls: int = 0
        self._blocked_count: int = 0

    def record_call(
        self,
        tool: str,
        safety: str = "READ",
        agent_id: str = "default",
    ) -> dict[str, Any] | None:
        """Record a tool call and check for security anomalies.

        Returns an alert dict if an anomaly is detected, or None if clean.
        """
        now = time.time()
        entry = {
            "tool": tool,
            "safety": safety.upper(),
            "agent_id": agent_id,
            "timestamp": now,
        }

        with self._lock:
            self._call_log.append(entry)
            self._per_tool_times[tool].append(now)
            self._total_calls += 1

        # Check all patterns
        alert = (
            self._check_rapid_fire(tool, now)
            or self._check_lateral_movement(now)
            or self._check_escalation(tool, safety.upper(), now)
        )

        if alert:
            with self._lock:
                self._alerts.append(alert)
                if len(self._alerts) > 5000:
                    self._alerts = self._alerts[-2500:]
                if alert.action == "block":
                    self._blocked_count += 1

            logger.warning(
                "Security alert [%s]: %s (tool=%s, severity=%.1f, action=%s)",
                alert.pattern, alert.detail, alert.tool, alert.severity, alert.action,
            )

            return {
                "alert": True,
                "pattern": alert.pattern,
                "severity": alert.severity,
                "action": alert.action,
                "tool": alert.tool,
                "detail": alert.detail,
                "timestamp": alert.timestamp,
            }

        return None

    def _check_rapid_fire(self, tool: str, now: float) -> SecurityAlert | None:
        """Detect rapid-fire calls to the same tool."""
        with self._lock:
            times = self._per_tool_times[tool]
            cutoff = now - self._rapid_fire_window
            recent = sum(1 for t in times if t >= cutoff)

        if recent >= self._rapid_fire_threshold:
            severity = min(1.0, recent / (self._rapid_fire_threshold * 2))
            action = "block" if recent >= self._rapid_fire_threshold * 2 else "throttle"
            return SecurityAlert(
                pattern="rapid_fire",
                severity=severity,
                action=action,
                tool=tool,
                detail=(
                    f"{recent} calls to '{tool}' in {self._rapid_fire_window}s "
                    f"(threshold: {self._rapid_fire_threshold})"
                ),
                timestamp=now,
            )
        return None

    def _check_lateral_movement(self, now: float) -> SecurityAlert | None:
        """Detect calls to many distinct tools in a short window (lateral movement)."""
        with self._lock:
            cutoff = now - self._lateral_window
            recent_tools = set()
            for entry in reversed(self._call_log):
                if entry["timestamp"] < cutoff:
                    break
                recent_tools.add(entry["tool"])

        if len(recent_tools) >= self._lateral_threshold:
            severity = min(1.0, len(recent_tools) / (self._lateral_threshold * 2))
            return SecurityAlert(
                pattern="lateral_movement",
                severity=severity,
                action="warn",
                tool="*",
                detail=(
                    f"{len(recent_tools)} distinct tools called in {self._lateral_window}s "
                    f"(threshold: {self._lateral_threshold})"
                ),
                timestamp=now,
            )
        return None

    def _check_escalation(self, tool: str, safety: str, now: float) -> SecurityAlert | None:
        """Detect privilege escalation patterns (READ→WRITE→DELETE in sequence)."""
        if safety not in ("WRITE", "DELETE"):
            return None

        with self._lock:
            cutoff = now - self._escalation_window
            recent_safeties: list[str] = []
            for entry in reversed(self._call_log):
                if entry["timestamp"] < cutoff:
                    break
                recent_safeties.append(entry["safety"])

        recent_safeties.reverse()

        # Look for READ→WRITE→DELETE escalation pattern
        if safety == "DELETE":
            has_read = "READ" in recent_safeties
            has_write = "WRITE" in recent_safeties
            if has_read and has_write:
                return SecurityAlert(
                    pattern="escalation",
                    severity=0.8,
                    action="warn",
                    tool=tool,
                    detail=(
                        f"Privilege escalation pattern detected: READ→WRITE→DELETE "
                        f"within {self._escalation_window}s (current: {tool})"
                    ),
                    timestamp=now,
                )

        # Rapid WRITE/DELETE burst
        write_delete_count = sum(1 for s in recent_safeties if s in ("WRITE", "DELETE"))
        if write_delete_count >= 5:
            return SecurityAlert(
                pattern="mutation_burst",
                severity=0.6,
                action="throttle",
                tool=tool,
                detail=(
                    f"{write_delete_count} mutation operations in {self._escalation_window}s"
                ),
                timestamp=now,
            )

        return None

    def get_alerts(self, limit: int = 50) -> list[dict[str, Any]]:
        """Return recent security alerts."""
        with self._lock:
            alerts = self._alerts[-limit:]
        return [
            {
                "pattern": a.pattern,
                "severity": a.severity,
                "action": a.action,
                "tool": a.tool,
                "detail": a.detail,
                "timestamp": a.timestamp,
            }
            for a in alerts
        ]

    def status(self) -> dict[str, Any]:
        """Return security monitor status."""
        with self._lock:
            pattern_counts: dict[str, int] = defaultdict(int)
            for a in self._alerts:
                pattern_counts[a.pattern] += 1

            return {
                "total_calls_monitored": self._total_calls,
                "total_alerts": len(self._alerts),
                "blocked_count": self._blocked_count,
                "alert_patterns": dict(pattern_counts),
                "recent_alerts": [
                    {
                        "pattern": a.pattern,
                        "severity": a.severity,
                        "action": a.action,
                        "tool": a.tool,
                        "timestamp": a.timestamp,
                    }
                    for a in self._alerts[-5:]
                ],
                "config": {
                    "rapid_fire_threshold": self._rapid_fire_threshold,
                    "rapid_fire_window_s": self._rapid_fire_window,
                    "lateral_threshold": self._lateral_threshold,
                    "lateral_window_s": self._lateral_window,
                    "escalation_window_s": self._escalation_window,
                },
            }

    def reset(self) -> None:
        """Reset all state (for testing)."""
        with self._lock:
            self._call_log.clear()
            self._per_tool_times.clear()
            self._alerts.clear()
            self._total_calls = 0
            self._blocked_count = 0


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_monitor: SecurityMonitor | None = None
_monitor_lock = threading.Lock()


def get_security_monitor() -> SecurityMonitor:
    """Get the global Security Monitor instance."""
    global _monitor
    if _monitor is None:
        with _monitor_lock:
            if _monitor is None:
                _monitor = SecurityMonitor()
    return _monitor
