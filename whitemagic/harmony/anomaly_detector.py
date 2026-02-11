"""Anomaly Detection on Harmony Vector
=====================================
Statistical anomaly detector using z-score on the 7 Harmony dimensions
over a sliding window. Emits ANOMALY_DETECTED events when drift exceeds
a configurable threshold.

Usage:
    from whitemagic.harmony.anomaly_detector import get_anomaly_detector
    detector = get_anomaly_detector()
    detector.ingest(harmony_snapshot)
    alerts = detector.check()
"""

from __future__ import annotations

import logging
import math
import threading
import time
from collections import deque
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)

HARMONY_DIMENSIONS = ("balance", "throughput", "latency", "error_rate", "dharma", "karma_debt", "energy")


@dataclass
class AnomalyAlert:
    """A detected anomaly on a Harmony dimension."""

    dimension: str
    current_value: float
    mean: float
    std_dev: float
    z_score: float
    direction: str  # "high" or "low"
    severity: str   # "warning" or "critical"
    timestamp: float = 0.0

    def __post_init__(self) -> Any:
        if self.timestamp == 0.0:
            self.timestamp = time.time()

    def to_dict(self) -> dict[str, Any]:
        return {
            "dimension": self.dimension,
            "current": round(self.current_value, 4),
            "mean": round(self.mean, 4),
            "std_dev": round(self.std_dev, 4),
            "z_score": round(self.z_score, 2),
            "direction": self.direction,
            "severity": self.severity,
            "timestamp": self.timestamp,
        }


class AnomalyDetector:
    """Z-score based anomaly detector for Harmony Vector dimensions.

    Maintains a sliding window of observations per dimension.
    When a new observation's z-score exceeds the threshold,
    an alert is generated.
    """

    def __init__(
        self,
        window_size: int = 100,
        warning_threshold: float = 2.0,
        critical_threshold: float = 3.0,
    ) -> None:
        self._window_size = window_size
        self._warning_z = warning_threshold
        self._critical_z = critical_threshold
        self._lock = threading.Lock()

        # Per-dimension sliding windows
        self._windows: dict[str, deque] = {
            dim: deque(maxlen=window_size) for dim in HARMONY_DIMENSIONS
        }

        # Alert history
        self._alerts: deque = deque(maxlen=500)
        self._total_alerts = 0
        self._total_ingested = 0

    def ingest(self, snapshot: dict[str, float]) -> Any:
        """Ingest a Harmony Vector snapshot and check for anomalies."""
        with self._lock:
            self._total_ingested += 1
            alerts = []

            for dim in HARMONY_DIMENSIONS:
                value = snapshot.get(dim)
                if value is None:
                    continue

                window = self._windows[dim]
                window.append(value)

                # Need at least 10 observations for meaningful stats
                if len(window) < 10:
                    continue

                mean = sum(window) / len(window)
                variance = sum((x - mean) ** 2 for x in window) / len(window)
                std_dev = math.sqrt(variance) if variance > 0 else 0.0

                if std_dev < 1e-10:
                    continue  # No variance — can't detect anomalies

                z_score = (value - mean) / std_dev
                abs_z = abs(z_score)

                if abs_z >= self._warning_z:
                    direction = "high" if z_score > 0 else "low"
                    severity = "critical" if abs_z >= self._critical_z else "warning"

                    alert = AnomalyAlert(
                        dimension=dim,
                        current_value=value,
                        mean=mean,
                        std_dev=std_dev,
                        z_score=z_score,
                        direction=direction,
                        severity=severity,
                    )
                    alerts.append(alert)
                    self._alerts.append(alert)
                    self._total_alerts += 1

            # Emit events for critical alerts
            for alert in alerts:
                if alert.severity == "critical":
                    self._emit_anomaly(alert)

            return alerts

    def _emit_anomaly(self, alert: AnomalyAlert) -> Any:
        """Emit ANOMALY_DETECTED to Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            bus = get_bus()
            bus.emit(ResonanceEvent(
                event_type=EventType.ANOMALY_DETECTED,
                source="harmony_anomaly_detector",
                data=alert.to_dict(),
            ))
        except Exception:
            pass

    def check(self) -> list[dict[str, Any]]:
        """Check current state — returns any active alerts from last observation."""
        with self._lock:
            # Re-check last values
            alerts = []
            for dim in HARMONY_DIMENSIONS:
                window = self._windows[dim]
                if len(window) < 10:
                    continue

                value = window[-1]
                mean = sum(window) / len(window)
                variance = sum((x - mean) ** 2 for x in window) / len(window)
                std_dev = math.sqrt(variance) if variance > 0 else 0.0

                if std_dev < 1e-10:
                    continue

                z_score = (value - mean) / std_dev
                abs_z = abs(z_score)

                if abs_z >= self._warning_z:
                    alerts.append({
                        "dimension": dim,
                        "z_score": round(z_score, 2),
                        "current": round(value, 4),
                        "mean": round(mean, 4),
                        "severity": "critical" if abs_z >= self._critical_z else "warning",
                    })

            return alerts

    def recent_alerts(self, limit: int = 20) -> list[dict[str, Any]]:
        """Get recent alerts."""
        with self._lock:
            return [a.to_dict() for a in list(self._alerts)[-limit:]]

    def status(self) -> dict[str, Any]:
        """Get detector status."""
        with self._lock:
            dim_stats = {}
            for dim in HARMONY_DIMENSIONS:
                window = self._windows[dim]
                if window:
                    values = list(window)
                    dim_stats[dim] = {
                        "observations": len(values),
                        "current": round(values[-1], 4) if values else None,
                        "mean": round(sum(values) / len(values), 4),
                        "min": round(min(values), 4),
                        "max": round(max(values), 4),
                    }

            return {
                "window_size": self._window_size,
                "warning_threshold": self._warning_z,
                "critical_threshold": self._critical_z,
                "total_ingested": self._total_ingested,
                "total_alerts": self._total_alerts,
                "dimensions": dim_stats,
                "active_alerts": self.check(),
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_detector: AnomalyDetector | None = None
_det_lock = threading.Lock()


def get_anomaly_detector() -> AnomalyDetector:
    global _detector
    if _detector is None:
        with _det_lock:
            if _detector is None:
                _detector = AnomalyDetector()
    return _detector
