"""Homeostasis compatibility facade — delegates to the real subsystems.

This module provides backward-compatible ``HomeostaticMonitor`` and
``Homeostasis`` names used by the CLI (``cli_app.py``, ``doctor.py``)
and ``core/immune/defense/``.

Under the hood it delegates to:
- :mod:`whitemagic.harmony.vector` — 7-dimension Harmony Vector
- :mod:`whitemagic.harmony.homeostatic_loop` — graduated corrective loop
"""

from __future__ import annotations

import logging
from typing import Any, Dict

logger = logging.getLogger(__name__)


class HomeostaticMonitor:
    """Facade that reads live health from HarmonyVector + HomeostaticLoop."""

    def __init__(self) -> None:
        self._hv = None
        self._loop = None

    # Lazy accessors so imports don't fail at module load time
    def _get_harmony_vector(self) -> Any:
        if self._hv is None:
            try:
                from whitemagic.harmony.vector import get_harmony_vector
                self._hv = get_harmony_vector()  # type: ignore[assignment]
            except Exception:
                pass
        return self._hv

    def _get_loop(self) -> Any:
        if self._loop is None:
            try:
                from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
                self._loop = get_homeostatic_loop()  # type: ignore[assignment]
            except Exception:
                pass
        return self._loop

    def monitor_initialization(self) -> bool:
        """Initialize monitoring — attaches the homeostatic loop."""
        loop = self._get_loop()
        if loop is not None:
            loop.attach()
            return True
        return False

    def check_status(self) -> Dict[str, Any]:
        """Return system status from the real Harmony Vector."""
        hv = self._get_harmony_vector()
        loop = self._get_loop()

        if hv is None:
            return {"status": "unavailable", "metrics": {}, "alerts": []}

        try:
            snap = hv.snapshot()
        except Exception as exc:
            logger.debug("Harmony snapshot failed: %s", exc)
            return {"status": "error", "metrics": {}, "alerts": [str(exc)]}

        # Map 7 harmony dimensions to the legacy metrics dict
        metrics: Dict[str, Any] = {
            "balance": snap.balance,
            "throughput": snap.throughput,
            "latency": snap.latency,
            "error_rate": snap.error_rate,
            "dharma": snap.dharma,
            "karma_debt": snap.karma_debt,
            "energy": snap.energy,
            "harmony_score": snap.harmony_score,
        }

        # Derive alerts from dimensions below threshold (< 0.5)
        alerts = []
        for dim in ("error_rate", "dharma", "karma_debt", "energy", "latency"):
            val = getattr(snap, dim, 1.0)
            if val < 0.5:
                alerts.append(f"Low {dim}: {val:.2f}")

        # Determine status string
        if snap.harmony_score >= 0.7:
            status = "balanced"
        elif snap.harmony_score >= 0.4:
            status = "stressed"
        else:
            status = "imbalanced"

        result: Dict[str, Any] = {
            "status": status,
            "metrics": metrics,
            "alerts": alerts,
        }

        # Attach loop stats if available
        if loop is not None:
            try:
                result["homeostatic_loop"] = loop.get_stats()
            except Exception:
                pass

        return result

    def record_metric(self, metric: str, value: float) -> None:
        """Record a metric — delegates to HarmonyVector where possible."""
        hv = self._get_harmony_vector()
        if hv is not None:
            try:
                hv.record_call(tool_name=f"metric.{metric}", success=(value <= 1.0))
            except Exception:
                pass

    def set_threshold(self, metric: str, threshold: float) -> None:
        """Set alert threshold (no-op — thresholds live in HomeostaticConfig)."""
        logger.debug(
            "set_threshold(%s, %s) is a no-op; configure HomeostaticConfig instead.",
            metric, threshold,
        )

    def get_balance(self) -> float:
        """Return the composite harmony score (0.0–1.0)."""
        hv = self._get_harmony_vector()
        if hv is None:
            return 1.0  # optimistic default when subsystem unavailable
        try:
            return float(hv.snapshot().harmony_score)
        except Exception:
            return 1.0


class Homeostasis(HomeostaticMonitor):
    """Alias kept for backward compatibility."""
    pass


# Global instance
_homeostasis = HomeostaticMonitor()


def get_homeostasis() -> HomeostaticMonitor:
    """Get the global homeostasis monitor."""
    return _homeostasis


__all__ = ["HomeostaticMonitor", "Homeostasis", "get_homeostasis"]
