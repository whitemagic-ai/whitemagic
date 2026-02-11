"""Harmony Metrics compatibility facade — delegates to the real subsystems.

Provides the legacy ``BalanceMonitor`` interface used by older code paths.
Under the hood it reads from:

- :class:`whitemagic.harmony.vector.HarmonyVector` — 7-dimension health
- :class:`whitemagic.harmony.homeostatic_loop.HomeostaticLoop` — corrective loop
- :class:`whitemagic.homeostasis.HomeostaticMonitor` — compat facade

For new code, prefer importing directly from :mod:`whitemagic.harmony.vector`.
"""

from __future__ import annotations

import logging
from typing import Any, Dict

logger = logging.getLogger(__name__)


class BalanceMonitor:
    """Reads live balance from the real HarmonyVector + HomeostaticLoop."""

    def __init__(self) -> None:
        self._hv = None
        self._loop = None

    def _get_harmony_vector(self) -> Any:
        if self._hv is None:
            try:
                from whitemagic.harmony.vector import get_harmony_vector
                self._hv = get_harmony_vector()
            except Exception:
                pass
        return self._hv

    def _get_loop(self) -> Any:
        if self._loop is None:
            try:
                from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
                self._loop = get_homeostatic_loop()
            except Exception:
                pass
        return self._loop

    def get_balance(self) -> float:
        """Return the composite harmony score (0.0–1.0)."""
        hv = self._get_harmony_vector()
        if hv is None:
            return 1.0
        try:
            return hv.snapshot().harmony_score
        except Exception:
            return 1.0

    def get_status(self) -> Dict[str, Any]:
        """Return full system status including all 7 harmony dimensions."""
        hv = self._get_harmony_vector()
        loop = self._get_loop()

        result: Dict[str, Any] = {"balance": self.get_balance()}

        if hv is not None:
            try:
                snap = hv.snapshot()
                result.update({
                    "harmony_score": snap.harmony_score,
                    "dimensions": {
                        "balance": snap.balance,
                        "throughput": snap.throughput,
                        "latency": snap.latency,
                        "error_rate": snap.error_rate,
                        "dharma": snap.dharma,
                        "karma_debt": snap.karma_debt,
                        "energy": snap.energy,
                    },
                    "guna": snap.guna.value if hasattr(snap, "guna") else "unknown",
                    "calls_in_window": snap.calls_in_window,
                    "errors_in_window": snap.errors_in_window,
                })
            except Exception as exc:
                result["error"] = str(exc)

        if loop is not None:
            try:
                result["homeostatic_loop"] = loop.get_stats()
            except Exception:
                pass

        return result

    def get_snapshot(self) -> Dict[str, Any]:
        """Return the raw HarmonySnapshot as a dict (for JSON serialization)."""
        hv = self._get_harmony_vector()
        if hv is None:
            return {}
        try:
            return hv.snapshot().to_dict()
        except Exception:
            return {}


_balance_monitor = BalanceMonitor()


def get_balance_monitor() -> BalanceMonitor:
    """Get the global balance monitor."""
    return _balance_monitor


__all__ = ["BalanceMonitor", "get_balance_monitor"]
