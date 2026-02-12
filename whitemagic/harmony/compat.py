"""Harmony compatibility facade — delegates to live harmony subsystems.

This module provides legacy ``BalanceMonitor`` APIs for older code paths while
internally using HarmonyVector and HomeostaticLoop.
"""

from __future__ import annotations

import logging
from typing import Any

logger = logging.getLogger(__name__)


class BalanceMonitor:
    """Read live balance from HarmonyVector + HomeostaticLoop."""

    def __init__(self) -> None:
        self._hv = None
        self._loop = None

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

    def get_balance(self) -> float:
        """Return the composite harmony score (0.0-1.0)."""
        hv = self._get_harmony_vector()
        if hv is None:
            return 1.0
        try:
            return float(hv.snapshot().harmony_score)
        except Exception:
            return 1.0

    def get_status(self) -> dict[str, Any]:
        """Return full system status including all 7 harmony dimensions."""
        hv = self._get_harmony_vector()
        loop = self._get_loop()

        result: dict[str, Any] = {"balance": self.get_balance()}

        if hv is not None:
            try:
                snap = hv.snapshot()
                result.update(
                    {
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
                    }
                )
            except Exception as exc:
                result["error"] = str(exc)

        if loop is not None:
            try:
                result["homeostatic_loop"] = loop.get_stats()
            except Exception:
                pass

        return result

    def get_snapshot(self) -> dict[str, Any]:
        """Return raw HarmonySnapshot as JSON-serializable dict."""
        hv = self._get_harmony_vector()
        if hv is None:
            return {}
        try:
            return dict(hv.snapshot().to_dict())
        except Exception:
            return {}


_balance_monitor = BalanceMonitor()


def get_balance_monitor() -> BalanceMonitor:
    """Get the global balance monitor."""
    return _balance_monitor


__all__ = ["BalanceMonitor", "get_balance_monitor"]
