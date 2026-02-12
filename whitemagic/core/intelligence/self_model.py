"""Self-Model / Predictive Introspection.
=======================================
Inspired by CyberBrains' Self-Model / Introspection concept:

  A self-supervised module that predicts its own next internal states
  (confidence, errors). Feeds back into executive to calibrate
  uncertainty and risk.

This module extends Gnosis from a read-only snapshot into a *predictive*
system that can forecast trends and warn about upcoming issues.

It tracks rolling windows of key system metrics and uses simple linear
regression to project future values, generating alerts when thresholds
are likely to be crossed.

Tracked signals:
  - Harmony Vector energy
  - Karma debt
  - Error rate
  - Memory galactic zone distribution shifts
  - Drive composite (from Emotion & Drive Core)
  - Circuit breaker trip frequency

Usage:
    from whitemagic.core.intelligence.self_model import (
        get_self_model, SelfModel, Forecast
    )

    model = get_self_model()
    model.record("energy", 0.72)
    model.record("error_rate", 0.05)
    forecasts = model.forecast_all()
"""

import logging
import threading
import time
from collections import deque
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data Structures
# ---------------------------------------------------------------------------

@dataclass
class Forecast:
    """A single metric forecast."""

    metric: str
    current: float
    predicted: float          # Predicted value N steps ahead
    trend: str                # "rising", "falling", "stable"
    slope: float              # Rate of change per step
    confidence: float         # How reliable the forecast is (0-1)
    steps_ahead: int          # How many steps into the future
    alert: str | None      # Warning message if threshold likely crossed
    threshold_eta: int | None  # Steps until threshold is crossed (None if safe)

    def to_dict(self) -> dict[str, Any]:
        return {
            "metric": self.metric,
            "current": round(self.current, 4),
            "predicted": round(self.predicted, 4),
            "trend": self.trend,
            "slope": round(self.slope, 6),
            "confidence": round(self.confidence, 4),
            "steps_ahead": self.steps_ahead,
            "alert": self.alert,
            "threshold_eta": self.threshold_eta,
        }


# Metric definitions: name -> (warn_low, warn_high, critical_low, critical_high)
_METRIC_THRESHOLDS: dict[str, tuple[float | None, float | None,
                                     float | None, float | None]] = {
    "energy":        (0.3, None, 0.15, None),
    "karma_debt":    (None, 0.5, None, 0.8),
    "error_rate":    (None, 0.15, None, 0.30),
    "dharma_score":  (0.5, None, 0.3, None),
    "drive_composite": (0.25, None, 0.15, None),
    "breaker_trips": (None, 3.0, None, 5.0),
    "latency":       (None, 0.5, None, 1.0),
    "throughput":     (0.3, None, 0.1, None),
    "balance":       (0.3, None, 0.15, None),
}

_DEFAULT_WINDOW = 100
_DEFAULT_FORECAST_STEPS = 10


# ---------------------------------------------------------------------------
# SelfModel
# ---------------------------------------------------------------------------

class SelfModel:
    """Predictive introspection engine.

    Maintains rolling windows of system metrics and uses simple linear
    regression to forecast trends and generate early warnings.
    """

    def __init__(self, window_size: int = _DEFAULT_WINDOW,
                 forecast_steps: int = _DEFAULT_FORECAST_STEPS):
        self._lock = threading.Lock()
        self._window_size = window_size
        self._forecast_steps = forecast_steps
        self._series: dict[str, deque[tuple[float, float]]] = {}  # metric -> deque of (time, value)
        self._record_count = 0

    # ------------------------------------------------------------------
    # Recording
    # ------------------------------------------------------------------

    def record(self, metric: str, value: float) -> None:
        """Record a metric observation."""
        with self._lock:
            if metric not in self._series:
                self._series[metric] = deque(maxlen=self._window_size)
            self._series[metric].append((time.time(), float(value)))
            self._record_count += 1

    def record_batch(self, observations: dict[str, float]) -> None:
        """Record multiple metrics at once."""
        for metric, value in observations.items():
            self.record(metric, value)

    # ------------------------------------------------------------------
    # Forecasting
    # ------------------------------------------------------------------

    def forecast(self, metric: str, steps_ahead: int | None = None) -> Forecast | None:
        """Forecast a single metric."""
        steps = steps_ahead or self._forecast_steps
        with self._lock:
            series = self._series.get(metric)
            if not series or len(series) < 3:
                return None

            values = [v for _, v in series]
            current = values[-1]

            # Simple linear regression on index
            n = len(values)
            x_mean = (n - 1) / 2.0
            y_mean = sum(values) / n
            numerator = sum((i - x_mean) * (v - y_mean) for i, v in enumerate(values))
            denominator = sum((i - x_mean) ** 2 for i in range(n))

            if denominator == 0:
                slope = 0.0
            else:
                slope = numerator / denominator

            predicted = current + slope * steps

            # Confidence based on R² and sample size
            ss_res = sum((v - (y_mean + slope * (i - x_mean))) ** 2
                         for i, v in enumerate(values))
            ss_tot = sum((v - y_mean) ** 2 for v in values)
            r_squared = 1.0 - (ss_res / ss_tot) if ss_tot > 0 else 0.0
            size_factor = min(1.0, n / self._window_size)
            confidence = max(0.0, min(1.0, r_squared * 0.7 + size_factor * 0.3))

            # Trend
            if abs(slope) < 0.001:
                trend = "stable"
            elif slope > 0:
                trend = "rising"
            else:
                trend = "falling"

            # Threshold alerts
            alert = None
            threshold_eta = None
            thresholds = _METRIC_THRESHOLDS.get(metric)
            if thresholds and slope != 0:
                warn_low, warn_high, crit_low, crit_high = thresholds

                # Check critical thresholds first
                if crit_low is not None and slope < 0:
                    steps_to_crit = (crit_low - current) / slope
                    if 0 < steps_to_crit <= steps * 2:
                        threshold_eta = int(steps_to_crit)
                        alert = (f"{metric} predicted to hit critical low "
                                 f"({crit_low}) in ~{threshold_eta} steps")

                if crit_high is not None and slope > 0:
                    steps_to_crit = (crit_high - current) / slope
                    if 0 < steps_to_crit <= steps * 2:
                        threshold_eta = int(steps_to_crit)
                        alert = (f"{metric} predicted to hit critical high "
                                 f"({crit_high}) in ~{threshold_eta} steps")

                # Then warning thresholds
                if alert is None:
                    if warn_low is not None and slope < 0:
                        steps_to_warn = (warn_low - current) / slope
                        if 0 < steps_to_warn <= steps * 2:
                            threshold_eta = int(steps_to_warn)
                            alert = (f"{metric} trending toward warning low "
                                     f"({warn_low}) in ~{threshold_eta} steps")

                    if warn_high is not None and slope > 0:
                        steps_to_warn = (warn_high - current) / slope
                        if 0 < steps_to_warn <= steps * 2:
                            threshold_eta = int(steps_to_warn)
                            alert = (f"{metric} trending toward warning high "
                                     f"({warn_high}) in ~{threshold_eta} steps")

            return Forecast(
                metric=metric,
                current=current,
                predicted=predicted,
                trend=trend,
                slope=slope,
                confidence=confidence,
                steps_ahead=steps,
                alert=alert,
                threshold_eta=threshold_eta,
            )

    def forecast_all(self, steps_ahead: int | None = None) -> dict[str, Forecast]:
        """Forecast all tracked metrics. Uses Julia Holt-Winters when available."""
        steps = steps_ahead or self._forecast_steps

        # Try Julia batch forecasting first (Holt-Winters with confidence intervals)
        try:
            from whitemagic.core.acceleration.julia_bridge import julia_batch_forecast
            metrics_data = {}
            with self._lock:
                for metric, series in self._series.items():
                    if len(series) >= 3:
                        metrics_data[metric] = [v for _, v in series]

            if metrics_data:
                julia_result = julia_batch_forecast(metrics_data, steps=steps)
                if julia_result and "forecasts" in julia_result:
                    results = {}
                    for metric, fc in julia_result["forecasts"].items():
                        values = metrics_data.get(metric, [])
                        current = values[-1] if values else 0.0
                        predicted = fc.get("forecasts", [current])[-1] if fc.get("forecasts") else current
                        trend_dir = fc.get("trend_direction", "stable")
                        slope = fc.get("trend", 0.0)
                        results[metric] = Forecast(
                            metric=metric,
                            current=current,
                            predicted=predicted,
                            trend="rising" if trend_dir == "increasing" else ("falling" if trend_dir == "decreasing" else "stable"),
                            slope=slope,
                            confidence=min(1.0, 1.0 - fc.get("residual_std", 0.5)),
                            steps_ahead=steps,
                            alert=None,
                            threshold_eta=None,
                        )
                    if results:
                        return results
        except Exception:
            pass  # Julia unavailable, fall through to Python

        results = {}
        with self._lock:
            metrics = list(self._series.keys())
        for metric in metrics:
            f = self.forecast(metric, steps_ahead)
            if f:
                results[metric] = f
        return results

    def get_alerts(self) -> list[Forecast]:
        """Return only forecasts that have active alerts."""
        all_forecasts = self.forecast_all()
        return [f for f in all_forecasts.values() if f.alert is not None]

    # ------------------------------------------------------------------
    # Gnosis integration
    # ------------------------------------------------------------------

    def gnosis_portal(self) -> dict[str, Any]:
        """Return a Gnosis-compatible introspection dict."""
        forecasts = self.forecast_all()
        alerts = [f for f in forecasts.values() if f.alert]

        return {
            "tracked_metrics": len(forecasts),
            "total_observations": self._record_count,
            "alerts": [a.to_dict() for a in alerts],
            "alert_count": len(alerts),
            "forecasts": {k: v.to_dict() for k, v in forecasts.items()},
        }

    def get_stats(self) -> dict[str, Any]:
        """Return statistics for introspection."""
        with self._lock:
            return {
                "tracked_metrics": list(self._series.keys()),
                "metric_count": len(self._series),
                "total_observations": self._record_count,
                "window_size": self._window_size,
                "forecast_steps": self._forecast_steps,
                "series_lengths": {k: len(v) for k, v in self._series.items()},
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_instance: SelfModel | None = None
_instance_lock = threading.Lock()


def get_self_model() -> SelfModel:
    """Get or create the global SelfModel singleton."""
    global _instance
    with _instance_lock:
        if _instance is None:
            _instance = SelfModel()
        return _instance
