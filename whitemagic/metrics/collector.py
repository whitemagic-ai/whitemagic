"""
Simple metrics collector for automation systems.
Placeholder implementation to resolve import errors.
"""

from __future__ import annotations

from datetime import datetime
from typing import Any


class MetricsCollector:
    """Simple metrics collection system."""

    def __init__(self) -> None:
        self.metrics: dict[str, Any] = {}

    def increment(self, metric_name: str, value: int = 1) -> None:
        """Increment a metric."""
        self.metrics[metric_name] = self.metrics.get(metric_name, 0) + value

    def gauge(self, metric_name: str, value: float) -> None:
        """Set a gauge value."""
        self.metrics[metric_name] = value

    def timing(self, metric_name: str, duration_ms: float) -> None:
        """Record a timing."""
        self.metrics[f"{metric_name}_ms"] = duration_ms

    def get_metrics(self) -> dict[str, Any]:
        """Get all collected metrics."""
        return {
            "metrics": self.metrics,
            "timestamp": datetime.now().isoformat()
        }

    def get_summary(self, categories: list[str] | None = None) -> dict[str, Any]:
        """Get metrics summary, optionally filtered by categories."""
        if categories:
            filtered = {k: v for k, v in self.metrics.items()
                       if any(cat in k for cat in categories)}
            return {
                "metrics": filtered,
                "categories": categories,
                "timestamp": datetime.now().isoformat()
            }
        return self.get_metrics()
