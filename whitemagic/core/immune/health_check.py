#!/usr/bin/env python3
"""🏥 Health Check System
Immune system component for monitoring system health.
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class HealthStatus(Enum):
    HEALTHY = "healthy"
    DEGRADED = "degraded"
    CRITICAL = "critical"
    UNKNOWN = "unknown"


@dataclass
class HealthMetric:
    """A single health metric."""

    name: str
    value: float
    threshold_warning: float
    threshold_critical: float
    unit: str = ""
    timestamp: datetime = field(default_factory=datetime.now)

    @property
    def status(self) -> HealthStatus:
        if self.value >= self.threshold_critical:
            return HealthStatus.CRITICAL
        elif self.value >= self.threshold_warning:
            return HealthStatus.DEGRADED
        return HealthStatus.HEALTHY


@dataclass
class HealthCheck:
    """System health check - runs diagnostics across all systems.
    """

    name: str = "WhiteMagic Health Check"
    metrics: dict[str, HealthMetric] = field(default_factory=dict)
    last_check: datetime | None = None

    def add_metric(self, name: str, value: float,
                   warning: float = 0.7, critical: float = 0.9,
                   unit: str = "") -> "HealthCheck":
        """Add a health metric."""
        self.metrics[name] = HealthMetric(
            name=name,
            value=value,
            threshold_warning=warning,
            threshold_critical=critical,
            unit=unit,
        )
        return self

    def check_all(self) -> dict[str, Any]:
        """Run all health checks."""
        self.last_check = datetime.now()

        metrics: dict[str, dict[str, Any]] = {}
        issues: list[str] = []
        overall_status = HealthStatus.HEALTHY

        for name, metric in self.metrics.items():
            status = metric.status
            metrics[name] = {
                "value": metric.value,
                "status": status.value,
                "unit": metric.unit,
            }

            if status == HealthStatus.CRITICAL:
                overall_status = HealthStatus.CRITICAL
                issues.append(f"CRITICAL: {name} = {metric.value}")
            elif status == HealthStatus.DEGRADED:
                if overall_status != HealthStatus.CRITICAL:
                    overall_status = HealthStatus.DEGRADED
                issues.append(f"WARNING: {name} = {metric.value}")

        return {
            "timestamp": self.last_check.isoformat(),
            "overall_status": overall_status.value,
            "metrics": metrics,
            "issues": issues,
        }

    def quick_check(self) -> bool:
        """Quick health check - returns True if healthy."""
        result = self.check_all()
        status = result.get("overall_status")
        return isinstance(status, str) and status == "healthy"


# Pre-configured health checks
def create_system_health_check() -> HealthCheck:
    """Create a standard system health check."""
    hc = HealthCheck(name="System Health")

    # Default metrics (will be updated with real values)
    hc.add_metric("memory_usage", 0.3, warning=0.7, critical=0.9, unit="%")
    hc.add_metric("error_rate", 0.01, warning=0.05, critical=0.1, unit="%")
    hc.add_metric("response_time", 0.1, warning=0.5, critical=1.0, unit="s")
    hc.add_metric("pattern_coherence", 0.9, warning=0.5, critical=0.3, unit="")

    return hc


# Singleton
_health_check: HealthCheck | None = None

def get_health_check() -> HealthCheck:
    global _health_check
    if _health_check is None:
        _health_check = create_system_health_check()
    return _health_check
