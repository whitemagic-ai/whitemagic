"""Prometheus metrics collection for performance monitoring.

Tracks:
- Request rates
- Response times
- Rust vs Python usage
- Error rates
- Resource utilization
"""

import time
from collections.abc import Awaitable, Callable
from functools import wraps
from importlib.util import find_spec
from typing import Any, TypeVar, cast

from prometheus_client import Counter, Gauge, Histogram, Info

# Request metrics
requests_total = Counter(
    "whitemagic_requests_total",
    "Total HTTP requests",
    ["method", "endpoint", "status"],
)

request_duration = Histogram(
    "whitemagic_request_duration_seconds",
    "HTTP request duration",
    ["method", "endpoint"],
    buckets=[0.01, 0.05, 0.1, 0.5, 1.0, 2.0, 5.0, 10.0],
)

# Rust performance metrics
rust_operations = Counter(
    "whitemagic_rust_operations_total",
    "Total Rust operations",
    ["operation", "status"],
)

rust_speedup = Histogram(
    "whitemagic_rust_speedup_ratio",
    "Rust vs Python speedup ratio",
    ["operation"],
    buckets=[1, 2, 5, 10, 20, 50, 100],
)

# Memory system metrics
memory_operations = Counter(
    "whitemagic_memory_operations_total",
    "Memory system operations",
    ["operation", "engine"],
)

consolidation_duration = Histogram(
    "whitemagic_consolidation_duration_seconds",
    "Memory consolidation duration",
    ["engine"],
    buckets=[0.1, 0.5, 1.0, 5.0, 10.0, 30.0, 60.0, 120.0],
)

# System health metrics
system_health = Gauge(
    "whitemagic_system_health",
    "System health status (1=healthy, 0=unhealthy)",
)

rust_available = Gauge(
    "whitemagic_rust_available",
    "Rust module availability (1=available, 0=unavailable)",
)

# Active connections
active_connections = Gauge(
    "whitemagic_active_connections",
    "Number of active connections",
)

# File processing metrics
files_processed = Counter(
    "whitemagic_files_processed_total",
    "Total files processed",
    ["engine"],
)

processing_rate = Gauge(
    "whitemagic_processing_rate_files_per_second",
    "Current file processing rate",
    ["engine"],
)

# Info metrics
app_info = Info(
    "whitemagic_app",
    "WhiteMagic application info",
)

F = TypeVar("F", bound=Callable[..., Awaitable[Any]])

def track_request_metrics(func: F) -> F:
    """Decorator to track request metrics."""
    @wraps(func)
    async def wrapper(*args: Any, **kwargs: Any) -> Any:
        start_time = time.time()
        status = 200

        try:
            result = await func(*args, **kwargs)
            return result
        except Exception:
            status = 500
            raise
        finally:
            duration = time.time() - start_time

            # Record metrics
            requests_total.labels(
                method="POST",
                endpoint=func.__name__,
                status=status,
            ).inc()

            request_duration.labels(
                method="POST",
                endpoint=func.__name__,
            ).observe(duration)

    return cast("F", wrapper)


def track_rust_operation(
    operation: str,
    duration: float,
    python_duration: float | None = None,
) -> None:
    """Track Rust operation metrics."""
    rust_operations.labels(
        operation=operation,
        status="success",
    ).inc()

    if python_duration and python_duration > 0:
        speedup = python_duration / duration if duration > 0 else 1
        rust_speedup.labels(operation=operation).observe(speedup)


def initialize_metrics() -> None:
    """Initialize metrics with default values."""
    system_health.set(1)

    # Check Rust availability
    has_rust = find_spec("whitemagic_rs") is not None
    rust_available.set(1 if has_rust else 0)

    # Set app info
    app_info.info({
        "version": "3.0.0",
        "rust_enabled": str(has_rust),
        "multi_language": "true",
    })
