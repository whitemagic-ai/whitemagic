# mypy: disable-error-code=no-untyped-def
"""Resource Limiter - Prevent System Overload

Protects against runaway resource usage in benchmarks and automation.
Learned from: Jan 6, 2026 crash (1000 thread spawn attempt).

v13 rewrite: stdlib-only (no psutil dependency). Uses os, resource, threading.
"""
from __future__ import annotations

import logging
import os
import resource
import threading
from collections.abc import Callable
from contextlib import contextmanager
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class ResourceLimits:
    """System resource limits (all auto-detected if left as None)."""

    max_threads: int | None = None
    max_memory_mb: int | None = None
    max_cpu_percent: float = 80.0
    check_interval_sec: float = 0.5


def _total_ram_mb() -> int:
    """Best-effort total RAM in MB using stdlib only."""
    try:
        pages = os.sysconf("SC_PHYS_PAGES")
        page_size = os.sysconf("SC_PAGE_SIZE")
        return int((pages * page_size) / (1024 * 1024))
    except (ValueError, OSError):
        return 4096  # fallback 4 GB


def _current_rss_mb() -> float:
    """Current process RSS in MB via /proc or resource module."""
    try:
        with open("/proc/self/status") as f:
            for line in f:
                if line.startswith("VmRSS:"):
                    return int(line.split()[1]) / 1024  # kB -> MB
    except (FileNotFoundError, OSError):
        pass
    # Fallback: resource module (maxrss is in KB on Linux, bytes on macOS)
    ru = resource.getrusage(resource.RUSAGE_SELF)
    return ru.ru_maxrss / 1024  # assume Linux (KB)


def _thread_count() -> int:
    """Current thread count."""
    return threading.active_count()


class ResourceLimiter:
    """Monitor and enforce resource limits.

    Prevents system crashes from runaway benchmarks or automation.
    Uses only Python stdlib — no psutil required.
    """

    def __init__(self, limits: ResourceLimits | None = None) -> None:
        if limits is None:
            limits = ResourceLimits()

        cpu_count = os.cpu_count() or 4
        if limits.max_threads is None:
            limits.max_threads = cpu_count * 2
        if limits.max_memory_mb is None:
            limits.max_memory_mb = int(_total_ram_mb() * 0.5)

        self.limits = limits
        self._monitoring = False
        self._monitor_thread: threading.Thread | None = None
        self._violation_callback: Callable[[str], None] | None = None

    def check_safe(self) -> tuple[bool, str]:
        """Check if current resource usage is safe.

        Returns:
            (is_safe, reason)

        """
        tc = _thread_count()
        if self.limits.max_threads and tc > self.limits.max_threads:
            return False, f"Thread count {tc} exceeds limit {self.limits.max_threads}"

        rss = _current_rss_mb()
        if self.limits.max_memory_mb and rss > self.limits.max_memory_mb:
            return False, f"Memory {rss:.0f}MB exceeds limit {self.limits.max_memory_mb}MB"

        return True, "OK"

    def get_safe_thread_count(self, requested: int) -> int:
        """Clamp *requested* thread count to a safe maximum."""
        return min(requested, self.limits.max_threads or requested)

    def start_monitoring(
        self, violation_callback: Callable[[str], None] | None = None,
    ) -> None:
        """Start background resource monitoring."""
        if self._monitoring:
            return
        self._monitoring = True
        self._violation_callback = violation_callback

        def _monitor() -> None:
            stop = threading.Event()
            while self._monitoring:
                is_safe, reason = self.check_safe()
                if not is_safe and self._violation_callback:
                    self._violation_callback(reason)
                stop.wait(self.limits.check_interval_sec)

        self._monitor_thread = threading.Thread(target=_monitor, daemon=True)
        self._monitor_thread.start()

    def stop_monitoring(self) -> None:
        """Stop background monitoring."""
        self._monitoring = False
        if self._monitor_thread:
            self._monitor_thread.join(timeout=2.0)


@contextmanager
def resource_guard(limits: ResourceLimits | None = None):
    """Context manager for safe resource usage.

    Usage::

        with resource_guard() as limiter:
            safe_threads = limiter.get_safe_thread_count(1000)
            with ThreadPoolExecutor(max_workers=safe_threads):
                ...
    """
    limiter = ResourceLimiter(limits)
    limiter.start_monitoring(
        violation_callback=lambda reason: logger.warning(
            "Resource limit: %s", reason,
        ),
    )
    try:
        yield limiter
    finally:
        limiter.stop_monitoring()


def get_safe_limits() -> dict[str, Any]:
    """Get recommended safe limits for this system (stdlib-only)."""
    cpu_count = os.cpu_count() or 4
    total_mb = _total_ram_mb()
    return {
        "cpu_cores": cpu_count,
        "max_safe_threads": cpu_count * 2,
        "total_ram_mb": total_mb,
        "max_safe_ram_mb": int(total_mb * 0.5),
        "recommendation": (
            f"Use max {cpu_count * 2} threads, "
            f"keep under {int(total_mb * 0.5)}MB RAM"
        ),
    }
