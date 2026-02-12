"""Lightweight performance benchmarks for MCP tooling.

Restored from _archived/benchmarks/ and modernized for v13.
No external dependencies — uses only stdlib.
"""
from __future__ import annotations

import importlib
import time
from collections.abc import Callable, Iterable
from dataclasses import dataclass, field
from typing import Any

from whitemagic.config.paths import WM_ROOT

DEFAULT_MODULES = [
    "whitemagic",
    "whitemagic.core.resonance.gan_ying",
    "whitemagic.core.memory.manager",
    "whitemagic.tools.unified_api",
    "whitemagic.tools.dispatch_table",
]


@dataclass
class BenchmarkResult:
    """Single benchmark measurement."""

    label: str
    duration_ms: float
    metadata: dict[str, Any] = field(default_factory=dict)


class PerformanceBenchmark:
    """Performance benchmark suite for WhiteMagic subsystems.

    Covers import latency, tool dispatch overhead, and memory/state footprint.
    All benchmarks are read-only and deterministic (no mutations).
    """

    # ------------------------------------------------------------------
    # Import benchmarks
    # ------------------------------------------------------------------

    def _time_imports(self, modules: Iterable[str]) -> list[BenchmarkResult]:
        results: list[BenchmarkResult] = []
        for module in modules:
            start = time.perf_counter()
            try:
                importlib.import_module(module)
                duration = (time.perf_counter() - start) * 1000
                results.append(BenchmarkResult(label=module, duration_ms=duration))
            except ImportError:
                results.append(
                    BenchmarkResult(
                        label=module,
                        duration_ms=-1,
                        metadata={"error": "ImportError"},
                    ),
                )
        return results

    def benchmark_import_time(
        self, modules: list[str] | None = None,
    ) -> dict[str, float]:
        """Measure import latency (ms) for common modules."""
        modules = modules or list(DEFAULT_MODULES)
        results = self._time_imports(modules)
        return {r.label: round(r.duration_ms, 3) for r in results}

    # ------------------------------------------------------------------
    # Tool dispatch benchmarks
    # ------------------------------------------------------------------

    def benchmark_tool_dispatch(
        self,
        tool_name: str = "health_report",
        iterations: int = 5,
    ) -> dict[str, Any]:
        """Measure average tool dispatch latency (ms)."""
        try:
            from whitemagic.tools.dispatch_table import dispatch
        except ImportError:
            return {"error": "dispatch_table not available"}

        times: list[float] = []
        for _ in range(iterations):
            start = time.perf_counter()
            dispatch(tool_name)
            times.append((time.perf_counter() - start) * 1000)

        return {
            "tool": tool_name,
            "iterations": iterations,
            "mean_ms": round(sum(times) / len(times), 3) if times else 0,
            "min_ms": round(min(times), 3) if times else 0,
            "max_ms": round(max(times), 3) if times else 0,
        }

    # ------------------------------------------------------------------
    # Custom benchmark runner
    # ------------------------------------------------------------------

    def time_callable(
        self,
        fn: Callable[..., Any],
        *args: Any,
        iterations: int = 1,
        label: str = "",
        **kwargs: Any,
    ) -> BenchmarkResult:
        """Time an arbitrary callable."""
        total = 0.0
        for _ in range(iterations):
            start = time.perf_counter()
            fn(*args, **kwargs)
            total += (time.perf_counter() - start) * 1000
        avg = total / iterations if iterations else 0
        return BenchmarkResult(
            label=label or getattr(fn, "__name__", "callable"),
            duration_ms=round(avg, 3),
            metadata={"iterations": iterations},
        )

    # ------------------------------------------------------------------
    # State / footprint
    # ------------------------------------------------------------------

    def _memory_stats(self) -> dict[str, int]:
        """Return basic state-directory stats without mutating state."""
        state_dir = WM_ROOT
        if not state_dir.exists():
            return {"state_dir_exists": False}
        memory_dir = state_dir / "memory"
        return {
            "state_dir_exists": True,
            "db_size_bytes": (
                (memory_dir / "whitemagic.db").stat().st_size
                if (memory_dir / "whitemagic.db").exists()
                else 0
            ),
            "log_files": len(list((state_dir / "logs").glob("*")))
            if (state_dir / "logs").exists()
            else 0,
        }

    # ------------------------------------------------------------------
    # Full suite
    # ------------------------------------------------------------------

    def run_full_benchmark(self) -> dict[str, Any]:
        """Run the complete benchmark suite and return results."""
        return {
            "imports_ms": self.benchmark_import_time(),
            "tool_dispatch": self.benchmark_tool_dispatch(),
            "state_footprint": self._memory_stats(),
        }
