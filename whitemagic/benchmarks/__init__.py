"""Performance benchmark suite for WhiteMagic subsystems.

Covers import latency, tool dispatch overhead, and state footprint.
All benchmarks are read-only and deterministic.
"""

from .performance_suite import BenchmarkResult, PerformanceBenchmark

__all__ = ["BenchmarkResult", "PerformanceBenchmark"]
