
from typing import Any


def run_benchmarks(
    suite: str = "full",
    benchmark_type: str | None = None,
    mode: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Run benchmark suites."""
    from whitemagic.benchmarks.performance_suite import PerformanceBenchmark

    # Map benchmark_type to suite for compatibility
    if benchmark_type:
        suite = benchmark_type
    elif mode:
        suite = mode

    benchmark = PerformanceBenchmark()
    if suite == "full":
        results = benchmark.run_full_benchmark()
    else:
        results = benchmark.benchmark_import_time()
    return {"status": "completed", "suite": suite, "benchmark_type": suite, "results": results}
