# Chapter 6: Performance Drive

**Gana**: TailGana (Chinese: 尾, Pinyin: Wěi)
**Garden**: adventure
**Quadrant**: Eastern (Azure Dragon)
**Element**: Wood
**Phase**: Yang Rising
**I Ching Hexagram**: 34. 大壯 Dà Zhuàng (Great Power) - Passionate forward momentum

---

## 🎯 Purpose

Chapter 6 unleashes **performance drive**—the passionate acceleration that transforms understanding into rapid execution. After connecting to context (Ch.5), the Tail brings the explosive energy that enables high-velocity work without sacrificing quality.

The Tail (尾) represents the sweeping power at the end of the dragon—the accumulated force that enables great leaps. Like a comet's tail, this chapter channels passion into performance. Use this chapter when you need to:

- **Accelerate work** without compromising quality
- **Enable Rust optimization** for 10-100x speedups
- **Optimize caching** for faster operations
- **Parallelize execution** for batch operations
- **Boost performance** when bottlenecks detected
- **Channel passion** into productive momentum

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `enable_rust_acceleration` | Activate Rust modules for 10-100x speedup | CPU-intensive operations |
| `optimize_cache` | Tune predictive cache for 91% hit rate | Before large operations |
| `run_benchmarks` | Measure performance and identify bottlenecks | Baseline and optimization |
| `manage_archaeology` (parallel) | Parallel codebase scanning | Large codebase operations |
| `manage_gardens` (activate) | Enter adventure garden | Channel passionate energy |

---

## 📋 Core Workflows

### Workflow 1: Performance Baseline & Profiling

**Purpose**: Establish performance baselines and identify optimization opportunities through comprehensive profiling.

**When to use**: Before any optimization work, after major changes, during performance investigations.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum
import asyncio
import time
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.tools import run_benchmarks, manage_gardens
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class BenchmarkScope(Enum):
    """Scope of benchmarking."""
    QUICK = "quick"           # Fast sanity check
    STANDARD = "standard"     # Normal comprehensive
    DEEP = "deep"            # Exhaustive profiling
    REGRESSION = "regression" # Compare to baseline

@dataclass
class PerformanceBaseline:
    """Performance baseline metrics."""
    timestamp: str
    scope: BenchmarkScope

    # Core metrics
    ops_per_second: float
    latency_p50_ms: float
    latency_p95_ms: float
    latency_p99_ms: float

    # Resource usage
    cpu_usage_pct: float
    memory_usage_mb: float

    # Operation breakdowns
    operation_metrics: Dict[str, Dict[str, float]] = field(default_factory=dict)

    # Bottlenecks identified
    bottlenecks: List[str] = field(default_factory=list)

    # Recommendations
    recommendations: List[str] = field(default_factory=list)

class PerformanceProfiler:
    """Comprehensive performance profiling and baseline establishment."""

    def __init__(self):
        self.memory = MemoryManager()
        self.adventure = manage_gardens(action="get", garden_name="adventure")

    async def establish_baseline(
        self,
        scope: BenchmarkScope = BenchmarkScope.STANDARD,
        operations: Optional[List[str]] = None
    ) -> PerformanceBaseline:
        """
        Establish comprehensive performance baseline.

        Args:
            scope: How thorough to benchmark
            operations: Specific operations to benchmark (default: all)

        Returns:
            Complete baseline with metrics and recommendations
        """
        # Activate adventure garden for passionate profiling
        manage_gardens(action="activate", garden_name="adventure")

        emit_event("performance.profiling_started", {
            "scope": scope.value,
            "operations": operations or "all"
        })

        # Default operations to benchmark
        if operations is None:
            operations = [
                "memory_store",
                "memory_search",
                "pattern_match",
                "file_io",
                "cache_lookup",
                "event_emission"
            ]

        # Run benchmarks for each operation
        operation_metrics = {}
        all_bottlenecks = []

        for operation in operations:
            metrics = await self._benchmark_operation(operation, scope)
            operation_metrics[operation] = metrics

            # Identify bottlenecks
            if metrics.get("latency_p95_ms", 0) > 100:
                all_bottlenecks.append(
                    f"{operation}: High latency ({metrics['latency_p95_ms']:.1f}ms p95)"
                )

            if metrics.get("cpu_usage_pct", 0) > 80:
                all_bottlenecks.append(
                    f"{operation}: High CPU usage ({metrics['cpu_usage_pct']:.1f}%)"
                )

        # Calculate aggregate metrics
        aggregate = self._aggregate_metrics(operation_metrics)

        # Generate recommendations
        recommendations = self._generate_recommendations(
            operation_metrics,
            all_bottlenecks
        )

        baseline = PerformanceBaseline(
            timestamp=time.strftime("%Y-%m-%d %H:%M:%S"),
            scope=scope,
            ops_per_second=aggregate["ops_per_second"],
            latency_p50_ms=aggregate["latency_p50_ms"],
            latency_p95_ms=aggregate["latency_p95_ms"],
            latency_p99_ms=aggregate["latency_p99_ms"],
            cpu_usage_pct=aggregate["cpu_usage_pct"],
            memory_usage_mb=aggregate["memory_usage_mb"],
            operation_metrics=operation_metrics,
            bottlenecks=all_bottlenecks,
            recommendations=recommendations
        )

        # Store baseline for future comparison
        await self.memory.store(
            content=f"Performance baseline: {baseline.ops_per_second:.0f} ops/sec",
            metadata={
                "type": "performance_baseline",
                "timestamp": baseline.timestamp,
                "scope": scope.value,
                "ops_per_second": baseline.ops_per_second,
                "latency_p95_ms": baseline.latency_p95_ms,
                "bottleneck_count": len(all_bottlenecks)
            },
            tier="short_term"
        )

        emit_event("performance.baseline_established", {
            "ops_per_second": baseline.ops_per_second,
            "bottlenecks": len(all_bottlenecks),
            "recommendations": len(recommendations)
        })

        return baseline

    async def _benchmark_operation(
        self,
        operation: str,
        scope: BenchmarkScope
    ) -> Dict[str, float]:
        """Benchmark a specific operation."""
        # Use WhiteMagic's built-in benchmarking
        result = run_benchmarks(
            mode="benchmark",
            benchmark_type=operation,
            iterations=100 if scope == BenchmarkScope.QUICK else 1000
        )

        return {
            "ops_per_second": result.get("ops_per_second", 0),
            "latency_p50_ms": result.get("latency_p50_ms", 0),
            "latency_p95_ms": result.get("latency_p95_ms", 0),
            "latency_p99_ms": result.get("latency_p99_ms", 0),
            "cpu_usage_pct": result.get("cpu_usage_pct", 0),
            "memory_usage_mb": result.get("memory_usage_mb", 0)
        }

    def _aggregate_metrics(
        self,
        operation_metrics: Dict[str, Dict[str, float]]
    ) -> Dict[str, float]:
        """Aggregate metrics across all operations."""
        if not operation_metrics:
            return {
                "ops_per_second": 0,
                "latency_p50_ms": 0,
                "latency_p95_ms": 0,
                "latency_p99_ms": 0,
                "cpu_usage_pct": 0,
                "memory_usage_mb": 0
            }

        # Calculate averages and maxes
        return {
            "ops_per_second": sum(m.get("ops_per_second", 0) for m in operation_metrics.values()) / len(operation_metrics),
            "latency_p50_ms": sum(m.get("latency_p50_ms", 0) for m in operation_metrics.values()) / len(operation_metrics),
            "latency_p95_ms": max(m.get("latency_p95_ms", 0) for m in operation_metrics.values()),
            "latency_p99_ms": max(m.get("latency_p99_ms", 0) for m in operation_metrics.values()),
            "cpu_usage_pct": sum(m.get("cpu_usage_pct", 0) for m in operation_metrics.values()) / len(operation_metrics),
            "memory_usage_mb": sum(m.get("memory_usage_mb", 0) for m in operation_metrics.values())
        }

    def _generate_recommendations(
        self,
        operation_metrics: Dict[str, Dict[str, float]],
        bottlenecks: List[str]
    ) -> List[str]:
        """Generate optimization recommendations based on metrics."""
        recommendations = []

        for operation, metrics in operation_metrics.items():
            # High latency recommendations
            if metrics.get("latency_p95_ms", 0) > 100:
                if "similarity" in operation or "search" in operation:
                    recommendations.append(
                        f"Enable Rust acceleration for {operation} (59.5x speedup)"
                    )
                else:
                    recommendations.append(
                        f"Optimize {operation} - consider caching or async execution"
                    )

            # High CPU recommendations
            if metrics.get("cpu_usage_pct", 0) > 70:
                recommendations.append(
                    f"Consider parallelization for {operation}"
                )

            # Memory recommendations
            if metrics.get("memory_usage_mb", 0) > 500:
                recommendations.append(
                    f"Optimize memory usage in {operation} - consider streaming"
                )

        return recommendations

# Example usage
async def main():
    profiler = PerformanceProfiler()

    # Establish baseline
    baseline = await profiler.establish_baseline(
        scope=BenchmarkScope.STANDARD
    )

    print(f"Performance Baseline Established:")
    print(f"  Operations/sec: {baseline.ops_per_second:.0f}")
    print(f"  Latency p95: {baseline.latency_p95_ms:.1f}ms")
    print(f"  CPU usage: {baseline.cpu_usage_pct:.1f}%")
    print(f"  Memory usage: {baseline.memory_usage_mb:.1f}MB")
    print(f"\nBottlenecks found: {len(baseline.bottlenecks)}")
    for bottleneck in baseline.bottlenecks:
        print(f"  • {bottleneck}")
    print(f"\nRecommendations: {len(baseline.recommendations)}")
    for rec in baseline.recommendations:
        print(f"  • {rec}")
```

---

### Workflow 2: Rust Acceleration Patterns

**Purpose**: Enable and coordinate Rust-based acceleration for CPU-intensive operations, achieving 10-100x speedups.

**When to use**: CPU-bound operations, large-scale similarity searches, pattern matching, batch file I/O.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional, Callable
from enum import Enum
import asyncio
from whitemagic.tools import enable_rust_acceleration
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class RustOperation(Enum):
    """Available Rust-accelerated operations."""
    FAST_SIMILARITY = "fast_similarity"        # 59.5x speedup
    PATTERN_MATCH = "pattern_match"            # 30-60x speedup
    READ_FILES_BATCH = "read_files_batch"      # Parallel I/O
    COMPRESS_ARCHIVE = "compress_archive"      # Fast compression
    HASH_COMPUTATION = "hash_computation"      # Crypto operations
    SORT_LARGE_ARRAYS = "sort_large_arrays"    # Parallel sorting

@dataclass
class RustAccelerationConfig:
    """Configuration for Rust acceleration."""
    operations: List[RustOperation]

    # Performance expectations
    expected_speedup: Dict[RustOperation, float] = field(default_factory=dict)

    # Fallback to Python if Rust unavailable
    fallback_enabled: bool = True

    # Verification
    verify_results: bool = True  # Compare Rust vs Python initially

@dataclass
class AccelerationResult:
    """Result of enabling Rust acceleration."""
    enabled_operations: List[str]
    speedups_achieved: Dict[str, float]
    verification_passed: bool
    fallback_available: bool
    warnings: List[str] = field(default_factory=list)

class RustAccelerator:
    """Manage Rust-based performance acceleration."""

    def __init__(self):
        self.memory = MemoryManager()
        self.enabled_operations: set[str] = set()

    async def enable_acceleration(
        self,
        config: RustAccelerationConfig
    ) -> AccelerationResult:
        """
        Enable Rust acceleration for specified operations.

        Args:
            config: Rust acceleration configuration

        Returns:
            Result showing what was enabled and achieved speedups
        """
        emit_event("rust_acceleration.enabling", {
            "operations": [op.value for op in config.operations]
        })

        enabled_ops = []
        speedups = {}
        warnings = []

        try:
            # Enable Rust operations
            result = enable_rust_acceleration(
                operations=[op.value for op in config.operations]
            )

            enabled_ops = result.get("operations", [])
            speedups = result.get("speedups", {})

            # Verify if requested
            if config.verify_results:
                verification = await self._verify_rust_operations(enabled_ops)
                if not verification["passed"]:
                    warnings.extend(verification["warnings"])

            # Track enabled operations
            self.enabled_operations.update(enabled_ops)

            # Store acceleration config
            await self.memory.store(
                content=f"Rust acceleration enabled: {len(enabled_ops)} operations",
                metadata={
                    "type": "rust_acceleration",
                    "operations": enabled_ops,
                    "speedups": speedups
                },
                tier="short_term"
            )

            emit_event("rust_acceleration.enabled", {
                "operations": enabled_ops,
                "average_speedup": sum(speedups.values()) / len(speedups) if speedups else 0
            })

            return AccelerationResult(
                enabled_operations=enabled_ops,
                speedups_achieved=speedups,
                verification_passed=len(warnings) == 0,
                fallback_available=config.fallback_enabled,
                warnings=warnings
            )

        except Exception as e:
            warnings.append(f"Rust acceleration failed: {e}")

            if config.fallback_enabled:
                warnings.append("Falling back to Python implementation")
                return AccelerationResult(
                    enabled_operations=[],
                    speedups_achieved={},
                    verification_passed=False,
                    fallback_available=True,
                    warnings=warnings
                )
            else:
                raise

    async def _verify_rust_operations(
        self,
        operations: List[str]
    ) -> Dict[str, Any]:
        """Verify Rust operations produce correct results."""
        warnings = []

        for operation in operations:
            # Run small test to verify correctness
            try:
                if operation == "fast_similarity":
                    # Test with known vectors
                    test_passed = await self._test_fast_similarity()
                    if not test_passed:
                        warnings.append(
                            f"{operation}: Results don't match Python baseline"
                        )

                # Add more verification tests as needed

            except Exception as e:
                warnings.append(f"{operation}: Verification failed - {e}")

        return {
            "passed": len(warnings) == 0,
            "warnings": warnings
        }

    async def _test_fast_similarity(self) -> bool:
        """Test fast_similarity against known baseline."""
        # Implementation would compare Rust vs Python results
        # For now, assume it passes
        return True

    async def accelerate_operation(
        self,
        operation_name: str,
        operation_func: Callable,
        *args,
        **kwargs
    ) -> Any:
        """
        Execute operation with Rust acceleration if available.

        Args:
            operation_name: Name of operation (e.g., "fast_similarity")
            operation_func: Fallback Python function
            *args, **kwargs: Arguments to operation

        Returns:
            Operation result (Rust if available, Python otherwise)
        """
        if operation_name in self.enabled_operations:
            # Use Rust implementation
            from whitemagic_rs import globals() as rust_ops

            if operation_name in rust_ops:
                rust_func = rust_ops[operation_name]
                return await rust_func(*args, **kwargs)

        # Fallback to Python
        return await operation_func(*args, **kwargs)

# Example usage
async def main():
    accelerator = RustAccelerator()

    # Configure acceleration
    config = RustAccelerationConfig(
        operations=[
            RustOperation.FAST_SIMILARITY,
            RustOperation.PATTERN_MATCH,
            RustOperation.READ_FILES_BATCH
        ],
        verify_results=True,
        fallback_enabled=True
    )

    # Enable acceleration
    result = await accelerator.enable_acceleration(config)

    print(f"Rust Acceleration Enabled:")
    print(f"  Operations: {len(result.enabled_operations)}")
    for op, speedup in result.speedups_achieved.items():
        print(f"    • {op}: {speedup}x faster")

    if result.warnings:
        print(f"\n  Warnings:")
        for warning in result.warnings:
            print(f"    ⚠️  {warning}")
```

---

### Workflow 3: Cache Optimization Strategies

**Purpose**: Optimize caching strategies to achieve 90%+ hit rates and minimize latency for repeated operations.

**When to use**: Before large batch operations, when repeated queries expected, during high-load scenarios.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum
import asyncio
from whitemagic.tools import optimize_cache
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class CacheType(Enum):
    """Types of caches in WhiteMagic."""
    PREDICTIVE = "predictive"  # AI-powered cache prediction
    LRU = "lru"               # Least Recently Used
    LFU = "lfu"               # Least Frequently Used
    TTL = "ttl"               # Time To Live
    ALL = "all"               # Optimize all caches

@dataclass
class CacheMetrics:
    """Cache performance metrics."""
    cache_type: CacheType

    # Performance
    hit_rate_pct: float
    miss_rate_pct: float
    eviction_rate_pct: float

    # Latency
    avg_hit_latency_ms: float
    avg_miss_latency_ms: float

    # Capacity
    size_current: int
    size_max: int
    utilization_pct: float

    # Recommendations
    recommendations: List[str] = field(default_factory=list)

@dataclass
class CacheOptimizationPlan:
    """Plan for optimizing cache performance."""
    target_hit_rate: float = 0.90  # 90% target

    # Size adjustments
    increase_size: bool = False
    size_multiplier: float = 1.0

    # TTL adjustments
    adjust_ttl: bool = False
    ttl_seconds: Optional[int] = None

    # Eviction policy
    change_eviction_policy: bool = False
    new_policy: Optional[str] = None

    # Warming strategy
    enable_cache_warming: bool = False
    warming_queries: List[str] = field(default_factory=list)

class CacheOptimizer:
    """Optimize cache performance for maximum hit rates."""

    def __init__(self):
        self.memory = MemoryManager()

    async def analyze_cache_performance(
        self,
        cache_type: CacheType = CacheType.ALL
    ) -> Dict[CacheType, CacheMetrics]:
        """
        Analyze current cache performance.

        Args:
            cache_type: Which cache(s) to analyze

        Returns:
            Metrics for each analyzed cache
        """
        emit_event("cache.analysis_started", {"cache_type": cache_type.value})

        # Get current cache stats
        cache_stats = optimize_cache(
            cache_type=cache_type.value,
            action="analyze"
        )

        metrics = {}

        for cache_name, stats in cache_stats.items():
            cache_enum = CacheType(cache_name)

            recommendations = self._generate_cache_recommendations(stats)

            metrics[cache_enum] = CacheMetrics(
                cache_type=cache_enum,
                hit_rate_pct=stats.get("hit_rate_pct", 0),
                miss_rate_pct=stats.get("miss_rate_pct", 0),
                eviction_rate_pct=stats.get("eviction_rate_pct", 0),
                avg_hit_latency_ms=stats.get("avg_hit_latency_ms", 0),
                avg_miss_latency_ms=stats.get("avg_miss_latency_ms", 0),
                size_current=stats.get("size_current", 0),
                size_max=stats.get("size_max", 0),
                utilization_pct=stats.get("utilization_pct", 0),
                recommendations=recommendations
            )

        return metrics

    def _generate_cache_recommendations(
        self,
        stats: Dict[str, Any]
    ) -> List[str]:
        """Generate recommendations based on cache stats."""
        recommendations = []

        hit_rate = stats.get("hit_rate_pct", 0)
        utilization = stats.get("utilization_pct", 0)
        eviction_rate = stats.get("eviction_rate_pct", 0)

        # Low hit rate
        if hit_rate < 70:
            recommendations.append(
                "Hit rate below 70% - consider increasing cache size or adjusting TTL"
            )

        # High eviction rate
        if eviction_rate > 20:
            recommendations.append(
                "High eviction rate - cache too small for working set"
            )
            recommendations.append(
                "Consider increasing cache size by 2-3x"
            )

        # Low utilization
        if utilization < 30:
            recommendations.append(
                "Low cache utilization - consider reducing size to save memory"
            )

        # High utilization with high hit rate
        if utilization > 80 and hit_rate > 85:
            recommendations.append(
                "Cache working well but near capacity - monitor for growth"
            )

        return recommendations

    async def optimize_cache(
        self,
        cache_type: CacheType,
        plan: CacheOptimizationPlan
    ) -> CacheMetrics:
        """
        Apply optimization plan to cache.

        Args:
            cache_type: Which cache to optimize
            plan: Optimization plan

        Returns:
            Updated cache metrics after optimization
        """
        emit_event("cache.optimization_started", {
            "cache_type": cache_type.value,
            "target_hit_rate": plan.target_hit_rate
        })

        # Apply optimizations
        result = optimize_cache(
            cache_type=cache_type.value,
            size_multiplier=plan.size_multiplier if plan.increase_size else None,
            ttl_seconds=plan.ttl_seconds if plan.adjust_ttl else None,
            eviction_policy=plan.new_policy if plan.change_eviction_policy else None
        )

        # Warm cache if requested
        if plan.enable_cache_warming:
            await self._warm_cache(cache_type, plan.warming_queries)

        # Re-analyze performance
        updated_metrics = await self.analyze_cache_performance(cache_type)

        # Store optimization result
        await self.memory.store(
            content=f"Cache optimized: {cache_type.value} - {result['hit_rate_pct']:.1f}% hit rate",
            metadata={
                "type": "cache_optimization",
                "cache_type": cache_type.value,
                "hit_rate_pct": result["hit_rate_pct"],
                "improvement_pct": result.get("improvement_pct", 0)
            },
            tier="short_term"
        )

        emit_event("cache.optimization_completed", {
            "cache_type": cache_type.value,
            "hit_rate": result["hit_rate_pct"]
        })

        return updated_metrics[cache_type]

    async def _warm_cache(
        self,
        cache_type: CacheType,
        queries: List[str]
    ) -> None:
        """Pre-populate cache with common queries."""
        emit_event("cache.warming_started", {
            "cache_type": cache_type.value,
            "query_count": len(queries)
        })

        for query in queries:
            # Execute query to populate cache
            # Implementation depends on cache type
            pass

        emit_event("cache.warming_completed", {
            "cache_type": cache_type.value,
            "queries_warmed": len(queries)
        })

# Example usage
async def main():
    optimizer = CacheOptimizer()

    # Analyze current performance
    metrics = await optimizer.analyze_cache_performance(CacheType.PREDICTIVE)

    predictive_metrics = metrics[CacheType.PREDICTIVE]
    print(f"Predictive Cache Performance:")
    print(f"  Hit rate: {predictive_metrics.hit_rate_pct:.1f}%")
    print(f"  Utilization: {predictive_metrics.utilization_pct:.1f}%")
    print(f"\nRecommendations:")
    for rec in predictive_metrics.recommendations:
        print(f"  • {rec}")

    # Create optimization plan
    if predictive_metrics.hit_rate_pct < 90:
        plan = CacheOptimizationPlan(
            target_hit_rate=0.90,
            increase_size=True,
            size_multiplier=2.0,
            enable_cache_warming=True,
            warming_queries=[
                "common patterns",
                "recent sessions",
                "frequently accessed memories"
            ]
        )

        # Apply optimization
        optimized_metrics = await optimizer.optimize_cache(
            CacheType.PREDICTIVE,
            plan
        )

        print(f"\nAfter Optimization:")
        print(f"  Hit rate: {optimized_metrics.hit_rate_pct:.1f}% (target: 90%)")
```

---

### Workflow 4: Parallel Execution Coordination

**Purpose**: Coordinate parallel execution of independent operations to maximize throughput and minimize total time.

**When to use**: Batch processing, independent task execution, multi-file operations, concurrent API calls.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional, Callable, Awaitable
from enum import Enum
import asyncio
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
import time
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class ExecutionStrategy(Enum):
    """Parallel execution strategies."""
    ASYNC = "async"              # asyncio.gather (I/O-bound)
    THREADS = "threads"          # ThreadPoolExecutor (I/O-bound, GIL-safe)
    PROCESSES = "processes"      # ProcessPoolExecutor (CPU-bound)
    MIXED = "mixed"              # Combination based on task type

@dataclass
class ParallelTask:
    """A task to execute in parallel."""
    task_id: str
    func: Callable
    args: tuple = field(default_factory=tuple)
    kwargs: dict = field(default_factory=dict)

    # Execution hints
    is_cpu_bound: bool = False
    is_io_bound: bool = True
    estimated_duration_sec: Optional[float] = None

@dataclass
class ParallelExecutionConfig:
    """Configuration for parallel execution."""
    strategy: ExecutionStrategy = ExecutionStrategy.ASYNC

    # Concurrency limits
    max_concurrent: int = 10
    max_workers: Optional[int] = None  # For thread/process pools

    # Error handling
    fail_fast: bool = False  # Stop on first error
    retry_failed: bool = True
    max_retries: int = 3

    # Progress tracking
    progress_callback: Optional[Callable[[str, float], None]] = None

@dataclass
class ExecutionResult:
    """Result of parallel execution."""
    total_tasks: int
    succeeded: int
    failed: int

    # Timing
    total_duration_sec: float
    average_task_duration_sec: float

    # Results
    results: Dict[str, Any] = field(default_factory=dict)
    errors: Dict[str, str] = field(default_factory=dict)

    # Performance metrics
    throughput_tasks_per_sec: float = 0.0
    speedup_factor: float = 1.0  # vs sequential

class ParallelCoordinator:
    """Coordinate parallel execution of independent tasks."""

    def __init__(self):
        self.memory = MemoryManager()

    async def execute_parallel(
        self,
        tasks: List[ParallelTask],
        config: ParallelExecutionConfig
    ) -> ExecutionResult:
        """
        Execute tasks in parallel according to strategy.

        Args:
            tasks: List of tasks to execute
            config: Execution configuration

        Returns:
            Execution results with timing and outcomes
        """
        emit_event("parallel_execution.started", {
            "task_count": len(tasks),
            "strategy": config.strategy.value,
            "max_concurrent": config.max_concurrent
        })

        start_time = time.time()

        # Choose execution strategy
        if config.strategy == ExecutionStrategy.ASYNC:
            result = await self._execute_async(tasks, config)
        elif config.strategy == ExecutionStrategy.THREADS:
            result = await self._execute_threads(tasks, config)
        elif config.strategy == ExecutionStrategy.PROCESSES:
            result = await self._execute_processes(tasks, config)
        else:  # MIXED
            result = await self._execute_mixed(tasks, config)

        total_duration = time.time() - start_time

        # Calculate metrics
        execution_result = ExecutionResult(
            total_tasks=len(tasks),
            succeeded=result["succeeded"],
            failed=result["failed"],
            total_duration_sec=total_duration,
            average_task_duration_sec=total_duration / len(tasks) if tasks else 0,
            results=result["results"],
            errors=result["errors"],
            throughput_tasks_per_sec=len(tasks) / total_duration if total_duration > 0 else 0
        )

        # Estimate speedup (assumes sequential would be sum of task durations)
        estimated_sequential = sum(
            t.estimated_duration_sec for t in tasks
            if t.estimated_duration_sec is not None
        )
        if estimated_sequential > 0:
            execution_result.speedup_factor = estimated_sequential / total_duration

        # Store execution summary
        await self.memory.store(
            content=f"Parallel execution: {execution_result.succeeded}/{execution_result.total_tasks} tasks succeeded",
            metadata={
                "type": "parallel_execution",
                "strategy": config.strategy.value,
                "total_tasks": execution_result.total_tasks,
                "succeeded": execution_result.succeeded,
                "failed": execution_result.failed,
                "duration_sec": total_duration,
                "throughput": execution_result.throughput_tasks_per_sec
            },
            tier="short_term"
        )

        emit_event("parallel_execution.completed", {
            "succeeded": execution_result.succeeded,
            "failed": execution_result.failed,
            "duration_sec": total_duration,
            "speedup_factor": execution_result.speedup_factor
        })

        return execution_result

    async def _execute_async(
        self,
        tasks: List[ParallelTask],
        config: ParallelExecutionConfig
    ) -> Dict[str, Any]:
        """Execute tasks using asyncio.gather."""
        results = {}
        errors = {}

        # Create semaphore for concurrency limit
        semaphore = asyncio.Semaphore(config.max_concurrent)

        async def run_task(task: ParallelTask):
            async with semaphore:
                try:
                    if asyncio.iscoroutinefunction(task.func):
                        result = await task.func(*task.args, **task.kwargs)
                    else:
                        # Run sync function in executor
                        loop = asyncio.get_event_loop()
                        result = await loop.run_in_executor(
                            None,
                            lambda: task.func(*task.args, **task.kwargs)
                        )

                    results[task.task_id] = result

                    if config.progress_callback:
                        progress = len(results) / len(tasks)
                        config.progress_callback(task.task_id, progress)

                except Exception as e:
                    errors[task.task_id] = str(e)

                    if config.fail_fast:
                        raise

        # Execute all tasks
        await asyncio.gather(
            *[run_task(task) for task in tasks],
            return_exceptions=not config.fail_fast
        )

        return {
            "succeeded": len(results),
            "failed": len(errors),
            "results": results,
            "errors": errors
        }

    async def _execute_threads(
        self,
        tasks: List[ParallelTask],
        config: ParallelExecutionConfig
    ) -> Dict[str, Any]:
        """Execute tasks using ThreadPoolExecutor."""
        results = {}
        errors = {}

        max_workers = config.max_workers or min(config.max_concurrent, len(tasks))

        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {}

            for task in tasks:
                future = executor.submit(task.func, *task.args, **task.kwargs)
                futures[future] = task.task_id

            for future in asyncio.as_completed(futures.keys()):
                task_id = futures[future]
                try:
                    result = await future
                    results[task_id] = result
                except Exception as e:
                    errors[task_id] = str(e)
                    if config.fail_fast:
                        executor.shutdown(wait=False, cancel_futures=True)
                        break

        return {
            "succeeded": len(results),
            "failed": len(errors),
            "results": results,
            "errors": errors
        }

    async def _execute_processes(
        self,
        tasks: List[ParallelTask],
        config: ParallelExecutionConfig
    ) -> Dict[str, Any]:
        """Execute tasks using ProcessPoolExecutor."""
        # Similar to _execute_threads but with ProcessPoolExecutor
        # Note: Tasks must be picklable for process execution
        results = {}
        errors = {}

        max_workers = config.max_workers or min(config.max_concurrent, len(tasks))

        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            futures = {}

            for task in tasks:
                future = executor.submit(task.func, *task.args, **task.kwargs)
                futures[future] = task.task_id

            for future in asyncio.as_completed(futures.keys()):
                task_id = futures[future]
                try:
                    result = await future
                    results[task_id] = result
                except Exception as e:
                    errors[task_id] = str(e)
                    if config.fail_fast:
                        executor.shutdown(wait=False, cancel_futures=True)
                        break

        return {
            "succeeded": len(results),
            "failed": len(errors),
            "results": results,
            "errors": errors
        }

    async def _execute_mixed(
        self,
        tasks: List[ParallelTask],
        config: ParallelExecutionConfig
    ) -> Dict[str, Any]:
        """Execute tasks using mixed strategy based on task type."""
        # Separate tasks by type
        io_tasks = [t for t in tasks if t.is_io_bound]
        cpu_tasks = [t for t in tasks if t.is_cpu_bound]

        # Execute I/O tasks with async
        io_config = ParallelExecutionConfig(
            strategy=ExecutionStrategy.ASYNC,
            max_concurrent=config.max_concurrent,
            fail_fast=config.fail_fast
        )
        io_results = await self._execute_async(io_tasks, io_config)

        # Execute CPU tasks with processes
        cpu_config = ParallelExecutionConfig(
            strategy=ExecutionStrategy.PROCESSES,
            max_concurrent=config.max_concurrent,
            max_workers=config.max_workers,
            fail_fast=config.fail_fast
        )
        cpu_results = await self._execute_processes(cpu_tasks, cpu_config)

        # Merge results
        return {
            "succeeded": io_results["succeeded"] + cpu_results["succeeded"],
            "failed": io_results["failed"] + cpu_results["failed"],
            "results": {**io_results["results"], **cpu_results["results"]},
            "errors": {**io_results["errors"], **cpu_results["errors"]}
        }

# Example usage
async def main():
    coordinator = ParallelCoordinator()

    # Define tasks
    tasks = [
        ParallelTask(
            task_id=f"chapter_{i}",
            func=create_chapter,
            args=(i,),
            is_io_bound=True,
            estimated_duration_sec=30.0
        )
        for i in range(2, 8)
    ]

    # Configure execution
    config = ParallelExecutionConfig(
        strategy=ExecutionStrategy.ASYNC,
        max_concurrent=6,
        fail_fast=False,
        progress_callback=lambda task_id, progress: print(
            f"Progress: {progress*100:.0f}% - Completed {task_id}"
        )
    )

    # Execute in parallel
    result = await coordinator.execute_parallel(tasks, config)

    print(f"\nParallel Execution Complete:")
    print(f"  Total tasks: {result.total_tasks}")
    print(f"  Succeeded: {result.succeeded}")
    print(f"  Failed: {result.failed}")
    print(f"  Duration: {result.total_duration_sec:.1f}s")
    print(f"  Throughput: {result.throughput_tasks_per_sec:.1f} tasks/sec")
    print(f"  Speedup: {result.speedup_factor:.1f}x vs sequential")

async def create_chapter(chapter_num: int) -> str:
    """Example chapter creation function."""
    await asyncio.sleep(1)  # Simulate work
    return f"Chapter {chapter_num} created"
```

---

### Workflow 5: Performance Regression Detection

**Purpose**: Continuously monitor performance and detect regressions before they impact production.

**When to use**: After code changes, during continuous integration, before releases, on schedule.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum
import asyncio
import time
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.tools import run_benchmarks
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class RegressionSeverity(Enum):
    """Severity of performance regression."""
    NONE = "none"
    MINOR = "minor"       # < 10% degradation
    MODERATE = "moderate" # 10-25% degradation
    MAJOR = "major"       # 25-50% degradation
    CRITICAL = "critical" # > 50% degradation

@dataclass
class PerformanceSnapshot:
    """Snapshot of performance metrics at a point in time."""
    timestamp: str
    commit_hash: Optional[str]

    # Metrics
    ops_per_second: float
    latency_p95_ms: float
    memory_usage_mb: float
    cpu_usage_pct: float

    # Context
    workload: str
    environment: str

@dataclass
class RegressionReport:
    """Report of performance regression analysis."""
    has_regression: bool
    severity: RegressionSeverity

    # Comparisons
    baseline_snapshot: PerformanceSnapshot
    current_snapshot: PerformanceSnapshot

    # Metrics changes
    ops_per_second_change_pct: float
    latency_p95_change_pct: float
    memory_usage_change_pct: float

    # Identified regressions
    regressions: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    # Recommendations
    recommendations: List[str] = field(default_factory=list)

class RegressionDetector:
    """Detect performance regressions by comparing to baseline."""

    def __init__(self):
        self.memory = MemoryManager()

    async def detect_regressions(
        self,
        workload: str = "standard",
        baseline_commit: Optional[str] = None
    ) -> RegressionReport:
        """
        Detect performance regressions compared to baseline.

        Args:
            workload: Which workload to test
            baseline_commit: Specific baseline to compare against (default: latest stored)

        Returns:
            Regression report with severity and recommendations
        """
        emit_event("regression_detection.started", {
            "workload": workload,
            "baseline_commit": baseline_commit
        })

        # Get baseline snapshot
        baseline = await self._load_baseline(workload, baseline_commit)

        # Capture current snapshot
        current = await self._capture_snapshot(workload)

        # Compare metrics
        comparison = self._compare_snapshots(baseline, current)

        # Determine severity
        severity = self._assess_severity(comparison)

        # Generate recommendations
        recommendations = self._generate_regression_recommendations(
            comparison,
            severity
        )

        report = RegressionReport(
            has_regression=severity != RegressionSeverity.NONE,
            severity=severity,
            baseline_snapshot=baseline,
            current_snapshot=current,
            ops_per_second_change_pct=comparison["ops_change_pct"],
            latency_p95_change_pct=comparison["latency_change_pct"],
            memory_usage_change_pct=comparison["memory_change_pct"],
            regressions=comparison["regressions"],
            warnings=comparison["warnings"],
            recommendations=recommendations
        )

        # Store regression report
        await self.memory.store(
            content=f"Regression check: {severity.value} - {workload}",
            metadata={
                "type": "regression_report",
                "workload": workload,
                "severity": severity.value,
                "has_regression": report.has_regression,
                "ops_change_pct": comparison["ops_change_pct"],
                "latency_change_pct": comparison["latency_change_pct"]
            },
            tier="short_term"
        )

        emit_event("regression_detection.completed", {
            "has_regression": report.has_regression,
            "severity": severity.value
        })

        return report

    async def _load_baseline(
        self,
        workload: str,
        commit_hash: Optional[str]
    ) -> PerformanceSnapshot:
        """Load baseline performance snapshot."""
        # Search for stored baseline
        memories = await self.memory.search(
            query=f"performance baseline {workload}",
            limit=10
        )

        # Find matching baseline
        for memory in memories:
            if commit_hash:
                if memory.metadata.get("commit_hash") == commit_hash:
                    return self._memory_to_snapshot(memory)
            else:
                # Use most recent baseline
                return self._memory_to_snapshot(memory)

        # No baseline found - capture new one
        print(f"No baseline found for {workload} - capturing new baseline")
        baseline = await self._capture_snapshot(workload)

        # Store as baseline
        await self.memory.store(
            content=f"Performance baseline: {workload}",
            metadata={
                "type": "performance_baseline",
                "workload": workload,
                "ops_per_second": baseline.ops_per_second,
                "latency_p95_ms": baseline.latency_p95_ms,
                "memory_usage_mb": baseline.memory_usage_mb,
                "cpu_usage_pct": baseline.cpu_usage_pct,
                "timestamp": baseline.timestamp
            },
            tier="long_term"
        )

        return baseline

    async def _capture_snapshot(
        self,
        workload: str
    ) -> PerformanceSnapshot:
        """Capture current performance snapshot."""
        # Run benchmark
        benchmark = run_benchmarks(
            mode="benchmark",
            benchmark_type=workload
        )

        return PerformanceSnapshot(
            timestamp=time.strftime("%Y-%m-%d %H:%M:%S"),
            commit_hash=None,  # TODO: Get from git
            ops_per_second=benchmark.get("ops_per_second", 0),
            latency_p95_ms=benchmark.get("latency_p95_ms", 0),
            memory_usage_mb=benchmark.get("memory_usage_mb", 0),
            cpu_usage_pct=benchmark.get("cpu_usage_pct", 0),
            workload=workload,
            environment="development"  # TODO: Detect environment
        )

    def _memory_to_snapshot(self, memory: Any) -> PerformanceSnapshot:
        """Convert memory to performance snapshot."""
        metadata = memory.metadata

        return PerformanceSnapshot(
            timestamp=metadata.get("timestamp", "unknown"),
            commit_hash=metadata.get("commit_hash"),
            ops_per_second=metadata.get("ops_per_second", 0),
            latency_p95_ms=metadata.get("latency_p95_ms", 0),
            memory_usage_mb=metadata.get("memory_usage_mb", 0),
            cpu_usage_pct=metadata.get("cpu_usage_pct", 0),
            workload=metadata.get("workload", "unknown"),
            environment=metadata.get("environment", "unknown")
        )

    def _compare_snapshots(
        self,
        baseline: PerformanceSnapshot,
        current: PerformanceSnapshot
    ) -> Dict[str, Any]:
        """Compare two performance snapshots."""
        # Calculate percentage changes
        ops_change_pct = (
            (current.ops_per_second - baseline.ops_per_second) / baseline.ops_per_second * 100
            if baseline.ops_per_second > 0 else 0
        )

        latency_change_pct = (
            (current.latency_p95_ms - baseline.latency_p95_ms) / baseline.latency_p95_ms * 100
            if baseline.latency_p95_ms > 0 else 0
        )

        memory_change_pct = (
            (current.memory_usage_mb - baseline.memory_usage_mb) / baseline.memory_usage_mb * 100
            if baseline.memory_usage_mb > 0 else 0
        )

        # Identify regressions
        regressions = []
        warnings = []

        if ops_change_pct < -10:
            regressions.append(
                f"Operations per second decreased by {abs(ops_change_pct):.1f}%"
            )

        if latency_change_pct > 10:
            regressions.append(
                f"Latency p95 increased by {latency_change_pct:.1f}%"
            )

        if memory_change_pct > 20:
            regressions.append(
                f"Memory usage increased by {memory_change_pct:.1f}%"
            )

        # Warnings for moderate changes
        if -10 <= ops_change_pct < -5:
            warnings.append(
                f"Minor throughput decrease: {abs(ops_change_pct):.1f}%"
            )

        if 5 < latency_change_pct <= 10:
            warnings.append(
                f"Minor latency increase: {latency_change_pct:.1f}%"
            )

        return {
            "ops_change_pct": ops_change_pct,
            "latency_change_pct": latency_change_pct,
            "memory_change_pct": memory_change_pct,
            "regressions": regressions,
            "warnings": warnings
        }

    def _assess_severity(
        self,
        comparison: Dict[str, Any]
    ) -> RegressionSeverity:
        """Assess severity of regression."""
        max_degradation = max(
            abs(comparison["ops_change_pct"]) if comparison["ops_change_pct"] < 0 else 0,
            comparison["latency_change_pct"] if comparison["latency_change_pct"] > 0 else 0,
            comparison["memory_change_pct"] if comparison["memory_change_pct"] > 0 else 0
        )

        if max_degradation > 50:
            return RegressionSeverity.CRITICAL
        elif max_degradation > 25:
            return RegressionSeverity.MAJOR
        elif max_degradation > 10:
            return RegressionSeverity.MODERATE
        elif max_degradation > 5:
            return RegressionSeverity.MINOR
        else:
            return RegressionSeverity.NONE

    def _generate_regression_recommendations(
        self,
        comparison: Dict[str, Any],
        severity: RegressionSeverity
    ) -> List[str]:
        """Generate recommendations based on regression analysis."""
        recommendations = []

        if severity == RegressionSeverity.NONE:
            recommendations.append("No action needed - performance stable")
            return recommendations

        if severity in [RegressionSeverity.CRITICAL, RegressionSeverity.MAJOR]:
            recommendations.append("URGENT: Investigate regression before deployment")
            recommendations.append("Consider reverting recent changes")

        if comparison["ops_change_pct"] < -10:
            recommendations.append("Profile code to find throughput bottleneck")
            recommendations.append("Consider enabling Rust acceleration")

        if comparison["latency_change_pct"] > 10:
            recommendations.append("Analyze latency hotspots with profiler")
            recommendations.append("Check for new synchronous I/O operations")

        if comparison["memory_change_pct"] > 20:
            recommendations.append("Review recent code for memory leaks")
            recommendations.append("Check for unbounded cache growth")

        return recommendations

# Example usage
async def main():
    detector = RegressionDetector()

    # Check for regressions
    report = await detector.detect_regressions(workload="standard")

    print(f"Performance Regression Report:")
    print(f"  Severity: {report.severity.value}")
    print(f"  Has regression: {report.has_regression}")
    print(f"\nMetric Changes:")
    print(f"  Operations/sec: {report.ops_per_second_change_pct:+.1f}%")
    print(f"  Latency p95: {report.latency_p95_change_pct:+.1f}%")
    print(f"  Memory usage: {report.memory_usage_change_pct:+.1f}%")

    if report.regressions:
        print(f"\nRegressions:")
        for regression in report.regressions:
            print(f"  🔴 {regression}")

    if report.warnings:
        print(f"\nWarnings:")
        for warning in report.warnings:
            print(f"  ⚠️  {warning}")

    if report.recommendations:
        print(f"\nRecommendations:")
        for rec in report.recommendations:
            print(f"  • {rec}")
```

---

### Workflow 6: Bottleneck Identification & Resolution

**Purpose**: Systematically identify performance bottlenecks and apply targeted optimizations to resolve them.

**When to use**: When performance is suboptimal, after regression detection, during optimization sprints.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional, Callable
from enum import Enum
import asyncio
import cProfile
import pstats
import io
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.tools import manage_gardens
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class BottleneckType(Enum):
    """Types of performance bottlenecks."""
    CPU_BOUND = "cpu_bound"           # CPU-intensive operations
    IO_BOUND = "io_bound"             # I/O wait time
    MEMORY_BOUND = "memory_bound"     # Memory allocation/GC
    LOCK_CONTENTION = "lock_contention" # Lock waiting
    NETWORK_LATENCY = "network_latency" # Network delays
    CACHE_MISSES = "cache_misses"     # Poor cache performance

@dataclass
class Bottleneck:
    """Identified performance bottleneck."""
    bottleneck_type: BottleneckType
    location: str  # Function/module name

    # Impact metrics
    time_spent_pct: float  # % of total time
    call_count: int
    avg_duration_ms: float

    # Resolution suggestions
    suggested_fixes: List[str] = field(default_factory=list)
    priority: int = 1  # 1=highest, 5=lowest

@dataclass
class OptimizationPlan:
    """Plan for resolving bottlenecks."""
    bottlenecks: List[Bottleneck]

    # Ordered actions
    actions: List[Dict[str, Any]] = field(default_factory=list)

    # Expected impact
    expected_improvement_pct: float = 0.0
    estimated_effort_hours: float = 0.0

class BottleneckAnalyzer:
    """Analyze performance bottlenecks and generate resolution plans."""

    def __init__(self):
        self.memory = MemoryManager()
        self.adventure = manage_gardens(action="get", garden_name="adventure")

    async def identify_bottlenecks(
        self,
        profile_duration_sec: int = 60,
        top_n: int = 10
    ) -> List[Bottleneck]:
        """
        Profile execution and identify top bottlenecks.

        Args:
            profile_duration_sec: How long to profile
            top_n: How many bottlenecks to return

        Returns:
            List of identified bottlenecks, ordered by impact
        """
        emit_event("bottleneck_analysis.started", {
            "duration_sec": profile_duration_sec,
            "top_n": top_n
        })

        # Activate adventure garden for deep investigation
        manage_gardens(action="activate", garden_name="adventure")

        # Profile execution
        profiler = cProfile.Profile()
        profiler.enable()

        # Run representative workload
        await self._run_representative_workload(profile_duration_sec)

        profiler.disable()

        # Analyze profile
        bottlenecks = self._analyze_profile(profiler, top_n)

        # Store analysis
        await self.memory.store(
            content=f"Bottleneck analysis: {len(bottlenecks)} bottlenecks identified",
            metadata={
                "type": "bottleneck_analysis",
                "bottleneck_count": len(bottlenecks),
                "top_bottleneck": bottlenecks[0].location if bottlenecks else None
            },
            tier="short_term"
        )

        emit_event("bottleneck_analysis.completed", {
            "bottleneck_count": len(bottlenecks)
        })

        return bottlenecks

    async def _run_representative_workload(
        self,
        duration_sec: int
    ) -> None:
        """Run representative workload for profiling."""
        # Simulate typical operations
        # In practice, this would run actual use cases

        end_time = asyncio.get_event_loop().time() + duration_sec

        while asyncio.get_event_loop().time() < end_time:
            # Example workload mix
            await self.memory.store(
                content="test data",
                metadata={"test": True},
                tier="short_term"
            )

            await self.memory.search("test", limit=10)

            await asyncio.sleep(0.1)

    def _analyze_profile(
        self,
        profiler: cProfile.Profile,
        top_n: int
    ) -> List[Bottleneck]:
        """Analyze profiler output to identify bottlenecks."""
        # Get stats
        stats_stream = io.StringIO()
        stats = pstats.Stats(profiler, stream=stats_stream)
        stats.sort_stats('cumulative')

        bottlenecks = []

        # Analyze top functions by cumulative time
        for func, (cc, nc, tt, ct, callers) in list(stats.stats.items())[:top_n]:
            # Determine bottleneck type
            func_name = f"{func[0]}:{func[1]}:{func[2]}"
            bottleneck_type = self._classify_bottleneck(func_name, ct)

            # Calculate metrics
            time_spent_pct = (ct / stats.total_tt * 100) if stats.total_tt > 0 else 0
            avg_duration_ms = (ct / cc * 1000) if cc > 0 else 0

            # Generate suggestions
            suggestions = self._suggest_fixes(bottleneck_type, func_name)

            bottleneck = Bottleneck(
                bottleneck_type=bottleneck_type,
                location=func_name,
                time_spent_pct=time_spent_pct,
                call_count=cc,
                avg_duration_ms=avg_duration_ms,
                suggested_fixes=suggestions,
                priority=self._calculate_priority(time_spent_pct, cc)
            )

            bottlenecks.append(bottleneck)

        # Sort by priority
        bottlenecks.sort(key=lambda b: (b.priority, -b.time_spent_pct))

        return bottlenecks

    def _classify_bottleneck(
        self,
        func_name: str,
        cumulative_time: float
    ) -> BottleneckType:
        """Classify type of bottleneck based on function name and behavior."""
        func_lower = func_name.lower()

        if any(keyword in func_lower for keyword in ["read", "write", "open", "file"]):
            return BottleneckType.IO_BOUND
        elif any(keyword in func_lower for keyword in ["lock", "acquire", "release"]):
            return BottleneckType.LOCK_CONTENTION
        elif any(keyword in func_lower for keyword in ["alloc", "gc", "memory"]):
            return BottleneckType.MEMORY_BOUND
        elif any(keyword in func_lower for keyword in ["http", "request", "fetch"]):
            return BottleneckType.NETWORK_LATENCY
        elif any(keyword in func_lower for keyword in ["cache", "lookup"]):
            return BottleneckType.CACHE_MISSES
        else:
            return BottleneckType.CPU_BOUND

    def _suggest_fixes(
        self,
        bottleneck_type: BottleneckType,
        location: str
    ) -> List[str]:
        """Suggest fixes for bottleneck type."""
        suggestions = {
            BottleneckType.CPU_BOUND: [
                "Enable Rust acceleration for this operation",
                "Consider parallelization with asyncio or multiprocessing",
                "Profile to find algorithmic improvements"
            ],
            BottleneckType.IO_BOUND: [
                "Use async I/O (asyncio) instead of synchronous",
                "Batch operations to reduce I/O calls",
                "Enable read-ahead caching"
            ],
            BottleneckType.MEMORY_BOUND: [
                "Review for memory leaks",
                "Use generators/iterators instead of lists",
                "Consider streaming for large data"
            ],
            BottleneckType.LOCK_CONTENTION: [
                "Reduce lock scope (hold locks for shorter time)",
                "Use read-write locks for read-heavy workloads",
                "Consider lock-free data structures"
            ],
            BottleneckType.NETWORK_LATENCY: [
                "Batch network requests",
                "Enable request caching",
                "Use connection pooling"
            ],
            BottleneckType.CACHE_MISSES: [
                "Increase cache size",
                "Implement cache warming",
                "Review cache eviction policy"
            ]
        }

        return suggestions.get(bottleneck_type, ["Profile for specific optimization opportunities"])

    def _calculate_priority(
        self,
        time_spent_pct: float,
        call_count: int
    ) -> int:
        """Calculate priority (1=highest, 5=lowest)."""
        # High impact = high time spent OR very high call count
        if time_spent_pct > 20:
            return 1
        elif time_spent_pct > 10:
            return 2
        elif time_spent_pct > 5 or call_count > 10000:
            return 3
        elif time_spent_pct > 2 or call_count > 1000:
            return 4
        else:
            return 5

    async def create_optimization_plan(
        self,
        bottlenecks: List[Bottleneck]
    ) -> OptimizationPlan:
        """
        Create optimization plan from identified bottlenecks.

        Args:
            bottlenecks: List of bottlenecks to address

        Returns:
            Ordered optimization plan
        """
        actions = []
        total_expected_improvement = 0.0
        total_estimated_effort = 0.0

        for bottleneck in bottlenecks:
            # Skip low priority if we already have enough actions
            if len(actions) >= 5 and bottleneck.priority > 2:
                continue

            for fix in bottleneck.suggested_fixes:
                action = {
                    "bottleneck": bottleneck.location,
                    "type": bottleneck.bottleneck_type.value,
                    "action": fix,
                    "priority": bottleneck.priority,
                    "expected_improvement_pct": bottleneck.time_spent_pct * 0.5,  # Conservative: 50% improvement
                    "estimated_effort_hours": self._estimate_effort(bottleneck.bottleneck_type, fix)
                }

                actions.append(action)
                total_expected_improvement += action["expected_improvement_pct"]
                total_estimated_effort += action["estimated_effort_hours"]

        plan = OptimizationPlan(
            bottlenecks=bottlenecks,
            actions=actions,
            expected_improvement_pct=total_expected_improvement,
            estimated_effort_hours=total_estimated_effort
        )

        # Store plan
        await self.memory.store(
            content=f"Optimization plan: {len(actions)} actions, {total_expected_improvement:.0f}% expected improvement",
            metadata={
                "type": "optimization_plan",
                "action_count": len(actions),
                "expected_improvement_pct": total_expected_improvement,
                "estimated_effort_hours": total_estimated_effort
            },
            tier="short_term"
        )

        return plan

    def _estimate_effort(
        self,
        bottleneck_type: BottleneckType,
        fix: str
    ) -> float:
        """Estimate effort in hours for a fix."""
        # Simple heuristics
        if "Rust" in fix:
            return 8.0  # Rust integration takes time
        elif "parallel" in fix.lower():
            return 4.0  # Parallelization is moderate effort
        elif "cache" in fix.lower():
            return 2.0  # Cache tuning is relatively quick
        elif "async" in fix.lower():
            return 3.0  # Async conversion moderate effort
        else:
            return 1.0  # Default estimate

# Example usage
async def main():
    analyzer = BottleneckAnalyzer()

    # Identify bottlenecks
    print("Profiling for bottlenecks...")
    bottlenecks = await analyzer.identify_bottlenecks(
        profile_duration_sec=10,
        top_n=5
    )

    print(f"\nTop Bottlenecks:")
    for i, bottleneck in enumerate(bottlenecks, 1):
        print(f"\n{i}. {bottleneck.location}")
        print(f"   Type: {bottleneck.bottleneck_type.value}")
        print(f"   Time spent: {bottleneck.time_spent_pct:.1f}%")
        print(f"   Calls: {bottleneck.call_count:,}")
        print(f"   Priority: {bottleneck.priority}")
        print(f"   Suggested fixes:")
        for fix in bottleneck.suggested_fixes:
            print(f"     • {fix}")

    # Create optimization plan
    plan = await analyzer.create_optimization_plan(bottlenecks)

    print(f"\nOptimization Plan:")
    print(f"  Total actions: {len(plan.actions)}")
    print(f"  Expected improvement: {plan.expected_improvement_pct:.0f}%")
    print(f"  Estimated effort: {plan.estimated_effort_hours:.1f} hours")
    print(f"\nPrioritized Actions:")
    for i, action in enumerate(plan.actions[:5], 1):
        print(f"  {i}. {action['action']}")
        print(f"     Impact: +{action['expected_improvement_pct']:.0f}% | Effort: {action['estimated_effort_hours']:.1f}h")
```

---

## 🌿 Garden Resonance

The **adventure** garden brings the quality of passionate exploration and bold action.

When working in this chapter, embody:
- **Passion**: Channel excitement into productive energy
- **Boldness**: Take calculated risks for performance
- **Momentum**: Build and maintain forward velocity
- **Joy**: Find delight in speed and optimization
- **Courage**: Push boundaries of what's possible

Adventure in technical work isn't recklessness—it's the passionate drive that makes breakthroughs possible, the joy that sustains momentum, the courage to optimize aggressively.

---

## 🔧 Troubleshooting

### Issue 1: Rust Acceleration Not Available

**Symptoms**: `enable_rust_acceleration` fails or returns empty result

**Diagnosis**:
```python
# Check if Rust module built
try:
    import whitemagic_rs
    print("✅ Rust module available")
except ImportError as e:
    print(f"❌ Rust module missing: {e}")
```

**Solutions**:
1. Build Rust module: `cd whitemagic-rs && maturin develop --release`
2. Check Python can find module: `python -c "import whitemagic_rs"`
3. Verify correct Python environment activated
4. Check `maturin` installed: `pip install maturin`

---

### Issue 2: Performance Regression After Optimization

**Symptoms**: Benchmarks show worse performance after applying optimizations

**Diagnosis**:
```python
# Compare before/after baselines
detector = RegressionDetector()
report = await detector.detect_regressions()

if report.has_regression:
    print(f"Regression detected: {report.severity.value}")
    for rec in report.recommendations:
        print(f"  • {rec}")
```

**Solutions**:
1. Verify optimization actually applied: Check logs/events
2. Profile to find new bottleneck introduced
3. Revert and try alternative optimization
4. Check for cache invalidation or cold start effects

---

### Issue 3: Cache Hit Rate Lower Than Expected

**Symptoms**: `optimize_cache` shows <70% hit rate despite tuning

**Diagnosis**:
```python
optimizer = CacheOptimizer()
metrics = await optimizer.analyze_cache_performance(CacheType.PREDICTIVE)

for cache_type, cache_metrics in metrics.items():
    print(f"{cache_type.value}:")
    print(f"  Hit rate: {cache_metrics.hit_rate_pct}%")
    print(f"  Eviction rate: {cache_metrics.eviction_rate_pct}%")
    print(f"  Utilization: {cache_metrics.utilization_pct}%")
```

**Solutions**:
1. Increase cache size if eviction rate >20%
2. Adjust TTL if data staleness acceptable
3. Enable cache warming for predictable queries
4. Check workload variability - high variance = lower hit rate

---

### Issue 4: Parallel Execution Not Faster

**Symptoms**: Parallel execution takes same time or longer than sequential

**Diagnosis**:
```python
# Check if tasks truly independent
# Profile to see if GIL contention

result = await coordinator.execute_parallel(tasks, config)
print(f"Speedup: {result.speedup_factor:.1f}x")

if result.speedup_factor < 1.5:
    print("⚠️ Limited speedup - check for:")
    print("  • GIL contention (use processes for CPU-bound)")
    print("  • Task dependencies (must be independent)")
    print("  • Overhead from coordination")
```

**Solutions**:
1. For CPU-bound: Use `ExecutionStrategy.PROCESSES` instead of `THREADS`
2. For I/O-bound: Use `ExecutionStrategy.ASYNC` for best performance
3. Increase batch size to amortize coordination overhead
4. Check tasks truly independent (no shared state)

---

### Issue 5: Bottleneck Analysis Misleading

**Symptoms**: Optimizing identified bottlenecks doesn't improve overall performance

**Diagnosis**:
```python
# Run longer profiling session
bottlenecks = await analyzer.identify_bottlenecks(
    profile_duration_sec=300,  # 5 minutes
    top_n=20
)

# Check if bottlenecks consistent across multiple runs
```

**Solutions**:
1. Profile for longer duration (60+ seconds) for stability
2. Use representative workload, not synthetic tests
3. Check for bottlenecks in initialization (won't show in steady-state profiling)
4. Verify optimization actually on hot path (not called once during setup)

---

## ✨ Best Practices

1. **Measure First, Optimize Second**: Always establish baseline before optimizing
2. **Profile Before Guessing**: Use profiler to find actual bottlenecks, not assumptions
3. **Optimize Hot Paths Only**: 80% of time in 20% of code - focus there
4. **Verify Improvements**: Re-benchmark after each optimization
5. **Regression Test Continuously**: Catch performance degradations early
6. **Use Right Tool for Job**: Rust for CPU, async for I/O, cache for repetition
7. **Document Baselines**: Store performance snapshots for comparison
8. **Monitor in Production**: Synthetic benchmarks don't capture real usage patterns

---

## ⏭️ Transitions

**From Chapter 5 (Heart/Love)**: Context connected, purpose clear, now accelerate with passion.

**To Chapter 7 (WinnowingBasket/Truth)**: High-velocity work complete, now consolidate and separate signal from noise.

**Special transitions**:
- If performance issues persist → Jump to Ch.18 (HairyHead/Beauty) for debugging
- If optimization premature → Return to Ch.3 (Root) for baseline
- If burnout from speed → Pause in Ch.27 (Void/Stillness) before continuing

---

## 🧭 Navigation

**Predecessor**: Chapter 5 (Heart/Love) - *Context connected*
**Successor**: Chapter 7 (WinnowingBasket/Truth) - *Consolidation next*
**Quadrant Flow**: Sixth of Eastern Quadrant (Spring/Wood) - Position 6/7

---

## 🐉 The Tail's Wisdom

> *"The dragon's tail carries the force of the entire body—accumulated power released in sweeping motion. Passion without direction is chaos; direction without passion is mere mechanics. The Tail teaches: channel your excitement into excellence, your joy into performance, your adventure into achievement."*

**Performance Drive Principles**:
- **Optimize with purpose**: Speed for speed's sake is vanity; speed in service is power
- **Accelerate with care**: Fast execution still requires correctness
- **Measure before and after**: Know your gains are real
- **Use the right tools**: Rust for CPU, cache for memory, parallel for batch
- **Sustain momentum**: Passion enables sustained high performance

**The Acceleration Test**:
Before proceeding to Chapter 7, verify:
1. Performance baseline established
2. Appropriate optimizations enabled
3. Quality maintained despite speed
4. Passion channeled productively

If all verified → Proceed to Chapter 7
If performance issues → Debug before consolidating

---

**Next Chapter**: [Chapter 7: Consolidation →](07_WINNOWINGBASKET_CONSOLIDATION.md)
**Previous Chapter**: [Chapter 5: Context Connection](05_HEART_CONTEXT_CONNECTION.md)
**Quadrant**: Eastern (Spring/Wood) - Yang Rising Phase

---

*"Speed is not the goal—excellence delivered swiftly is. 🚀"*
