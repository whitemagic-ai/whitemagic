"""Advanced Parallel Execution - Rust & I Ching Inspired

Combines patterns from:
- whitemagic-rs: rayon parallelism, async tokio
- whitemagic/parallel/pools.py: I Ching thread tiers
- asyncio.gather: Found in 31 Python files

Performance Target: 16k concurrent operations (from previous benchmarks)
"""

from __future__ import annotations

import asyncio
import inspect
import os
import time
from collections.abc import Callable
from concurrent.futures import Future, ThreadPoolExecutor
from dataclasses import dataclass
from enum import Enum
from typing import Any, TypeVar

T = TypeVar("T")


class ParallelTier(Enum):
    """I Ching-aligned parallelism tiers."""

    TIER_0_TRIGRAMS = 8      # 八卦 (8 trigrams) - Minimal
    TIER_1_BASIC = 16         # Basic parallelism
    TIER_2_MEDIUM = 32        # Medium parallelism
    TIER_3_HEXAGRAMS = 64     # 六十四卦 (64 hexagrams) - Sweet spot!
    TIER_4_HIGH = 128         # High parallelism
    TIER_5_EXTREME = 256      # Extreme parallelism
    TIER_6_RALPH_WIGGUM = 1024  # Ralph Wiggum level (asyncio only)


@dataclass
class ParallelTask:
    """Task for parallel execution."""

    id: str
    func: Callable[..., Any]
    args: tuple[Any, ...] = ()
    kwargs: dict[str, Any] | None = None

    def __post_init__(self) -> None:
        if self.kwargs is None:
            self.kwargs = {}


class AdaptiveParallelExecutor:
    """Adaptive parallel executor inspired by Rust rayon + I Ching tiers.

    Automatically scales based on task count and system resources.
    Uses asyncio for I/O-bound, threads for CPU-bound (avoiding ProcessPool memory issues).
    """

    def __init__(self, max_workers: int | None = None) -> None:
        """Initialize executor.

        Args:
            max_workers: Max workers. If None, auto-detect from task count.

        """
        self.max_workers = max_workers
        self.stats: dict[str, Any] = {
            "tasks_executed": 0,
            "total_duration_ms": 0.0,
            "tier_usage": {tier.name: 0 for tier in ParallelTier},
        }

    async def execute_parallel(
        self,
        tasks: list[ParallelTask],
        tier: ParallelTier | None = None,
    ) -> list[Any]:
        """Execute tasks in parallel using adaptive tiering.

        Strategy:
        1. Auto-select tier based on task count if not specified
        2. Use asyncio for I/O (most local model operations)
        3. Track performance metrics
        """
        start = time.time()

        # Auto-select tier
        if tier is None:
            tier = self._select_tier(len(tasks))

        # Execute with selected tier
        if len(tasks) <= tier.value:
            # Direct asyncio.gather (fastest for I/O)
            results = await self._execute_async(tasks)
        else:
            # Batched execution to respect tier limits
            results = await self._execute_batched(tasks, tier.value)

        # Update stats
        duration = (time.time() - start) * 1000
        self.stats["tasks_executed"] += len(tasks)
        self.stats["total_duration_ms"] += duration
        self.stats["tier_usage"][tier.name] += 1

        return results

    def _select_tier(self, task_count: int) -> ParallelTier:
        """Select appropriate tier based on task count."""
        if task_count <= 8:
            return ParallelTier.TIER_0_TRIGRAMS
        if task_count <= 16:
            return ParallelTier.TIER_1_BASIC
        if task_count <= 32:
            return ParallelTier.TIER_2_MEDIUM
        if task_count <= 64:
            return ParallelTier.TIER_3_HEXAGRAMS  # Sweet spot!
        if task_count <= 128:
            return ParallelTier.TIER_4_HIGH
        if task_count <= 256:
            return ParallelTier.TIER_5_EXTREME
        return ParallelTier.TIER_6_RALPH_WIGGUM  # Go big!

    async def _execute_async(self, tasks: list[ParallelTask]) -> list[Any]:
        """Execute tasks concurrently.

        Note: We intentionally avoid `loop.run_in_executor(...)` here.
        In some sandboxed environments it can fail to wake the event loop
        when executor futures complete, leading to hangs. Instead we submit
        sync work to a `ThreadPoolExecutor` directly and poll for completion.
        """
        if not tasks:
            return []

        # Conservative default: enough parallelism for I/O-ish sync work,
        # but capped to avoid creating an excessive number of threads.
        max_workers = self.max_workers or min(len(tasks), max(4, (os.cpu_count() or 4) * 2))

        async_tasks: list[tuple[int, asyncio.Task[Any]]] = []
        sync_futures: list[tuple[int, Future[Any]]] = []

        def _is_coro_func(func: Callable[..., Any]) -> bool:
            # Handle functools.partial + decorators reasonably.
            base = getattr(func, "func", func)
            return inspect.iscoroutinefunction(base)

        # Keep results aligned to input order.
        results: list[Any] = [None] * len(tasks)

        with ThreadPoolExecutor(max_workers=max_workers) as pool:
            for idx, task in enumerate(tasks):
                kwargs = task.kwargs or {}
                if _is_coro_func(task.func):
                    coro = task.func(*task.args, **kwargs)
                    async_tasks.append((idx, asyncio.create_task(coro)))
                else:
                    sync_futures.append((idx, pool.submit(task.func, *task.args, **kwargs)))

            pending_async = {t for _, t in async_tasks}
            pending_sync = {f for _, f in sync_futures}
            async_index = {t: idx for idx, t in async_tasks}
            sync_index = {f: idx for idx, f in sync_futures}

            # Poll until everything completes. This keeps the loop awake even if
            # executor future callbacks don't reliably wake it.
            poll_s = 0.001
            while pending_async or pending_sync:
                progress = False

                for t in list(pending_async):
                    if not t.done():
                        continue
                    pending_async.remove(t)
                    idx = async_index[t]
                    results[idx] = t.result()
                    progress = True

                for f in list(pending_sync):
                    if not f.done():
                        continue
                    pending_sync.remove(f)
                    idx = sync_index[f]
                    results[idx] = f.result()
                    progress = True

                if not progress and (pending_async or pending_sync):
                    await asyncio.sleep(poll_s)

        return results

    async def _execute_batched(
        self,
        tasks: list[ParallelTask],
        batch_size: int,
    ) -> list[Any]:
        """Execute tasks in batches."""
        results = []

        for i in range(0, len(tasks), batch_size):
            batch = tasks[i:i + batch_size]
            batch_results = await self._execute_async(batch)
            results.extend(batch_results)

        return results

    def get_stats(self) -> dict[str, Any]:
        """Get execution statistics."""
        avg_duration = (
            self.stats["total_duration_ms"] / self.stats["tier_usage"][ParallelTier.TIER_3_HEXAGRAMS.name]
            if self.stats["tier_usage"][ParallelTier.TIER_3_HEXAGRAMS.name] > 0
            else 0
        )

        return {
            **self.stats,
            "avg_duration_ms": avg_duration,
            "throughput_tasks_per_sec": (
                self.stats["tasks_executed"] / (self.stats["total_duration_ms"] / 1000)
                if self.stats["total_duration_ms"] > 0
                else 0
            ),
        }


class RustInspiredPipeline:
    """Pipeline executor inspired by Rust iterators and rayon.

    Patterns:
    - map/filter/reduce operations
    - Lazy evaluation
    - Automatic parallelization
    """

    def __init__(self, data: list[Any]) -> None:
        self.data = data
        self.operations: list[tuple[str, Callable[..., Any]]] = []

    def map(self, func: Callable[..., Any]) -> RustInspiredPipeline:
        """Map operation (like Rust iter.map())."""
        self.operations.append(("map", func))
        return self

    def filter(self, predicate: Callable[..., Any]) -> RustInspiredPipeline:
        """Filter operation (like Rust iter.filter())."""
        self.operations.append(("filter", predicate))
        return self

    def par_map(self, func: Callable[..., Any]) -> RustInspiredPipeline:
        """Parallel map (like Rust rayon par_iter().map())."""
        self.operations.append(("par_map", func))
        return self

    async def collect(self) -> list[Any]:
        """Execute pipeline and collect results."""
        result = self.data

        for op_type, op_func in self.operations:
            if op_type == "map":
                result = [op_func(item) for item in result]
            elif op_type == "filter":
                result = [item for item in result if op_func(item)]
            elif op_type == "par_map":
                # Parallel execution
                executor = AdaptiveParallelExecutor()
                tasks = [
                    ParallelTask(id=str(i), func=op_func, args=(item,))
                    for i, item in enumerate(result)
                ]
                result = await executor.execute_parallel(tasks)

        return result

    async def reduce(self, func: Callable[..., Any], initial: Any = None) -> Any:
        """Reduce operation (like Rust iter.fold())."""
        collected = await self.collect()

        if not collected:
            return initial

        if initial is None:
            result = collected[0]
            start_idx = 1
        else:
            result = initial
            start_idx = 0

        for item in collected[start_idx:]:
            result = func(result, item)

        return result


# Quick interfaces inspired by Rust convenience
async def par_map(data: list[T], func: Callable[[T], Any]) -> list[Any]:
    """Quick parallel map (like Rust rayon).

    Example:
        results = await par_map([1, 2, 3, 4], lambda x: x * 2)

    """
    executor = AdaptiveParallelExecutor()
    tasks = [
        ParallelTask(id=str(i), func=func, args=(item,))
        for i, item in enumerate(data)
    ]
    return await executor.execute_parallel(tasks)


async def par_filter_map(
    data: list[T],
    predicate: Callable[[T], bool],
    mapper: Callable[[T], Any],
) -> list[Any]:
    """Filter then map in parallel (Rust-style).

    Example:
        evens_squared = await par_filter_map(
            [1, 2, 3, 4],
            lambda x: x % 2 == 0,
            lambda x: x ** 2
        )

    """
    filtered = [item for item in data if predicate(item)]
    return await par_map(filtered, mapper)


async def pipeline(data: list[Any]) -> RustInspiredPipeline:
    """Create pipeline for chaining operations."""
    return RustInspiredPipeline(data)


# Example: Multi-agent batch processing
async def multi_agent_parallel(
    queries: list[str],
    agents: list[str],
    tier: ParallelTier = ParallelTier.TIER_3_HEXAGRAMS,
) -> dict[str, list[str]]:
    """Process queries across multiple agents in parallel.

    Achieves high throughput through tiered parallelism.
    """
    from whitemagic.cascade.multi_agent import create_agent_team

    coordinator = await create_agent_team()
    executor = AdaptiveParallelExecutor()

    # Create task for each (query, agent) pair
    tasks = []
    for query in queries:
        for agent_name in agents:
            if agent_name in coordinator.agents:
                agent = coordinator.agents[agent_name]
                tasks.append(
                    ParallelTask(
                        id=f"{query[:20]}_{agent_name}",
                        func=agent.generate_response,
                        args=(query,),
                    ),
                )

    # Execute all in parallel using tier
    results = await executor.execute_parallel(tasks, tier=tier)

    # Organize by agent
    organized: dict[str, list[str]] = {agent: [] for agent in agents}
    idx = 0
    for query in queries:
        for agent in agents:
            if agent in coordinator.agents:
                organized[agent].append(results[idx])
                idx += 1

    return organized
