"""Threading Pool Management - I Ching Aligned

Provides thread pool management with tiers aligned to I Ching hexagram counts.
Supports adaptive scaling and resource limits.

Philosophy:
- Tier 0: 8 threads (八卦 - 8 trigrams)
- Tier 1: 16 threads
- Tier 2: 32 threads
- Tier 3: 64 threads (六十四卦 - 64 hexagrams, sweet spot!)
- Tier 4: 128 threads
- Tier 5: 256 threads (maximum complexity)
"""

from __future__ import annotations

import asyncio
import functools
from collections.abc import Callable
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
from dataclasses import dataclass
from enum import Enum
from types import TracebackType
from typing import Any, TypeVar

T = TypeVar("T")


class ThreadingTier(Enum):
    """I Ching-aligned threading tiers."""

    TIER_0 = 8  # 八卦 (8 trigrams) - Minimal
    TIER_1 = 16  # Basic parallelism
    TIER_2 = 32  # Medium parallelism
    TIER_3 = 64  # 六十四卦 (64 hexagrams) - Optimal!
    TIER_4 = 128  # High parallelism
    TIER_5 = 256  # Maximum parallelism

    @classmethod
    def from_complexity(cls, complexity: int) -> ThreadingTier:
        """Select tier based on task complexity."""
        if complexity <= 10:
            return cls.TIER_0
        if complexity <= 25:
            return cls.TIER_1
        if complexity <= 50:
            return cls.TIER_2
        if complexity <= 100:
            return cls.TIER_3
        if complexity <= 200:
            return cls.TIER_4
        return cls.TIER_5


@dataclass
class PoolConfig:
    """Configuration for thread/process pools."""

    def __init__(
        self,
        io_workers: int | None = None,
        cpu_workers: int | None = None,
        api_workers: int | None = None,
        db_workers: int | None = None,
        **kwargs: Any,
    ) -> None:
        from whitemagic.config.concurrency import CPU_WORKERS, IO_WORKERS
        self.io_workers = io_workers or IO_WORKERS
        self.cpu_workers = cpu_workers or CPU_WORKERS
        self.api_workers = api_workers or (IO_WORKERS // 2)
        self.db_workers = db_workers or 4

        self.max_queue_size = kwargs.get("max_queue_size", 1000)
        self.timeout = kwargs.get("timeout", 300.0)
        self.enable_adaptive = kwargs.get("enable_adaptive", True)
        self.scale_threshold = kwargs.get("scale_threshold", 0.8)


class ThreadingManager:
    """Manages thread and process pools for parallel execution.

    Provides specialized pools for different workload types and
    handles lifecycle management.
    """

    def __init__(self, config: PoolConfig | None = None):
        """Initialize threading manager with optional configuration."""
        self.config = config or PoolConfig()

        # Thread pools for different workloads
        self._io_pool: ThreadPoolExecutor | None = None
        self._cpu_pool: ProcessPoolExecutor | None = None
        self._api_pool: ThreadPoolExecutor | None = None
        self._db_pool: ThreadPoolExecutor | None = None

        # State tracking
        self._active = False

    def start(self) -> None:
        """Start all thread pools."""
        if self._active:
            return

        self._io_pool = ThreadPoolExecutor(
            max_workers=self.config.io_workers, thread_name_prefix="wm-io-",
        )

        self._cpu_pool = ProcessPoolExecutor(max_workers=self.config.cpu_workers)

        self._api_pool = ThreadPoolExecutor(
            max_workers=self.config.api_workers, thread_name_prefix="wm-api-",
        )

        self._db_pool = ThreadPoolExecutor(
            max_workers=self.config.db_workers, thread_name_prefix="wm-db-",
        )

        self._active = True

    def shutdown(self, wait: bool = True) -> None:
        """Shutdown all thread pools."""
        if not self._active:
            return

        pools = [self._io_pool, self._cpu_pool, self._api_pool, self._db_pool]
        for pool in pools:
            if pool:
                pool.shutdown(wait=wait)

        self._io_pool = None
        self._cpu_pool = None
        self._api_pool = None
        self._db_pool = None
        self._active = False

    async def run_io_task(self, func: Callable[..., T], *args: Any, **kwargs: Any) -> T:
        """Run I/O-bound task in thread pool."""
        if not self._active:
            self.start()

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(self._io_pool, functools.partial(func, *args, **kwargs))

    async def run_cpu_task(self, func: Callable[..., T], *args: Any, **kwargs: Any) -> T:
        """Run CPU-bound task in process pool."""
        if not self._active:
            self.start()

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(self._cpu_pool, functools.partial(func, *args, **kwargs))

    async def run_api_task(self, func: Callable[..., T], *args: Any, **kwargs: Any) -> T:
        """Run API call in dedicated pool."""
        if not self._active:
            self.start()

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(self._api_pool, functools.partial(func, *args, **kwargs))

    async def run_db_task(self, func: Callable[..., T], *args: Any, **kwargs: Any) -> T:
        """Run database operation in dedicated pool."""
        if not self._active:
            self.start()

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(self._db_pool, functools.partial(func, *args, **kwargs))

    async def run_batch(
        self, tasks: list[tuple[Callable, tuple, dict]], pool_type: str = "io",
    ) -> list[Any]:
        """Run batch of tasks in parallel.

        Args:
            tasks: List of (func, args, kwargs) tuples
            pool_type: "io", "cpu", "api", or "db"

        Returns:
            List of results in same order as tasks

        """
        if pool_type == "io":
            runner = self.run_io_task
        elif pool_type == "cpu":
            runner = self.run_cpu_task
        elif pool_type == "api":
            runner = self.run_api_task
        elif pool_type == "db":
            runner = self.run_db_task
        else:
            raise ValueError(f"Invalid pool type: {pool_type}")

        # Create coroutines for all tasks
        coros = [runner(func, *args, **kwargs) for func, args, kwargs in tasks]

        # Execute all in parallel
        return await asyncio.gather(*coros, return_exceptions=True)

    def __enter__(self) -> "ThreadingManager":
        """Context manager entry."""
        self.start()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        """Context manager exit."""
        self.shutdown()

    @property
    def is_active(self) -> bool:
        """Check if pools are active."""
        return self._active
