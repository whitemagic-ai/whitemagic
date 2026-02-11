"""Async/sync compatibility layer for WhiteMagic.
Enables gradual migration without breaking existing code.
"""

import asyncio
import atexit
import functools
from collections.abc import Callable, Coroutine
from concurrent.futures import ThreadPoolExecutor
from typing import Any, TypeVar, Union

T = TypeVar("T")

class AsyncCompat:
    """Manage async/sync compatibility."""

    _executor: ThreadPoolExecutor | None = None

    @classmethod
    def get_executor(cls) -> ThreadPoolExecutor:
        """Get or create thread pool executor."""
        if cls._executor is None:
            from whitemagic.config.concurrency import IO_WORKERS
            cls._executor = ThreadPoolExecutor(max_workers=IO_WORKERS)
            # Register shutdown hook to cleanup resources
            atexit.register(cls.shutdown_executor)
        return cls._executor

    @classmethod
    def shutdown_executor(cls) -> None:
        """Shutdown the executor."""
        if cls._executor is not None:
            cls._executor.shutdown(wait=True)
            cls._executor = None

def async_compat(func: Callable[..., T]) -> Callable[..., Union[T, Coroutine[Any, Any, T]]]:
    """Decorator that makes a function work in both sync and async contexts.

    Usage:
        @async_compat
        def process_memory(data):
            return expensive_operation(data)

        # Sync: result = process_memory(data)
        # Async: result = await process_memory(data)
    """
    if asyncio.iscoroutinefunction(func):
        @functools.wraps(func)
        def async_wrapper(*args: Any, **kwargs: Any) -> Any:
            try:
                asyncio.get_running_loop()
                # If we're already in a loop, return the coroutine directly
                return func(*args, **kwargs)
            except RuntimeError:
                # No running loop, safe to use asyncio.run
                return asyncio.run(func(*args, **kwargs))

        return async_wrapper

    @functools.wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        try:
            loop = asyncio.get_running_loop()
        except RuntimeError:
            # No event loop - run sync
            return func(*args, **kwargs)

        # In async context - run in executor
        executor = AsyncCompat.get_executor()
        return loop.run_in_executor(executor, lambda: func(*args, **kwargs))

    return wrapper

def ensure_async(func: Callable) -> Callable:
    """Ensure function is async, converting if necessary.

    Usage:
        @ensure_async
        async def async_func():
            pass

        # Or wrap sync function:
        async_version = ensure_async(sync_func)
    """
    if asyncio.iscoroutinefunction(func):
        return func

    @functools.wraps(func)
    async def async_wrapper(*args: Any, **kwargs: Any) -> Any:
        executor = AsyncCompat.get_executor()
        loop = asyncio.get_running_loop()
        return await loop.run_in_executor(executor, lambda: func(*args, **kwargs))

    return async_wrapper

def run_async(coro: Coroutine) -> Any:
    """Run coroutine in existing event loop or create new one.

    Useful for bridging async and sync code.
    """
    try:
        loop = asyncio.get_running_loop()
        return loop.create_task(coro)
    except RuntimeError:
        # No event loop - run to completion
        return asyncio.run(coro)

class AsyncContext:
    """Context manager for async operations."""

    def __init__(self) -> None:
        self._tasks: list[asyncio.Task] = []

    async def __aenter__(self) -> "AsyncContext":
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # Cancel all tasks on exit
        for task in self._tasks:
            if not task.done():
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass

    def add_task(self, coro: Coroutine) -> asyncio.Task:
        """Add task to be managed by this context."""
        task = asyncio.create_task(coro)
        self._tasks.append(task)
        return task

# Utility functions for common patterns
async def gather_with_concurrency(*coros: Coroutine, max_concurrent: int = 100) -> list[Any]:
    """Gather coroutines with concurrency limit.
    """
    semaphore = asyncio.Semaphore(max_concurrent)

    async def _sem_coro(coro: Coroutine) -> Any:
        async with semaphore:
            return await coro

    return await asyncio.gather(*[_sem_coro(c) for c in coros])

async def batch_process(items: list, processor: Callable, batch_size: int = 100) -> list[Any]:
    """Process items in batches for better performance.
    """
    results = []
    for i in range(0, len(items), batch_size):
        batch = items[i:i + batch_size]
        batch_results = await asyncio.gather(*[
            processor(item) for item in batch
        ])
        results.extend(batch_results)
    return results
