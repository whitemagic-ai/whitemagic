"""Adaptive Threading Controller - Dynamic Resource Management

Automatically adjusts threading levels based on system load and task complexity.
Implements I Ching-aligned scaling (8→16→32→64→128→256).

Philosophy:
- Start conservative (8-16 threads)
- Scale up based on load
- Scale down when idle
- Respect system limits
"""

from __future__ import annotations

from dataclasses import dataclass

import psutil

from whitemagic.parallel.pools import PoolConfig, ThreadingTier


@dataclass
class SystemMetrics:
    """Current system resource metrics."""

    cpu_percent: float
    memory_percent: float
    active_threads: int
    load_average: float

    @classmethod
    def current(cls) -> SystemMetrics:
        """Get current system metrics."""
        import os

        return cls(
            cpu_percent=psutil.cpu_percent(interval=0.1),
            memory_percent=psutil.virtual_memory().percent,
            active_threads=psutil.Process(os.getpid()).num_threads(),
            load_average=psutil.getloadavg()[0] if hasattr(psutil, "getloadavg") else 0.0,
        )


class AdaptiveThreadingController:
    """Dynamically adjusts threading based on system state.

    Monitors CPU, memory, and task queue to optimize thread count.
    Uses I Ching-aligned tiers for philosophical consistency.
    """

    def __init__(
        self,
        initial_tier: ThreadingTier = ThreadingTier.TIER_1,
        cpu_threshold: float = 80.0,
        memory_threshold: float = 85.0,
    ):
        """Initialize adaptive controller.

        Args:
            initial_tier: Starting threading tier
            cpu_threshold: CPU % to trigger scale-down
            memory_threshold: Memory % to trigger scale-down

        """
        self.current_tier = initial_tier
        self.cpu_threshold = cpu_threshold
        self.memory_threshold = memory_threshold

        # History for decisions
        self._metrics_history: list = []
        self._max_history = 10

    def recommend_tier(self, task_count: int, task_complexity: int = 50) -> ThreadingTier:
        """Recommend threading tier based on current state.

        Args:
            task_count: Number of pending tasks
            task_complexity: Estimated complexity (1-100)

        Returns:
            Recommended ThreadingTier

        """
        # Get current metrics
        metrics = SystemMetrics.current()

        # Store in history
        self._metrics_history.append(metrics)
        if len(self._metrics_history) > self._max_history:
            self._metrics_history.pop(0)

        # Check if we should scale down
        if (
            metrics.cpu_percent > self.cpu_threshold
            or metrics.memory_percent > self.memory_threshold
        ):
            # Scale down one tier if possible
            return self._scale_down()

        # Recommend tier based on task count and complexity
        if task_count < 10:
            recommended = ThreadingTier.TIER_0
        elif task_count < 25:
            recommended = ThreadingTier.TIER_1
        elif task_count < 50:
            recommended = ThreadingTier.TIER_2
        elif task_count < 100:
            recommended = ThreadingTier.TIER_3
        elif task_count < 200:
            recommended = ThreadingTier.TIER_4
        else:
            recommended = ThreadingTier.TIER_5

        # Adjust for complexity
        if task_complexity > 75:
            # High complexity, scale down
            tier_value = max(0, list(ThreadingTier).index(recommended) - 1)
            recommended = list(ThreadingTier)[tier_value]
        elif task_complexity < 25:
            # Low complexity, can scale up
            tier_value = min(len(ThreadingTier) - 1, list(ThreadingTier).index(recommended) + 1)
            recommended = list(ThreadingTier)[tier_value]

        self.current_tier = recommended
        return recommended

    def _scale_down(self) -> ThreadingTier:
        """Scale down one tier."""
        tiers = list(ThreadingTier)
        current_index = tiers.index(self.current_tier)

        if current_index > 0:
            return tiers[current_index - 1]
        return self.current_tier

    def _scale_up(self) -> ThreadingTier:
        """Scale up one tier."""
        tiers = list(ThreadingTier)
        current_index = tiers.index(self.current_tier)

        if current_index < len(tiers) - 1:
            return tiers[current_index + 1]
        return self.current_tier

    def get_pool_config(self) -> PoolConfig:
        """Get pool configuration for current tier."""
        workers = self.current_tier.value

        return PoolConfig(
            io_workers=workers,
            cpu_workers=max(4, workers // 4),
            api_workers=max(8, workers // 2),
            db_workers=max(4, workers // 8),
        )

    @property
    def avg_cpu_usage(self) -> float:
        """Get average CPU usage from history."""
        if not self._metrics_history:
            return 0.0
        return float(sum(m.cpu_percent for m in self._metrics_history) / len(self._metrics_history))

    @property
    def avg_memory_usage(self) -> float:
        """Get average memory usage from history."""
        if not self._metrics_history:
            return 0.0
        return float(sum(m.memory_percent for m in self._metrics_history) / len(self._metrics_history))
