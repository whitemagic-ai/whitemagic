"""Global Concurrency configuration for WhiteMagic.

Centralized management of worker limits to prevent resource exhaustion.
"""

from typing import Any
import multiprocessing
import os


def get_safe_cpu_count() -> int:
    try:
        count = multiprocessing.cpu_count()
        # Reserve cores for OS and IDE
        return max(1, count - 2)
    except (ImportError, NotImplementedError):
        return 2

# Global Limit for Heavy Processes
# Increased to 4 based on asyncio testing showing CPU headroom (avg 45%, max 92%, system can handle more)
_env_max_workers = os.environ.get("WHITEMAGIC_MAX_WORKERS")
try:
    MAX_WORKERS = int(_env_max_workers) if _env_max_workers else 2
except ValueError:
    MAX_WORKERS = 2

# Process Pool Limits (Heavy - High Memory Overhead)
# Used for: CloneArmy search, Local Analysis, Test Runner
CPU_WORKERS = MAX_WORKERS
CLONE_ARMY_WORKERS = MAX_WORKERS
TEST_RUNNER_WORKERS = MAX_WORKERS

# Thread Pool Limits (Light - Low Memory Overhead)
# Used for: File I/O, Network calls, Gan Ying Events
# We can sustain many more threads than processes
IO_WORKERS = min(32, (multiprocessing.cpu_count() or 4) * 4)

# Asyncio Limits (Very Light - Zero Memory Overhead)
# Used for: 16,000+ Shadow Clones
ASYNC_TASK_LIMIT = 16000


def get_max_workers() -> Any:
    """Get maximum worker count for parallel operations.

    Respects WHITEMAGIC_MAX_WORKERS environment variable.
    Falls back to safe defaults based on CPU count.

    Returns:
        Maximum number of workers to use

    """
    import os
    # Check environment override
    env_workers = os.getenv("WHITEMAGIC_MAX_WORKERS")
    if env_workers:
        try:
            return int(env_workers)
        except ValueError:
            pass

    # Safe default based on CPU count
    cpu_count = get_safe_cpu_count()
    if cpu_count >= 8:
        return 2  # Conservative for resource-constrained systems
    if cpu_count >= 4:
        return max(1, cpu_count // 2)
    return 1

def get_concurrency_config() -> Any:
    """Get complete concurrency configuration."""
    import multiprocessing
    import os
    return {
        "max_workers": get_max_workers(),
        "cpu_count": multiprocessing.cpu_count(),
        "env_override": os.getenv("WHITEMAGIC_MAX_WORKERS") is not None,
    }
