"""Safety mechanisms for WhiteMagic.

Provides resource limiting and system overload protection.
Stdlib-only — no psutil dependency.
"""

from .resource_limiter import (
    ResourceLimiter,
    ResourceLimits,
    get_safe_limits,
    resource_guard,
)

__all__ = [
    "ResourceLimiter",
    "ResourceLimits",
    "get_safe_limits",
    "resource_guard",
]
