"""Cache module for WhiteMagic

Provides:
- Redis caching layer
- Cache management helpers
"""

from .redis import (
    CacheConfig,
    RedisCache,
    clear_redis_cache,
    get_redis_cache,
)

__all__ = [
    "CacheConfig",
    "RedisCache",
    "get_redis_cache",
    "clear_redis_cache",
]
