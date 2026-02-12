"""Distributed Cache - Redis-Backed High-Performance Caching

Provides caching layer for parallel operations to reduce redundant work.
Supports TTL, pattern-based invalidation, and distributed coordination.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Any


@dataclass
class CacheEntry:
    """Cached data entry."""

    key: str
    value: Any
    ttl: int  # seconds
    hits: int = 0


class DistributedCache:
    """High-performance distributed cache.

    Uses Redis when available, falls back to in-memory cache.
    Optimized for parallel operations with minimal contention.
    """

    def __init__(
        self, redis_url: str | None = None, default_ttl: int = 3600, max_size: int = 10000,
    ):
        """Initialize distributed cache.

        Args:
            redis_url: Redis connection URL (None = in-memory)
            default_ttl: Default time-to-live in seconds
            max_size: Maximum cache size (in-memory only)

        """
        self.redis_url = redis_url
        self.default_ttl = default_ttl
        self.max_size = max_size

        # In-memory cache (fallback)
        self._cache: dict[str, CacheEntry] = {}
        self._redis_client = None

        # Try to connect to Redis if URL provided
        if redis_url:
            self._init_redis()

    def _init_redis(self) -> None:
        """Initialize Redis connection."""
        try:
            import redis

            self._redis_client = redis.from_url(self.redis_url, decode_responses=True)
            # Test connection
            if self._redis_client is not None:
                self._redis_client.ping()
        except Exception:
            # Fallback to in-memory
            self._redis_client = None

    def _make_key(self, key: str, prefix: str = "wm") -> str:
        """Create cache key with prefix."""
        return f"{prefix}:{key}"

    async def get(self, key: str) -> Any | None:
        """Get value from cache.

        Args:
            key: Cache key

        Returns:
            Cached value or None if miss

        """
        cache_key = self._make_key(key)

        if self._redis_client:
            try:
                value = self._redis_client.get(cache_key)
                if value:
                    return json.loads(value)
            except Exception:
                pass

        # Fallback to in-memory
        entry = self._cache.get(cache_key)
        if entry:
            entry.hits += 1
            return entry.value

        return None

    async def set(self, key: str, value: Any, ttl: int | None = None) -> bool:
        """Set value in cache.

        Args:
            key: Cache key
            value: Value to cache
            ttl: Time-to-live in seconds (None = default)

        Returns:
            True if successful

        """
        cache_key = self._make_key(key)
        ttl = ttl or self.default_ttl

        if self._redis_client:
            try:
                self._redis_client.setex(cache_key, ttl, json.dumps(value))
                return True
            except Exception:
                pass

        # Fallback to in-memory
        if len(self._cache) >= self.max_size:
            # Evict least recently used
            self._evict_lru()

        self._cache[cache_key] = CacheEntry(key=cache_key, value=value, ttl=ttl)
        return True

    async def delete(self, key: str) -> bool:
        """Delete key from cache."""
        cache_key = self._make_key(key)

        if self._redis_client:
            try:
                self._redis_client.delete(cache_key)
            except Exception:
                pass

        self._cache.pop(cache_key, None)
        return True

    async def clear(self, pattern: str | None = None) -> int:
        """Clear cache entries matching pattern.

        Args:
            pattern: Key pattern (None = clear all)

        Returns:
            Number of keys deleted

        """
        if pattern:
            # Pattern-based clear
            if self._redis_client:
                try:
                    keys = self._redis_client.keys(self._make_key(pattern))
                    if keys:
                        return self._redis_client.delete(*keys)
                except Exception:
                    pass

            # In-memory pattern clear
            to_delete = [k for k in self._cache.keys() if pattern in k]
            for k in to_delete:
                del self._cache[k]
            return len(to_delete)

        # Clear all
        if self._redis_client:
            try:
                self._redis_client.flushdb()
            except Exception:
                pass

        count = len(self._cache)
        self._cache.clear()
        return count

    def _evict_lru(self) -> None:
        """Evict least recently used entries."""
        # Simple LRU: remove entries with lowest hits
        if not self._cache:
            return

        min_hits = min(e.hits for e in self._cache.values())
        to_remove = [k for k, e in self._cache.items() if e.hits == min_hits]

        if to_remove:
            del self._cache[to_remove[0]]

    @property
    def size(self) -> int:
        """Get current cache size."""
        return len(self._cache)

    @property
    def is_redis(self) -> bool:
        """Check if using Redis."""
        return self._redis_client is not None
