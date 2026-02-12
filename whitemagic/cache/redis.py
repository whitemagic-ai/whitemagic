"""Redis Caching Layer for WhiteMagic

Provides:
- Redis connection management
- Cache decorators
- Query result caching
- Session storage
- Rate limiting support
"""

import json
import pickle
import hashlib
from typing import Any, Optional, Callable, TypeVar, cast
from functools import wraps
from dataclasses import dataclass

try:
    import redis
    from redis.asyncio import Redis as AsyncRedis
    from redis.exceptions import RedisError
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False

from whitemagic.logging_config import get_logger

logger = get_logger(__name__)

T = TypeVar('T')


@dataclass
class CacheConfig:
    """Redis cache configuration."""
    host: str = "localhost"
    port: int = 6379
    db: int = 0
    password: Optional[str] = None
    ssl: bool = False
    ssl_cert_reqs: str = "required"
    connection_pool_max_connections: int = 50
    socket_timeout: int = 5
    socket_connect_timeout: int = 5
    retry_on_timeout: bool = True
    health_check_interval: int = 30
    default_ttl: int = 3600  # 1 hour
    key_prefix: str = "whitemagic:"
    serializer: str = "json"  # json, pickle, msgpack


class RedisCache:
    """Redis cache manager."""

    def __init__(self, config: CacheConfig):
        self.config = config
        self._client: Optional[redis.Redis] = None
        self._async_client: Optional[AsyncRedis] = None
        self._connection_pool: Optional[redis.ConnectionPool] = None
        self._connected = False

        if REDIS_AVAILABLE:
            self._initialize()

    def _initialize(self) -> None:
        """Initialize Redis connection."""
        try:
            # Prepare connection pool arguments
            pool_kwargs = {
                "host": self.config.host,
                "port": self.config.port,
                "db": self.config.db,
                "password": self.config.password,
                "max_connections": self.config.connection_pool_max_connections,
                "socket_timeout": self.config.socket_timeout,
                "socket_connect_timeout": self.config.socket_connect_timeout,
                "retry_on_timeout": self.config.retry_on_timeout,
                "health_check_interval": self.config.health_check_interval,
                "decode_responses": False
            }

            # Only add SSL args if enabled to avoid library compatibility issues
            if self.config.ssl:
                pool_kwargs["ssl"] = True
                pool_kwargs["ssl_cert_reqs"] = self.config.ssl_cert_reqs

            # Create connection pool
            self._connection_pool = redis.ConnectionPool(**pool_kwargs)  # type: ignore[arg-type]

            # Create sync client
            self._client = redis.Redis(connection_pool=self._connection_pool)

            # Create async client
            self._async_client = AsyncRedis(connection_pool=self._connection_pool)  # type: ignore[arg-type]

            # Test connection
            self._client.ping()
            self._connected = True
            logger.info(f"Connected to Redis at {self.config.host}:{self.config.port}")

        except Exception as e:
            logger.error(f"Failed to connect to Redis: {e}")
            self._connected = False

    def is_connected(self) -> bool:
        """Check if Redis is connected."""
        if not self._client:
            return False

        try:
            self._client.ping()
            return True
        except Exception:
            return False

    def ping(self) -> bool:
        """Ping Redis to verify connection."""
        if not self._client:
            return False
        try:
            return bool(self._client.ping())
        except Exception:
            return False

    def _serialize(self, value: Any) -> bytes:
        """Serialize value for storage."""
        if self.config.serializer == "json":
            return json.dumps(value, default=str).encode('utf-8')
        elif self.config.serializer == "pickle":
            return pickle.dumps(value)
        else:
            raise ValueError(f"Unsupported serializer: {self.config.serializer}")

    def _deserialize(self, value: bytes) -> Any:
        """Deserialize value from storage."""
        if self.config.serializer == "json":
            return json.loads(value.decode('utf-8'))
        elif self.config.serializer == "pickle":
            # Restricted unpickling for security
            import io
            import pickle

            class RestrictedUnpickler(pickle.Unpickler):
                def find_class(self, module: str, name: str) -> type:  # type: ignore[override]
                    # Only allow safe classes
                    if module == "builtins" and name in ("dict", "list", "str", "int", "float", "bool", "None"):
                        return type(getattr(__builtins__, name))
                    if module == "collections" and name in ("defaultdict", "OrderedDict", "Counter"):
                        return type(getattr(__import__(module), name))
                    if module == "datetime" and name in ("datetime", "timedelta"):
                        return type(getattr(__import__(module), name))
                    raise pickle.UnpicklingError(f"Forbidden: {module}.{name}")

            return RestrictedUnpickler(io.BytesIO(value)).load()
        else:
            raise ValueError(f"Unsupported serializer: {self.config.serializer}")

    def _make_key(self, key: str) -> str:
        """Create full key with prefix."""
        return f"{self.config.key_prefix}{key}"

    def get(self, key: str, default: Any = None) -> Any:
        """Get value from cache."""
        if not self._connected:
            return default

        try:
            full_key = self._make_key(key)
            assert self._client is not None
            value = self._client.get(full_key)

            if value is None:
                return default

            return self._deserialize(value)  # type: ignore[arg-type]

        except RedisError as e:
            logger.error(f"Cache get error: {e}")
            return default

    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> bool:
        """Set value in cache."""
        if not self._connected:
            return False

        try:
            full_key = self._make_key(key)
            serialized = self._serialize(value)

            if ttl is None:
                ttl = self.config.default_ttl

            assert self._client is not None
            return bool(self._client.setex(full_key, ttl, serialized))

        except RedisError as e:
            logger.error(f"Cache set error: {e}")
            return False

    def delete(self, key: str) -> bool:
        """Delete key from cache."""
        if not self._connected:
            return False

        try:
            full_key = self._make_key(key)
            assert self._client is not None
            return bool(self._client.delete(full_key))

        except RedisError as e:
            logger.error(f"Cache delete error: {e}")
            return False

    def exists(self, key: str) -> bool:
        """Check if key exists in cache."""
        if not self._connected:
            return False

        try:
            full_key = self._make_key(key)
            assert self._client is not None
            return bool(self._client.exists(full_key))

        except RedisError as e:
            logger.error(f"Cache exists error: {e}")
            return False

    def expire(self, key: str, ttl: int) -> bool:
        """Set TTL for existing key."""
        if not self._connected:
            return False

        try:
            full_key = self._make_key(key)
            assert self._client is not None
            return bool(self._client.expire(full_key, ttl))

        except RedisError as e:
            logger.error(f"Cache expire error: {e}")
            return False

    def ttl(self, key: str) -> int:
        """Get TTL for key."""
        if not self._connected:
            return -1

        try:
            full_key = self._make_key(key)
            assert self._client is not None
            return int(self._client.ttl(full_key))  # type: ignore[arg-type]

        except RedisError as e:
            logger.error(f"Cache TTL error: {e}")
            return -1

    def clear(self, pattern: Optional[str] = None) -> int:
        """Clear cache keys by pattern."""
        if not self._connected:
            return 0

        try:
            if pattern:
                full_pattern = self._make_key(pattern)
                assert self._client is not None
                keys = self._client.keys(full_pattern)
            else:
                full_pattern = self._make_key("*")
                assert self._client is not None
                keys = self._client.keys(full_pattern)

            if keys:
                assert self._client is not None
                return int(self._client.delete(*keys))  # type: ignore[arg-type,misc]
            return 0

        except RedisError as e:
            logger.error(f"Cache clear error: {e}")
            return 0

    # Async methods
    async def aget(self, key: str, default: Any = None) -> Any:
        """Async get value from cache."""
        if not self._async_client:
            return default

        try:
            full_key = self._make_key(key)
            value = await self._async_client.get(full_key)

            if value is None:
                return default

            return self._deserialize(value)

        except RedisError as e:
            logger.error(f"Async cache get error: {e}")
            return default

    async def aset(self, key: str, value: Any, ttl: Optional[int] = None) -> bool:
        """Async set value in cache."""
        if not self._async_client:
            return False

        try:
            full_key = self._make_key(key)
            serialized = self._serialize(value)

            if ttl is None:
                ttl = self.config.default_ttl

            return bool(await self._async_client.setex(full_key, ttl, serialized))

        except RedisError as e:
            logger.error(f"Async cache set error: {e}")
            return False

    # Cache decorators
    def cache(self, ttl: Optional[int] = None, key_prefix: str = "") -> Callable:
        """Decorator to cache function results."""
        def decorator(func: Callable[..., T]) -> Callable[..., T]:
            @wraps(func)
            def wrapper(*args: Any, **kwargs: Any) -> T:
                # Generate cache key from function name and arguments
                key_parts = [key_prefix, func.__name__]
                key_parts.extend(str(arg) for arg in args)
                key_parts.extend(f"{k}={v}" for k, v in sorted(kwargs.items()))
                cache_key = hashlib.md5(":".join(key_parts).encode()).hexdigest()

                # Try to get from cache
                cached = self.get(cache_key)
                if cached is not None:
                    return cast(T, cached)

                # Call function and cache result
                result = func(*args, **kwargs)
                self.set(cache_key, result, ttl)
                return result
            return wrapper
        return decorator


# Global cache instance
_redis_cache: Optional[RedisCache] = None


def get_redis_cache(config: Optional[CacheConfig] = None) -> RedisCache:
    """Get or create global Redis cache instance."""
    global _redis_cache

    if _redis_cache is None:
        if config is None:
            config = CacheConfig()
        _redis_cache = RedisCache(config)

    return _redis_cache


def clear_redis_cache() -> None:
    """Clear the global Redis cache instance."""
    global _redis_cache
    if _redis_cache:
        _redis_cache.clear()
        _redis_cache = None
