"""Broker tool handlers — async Redis message broker for AI agent coordination.

Provides pub/sub messaging, history retrieval, and broker status via the
Whitemagic tool contract.  Redis is an optional dependency (``whitemagic[cache]``).
"""
import asyncio
import json
import os
import socket
import time
from collections.abc import Coroutine
from datetime import datetime
from typing import Any, Optional, TypeVar


def _emit(event_type_name: str, data: dict[str, Any]) -> None:
    """Best-effort Gan Ying event emission."""
    try:
        from whitemagic.core.resonance import emit_event
        emit_event(event_type_name, data, source="broker")
    except Exception:
        pass


# ---------------------------------------------------------------------------
# Lazy Redis import helper
# ---------------------------------------------------------------------------

def _require_redis() -> Any:
    """Import ``redis.asyncio`` or raise a clear error."""
    try:
        import redis.asyncio as aioredis
        return aioredis
    except ImportError:
        raise ImportError(
            "Redis is required for broker tools. "
            "Install with: pip install 'whitemagic[cache]'",
        )


# ---------------------------------------------------------------------------
# Internal async broker (singleton-ish, lazily created)
# ---------------------------------------------------------------------------

_BROKER_INSTANCE: Optional["_AsyncBroker"] = None
_BROKER_LOCK = asyncio.Lock() if hasattr(asyncio, "Lock") else None


class _AsyncBroker:
    """Lightweight async Redis broker used by handler functions."""

    def __init__(self, host: str = "localhost", port: int = 6379, pool_size: int = 50) -> None:
        self.host = host
        self.port = port
        self.pool_size = pool_size
        self.redis: Any = None
        self._connect_lock = asyncio.Lock()

    async def connect(self) -> None:
        async with self._connect_lock:
            if self.redis is None:
                aioredis = _require_redis()
                connect_timeout = float(os.getenv("WHITEMAGIC_REDIS_CONNECT_TIMEOUT", "1.0"))
                socket_timeout = float(os.getenv("WHITEMAGIC_REDIS_SOCKET_TIMEOUT", "1.0"))
                pool = aioredis.ConnectionPool(
                    host=self.host,
                    port=self.port,
                    decode_responses=True,
                    max_connections=self.pool_size,
                    socket_connect_timeout=connect_timeout,
                    socket_timeout=socket_timeout,
                    retry_on_timeout=False,
                )
                self.redis = aioredis.Redis(connection_pool=pool)

    async def disconnect(self) -> None:
        if self.redis:
            await self.redis.close()
            self.redis = None

    async def publish(self, channel: str, message: dict[str, Any]) -> str:
        if not self.redis:
            await self.connect()
        redis = self.redis
        if redis is None:
            raise RuntimeError("Redis client unavailable after connect")
        message["timestamp"] = datetime.now().isoformat()
        msg_id = f"{channel}_{time.time()}"
        message["id"] = msg_id
        serialized = json.dumps(message)
        await asyncio.gather(
            redis.publish(channel, serialized),
            redis.lpush(f"history:{channel}", serialized),
        )
        asyncio.create_task(redis.ltrim(f"history:{channel}", 0, 99))
        return msg_id

    async def history(self, channel: str, limit: int = 20) -> list[dict[str, Any]]:
        if not self.redis:
            await self.connect()
        redis = self.redis
        if redis is None:
            raise RuntimeError("Redis client unavailable after connect")
        raw = await redis.lrange(f"history:{channel}", 0, limit - 1)
        results: list[dict[str, Any]] = []
        for item in raw:
            try:
                results.append(json.loads(item))
            except (json.JSONDecodeError, TypeError):
                pass
        return results

    async def status(self) -> dict[str, Any]:
        if not self.redis:
            await self.connect()
        redis = self.redis
        if redis is None:
            raise RuntimeError("Redis client unavailable after connect")
        info = await redis.info(section="server")
        clients = await redis.info(section="clients")
        return {
            "connected": True,
            "host": self.host,
            "port": self.port,
            "redis_version": info.get("redis_version", "unknown"),
            "connected_clients": clients.get("connected_clients", 0),
            "uptime_seconds": info.get("uptime_in_seconds", 0),
        }


async def _get_broker(host: str = "localhost", port: int = 6379) -> _AsyncBroker:
    global _BROKER_INSTANCE
    if _BROKER_INSTANCE is None:
        _BROKER_INSTANCE = _AsyncBroker(host=host, port=port)
        await _BROKER_INSTANCE.connect()
    return _BROKER_INSTANCE


T = TypeVar("T")


def _run(coro: Coroutine[Any, Any, T]) -> T:
    """Run an async coroutine from a sync handler context."""
    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(coro)
    # If we're already in an event loop (e.g. MCP stdio), use a thread.
    from concurrent.futures import ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=1) as pool:
        return pool.submit(asyncio.run, coro).result()


# ---------------------------------------------------------------------------
# Public handler functions (called by dispatch table)
# ---------------------------------------------------------------------------

def handle_broker_publish(**kwargs: Any) -> dict[str, Any]:
    """Publish a message to a Redis channel."""
    channel = kwargs.get("channel")
    if not channel:
        return {"status": "error", "error": "channel is required"}
    host = kwargs.get("host", "localhost")
    port = int(kwargs.get("port", 6379))
    probe_timeout = float(kwargs.get("probe_timeout", os.getenv("WHITEMAGIC_REDIS_PROBE_TIMEOUT", "0.5")))

    try:
        with socket.create_connection((host, port), timeout=probe_timeout):
            pass
    except OSError:
        return {
            "status": "error",
            "error": f"Redis {host}:{port} is unreachable",
            "error_code": "unreachable",
            "connected": False,
            "host": host,
            "port": port,
        }

    message: dict[str, Any] = kwargs.get("message", {})
    if isinstance(message, str):
        message = {"content": message}
    sender = kwargs.get("sender")
    if sender:
        message["sender"] = sender
    priority = kwargs.get("priority", "normal")
    message["priority"] = priority

    async def _do() -> str:
        broker = await _get_broker(host=host, port=port)
        msg_id = await broker.publish(channel, message)
        return msg_id

    try:
        msg_id = _run(_do())
        _emit("BROKER_MESSAGE_PUBLISHED", {"channel": channel, "msg_id": msg_id, "sender": sender})
        return {
            "status": "success",
            "message": f"Published to {channel}",
            "msg_id": msg_id,
            "channel": channel,
        }
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}
    except Exception as exc:
        _emit("BROKER_DISCONNECTED", {"error": str(exc)})
        return {"status": "error", "error": str(exc)}


def handle_broker_history(**kwargs: Any) -> dict[str, Any]:
    """Retrieve recent message history from a channel."""
    channel = kwargs.get("channel")
    if not channel:
        return {"status": "error", "error": "channel is required"}
    limit = kwargs.get("limit", 20)
    host = kwargs.get("host", "localhost")
    port = int(kwargs.get("port", 6379))
    probe_timeout = float(kwargs.get("probe_timeout", os.getenv("WHITEMAGIC_REDIS_PROBE_TIMEOUT", "0.5")))

    try:
        with socket.create_connection((host, port), timeout=probe_timeout):
            pass
    except OSError:
        return {
            "status": "error",
            "error": f"Redis {host}:{port} is unreachable",
            "error_code": "unreachable",
            "connected": False,
            "host": host,
            "port": port,
            "channel": channel,
        }

    async def _do() -> list[dict[str, Any]]:
        broker = await _get_broker(host=host, port=port)
        return await broker.history(channel, limit=limit)

    try:
        messages = _run(_do())
        return {
            "status": "success",
            "channel": channel,
            "count": len(messages),
            "messages": messages,
        }
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}
    except Exception as exc:
        return {"status": "error", "error": str(exc)}


def handle_broker_status(**kwargs: Any) -> dict[str, Any]:
    """Check Redis broker connectivity and status."""
    host = kwargs.get("host", "localhost")
    port = int(kwargs.get("port", 6379))
    probe_timeout = float(kwargs.get("probe_timeout", os.getenv("WHITEMAGIC_REDIS_PROBE_TIMEOUT", "0.5")))
    timeout = float(kwargs.get("timeout", os.getenv("WHITEMAGIC_BROKER_STATUS_TIMEOUT", "1.5")))

    async def _do() -> dict[str, Any]:
        broker = await asyncio.wait_for(_get_broker(host=host, port=port), timeout=timeout)
        return await asyncio.wait_for(broker.status(), timeout=timeout)

    try:
        with socket.create_connection((host, port), timeout=probe_timeout):
            pass
    except OSError:
        return {
            "status": "error",
            "error": f"Redis {host}:{port} is unreachable",
            "error_code": "unreachable",
            "connected": False,
            "host": host,
            "port": port,
        }

    try:
        info = _run(_do())
        return {"status": "success", **info}
    except asyncio.TimeoutError:
        return {
            "status": "error",
            "error": f"Broker status timed out after {timeout}s",
            "error_code": "timeout",
            "connected": False,
        }
    except ImportError as exc:
        return {
            "status": "error",
            "error": str(exc),
            "error_code": "missing_dependency",
            "connected": False,
        }
    except Exception as exc:
        return {"status": "error", "error": str(exc), "connected": False}
