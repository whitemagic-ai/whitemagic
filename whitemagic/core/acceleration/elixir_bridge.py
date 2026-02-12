"""Elixir OTP Bridge — Python ↔ Elixir via Redis Pub/Sub.
=======================================================
Bridges to Elixir OTP modules for event-driven processing. The Elixir
application runs as a separate BEAM node; Python communicates via Redis.

Elixir modules:
- CascadeExecutor — Parallel batch tool execution (GenServer pool)
- GardenPubSub — Garden event broadcasting (Registry PubSub)
- HarmonyMonitor — 7-dimension health monitoring (OTP supervised)
- RedisBridge — Bidirectional event bridging with backpressure

Falls back to pure Python when Elixir/Redis is not available.

Usage:
    from whitemagic.core.acceleration.elixir_bridge import (
        elixir_cascade_execute, elixir_publish_garden_event,
        elixir_harmony_status, elixir_bridge_status
    )
"""
from __future__ import annotations

import json
import logging
import os
import threading
import time
from typing import Any

logger = logging.getLogger(__name__)

_redis_client: Any | None = None
_redis_lock = threading.Lock()
_HAS_ELIXIR = False
_ELIXIR_CHANNEL = "whitemagic_elixir"
_RESPONSE_TIMEOUT = 5.0


def _get_redis() -> Any:
    """Lazy-load Redis client for Elixir bridge."""
    global _redis_client, _HAS_ELIXIR
    if _redis_client is not None:
        return _redis_client
    with _redis_lock:
        if _redis_client is not None:
            return _redis_client
        try:
            import redis
            url = os.environ.get("REDIS_URL", "redis://localhost:6379")
            client = redis.from_url(url, decode_responses=True)
            client.ping()

            # Check if Elixir node is responding
            test_channel = f"{_ELIXIR_CHANNEL}:ping"
            client.publish(test_channel, json.dumps({"type": "ping"}))

            _redis_client = client
            _HAS_ELIXIR = True
            logger.info("Elixir bridge connected via Redis: %s", url)
            return client
        except Exception as e:
            logger.debug("Elixir bridge unavailable (Redis not connected): %s", e)
            return None


def _send_elixir_command(module: str, command: str, payload: dict[str, Any]) -> dict[str, Any] | None:
    """Send a command to an Elixir module via Redis and await response."""
    client = _get_redis()
    if client is None:
        return None

    request_id = f"{module}_{command}_{time.time_ns()}"
    message = {
        "module": module,
        "command": command,
        "payload": payload,
        "request_id": request_id,
        "_origin": "python_bridge",
    }

    try:
        response_channel = f"{_ELIXIR_CHANNEL}:response:{request_id}"
        pubsub = client.pubsub()
        pubsub.subscribe(response_channel)

        client.publish(f"{_ELIXIR_CHANNEL}:commands", json.dumps(message))

        # Wait for response with timeout
        deadline = time.monotonic() + _RESPONSE_TIMEOUT
        while time.monotonic() < deadline:
            msg = pubsub.get_message(timeout=0.5)
            if msg and msg["type"] == "message":
                pubsub.unsubscribe(response_channel)
                parsed = json.loads(msg["data"])
                if isinstance(parsed, dict):
                    return parsed

        pubsub.unsubscribe(response_channel)
        logger.debug("Elixir %s.%s timed out after %.1fs", module, command, _RESPONSE_TIMEOUT)
    except Exception as e:
        logger.debug("Elixir command %s.%s failed: %s", module, command, e)

    return None


# ---------------------------------------------------------------------------
# CascadeExecutor — Parallel batch tool execution
# ---------------------------------------------------------------------------

def elixir_cascade_execute(
    tool_name: str,
    args: dict[str, Any],
    timeout_ms: int = 30000,
    priority: str = "normal",
) -> dict[str, Any] | None:
    """Execute a tool via Elixir CascadeExecutor GenServer pool.

    Returns:
        Dict with execution result, or None if Elixir unavailable.

    """
    return _send_elixir_command("cascade_executor", "execute", {
        "tool_name": tool_name,
        "args": args,
        "timeout_ms": timeout_ms,
        "priority": priority,
    })


def elixir_cascade_pipeline(
    tasks: list[dict[str, Any]],
    mode: str = "parallel",
    max_failures: int = -1,
) -> dict[str, Any] | None:
    """Execute a pipeline of tasks via Elixir CascadeExecutor.

    Args:
        tasks: List of {tool_name, args} dicts.
        mode: "parallel", "sequential", or "fan_out_fan_in".
        max_failures: Max failures before abort (-1 = no limit).

    Returns:
        Dict with pipeline results, or None.

    """
    if max_failures < 0:
        max_failures = len(tasks)
    return _send_elixir_command("cascade_executor", "execute_pipeline", {
        "tasks": tasks,
        "mode": mode,
        "max_failures": max_failures,
    })


# ---------------------------------------------------------------------------
# GardenPubSub — Garden event broadcasting
# ---------------------------------------------------------------------------

def elixir_publish_garden_event(
    garden_name: str,
    event_type: str,
    payload: dict[str, Any] | None = None,
) -> dict[str, Any] | None:
    """Publish an event to a garden's PubSub topic via Elixir.

    Returns:
        Dict with subscriber count and delivery status, or None.

    """
    return _send_elixir_command("garden_pubsub", "publish", {
        "garden": garden_name,
        "event_type": event_type,
        "payload": payload or {},
    })


def elixir_subscribe_garden(garden_name: str) -> dict[str, Any] | None:
    """Subscribe to a garden's event topic."""
    return _send_elixir_command("garden_pubsub", "subscribe", {
        "garden": garden_name,
    })


# ---------------------------------------------------------------------------
# HarmonyMonitor — 7-dimension health monitoring
# ---------------------------------------------------------------------------

def elixir_harmony_status() -> dict[str, Any] | None:
    """Get Harmony Vector status from Elixir's OTP-supervised monitor.

    Returns:
        Dict with 7-dimension health snapshot, or None.

    """
    return _send_elixir_command("harmony_monitor", "status", {})


def elixir_harmony_check() -> dict[str, Any] | None:
    """Run a deep health check via Elixir's HarmonyMonitor.

    Returns:
        Dict with check results and recommendations, or None.

    """
    return _send_elixir_command("harmony_monitor", "deep_check", {})


# ---------------------------------------------------------------------------
# RedisBridge — Event bridging stats
# ---------------------------------------------------------------------------

def elixir_redis_bridge_stats() -> dict[str, Any] | None:
    """Get statistics from the Elixir Redis bridge."""
    return _send_elixir_command("redis_bridge", "stats", {})


def elixir_redis_bridge_health() -> dict[str, Any] | None:
    """Get health status from the Elixir Redis bridge."""
    return _send_elixir_command("redis_bridge", "health", {})


# ---------------------------------------------------------------------------
# Status
# ---------------------------------------------------------------------------

def elixir_bridge_status() -> dict[str, Any]:
    """Get Elixir bridge status."""
    _get_redis()
    return {
        "has_elixir": _HAS_ELIXIR,
        "redis_connected": _redis_client is not None,
        "channel": _ELIXIR_CHANNEL,
        "modules": {
            "cascade_executor": _HAS_ELIXIR,
            "garden_pubsub": _HAS_ELIXIR,
            "harmony_monitor": _HAS_ELIXIR,
            "redis_bridge": _HAS_ELIXIR,
        },
        "backend": "elixir_otp" if _HAS_ELIXIR else "python_fallback",
    }
