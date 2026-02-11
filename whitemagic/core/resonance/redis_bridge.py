from __future__ import annotations

import json
import logging
import os
import threading
from datetime import datetime
from typing import Any

import redis

from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)

logger = logging.getLogger(__name__)

# Event types worth publishing to Redis for multi-machine coordination
_OUTBOUND_EVENT_TYPES = {
    EventType.TASK_CREATED,
    EventType.TASK_COMPLETED,
    EventType.TASK_FAILED,
    EventType.VOTE_SESSION_CREATED,
    EventType.VOTE_CAST,
    EventType.VOTE_CONSENSUS_REACHED,
    EventType.VOTE_SESSION_CLOSED,
    EventType.BROKER_MESSAGE_PUBLISHED,
    EventType.AGENT_REGISTERED,
    EventType.AGENT_HEARTBEAT,
    EventType.AGENT_DEREGISTERED,
    EventType.SYSTEM_HEALTH_CHANGED,
    EventType.WARNING_ISSUED,
    EventType.BOUNDARY_VIOLATED,
}


class RedisBridge:
    """Bridges the external Redis 'ganying' channel with the internal Python Gan Ying Bus.

    Bidirectional flow:
    Redis ('ganying' channel) <-> RedisBridge <-> GanYingBus <-> Python Components

    Inbound:  Redis messages are parsed and emitted to the internal bus.
    Outbound: Selected bus events are published to Redis for other nodes.
    Source tagging prevents echo loops.
    """

    _BRIDGE_SOURCE_TAG = "_redis_bridge_"  # Prevents echo loops

    def __init__(self, redis_url: str | None = None, channel: str = "ganying",
                 enable_outbound: bool = True) -> None:
        configured_url = redis_url or os.getenv("REDIS_URL", "redis://localhost:6379")
        self.redis_url: str = configured_url or "redis://localhost:6379"
        self.channel = channel
        self.bus = get_bus()
        self._stop_event = threading.Event()
        self._thread: threading.Thread | None = None
        self.redis_client: redis.Redis | None = None
        self._pub_client: redis.Redis | None = None
        self._enable_outbound = enable_outbound
        self._outbound_count = 0
        self._inbound_count = 0

    def start(self) -> None:
        """Start listening to Redis in a background thread."""
        if self._thread and self._thread.is_alive():
            logger.warning("RedisBridge is already running")
            return

        try:
            self.redis_client = redis.Redis.from_url(self.redis_url, decode_responses=True)
            self.redis_client.ping() # Test connection
            logger.info(f"✅ RedisBridge connected to {self.redis_url}")
        except Exception as e:
            logger.error(f"❌ RedisBridge failed to connect to Redis: {e}")
            return

        self._stop_event.clear()
        self._thread = threading.Thread(target=self._listen_loop, daemon=True, name="RedisBridgeThread")
        self._thread.start()
        logger.info(f"🎧 RedisBridge listening on channel '{self.channel}'")

        # Outbound: register bus listener to publish events to Redis
        if self._enable_outbound:
            try:
                self._pub_client = redis.Redis.from_url(self.redis_url, decode_responses=True)
                self.bus.listen_all(self._outbound_handler)
                logger.info(f"📡 RedisBridge outbound enabled ({len(_OUTBOUND_EVENT_TYPES)} event types)")
            except Exception as e:
                logger.warning(f"RedisBridge outbound setup failed: {e}")

    def stop(self) -> None:
        """Stop the listener thread."""
        self._stop_event.set()
        if self.redis_client:
            try:
                # Unsubscribe to unblock the listen loop if possible, or just close
                self.redis_client.close()
            except Exception:
                pass
        if self._thread:
            self._thread.join(timeout=2.0)
        logger.info("RedisBridge stopped")

    def _listen_loop(self) -> None:
        """Main loop for consuming Redis messages."""
        if not self.redis_client:
            return

        pubsub = self.redis_client.pubsub()
        pubsub.subscribe(self.channel)

        while not self._stop_event.is_set():
            try:
                # get_message with timeout to allow checking stop_event
                message = pubsub.get_message(ignore_subscribe_messages=True, timeout=1.0)

                if message and message["type"] == "message":
                    self._process_message(message["data"])
            except redis.ConnectionError:
                logger.error("Redis connection lost, attempting reconnect...")
                # Simple reconnect logic could go here
                break
            except Exception as e:
                logger.error(f"Error in RedisBridge loop: {e}")

    def _process_message(self, raw_data: str) -> None:
        """Parse Redis message and emit to internal bus (inbound)."""
        try:
            payload = json.loads(raw_data)

            # Skip messages we published ourselves (echo prevention)
            if payload.get("_bridge_origin") == self._BRIDGE_SOURCE_TAG:
                return

            # Extract fields
            event_type_str = payload.get("event_type")
            source = payload.get("source", "redis_bridge")
            data = payload.get("data", {})
            confidence = payload.get("confidence", 1.0)

            # Map string event type to Enum
            event_type = None
            # Try by value first (e.g. "mesh_signal")
            try:
                event_type = EventType(event_type_str)
            except ValueError:
                # Try by name (e.g. "MESH_SIGNAL")
                try:
                    event_type = EventType[event_type_str]
                except KeyError:
                    pass

            if not event_type:
                logger.debug(f"Unknown event type from Redis: {event_type_str}")
                return

            # Tag as inbound so outbound handler won't re-publish
            data["_inbound"] = True

            # Construct ResonanceEvent
            event = ResonanceEvent(
                source=source,
                event_type=event_type,
                data=data,
                timestamp=datetime.now(),
                confidence=confidence,
            )

            # Emit to internal bus
            self.bus.emit(event)
            self._inbound_count += 1
            logger.debug(f"Bridged Redis event {event_type} to internal bus")

            # Forward to mesh awareness for peer tracking
            try:
                from whitemagic.mesh.awareness import get_mesh_awareness
                get_mesh_awareness().process_redis_message(payload)
            except Exception:
                pass

        except json.JSONDecodeError:
            logger.warning(f"Received non-JSON message on Redis: {raw_data}")
        except Exception as e:
            logger.error(f"Error processing Redis message: {e}")

    def _outbound_handler(self, event: ResonanceEvent) -> None:
        """Publish selected bus events to Redis (outbound)."""
        if not self._pub_client:
            return
        if event.event_type not in _OUTBOUND_EVENT_TYPES:
            return
        # Don't re-publish events that came from Redis
        if isinstance(event.data, dict) and event.data.get("_inbound"):
            return
        try:
            payload = json.dumps({
                "event_type": event.event_type.value,
                "source": event.source,
                "data": event.data,
                "confidence": event.confidence,
                "timestamp": event.timestamp.isoformat() if isinstance(event.timestamp, datetime) else str(event.timestamp),
                "_bridge_origin": self._BRIDGE_SOURCE_TAG,
            })
            self._pub_client.publish(self.channel, payload)
            self._outbound_count += 1
            logger.debug(f"Published {event.event_type.value} to Redis channel '{self.channel}'")
        except Exception as e:
            logger.debug(f"Failed to publish event to Redis: {e}")

    def get_stats(self) -> dict[str, Any]:
        """Return bridge statistics for introspection."""
        return {
            "inbound_count": self._inbound_count,
            "outbound_count": self._outbound_count,
            "outbound_enabled": self._enable_outbound,
            "channel": self.channel,
        }

# Global singleton helper
_bridge: RedisBridge | None = None

def start_redis_bridge() -> RedisBridge:
    global _bridge
    if _bridge is None:
        _bridge = RedisBridge()
        _bridge.start()
    return _bridge

def stop_redis_bridge() -> None:
    global _bridge
    if _bridge:
        _bridge.stop()
        _bridge = None
