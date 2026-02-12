"""EventRing Bridge — Python interface to Rust LMAX Disruptor ring buffer (Leap 7b).

Provides lock-free event publishing and consumption for the Gan Ying bus.
Rust PyO3 is the fast path; Python fallback uses a simple collections.deque.

The EventRing replaces Redis pub/sub for intra-process event propagation.
Redis stays for cross-machine sync.

Target: ~100ns publish latency, 1M+ events/sec throughput via Rust.
"""

from __future__ import annotations

import collections
import logging
import threading
import time
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)

# Event type IDs (must match consumers)
EVENT_TYPES = {
    "system_started": 1,
    "memory_created": 2,
    "memory_accessed": 3,
    "pattern_detected": 4,
    "resonance_amplified": 5,
    "sympathetic_resonance": 6,
    "gratitude_felt": 7,
    "breakthrough_achieved": 8,
    "tool_called": 9,
    "tool_completed": 10,
    "error_occurred": 11,
    "dream_started": 12,
    "dream_completed": 13,
    "consolidation_started": 14,
    "consolidation_completed": 15,
    "boundary_exceeded": 16,
    "circuit_opened": 17,
    "circuit_closed": 18,
    "agent_spawned": 19,
    "agent_completed": 20,
    "swarm_breathe": 21,
    "kaizen_proposal": 22,
    "serendipity_surface": 23,
    "prediction_generated": 24,
    "emergence_detected": 25,
}

# Reverse lookup
EVENT_NAMES = {v: k for k, v in EVENT_TYPES.items()}


@dataclass
class RingEvent:
    """An event read from the ring buffer."""
    sequence: int
    event_type: str
    source_id: int
    timestamp_ns: int
    confidence: float
    data: bytes = b""

    @property
    def age_ms(self) -> float:
        """Age of this event in milliseconds."""
        now_ns = int(time.time() * 1_000_000_000)
        return (now_ns - self.timestamp_ns) / 1_000_000


@dataclass
class _FallbackSlot:
    """A slot in the Python fallback ring."""
    sequence: int = 0
    event_type: int = 0
    source_id: int = 0
    timestamp_ns: int = 0
    confidence: float = 0.0
    data: bytes = b""


class _FallbackRing:
    """Pure Python fallback ring buffer using deque."""

    def __init__(self, size: int = 65536) -> None:
        self._ring: collections.deque[_FallbackSlot] = collections.deque(maxlen=size)
        self._lock = threading.Lock()
        self._sequence = 0
        self._consumer_cursors: dict[int, int] = {}
        self._next_consumer_id = 0
        self._total_published = 0
        self._total_consumed = 0

    def publish(self, event_type: int, source_id: int, confidence: float, data: bytes) -> int:
        with self._lock:
            self._sequence += 1
            slot = _FallbackSlot(
                sequence=self._sequence,
                event_type=event_type,
                source_id=source_id,
                timestamp_ns=int(time.time() * 1_000_000_000),
                confidence=confidence,
                data=data,
            )
            self._ring.append(slot)
            self._total_published += 1
            return self._sequence

    def register_consumer(self) -> int:
        with self._lock:
            cid = self._next_consumer_id
            self._next_consumer_id += 1
            self._consumer_cursors[cid] = self._sequence
            return cid

    def poll(self, consumer_id: int, max_events: int = 100) -> list[_FallbackSlot]:
        with self._lock:
            cursor = self._consumer_cursors.get(consumer_id, 0)
            events: list[_FallbackSlot] = []
            for slot in self._ring:
                if slot.sequence > cursor and len(events) < max_events:
                    events.append(slot)
            if events:
                self._consumer_cursors[consumer_id] = events[-1].sequence
                self._total_consumed += len(events)
            return events

    def stats(self) -> dict[str, int]:
        return {
            "write_cursor": self._sequence,
            "total_published": self._total_published,
            "total_consumed": self._total_consumed,
            "active_consumers": len(self._consumer_cursors),
            "ring_size": self._ring.maxlen or 0,
        }


def _fnv1a(s: str) -> int:
    """FNV-1a hash matching the Rust implementation."""
    h = 0xCBF29CE484222325
    for b in s.encode():
        h ^= b
        h = (h * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return h


class EventRingBridge:
    """Python bridge to the Rust EventRing.

    Fast path: Rust PyO3 (whitemagic_rs.ring_*)
    Fallback: Python deque-based ring buffer
    """

    def __init__(self) -> None:
        self._rust_available = False
        self._fallback: _FallbackRing | None = None
        self._try_rust()

    def _try_rust(self) -> None:
        try:
            import whitemagic_rs
            if hasattr(whitemagic_rs, "ring_publish"):
                self._rust_available = True
                logger.info("🦀 EventRing: Rust LMAX path active")
                return
        except ImportError:
            pass
        self._fallback = _FallbackRing()
        logger.info("⚡ EventRing: Python deque fallback")

    def publish(
        self,
        event_type: str,
        source: str = "unknown",
        confidence: float = 1.0,
        data: bytes = b"",
    ) -> int:
        """Publish an event. Returns sequence number."""
        type_id = EVENT_TYPES.get(event_type, 0)

        if self._rust_available:
            try:
                import whitemagic_rs
                sequence = whitemagic_rs.ring_publish(type_id, source, confidence, data)
                if isinstance(sequence, (int, float)):
                    return int(sequence)
            except Exception:
                pass

        if self._fallback is None:
            self._fallback = _FallbackRing()
        return self._fallback.publish(type_id, _fnv1a(source), confidence, data)

    def register_consumer(self) -> int | None:
        """Register a new consumer. Returns consumer ID."""
        if self._rust_available:
            try:
                import whitemagic_rs
                consumer_id = whitemagic_rs.ring_register_consumer()
                if isinstance(consumer_id, (int, float)):
                    return int(consumer_id)
            except Exception:
                pass

        if self._fallback is None:
            self._fallback = _FallbackRing()
        return self._fallback.register_consumer()

    def poll(self, consumer_id: int, max_events: int = 100) -> list[RingEvent]:
        """Poll for new events. Returns list of RingEvent."""
        if self._rust_available:
            try:
                import whitemagic_rs
                raw = whitemagic_rs.ring_poll(consumer_id, max_events)
                return [
                    RingEvent(
                        sequence=seq,
                        event_type=EVENT_NAMES.get(et, f"unknown_{et}"),
                        source_id=sid,
                        timestamp_ns=ts,
                        confidence=conf,
                        data=bytes(d),
                    )
                    for seq, et, sid, ts, conf, d in raw
                ]
            except Exception:
                pass

        if self._fallback is None:
            return []
        slots = self._fallback.poll(consumer_id, max_events)
        return [
            RingEvent(
                sequence=s.sequence,
                event_type=EVENT_NAMES.get(s.event_type, f"unknown_{s.event_type}"),
                source_id=s.source_id,
                timestamp_ns=s.timestamp_ns,
                confidence=s.confidence,
                data=s.data,
            )
            for s in slots
        ]

    def stats(self) -> dict[str, Any]:
        """Get ring buffer statistics."""
        if self._rust_available:
            try:
                import whitemagic_rs
                stats = whitemagic_rs.ring_stats()
                if isinstance(stats, dict):
                    return stats
            except Exception:
                pass

        if self._fallback is None:
            return {}
        return self._fallback.stats()


# Global singleton
_ring: EventRingBridge | None = None


def get_event_ring() -> EventRingBridge:
    """Get the global EventRing bridge instance."""
    global _ring
    if _ring is None:
        _ring = EventRingBridge()
    return _ring
