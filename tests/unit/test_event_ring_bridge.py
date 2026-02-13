"""Tests for EventRing Bridge (Leap 7b)."""

from whitemagic.core.acceleration.event_ring_bridge import (
    EVENT_NAMES,
    EVENT_TYPES,
    EventRingBridge,
    RingEvent,
    _FallbackRing,
    _fnv1a,
    get_event_ring,
)


class TestEventTypes:
    def test_all_types_have_names(self):
        for name, tid in EVENT_TYPES.items():
            assert EVENT_NAMES[tid] == name

    def test_type_ids_unique(self):
        ids = list(EVENT_TYPES.values())
        assert len(ids) == len(set(ids))

    def test_known_types(self):
        assert "system_started" in EVENT_TYPES
        assert "memory_created" in EVENT_TYPES
        assert "resonance_amplified" in EVENT_TYPES
        assert "emergence_detected" in EVENT_TYPES


class TestFnv1a:
    def test_deterministic(self):
        assert _fnv1a("test") == _fnv1a("test")

    def test_different_inputs(self):
        assert _fnv1a("a") != _fnv1a("b")

    def test_empty_string(self):
        h = _fnv1a("")
        assert isinstance(h, int)
        assert h > 0


class TestFallbackRing:
    def test_publish_returns_sequence(self):
        ring = _FallbackRing(size=128)
        seq = ring.publish(1, 0, 1.0, b"")
        assert seq == 1

    def test_publish_increments_sequence(self):
        ring = _FallbackRing(size=128)
        s1 = ring.publish(1, 0, 1.0, b"")
        s2 = ring.publish(2, 0, 1.0, b"")
        assert s2 == s1 + 1

    def test_consumer_receives_events(self):
        ring = _FallbackRing(size=128)
        cid = ring.register_consumer()
        ring.publish(1, 0, 0.95, b"hello")
        events = ring.poll(cid, 10)
        assert len(events) == 1
        assert events[0].event_type == 1
        assert events[0].confidence == 0.95
        assert events[0].data == b"hello"

    def test_consumer_isolation(self):
        ring = _FallbackRing(size=128)
        c1 = ring.register_consumer()
        c2 = ring.register_consumer()
        ring.publish(42, 0, 1.0, b"")
        e1 = ring.poll(c1, 10)
        e2 = ring.poll(c2, 10)
        assert len(e1) == 1
        assert len(e2) == 1

    def test_consumer_cursor_advances(self):
        ring = _FallbackRing(size=128)
        cid = ring.register_consumer()
        ring.publish(1, 0, 1.0, b"")
        ring.publish(2, 0, 1.0, b"")
        events = ring.poll(cid, 10)
        assert len(events) == 2
        # Second poll should return nothing
        events2 = ring.poll(cid, 10)
        assert len(events2) == 0

    def test_stats(self):
        ring = _FallbackRing(size=128)
        ring.register_consumer()
        ring.publish(1, 0, 1.0, b"")
        stats = ring.stats()
        assert stats["write_cursor"] == 1
        assert stats["total_published"] == 1
        assert stats["active_consumers"] == 1
        assert stats["ring_size"] == 128

    def test_ring_overflow(self):
        ring = _FallbackRing(size=4)
        ring.register_consumer()  # side-effect: enables consumption tracking
        for i in range(10):
            ring.publish(i, 0, 1.0, b"")
        # Only last 4 should be in ring
        stats = ring.stats()
        assert stats["total_published"] == 10


class TestRingEvent:
    def test_age_ms(self):
        import time
        now_ns = int(time.time() * 1_000_000_000)
        event = RingEvent(
            sequence=1,
            event_type="test",
            source_id=0,
            timestamp_ns=now_ns - 1_000_000,  # 1ms ago
            confidence=1.0,
        )
        assert event.age_ms > 0
        assert event.age_ms < 1000  # Less than 1 second


class TestEventRingBridge:
    def test_singleton(self):
        r1 = get_event_ring()
        r2 = get_event_ring()
        assert r1 is r2

    def test_instantiation(self):
        bridge = EventRingBridge()
        assert bridge is not None

    def test_publish_returns_sequence(self):
        bridge = EventRingBridge()
        seq = bridge.publish("system_started", "test")
        assert isinstance(seq, int)
        assert seq > 0

    def test_register_consumer(self):
        bridge = EventRingBridge()
        cid = bridge.register_consumer()
        assert cid is not None
        assert isinstance(cid, int)

    def test_publish_and_poll(self):
        bridge = EventRingBridge()
        cid = bridge.register_consumer()
        bridge.publish("memory_created", "test", 0.9, b"data")
        events = bridge.poll(cid, 10)
        assert len(events) >= 1
        assert isinstance(events[0], RingEvent)

    def test_stats(self):
        bridge = EventRingBridge()
        stats = bridge.stats()
        assert isinstance(stats, dict)
        assert "total_published" in stats or "write_cursor" in stats
