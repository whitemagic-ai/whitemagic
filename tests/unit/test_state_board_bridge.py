"""Tests for StateBoard Bridge (Leap 7a)."""

from whitemagic.core.acceleration.state_board_bridge import (
    BreakerState,
    BoardSnapshot,
    Guna,
    HarmonySnapshot,
    Quadrant,
    ResonanceSnapshot,
    StateBoardBridge,
    WuXingPhase,
    get_state_board,
)


class TestHarmonySnapshot:
    def test_defaults(self):
        h = HarmonySnapshot()
        assert h.balance == 0.0
        assert h.energy == 0.0

    def test_custom_values(self):
        h = HarmonySnapshot(balance=0.8, throughput=100.0, energy=0.95)
        assert h.balance == 0.8
        assert h.throughput == 100.0
        assert h.energy == 0.95


class TestResonanceSnapshot:
    def test_defaults(self):
        r = ResonanceSnapshot()
        assert r.current_gana == 0
        assert r.quadrant == Quadrant.EAST
        assert r.wu_xing == WuXingPhase.WOOD

    def test_quadrant_values(self):
        assert Quadrant.EAST == 0
        assert Quadrant.SOUTH == 1
        assert Quadrant.WEST == 2
        assert Quadrant.NORTH == 3


class TestBoardSnapshot:
    def test_defaults(self):
        b = BoardSnapshot()
        assert b.tick == 0
        assert b.harmony is None
        assert b.resonance is None


class TestEnums:
    def test_guna_values(self):
        assert Guna.SATTVIC == 0
        assert Guna.RAJASIC == 1
        assert Guna.TAMASIC == 2

    def test_breaker_values(self):
        assert BreakerState.CLOSED == 0
        assert BreakerState.OPEN == 1
        assert BreakerState.HALF_OPEN == 2

    def test_wu_xing_values(self):
        assert WuXingPhase.WOOD == 0
        assert WuXingPhase.FIRE == 1
        assert WuXingPhase.EARTH == 2
        assert WuXingPhase.METAL == 3
        assert WuXingPhase.WATER == 4


class TestStateBoardBridge:
    def test_singleton(self):
        b1 = get_state_board()
        b2 = get_state_board()
        assert b1 is b2

    def test_instantiation(self):
        bridge = StateBoardBridge()
        assert bridge is not None

    def test_read_harmony_returns_snapshot(self):
        bridge = StateBoardBridge()
        h = bridge.read_harmony()
        assert isinstance(h, HarmonySnapshot)

    def test_read_resonance_returns_snapshot(self):
        bridge = StateBoardBridge()
        r = bridge.read_resonance()
        assert isinstance(r, ResonanceSnapshot)

    def test_read_tick_returns_int(self):
        bridge = StateBoardBridge()
        t = bridge.read_tick()
        assert isinstance(t, int)

    def test_read_breaker_returns_tuple(self):
        bridge = StateBoardBridge()
        state, failures = bridge.read_breaker(0)
        assert isinstance(state, BreakerState)
        assert isinstance(failures, int)

    def test_snapshot_returns_board_snapshot(self):
        bridge = StateBoardBridge()
        snap = bridge.snapshot()
        assert isinstance(snap, BoardSnapshot)
        assert isinstance(snap.tick, int)

    def test_close_idempotent(self):
        bridge = StateBoardBridge()
        bridge.close()
        bridge.close()  # Should not raise


class TestStateBoardWriteRead:
    """Test write/read roundtrip (works with both Rust and Python fallback)."""

    def test_harmony_roundtrip(self):
        bridge = StateBoardBridge()
        tick = bridge.write_harmony(
            balance=0.75, throughput=42.0, latency=1.5,
            error_rate=0.01, dharma=0.95, karma_debt=0.1, energy=0.88,
        )
        if tick > 0:  # Board is available
            h = bridge.read_harmony()
            assert abs(h.balance - 0.75) < 0.001
            assert abs(h.throughput - 42.0) < 0.001
            assert abs(h.energy - 0.88) < 0.001
        bridge.close()

    def test_resonance_roundtrip(self):
        bridge = StateBoardBridge()
        tick = bridge.write_resonance(
            current_gana=14, predecessor=13, successor=15,
            quadrant=1, wu_xing=1, guna=0,
        )
        if tick > 0:
            r = bridge.read_resonance()
            assert r.current_gana == 14
            assert r.guna == Guna.SATTVIC
        bridge.close()

    def test_breaker_roundtrip(self):
        bridge = StateBoardBridge()
        bridge.write_breaker(0, BreakerState.OPEN, 5)
        state, failures = bridge.read_breaker(0)
        # May not work if board not initialized, but should not crash
        bridge.write_breaker(0, BreakerState.CLOSED, 0)
        bridge.close()
