"""Tests for Dispatch Bridge (Leap 7c)."""

from whitemagic.core.acceleration.dispatch_bridge import (
    DispatchBridge,
    DispatchResult,
    Maturity,
    ToolId,
    get_dispatch,
    _MATURITY_TABLE,
    _HANDLER_TABLE,
)


class TestToolId:
    def test_all_28_tools(self):
        assert ToolId.SESSION == 0
        assert ToolId.EMERGENCE == 27
        assert len(ToolId) == 28

    def test_known_tools(self):
        assert ToolId.RESONANCE == 13
        assert ToolId.KAIZEN == 17
        assert ToolId.CLONE_ARMY == 23
        assert ToolId.SWARM == 26


class TestDispatchResult:
    def test_values(self):
        assert DispatchResult.ALLOW == 0
        assert DispatchResult.RATE_LIMITED == 1
        assert DispatchResult.CIRCUIT_OPEN == 2
        assert DispatchResult.IMMATURE == 3
        assert DispatchResult.INVALID_TOOL == -1


class TestMaturity:
    def test_values(self):
        assert Maturity.EXPERIMENTAL == 0
        assert Maturity.BETA == 1
        assert Maturity.STABLE == 2
        assert Maturity.MATURE == 3


class TestMaturityTable:
    def test_all_28_engines_have_maturity(self):
        for i in range(28):
            assert i in _MATURITY_TABLE

    def test_no_experimental_engines(self):
        for slot, maturity in _MATURITY_TABLE.items():
            assert maturity >= Maturity.BETA, f"Engine {slot} is experimental"

    def test_session_is_mature(self):
        assert _MATURITY_TABLE[0] == Maturity.MATURE

    def test_emergence_is_beta(self):
        assert _MATURITY_TABLE[27] == Maturity.BETA


class TestHandlerTable:
    def test_all_28_handlers(self):
        assert len(_HANDLER_TABLE) == 28

    def test_handler_ids_sequential(self):
        for i in range(28):
            assert _HANDLER_TABLE[i] == 100 + i


class TestDispatchBridge:
    def test_singleton(self):
        d1 = get_dispatch()
        d2 = get_dispatch()
        assert d1 is d2

    def test_instantiation(self):
        bridge = DispatchBridge()
        assert bridge is not None

    def test_check_valid_tool(self):
        bridge = DispatchBridge()
        result = bridge.check(ToolId.SESSION)
        assert result == DispatchResult.ALLOW

    def test_check_invalid_tool(self):
        bridge = DispatchBridge()
        result = bridge.check(-1)
        assert result == DispatchResult.INVALID_TOOL
        result2 = bridge.check(999)
        assert result2 == DispatchResult.INVALID_TOOL

    def test_check_all_28_tools_allow(self):
        bridge = DispatchBridge()
        for i in range(28):
            result = bridge.check(i)
            # All tools are beta+ so they should be allowed
            # (assuming no open circuit breakers)
            assert result in (DispatchResult.ALLOW, DispatchResult.CIRCUIT_OPEN)

    def test_route_valid_tool(self):
        bridge = DispatchBridge()
        handler = bridge.route(ToolId.SESSION)
        assert handler == 100

    def test_route_resonance(self):
        bridge = DispatchBridge()
        handler = bridge.route(ToolId.RESONANCE)
        assert handler == 113

    def test_route_invalid_tool(self):
        bridge = DispatchBridge()
        handler = bridge.route(999)
        assert handler == 0

    def test_maturity_query(self):
        bridge = DispatchBridge()
        assert bridge.maturity(ToolId.SESSION) == Maturity.MATURE
        assert bridge.maturity(ToolId.EMERGENCE) == Maturity.BETA

    def test_stats(self):
        bridge = DispatchBridge()
        bridge.check(0)
        bridge.check(1)
        stats = bridge.stats()
        assert isinstance(stats, dict)
        assert "total_checks" in stats
        assert stats["total_checks"] >= 2
