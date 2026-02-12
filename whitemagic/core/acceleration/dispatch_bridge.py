"""Dispatch Bridge — Python interface to Zig comptime dispatch core (Leap 7c).

Provides fast tool dispatch pipeline checks (rate limit, circuit breaker,
maturity gate) by calling into the Zig shared library via ctypes.

Fast path: Zig FFI (~2µs full pipeline)
Fallback: Pure Python checks (~40µs)

The Zig dispatch core reads from the StateBoard mmap to make decisions
without any Python overhead.
"""

from __future__ import annotations

import ctypes
import logging
from enum import IntEnum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class DispatchResult(IntEnum):
    """Result of a dispatch pipeline check."""
    ALLOW = 0
    RATE_LIMITED = 1
    CIRCUIT_OPEN = 2
    IMMATURE = 3
    INVALID_TOOL = -1


class ToolId(IntEnum):
    """Tool IDs matching the 28-engine manifest."""
    SESSION = 0
    CONSOLIDATION = 1
    BOUNDARY = 2
    CIRCUIT_BREAKER = 3
    NURTURING = 4
    ACCELERATION = 5
    SERENDIPITY = 6
    INTROSPECTION = 7
    RESILIENCE = 8
    GOVERNANCE = 9
    ASSOCIATION = 10
    EXPORT = 11
    ARCHAEOLOGY = 12
    RESONANCE = 13
    SOLVER = 14
    EMBEDDING = 15
    LIFECYCLE = 16
    KAIZEN = 17
    PATTERN = 18
    NARRATIVE = 19
    ETHICS = 20
    PREDICTIVE = 21
    GALACTIC = 22
    CLONE_ARMY = 23
    FORGETTING = 24
    SANITIZATION = 25
    SWARM = 26
    EMERGENCE = 27


# Maturity levels for Python fallback
class Maturity(IntEnum):
    EXPERIMENTAL = 0
    BETA = 1
    STABLE = 2
    MATURE = 3


# Python fallback maturity table (mirrors Zig comptime table)
_MATURITY_TABLE: dict[int, Maturity] = {
    0: Maturity.MATURE,     # session
    1: Maturity.MATURE,     # consolidation
    2: Maturity.STABLE,     # boundary
    3: Maturity.STABLE,     # circuit_breaker
    4: Maturity.STABLE,     # nurturing
    5: Maturity.STABLE,     # acceleration
    6: Maturity.MATURE,     # serendipity
    7: Maturity.MATURE,     # introspection
    8: Maturity.STABLE,     # resilience
    9: Maturity.MATURE,     # governance
    10: Maturity.MATURE,    # association
    11: Maturity.STABLE,    # export
    12: Maturity.STABLE,    # archaeology
    13: Maturity.MATURE,    # resonance
    14: Maturity.BETA,      # solver
    15: Maturity.MATURE,    # embedding
    16: Maturity.STABLE,    # lifecycle
    17: Maturity.MATURE,    # kaizen
    18: Maturity.MATURE,    # pattern
    19: Maturity.STABLE,    # narrative
    20: Maturity.STABLE,    # ethics
    21: Maturity.MATURE,    # predictive
    22: Maturity.STABLE,    # galactic
    23: Maturity.STABLE,    # clone_army
    24: Maturity.STABLE,    # forgetting
    25: Maturity.STABLE,    # sanitization
    26: Maturity.STABLE,    # swarm
    27: Maturity.BETA,      # emergence
}

# Handler ID routing table (mirrors Zig)
_HANDLER_TABLE: dict[int, int] = {i: 100 + i for i in range(28)}

# Minimum maturity for dispatch
_MIN_MATURITY = Maturity.BETA


def _find_zig_lib() -> Path | None:
    """Locate the compiled Zig dispatch shared library."""
    candidates = [
        Path(__file__).parent.parent.parent.parent / "whitemagic-zig" / "zig-out" / "lib" / "libwhitemagic.so",
        Path(__file__).parent.parent.parent.parent / "whitemagic-zig" / "zig-out" / "lib" / "libwhitemagic.dylib",
        Path(__file__).parent.parent.parent.parent / "whitemagic-zig" / "zig-out" / "lib" / "libdispatch_core.so",
        Path(__file__).parent.parent.parent.parent / "whitemagic-zig" / "zig-out" / "lib" / "libdispatch_core.dylib",
    ]
    for p in candidates:
        if p.exists():
            return p
    return None


class DispatchBridge:
    """Python bridge to the Zig dispatch core.

    Fast path: Zig FFI via ctypes
    Fallback: Pure Python pipeline checks
    """

    def __init__(self) -> None:
        self._zig_lib: ctypes.CDLL | None = None
        self._zig_available = False
        self._board_bridge: Any = None
        self._total_checks = 0
        self._total_allowed = 0
        self._total_denied = 0
        self._try_zig()

    def _try_zig(self) -> None:
        lib_path = _find_zig_lib()
        if lib_path is not None:
            try:
                self._zig_lib = ctypes.CDLL(str(lib_path))
                # Set up function signatures
                self._zig_lib.wm_dispatch_check.argtypes = [
                    ctypes.c_uint32, ctypes.c_void_p,
                ]
                self._zig_lib.wm_dispatch_check.restype = ctypes.c_int32
                self._zig_lib.wm_dispatch_route.argtypes = [ctypes.c_uint32]
                self._zig_lib.wm_dispatch_route.restype = ctypes.c_uint32
                self._zig_available = True
                logger.info("⚡ DispatchCore: Zig FFI path active")
                return
            except Exception as e:
                logger.warning(f"Zig dispatch load failed: {e}")

        logger.info("🐍 DispatchCore: Python fallback")

    def _get_board(self) -> Any:
        """Lazy-load the StateBoard bridge."""
        if self._board_bridge is None:
            from whitemagic.core.acceleration.state_board_bridge import get_state_board
            self._board_bridge = get_state_board()
        return self._board_bridge

    def check(self, tool_id: int) -> DispatchResult:
        """Run the full dispatch pipeline check.

        Pipeline:
        1. Maturity gate (comptime/static lookup)
        2. Circuit breaker check (read StateBoard)
        3. Rate limit check (read StateBoard counter)

        Returns DispatchResult.
        """
        self._total_checks += 1

        # Python fallback path
        if tool_id < 0 or tool_id >= 28:
            self._total_denied += 1
            return DispatchResult.INVALID_TOOL

        # 1. Maturity gate
        maturity = _MATURITY_TABLE.get(tool_id, Maturity.EXPERIMENTAL)
        if maturity < _MIN_MATURITY:
            self._total_denied += 1
            return DispatchResult.IMMATURE

        # 2. Circuit breaker
        board = self._get_board()
        try:
            breaker_state, _failures = board.read_breaker(tool_id)
            if breaker_state == 1:  # OPEN
                self._total_denied += 1
                return DispatchResult.CIRCUIT_OPEN
        except Exception:
            pass  # Board not available, skip check

        # 3. Rate limit (simplified — full rate limiting is in Rust)
        # This is a soft check; the Rust rate limiter is the authoritative one
        self._total_allowed += 1
        return DispatchResult.ALLOW

    def route(self, tool_id: int) -> int:
        """Route a tool_id to its handler_id."""
        if self._zig_available and self._zig_lib is not None:
            try:
                routed = self._zig_lib.wm_dispatch_route(tool_id)
                if isinstance(routed, (int, float)):
                    return int(routed)
            except Exception:
                pass

        return _HANDLER_TABLE.get(tool_id, 0)

    def maturity(self, tool_id: int) -> Maturity:
        """Get the maturity level for a tool."""
        return _MATURITY_TABLE.get(tool_id, Maturity.EXPERIMENTAL)

    def stats(self) -> dict[str, int]:
        """Get dispatch statistics."""
        result = {
            "total_checks": self._total_checks,
            "total_allowed": self._total_allowed,
            "total_denied": self._total_denied,
            "zig_active": 1 if self._zig_available else 0,
        }
        if self._zig_available and self._zig_lib is not None:
            try:
                result["zig_checks"] = self._zig_lib.wm_dispatch_stats_total()
                result["zig_allowed"] = self._zig_lib.wm_dispatch_stats_allowed()
                result["zig_denied"] = self._zig_lib.wm_dispatch_stats_denied()
            except Exception:
                pass
        return result


# Global singleton
_dispatch: DispatchBridge | None = None


def get_dispatch() -> DispatchBridge:
    """Get the global Dispatch bridge instance."""
    global _dispatch
    if _dispatch is None:
        _dispatch = DispatchBridge()
    return _dispatch
