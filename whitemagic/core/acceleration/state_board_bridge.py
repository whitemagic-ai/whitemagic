"""StateBoard Bridge — Python interface to Rust shared-memory blackboard (Leap 7a).

Provides zero-copy reads of the StateBoard mmap file, with Rust PyO3 as the
fast path and Python mmap as the fallback.

The StateBoard holds:
- Harmony Vector (7 × f64)
- Circuit Breaker states (64 slots)
- Resonance snapshot (current Gana, Wu Xing, quadrant)
- System Guna (sattvic/rajasic/tamasic)
- Monotonic tick counter
"""

from __future__ import annotations

import logging
import mmap
import struct
from dataclasses import dataclass
from enum import IntEnum
from pathlib import Path
from typing import IO, Any

logger = logging.getLogger(__name__)

# StateBoard layout constants (must match state_board.rs)
_BOARD_SIZE = 4096
_MAGIC = 0x574D_424F_4152_4400
_OFF_TICK = 16
_OFF_HV = 64  # Harmony Vector starts here (7 × 8 bytes)
_OFF_GUNA = 128
_OFF_WU_XING = 136
_OFF_CURRENT_GANA = 144
_OFF_PREDECESSOR = 152
_OFF_SUCCESSOR = 160
_OFF_QUADRANT = 168
_OFF_ACTIVE_ENGINES = 176
_OFF_LAST_UPDATE = 184
_OFF_BREAKERS = 256
_BREAKER_SLOT = 16
_OFF_COUNTERS = 1280


class Guna(IntEnum):
    SATTVIC = 0
    RAJASIC = 1
    TAMASIC = 2


class BreakerState(IntEnum):
    CLOSED = 0
    OPEN = 1
    HALF_OPEN = 2


class WuXingPhase(IntEnum):
    WOOD = 0
    FIRE = 1
    EARTH = 2
    METAL = 3
    WATER = 4


class Quadrant(IntEnum):
    EAST = 0
    SOUTH = 1
    WEST = 2
    NORTH = 3


@dataclass
class HarmonySnapshot:
    """Snapshot of the Harmony Vector from the StateBoard."""
    balance: float = 0.0
    throughput: float = 0.0
    latency: float = 0.0
    error_rate: float = 0.0
    dharma: float = 0.0
    karma_debt: float = 0.0
    energy: float = 0.0


@dataclass
class ResonanceSnapshot:
    """Snapshot of resonance state from the StateBoard."""
    current_gana: int = 0
    predecessor: int = 0
    successor: int = 0
    quadrant: Quadrant = Quadrant.EAST
    wu_xing: WuXingPhase = WuXingPhase.WOOD
    guna: Guna = Guna.SATTVIC


@dataclass
class BoardSnapshot:
    """Complete snapshot of the StateBoard."""
    tick: int = 0
    harmony: HarmonySnapshot | None = None
    resonance: ResonanceSnapshot | None = None
    active_engines: int = 0
    last_update_ns: int = 0


class StateBoardBridge:
    """Python bridge to the Rust StateBoard shared-memory region.

    Fast path: Rust PyO3 (whitemagic_rs.board_*)
    Fallback: Python mmap reading the same file
    """

    def __init__(self) -> None:
        self._rust_available = False
        self._mmap: mmap.mmap | None = None
        self._mmap_file: IO[bytes] | None = None
        self._try_rust()

    def _try_rust(self) -> None:
        try:
            import whitemagic_rs
            if hasattr(whitemagic_rs, "board_read_harmony"):
                self._rust_available = True
                logger.info("🦀 StateBoard: Rust path active")
        except ImportError:
            logger.info("⚡ StateBoard: Python mmap fallback")

    def _ensure_mmap(self) -> mmap.mmap | None:
        if self._mmap is not None:
            return self._mmap

        # Find the board file
        if self._rust_available:
            try:
                import whitemagic_rs
                path = Path(whitemagic_rs.board_get_path())
            except Exception:
                path = self._default_path()
        else:
            path = self._default_path()

        if not path.exists():
            return None

        try:
            self._mmap_file = open(path, "r+b")
            self._mmap = mmap.mmap(self._mmap_file.fileno(), _BOARD_SIZE)
            return self._mmap
        except Exception as e:
            logger.warning(f"StateBoard mmap fallback failed: {e}")
            return None

    @staticmethod
    def _default_path() -> Path:
        from whitemagic.config.paths import CACHE_DIR
        return CACHE_DIR / "state_board.bin"

    # --- Read API ---

    def read_harmony(self) -> HarmonySnapshot:
        """Read the Harmony Vector. ~100ns via Rust, ~1µs via mmap."""
        if self._rust_available:
            try:
                import whitemagic_rs
                hv = whitemagic_rs.board_read_harmony()
                return HarmonySnapshot(
                    balance=hv.get("balance", 0.0),
                    throughput=hv.get("throughput", 0.0),
                    latency=hv.get("latency", 0.0),
                    error_rate=hv.get("error_rate", 0.0),
                    dharma=hv.get("dharma", 0.0),
                    karma_debt=hv.get("karma_debt", 0.0),
                    energy=hv.get("energy", 0.0),
                )
            except Exception:
                pass

        # Python mmap fallback
        mm = self._ensure_mmap()
        if mm is None:
            return HarmonySnapshot()

        vals = struct.unpack_from("<7d", mm, _OFF_HV)
        return HarmonySnapshot(*vals)

    def read_resonance(self) -> ResonanceSnapshot:
        """Read the resonance state."""
        if self._rust_available:
            try:
                import whitemagic_rs
                state = whitemagic_rs.board_read_state()
                return ResonanceSnapshot(
                    current_gana=state.get("current_gana", 0),
                    predecessor=state.get("predecessor_gana", 0),
                    successor=state.get("successor_gana", 0),
                    quadrant=Quadrant(state.get("quadrant", 0)),
                    wu_xing=WuXingPhase(state.get("wu_xing_phase", 0)),
                    guna=Guna(state.get("guna", 0)),
                )
            except Exception:
                pass

        mm = self._ensure_mmap()
        if mm is None:
            return ResonanceSnapshot()

        vals = struct.unpack_from("<6Q", mm, _OFF_GUNA)
        return ResonanceSnapshot(
            guna=Guna(vals[0]),
            wu_xing=WuXingPhase(vals[1]),
            current_gana=vals[2],
            predecessor=vals[3],
            successor=vals[4],
            quadrant=Quadrant(vals[5]),
        )

    def read_tick(self) -> int:
        """Read the monotonic tick counter."""
        if self._rust_available:
            try:
                import whitemagic_rs
                state = whitemagic_rs.board_read_state()
                if isinstance(state, dict):
                    tick = state.get("tick", 0)
                    if isinstance(tick, (int, float)):
                        return int(tick)
            except Exception:
                pass

        mm = self._ensure_mmap()
        if mm is None:
            return 0
        return int(struct.unpack_from("<Q", mm, _OFF_TICK)[0])

    def read_breaker(self, tool_slot: int) -> tuple[BreakerState, int]:
        """Read circuit breaker for a tool. Returns (state, failure_count)."""
        if self._rust_available:
            try:
                import whitemagic_rs
                state, failures = whitemagic_rs.board_read_breaker(tool_slot)
                return BreakerState(state), failures
            except Exception:
                pass

        mm = self._ensure_mmap()
        if mm is None:
            return BreakerState.CLOSED, 0
        off = _OFF_BREAKERS + tool_slot * _BREAKER_SLOT
        state, failures = struct.unpack_from("<2Q", mm, off)
        return BreakerState(state), failures

    def snapshot(self) -> BoardSnapshot:
        """Read the full board state as a snapshot."""
        return BoardSnapshot(
            tick=self.read_tick(),
            harmony=self.read_harmony(),
            resonance=self.read_resonance(),
        )

    # --- Write API ---

    def write_harmony(
        self,
        balance: float = 0.0,
        throughput: float = 0.0,
        latency: float = 0.0,
        error_rate: float = 0.0,
        dharma: float = 0.0,
        karma_debt: float = 0.0,
        energy: float = 0.0,
    ) -> int:
        """Write the Harmony Vector. Returns new tick."""
        if self._rust_available:
            try:
                import whitemagic_rs
                tick = whitemagic_rs.board_write_harmony(
                    balance, throughput, latency, error_rate, dharma, karma_debt, energy,
                )
                if isinstance(tick, (int, float)):
                    return int(tick)
            except Exception:
                pass

        mm = self._ensure_mmap()
        if mm is None:
            return 0
        struct.pack_into("<7d", mm, _OFF_HV,
                         balance, throughput, latency, error_rate,
                         dharma, karma_debt, energy)
        # Increment tick
        tick = int(struct.unpack_from("<Q", mm, _OFF_TICK)[0]) + 1
        struct.pack_into("<Q", mm, _OFF_TICK, tick)
        return int(tick)

    def write_resonance(
        self,
        current_gana: int,
        predecessor: int,
        successor: int,
        quadrant: int,
        wu_xing: int,
        guna: int,
    ) -> int:
        """Write the resonance state. Returns new tick."""
        if self._rust_available:
            try:
                import whitemagic_rs
                tick = whitemagic_rs.board_write_resonance(
                    current_gana, predecessor, successor, quadrant, wu_xing, guna,
                )
                if isinstance(tick, (int, float)):
                    return int(tick)
            except Exception:
                pass

        mm = self._ensure_mmap()
        if mm is None:
            return 0
        struct.pack_into("<6Q", mm, _OFF_GUNA,
                         guna, wu_xing, current_gana,
                         predecessor, successor, quadrant)
        tick = int(struct.unpack_from("<Q", mm, _OFF_TICK)[0]) + 1
        struct.pack_into("<Q", mm, _OFF_TICK, tick)
        return int(tick)

    def write_breaker(self, tool_slot: int, state: BreakerState, failures: int = 0) -> None:
        """Write a circuit breaker state."""
        if self._rust_available:
            try:
                import whitemagic_rs
                whitemagic_rs.board_write_breaker(tool_slot, int(state), failures)
                return
            except Exception:
                pass

        mm = self._ensure_mmap()
        if mm is None:
            return
        off = _OFF_BREAKERS + tool_slot * _BREAKER_SLOT
        struct.pack_into("<2Q", mm, off, int(state), failures)

    def close(self) -> None:
        """Close mmap resources."""
        if self._mmap is not None:
            self._mmap.close()
            self._mmap = None
        if self._mmap_file is not None:
            self._mmap_file.close()
            self._mmap_file = None


# Global singleton
_board: StateBoardBridge | None = None


def get_state_board() -> StateBoardBridge:
    """Get the global StateBoard bridge instance."""
    global _board
    if _board is None:
        _board = StateBoardBridge()
    return _board
