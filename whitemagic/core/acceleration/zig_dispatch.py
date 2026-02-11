"""Zig Comptime Dispatch — Python Bridge.
==========================================
Loads the compiled Zig dispatch_core shared library and exposes the
comptime-generated static dispatch pipeline: maturity gate → circuit
breaker → rate limiter, all in <2µs via mmap'd StateBoard.

Falls back to Python-only dispatch when the Zig library is not available.

FFI contract (C ABI):
  wm_dispatch_check(gana_id: u32, board_ptr: *u8) -> i32
    Returns: 0=ALLOW, 1=RATE_LIMITED, 2=CIRCUIT_OPEN, 3=IMMATURE, -1=INVALID

  wm_dispatch_route(gana_id: u32) -> u32
    Returns: handler_id for the given Gana

  wm_dispatch_stats_total() -> u64
  wm_dispatch_stats_allowed() -> u64
  wm_dispatch_stats_denied() -> u64
  wm_dispatch_reset_stats() -> void
  wm_dispatch_maturity(gana_id: u32) -> u8

Research context (from PRAT Agentic Primitives analysis):
  Zig comptime dispatch achieves O(1) tool routing via static lookup
  tables generated at compile time — no hashing, no collision checking,
  no pointer chasing. Combined with C-ABI FFI (~220ns overhead), this
  brings the full dispatch pipeline well under the 5µs latency budget.
"""
from __future__ import annotations

import ctypes
import logging
import os
import threading
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

_lib = None
_lib_lock = threading.Lock()
_HAS_ZIG_DISPATCH = False

# Canonical Gana name → numeric ID mapping (must match dispatch_core.zig GanaId)
GANA_NAME_TO_ID: dict[str, int] = {
    "gana_horn": 0,
    "gana_neck": 1,
    "gana_root": 2,
    "gana_room": 3,
    "gana_heart": 4,
    "gana_tail": 5,
    "gana_winnowing_basket": 6,
    "gana_ghost": 7,
    "gana_willow": 8,
    "gana_star": 9,
    "gana_extended_net": 10,
    "gana_wings": 11,
    "gana_chariot": 12,
    "gana_abundance": 13,
    "gana_straddling_legs": 14,
    "gana_mound": 15,
    "gana_stomach": 16,
    "gana_hairy_head": 17,
    "gana_net": 18,
    "gana_turtle_beak": 19,
    "gana_three_stars": 20,
    "gana_dipper": 21,
    "gana_ox": 22,
    "gana_girl": 23,
    "gana_void": 24,
    "gana_roof": 25,
    "gana_encampment": 26,
    "gana_wall": 27,
}

# Dispatch result codes (must match dispatch_core.zig DispatchResult)
ALLOW = 0
RATE_LIMITED = 1
CIRCUIT_OPEN = 2
IMMATURE = 3
INVALID_TOOL = -1

# Maturity levels
MATURITY_EXPERIMENTAL = 0
MATURITY_BETA = 1
MATURITY_STABLE = 2
MATURITY_MATURE = 3

_MATURITY_NAMES = {0: "experimental", 1: "beta", 2: "stable", 3: "mature"}


def _find_zig_lib() -> str | None:
    """Locate the compiled Zig shared library."""
    base = Path(__file__).resolve().parent.parent.parent.parent / "whitemagic-zig"
    candidates = [
        os.environ.get("WM_ZIG_LIB", ""),
        str(base / "zig-out" / "lib" / "libwhitemagic.so"),
        str(base / "libwhitemagic.so"),
        str(base / "zig-out" / "lib" / "libwhitemagic.dylib"),
    ]
    for path in candidates:
        if path and os.path.isfile(path):
            return path
    return None


def _load_lib() -> Any:
    """Load the Zig dispatch shared library."""
    global _lib, _HAS_ZIG_DISPATCH
    if _lib is not None:
        return _lib
    with _lib_lock:
        if _lib is not None:
            return _lib
        path = _find_zig_lib()
        if not path:
            logger.debug("Zig dispatch library not found — using Python fallback")
            return None
        try:
            lib = ctypes.CDLL(path)

            # wm_dispatch_check(gana_id: u32, board_ptr: *u8) -> i32
            lib.wm_dispatch_check.argtypes = [
                ctypes.c_uint32,
                ctypes.POINTER(ctypes.c_uint8),
            ]
            lib.wm_dispatch_check.restype = ctypes.c_int32

            # wm_dispatch_route(gana_id: u32) -> u32
            lib.wm_dispatch_route.argtypes = [ctypes.c_uint32]
            lib.wm_dispatch_route.restype = ctypes.c_uint32

            # Stats
            lib.wm_dispatch_stats_total.argtypes = []
            lib.wm_dispatch_stats_total.restype = ctypes.c_uint64
            lib.wm_dispatch_stats_allowed.argtypes = []
            lib.wm_dispatch_stats_allowed.restype = ctypes.c_uint64
            lib.wm_dispatch_stats_denied.argtypes = []
            lib.wm_dispatch_stats_denied.restype = ctypes.c_uint64

            # Reset
            lib.wm_dispatch_reset_stats.argtypes = []
            lib.wm_dispatch_reset_stats.restype = None

            # Maturity
            lib.wm_dispatch_maturity.argtypes = [ctypes.c_uint32]
            lib.wm_dispatch_maturity.restype = ctypes.c_uint8

            _lib = lib
            _HAS_ZIG_DISPATCH = True
            logger.info("Zig dispatch loaded: path=%s", path)
            return lib
        except Exception as e:
            logger.debug("Failed to load Zig dispatch library: %s", e)
            return None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def dispatch_check(gana_name: str, board: bytes | None = None) -> int:
    """Check if a Gana dispatch should be allowed.

    Args:
        gana_name: Canonical Gana name (e.g. "gana_horn")
        board: Optional StateBoard bytes (4096 bytes mmap'd).
               If None, uses a zeroed board (all circuits closed, no rate limits).

    Returns:
        ALLOW (0), RATE_LIMITED (1), CIRCUIT_OPEN (2), IMMATURE (3), or INVALID_TOOL (-1)
    """
    gana_id = GANA_NAME_TO_ID.get(gana_name)
    if gana_id is None:
        return INVALID_TOOL

    lib = _load_lib()
    if lib is not None:
        if board is None:
            board_arr = (ctypes.c_uint8 * 4096)()
        else:
            board_arr = (ctypes.c_uint8 * len(board)).from_buffer_copy(board)
        return int(lib.wm_dispatch_check(gana_id, board_arr))

    # Python fallback: always allow (no StateBoard available)
    return ALLOW


def dispatch_route(gana_name: str) -> int:
    """Route a Gana to its handler ID. Pure comptime lookup.

    Returns: handler_id (100-127) or 0 if invalid.
    """
    gana_id = GANA_NAME_TO_ID.get(gana_name)
    if gana_id is None:
        return 0

    lib = _load_lib()
    if lib is not None:
        return int(lib.wm_dispatch_route(gana_id))

    # Python fallback: gana_id + 100
    return gana_id + 100


def dispatch_maturity(gana_name: str) -> str:
    """Get the maturity level name for a Gana."""
    gana_id = GANA_NAME_TO_ID.get(gana_name)
    if gana_id is None:
        return "unknown"

    lib = _load_lib()
    if lib is not None:
        level = int(lib.wm_dispatch_maturity(gana_id))
        return _MATURITY_NAMES.get(level, "unknown")

    # Python fallback: assume stable
    return "stable"


def dispatch_stats() -> dict[str, int | str]:
    """Get dispatch pipeline statistics."""
    lib = _load_lib()
    if lib is not None:
        return {
            "total": int(lib.wm_dispatch_stats_total()),
            "allowed": int(lib.wm_dispatch_stats_allowed()),
            "denied": int(lib.wm_dispatch_stats_denied()),
            "backend": "zig_comptime",
        }
    return {"total": 0, "allowed": 0, "denied": 0, "backend": "python_fallback"}


def dispatch_reset_stats() -> None:
    """Reset dispatch statistics counters."""
    lib = _load_lib()
    if lib is not None:
        lib.wm_dispatch_reset_stats()


def zig_dispatch_status() -> dict[str, Any]:
    """Get Zig dispatch acceleration status."""
    _load_lib()
    return {
        "has_zig_dispatch": _HAS_ZIG_DISPATCH,
        "lib_path": _find_zig_lib() or "not found",
        "backend": "zig_comptime" if _HAS_ZIG_DISPATCH else "python_fallback",
        "gana_count": 28,
        "stats": dispatch_stats(),
    }
