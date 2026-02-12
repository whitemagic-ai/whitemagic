#!/usr/bin/env python3
"""
haskell_bridge.py — ctypes bridge to the compiled whitemagic-divination shared library.

Build the .so first:
    cd staging/core_system/haskell
    cabal build
    # then locate the .so with:  cabal list-bin whitemagic-divination --with-compiler=...
    # or copy it into this directory as libwhitemagic_divination.so

Usage:
    from haskell_bridge import HaskellDivination
    div = HaskellDivination()          # auto-finds .so relative to this file
    num = div.create_and_number([1,0,1,1,0,1])  # returns King Wen number 1-64
"""

import ctypes
import json
import os
import glob
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple


def _find_shared_lib() -> Optional[str]:
    """Locate the compiled shared library.

    Searches (in order):
      1. Same directory as this file (manual copy / symlink)
      2. cabal dist-newstyle build artefact tree
    """
    here = Path(__file__).resolve().parent

    # 1 — explicit drop
    for candidate in here.glob("libwhitemagic_divination*"):
        if candidate.suffix in (".so", ".dylib", ".dll"):
            return str(candidate)

    # 2 — cabal build tree
    dist = here / "dist-newstyle"
    if dist.is_dir():
        hits = sorted(dist.rglob("libwhitemagic_divination*.so"), key=os.path.getmtime)
        if hits:
            return str(hits[-1])  # newest build

    return None


class HaskellDivination:
    """Thin wrapper around the Haskell C FFI exports."""

    def __init__(self, lib_path: Optional[str] = None) -> None:
        path = lib_path or _find_shared_lib()
        if path is None:
            raise FileNotFoundError(
                "Cannot find libwhitemagic_divination.so. "
                "Run 'cabal build' inside staging/core_system/haskell first."
            )
        # The foreign-library (.so) bundles the GHC RTS but doesn't
        # auto-initialize it.  We must call hs_init() before any Haskell code.
        self._lib = ctypes.CDLL(path, mode=ctypes.RTLD_GLOBAL)
        # Initialize the GHC RTS (argc=0, argv=NULL is fine)
        self._lib.hs_init(ctypes.byref(ctypes.c_int(0)), None)
        self._setup_signatures()
        self._lib.initFFI()

    @staticmethod
    def _preload_ghc_rts() -> Any:
        """Pre-load the GHC RTS and base libraries with RTLD_GLOBAL."""
        ghc_lib_dir = None
        # Try to find GHC library directory
        for candidate in [
            glob.glob(os.path.expanduser("~/.ghcup/ghc/*/lib/ghc-*/lib/x86_64-linux-ghc-*")),
            glob.glob("/usr/lib/ghc-*/lib/x86_64-linux-ghc-*"),
        ]:
            if candidate:
                ghc_lib_dir = sorted(candidate)[-1]  # latest version
                break
        if not ghc_lib_dir:
            return  # Best-effort; if not found, CDLL will fail with a clear error
        # Load RTS and key base libs with RTLD_GLOBAL so symbols are visible
        rts_libs = sorted(glob.glob(os.path.join(ghc_lib_dir, "libHSrts-*-ghc*.so")))
        base_libs = sorted(glob.glob(os.path.join(ghc_lib_dir, "libHSghc-prim-*-ghc*.so")))
        base_libs += sorted(glob.glob(os.path.join(ghc_lib_dir, "libHSghc-bignum-*-ghc*.so")))
        base_libs += sorted(glob.glob(os.path.join(ghc_lib_dir, "libHSbase-*-ghc*.so")))
        for lib_path in rts_libs + base_libs:
            try:
                ctypes.CDLL(lib_path, mode=ctypes.RTLD_GLOBAL)
            except OSError:
                pass  # Non-fatal; the main load will fail with a clear error

    # ------------------------------------------------------------------
    # Signature declarations (mirrors FFI.hs exports)
    # ------------------------------------------------------------------
    def _setup_signatures(self) -> Any:
        lib = self._lib

        lib.initFFI.restype = None
        lib.initFFI.argtypes = []

        lib.c_create_hexagram.restype = ctypes.c_void_p
        lib.c_create_hexagram.argtypes = [
            ctypes.c_int, ctypes.c_int, ctypes.c_int,
            ctypes.c_int, ctypes.c_int, ctypes.c_int,
        ]

        lib.c_hexagram_to_number.restype = ctypes.c_int
        lib.c_hexagram_to_number.argtypes = [ctypes.c_void_p]

        lib.c_is_balanced_hexagram.restype = ctypes.c_int
        lib.c_is_balanced_hexagram.argtypes = [ctypes.c_void_p]

        lib.c_transition_hexagram.restype = ctypes.c_void_p
        lib.c_transition_hexagram.argtypes = [
            ctypes.c_void_p,
            ctypes.POINTER(ctypes.c_int),
            ctypes.c_int,
        ]

        lib.c_free_hexagram.restype = None
        lib.c_free_hexagram.argtypes = [ctypes.c_void_p]

        # --- DharmaRules FFI (v0.2) ---
        lib.c_dharma_evaluate.restype = ctypes.c_char_p
        lib.c_dharma_evaluate.argtypes = [
            ctypes.c_char_p, ctypes.c_char_p,
            ctypes.c_char_p, ctypes.c_char_p,
        ]

        lib.c_dharma_evaluate_all.restype = ctypes.c_char_p
        lib.c_dharma_evaluate_all.argtypes = [
            ctypes.c_char_p, ctypes.c_char_p,
            ctypes.c_char_p, ctypes.c_char_p,
        ]

        # --- DepGraph FFI (v0.2) ---
        lib.c_depgraph_plan.restype = ctypes.c_char_p
        lib.c_depgraph_plan.argtypes = [ctypes.c_char_p]

        lib.c_depgraph_next_steps.restype = ctypes.c_char_p
        lib.c_depgraph_next_steps.argtypes = [ctypes.c_char_p]

        lib.c_depgraph_topo_sort.restype = ctypes.c_char_p
        lib.c_depgraph_topo_sort.argtypes = []

        lib.c_free_string.restype = None
        lib.c_free_string.argtypes = [ctypes.c_char_p]

    # ------------------------------------------------------------------
    # High-level helpers
    # ------------------------------------------------------------------
    def create_hexagram(self, lines: List[int]) -> ctypes.c_void_p:
        """Create a hexagram from 6 line values (0=Yin, 1=Yang, bottom to top).

        Returns an opaque pointer; remember to call free_hexagram when done.
        """
        if len(lines) != 6:
            raise ValueError("Exactly 6 lines required (bottom to top)")
        return ctypes.c_void_p(self._lib.c_create_hexagram(*[int(b) for b in lines]))

    def hexagram_to_number(self, ptr: Any) -> int:
        """Convert hexagram pointer to King Wen number (1-64)."""
        return int(self._lib.c_hexagram_to_number(ptr))

    def is_balanced(self, ptr: Any) -> bool:
        """True when the hexagram has exactly 3 yin and 3 yang lines."""
        return int(self._lib.c_is_balanced_hexagram(ptr)) == 1

    def transition(self, ptr: Any, positions: List[int]) -> Any:
        """Flip lines at the given positions (1-6). Returns a NEW pointer."""
        arr = (ctypes.c_int * len(positions))(*positions)
        return self._lib.c_transition_hexagram(ptr, arr, len(positions))

    def free_hexagram(self, ptr: Any) -> Any:
        """Release memory allocated by create_hexagram / transition."""
        self._lib.c_free_hexagram(ptr)

    # ------------------------------------------------------------------
    # Convenience: create + number + free in one call
    # ------------------------------------------------------------------
    def create_and_number(self, lines: List[int]) -> int:
        """Create a hexagram, get its King Wen number, and free immediately."""
        ptr = self.create_hexagram(lines)
        num = self.hexagram_to_number(ptr)
        self.free_hexagram(ptr)
        return num

    # ------------------------------------------------------------------
    # Dharma Rules (v0.2)
    # ------------------------------------------------------------------
    def dharma_evaluate(
        self, tool: str, description: str = "",
        safety: str = "", profile: str = "default",
    ) -> Dict[str, Any]:
        """Evaluate an action against Dharma rules.

        Returns dict with: action, severity, explain, rule.
        """
        result = self._lib.c_dharma_evaluate(
            tool.encode(), description.encode(),
            safety.encode(), profile.encode(),
        )
        return json.loads(result.decode()) if result else {"action": "log", "severity": 0.0}

    def dharma_evaluate_all(
        self, tool: str, description: str = "",
        safety: str = "", profile: str = "default",
    ) -> List[Dict[str, Any]]:
        """Return ALL matching Dharma rule decisions."""
        result = self._lib.c_dharma_evaluate_all(
            tool.encode(), description.encode(),
            safety.encode(), profile.encode(),
        )
        return json.loads(result.decode()) if result else []

    # ------------------------------------------------------------------
    # Dependency Graph (v0.2)
    # ------------------------------------------------------------------
    def depgraph_plan(self, goal_tool: str) -> Dict[str, Any]:
        """Plan execution chain for a goal tool.

        Returns dict with: chain (list), suggestions (list of {tool, weight}).
        """
        result = self._lib.c_depgraph_plan(goal_tool.encode())
        return json.loads(result.decode()) if result else {"chain": [], "suggestions": []}

    def depgraph_next_steps(self, tool: str) -> List[Dict[str, Any]]:
        """Get suggested next tools after a given tool."""
        result = self._lib.c_depgraph_next_steps(tool.encode())
        return json.loads(result.decode()) if result else []

    def depgraph_topo_sort(self) -> List[str]:
        """Topological sort of all tools in the dependency graph."""
        result = self._lib.c_depgraph_topo_sort()
        parsed = json.loads(result.decode()) if result else []
        if isinstance(parsed, dict) and "error" in parsed:
            raise ValueError(parsed["error"])
        if isinstance(parsed, list):
            return [str(item) for item in parsed]
        return []

    def create_and_query(self, lines: List[int]) -> dict:
        """Full round-trip: create, query number + balance, free, return dict."""
        ptr = self.create_hexagram(lines)
        result = {
            "lines": lines,
            "king_wen_number": self.hexagram_to_number(ptr),
            "is_balanced": self.is_balanced(ptr),
        }
        self.free_hexagram(ptr)
        return result


# ---------------------------------------------------------------------------
# Quick smoke-test when run directly
# ---------------------------------------------------------------------------
if __name__ == "__main__":
    try:
        div = HaskellDivination()
        print("Haskell divination bridge initialised.")

        # The Creative (all Yang)
        creative = div.create_and_query([1, 1, 1, 1, 1, 1])
        print(f"  The Creative (all Yang): {creative}")

        # The Receptive (all Yin)
        receptive = div.create_and_query([0, 0, 0, 0, 0, 0])
        print(f"  The Receptive (all Yin): {receptive}")

        # A balanced hexagram example
        balanced = div.create_and_query([1, 0, 1, 0, 1, 0])
        print(f"  Alternating (balanced?): {balanced}")

    except FileNotFoundError as e:
        print(f"Bridge unavailable (library not built): {e}")
