"""Haskell Divination Bridge — wraps the FFI-based haskell_bridge.py."""

import logging
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional

logger = logging.getLogger("haskell_bridge")

# The FFI bridge lives at haskell/haskell_bridge.py (project root)
_HASKELL_DIR = Path(__file__).resolve().parents[3] / "haskell"


class HaskellBridge:
    """High-level interface to the Haskell divination library."""

    def __init__(self) -> None:
        self._divination = None
        self._available = False
        self._init()

    def _init(self) -> None:
        try:
            # Add haskell/ to path so we can import haskell_bridge
            haskell_dir = str(_HASKELL_DIR)
            if haskell_dir not in sys.path:
                sys.path.insert(0, haskell_dir)
            from haskell_bridge import HaskellDivination  # type: ignore[import-not-found]

            self._divination = HaskellDivination()
            self._available = True
            logger.info("Haskell divination bridge initialized")
        except (FileNotFoundError, OSError, ImportError) as e:
            logger.warning(f"Haskell bridge unavailable: {e}")
            self._available = False

    @property
    def available(self) -> bool:
        return bool(self._available)

    def cast_hexagram(self, lines: Optional[List[int]] = None) -> Dict[str, Any]:
        """Cast a hexagram from 6 line values (0=Yin, 1=Yang).

        If lines is None, generates random lines.
        Returns dict with king_wen_number, is_balanced, lines.
        """
        if lines is None:
            import random
            lines = [random.randint(0, 1) for _ in range(6)]

        if not self._available:
            logger.warning("Haskell bridge not available, returning simulated result")
            return {
                "status": "SIMULATED",
                "lines": lines,
                "king_wen_number": sum(lines) + 1,
                "is_balanced": lines.count(0) == lines.count(1),
            }

        try:
            assert self._divination is not None
            result: dict[str, Any] = self._divination.create_and_query(lines)
            result["status"] = "OK"
            return result
        except Exception as e:
            logger.error(f"Haskell bridge error: {e}")
            return {
                "status": "ERROR",
                "error": str(e),
                "lines": lines,
            }

    def check_availability(self) -> bool:
        return bool(self._available)


if __name__ == "__main__":
    bridge = HaskellBridge()
    print(f"Available: {bridge.available}")
    print(bridge.cast_hexagram([1, 1, 1, 1, 1, 1]))
    print(bridge.cast_hexagram([0, 0, 0, 0, 0, 0]))
    print(bridge.cast_hexagram())  # random
