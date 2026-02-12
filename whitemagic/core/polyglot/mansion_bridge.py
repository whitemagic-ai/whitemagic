"""MansionBridge — Unified Polyglot FFI Layer.

Philosophy: Polyglot specialized alternatives with Python as fallback core.

This module provides a unified API for all high-performance backends:
- Rust (whitemagic_rs) — PyO3 bindings
- Zig (via Rust FFI) — Ultra-low-level operations
- Mojo (accelerator binaries) — SIMD acceleration

Each operation automatically selects the fastest available backend,
with transparent fallback to Python if native code isn't available.
"""

import logging
import subprocess
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class Backend(Enum):
    """Available computation backends."""

    PYTHON = "python"
    RUST = "rust"
    ZIG = "zig"
    MOJO = "mojo"


@dataclass
class BackendStatus:
    """Status of a backend."""

    available: bool
    path: str | None = None
    version: str | None = None
    functions: list[str] = field(default_factory=list)


@dataclass
class MansionBridge:
    """Unified polyglot FFI layer for WhiteMagic.

    Provides automatic backend selection with fallback:
    1. Try Rust (fastest, most functions)
    2. Try Mojo (SIMD acceleration)
    3. Fall back to Python (always available)
    """

    # Project root
    project_root: Path = field(default_factory=lambda: Path(__file__).resolve().parent.parent.parent)

    # Backend status cache (initialized in __post_init__)
    _rust_status: BackendStatus = field(default_factory=lambda: BackendStatus(available=False))
    _mojo_status: BackendStatus = field(default_factory=lambda: BackendStatus(available=False))
    _zig_status: BackendStatus = field(default_factory=lambda: BackendStatus(available=False))

    def __post_init__(self) -> None:
        self._init_backends()

    def _init_backends(self) -> None:
        """Initialize and detect available backends."""
        self._rust_status = self._detect_rust()
        self._mojo_status = self._detect_mojo()
        self._zig_status = self._detect_zig()

        logger.info("MansionBridge initialized:")
        logger.info(f"  Rust: {'✅' if self._rust_status.available else '❌'}")
        logger.info(f"  Mojo: {'✅' if self._mojo_status.available else '❌'}")
        logger.info(f"  Zig:  {'✅' if self._zig_status.available else '❌'}")

    def _detect_rust(self) -> BackendStatus:
        """Detect Rust backend (whitemagic_rs)."""
        try:
            import whitemagic_rs
            functions = [
                name for name in dir(whitemagic_rs)
                if not name.startswith("_")
            ]
            return BackendStatus(
                available=True,
                path="whitemagic_rs (PyO3)",
                functions=functions,
            )
        except ImportError:
            return BackendStatus(available=False)

    def _detect_mojo(self) -> BackendStatus:
        """Detect Mojo backend."""
        # Check for compiled binaries
        bin_dir = self.project_root / "whitemagic-mojo" / "bin"
        mojo_binaries = []

        if bin_dir.exists():
             try:
                mojo_binaries = [
                    f.name for f in bin_dir.iterdir()
                    if f.is_file() and not f.name.startswith(".")
                ]
             except Exception:
                pass

        # Check for mojo executable
        mojo_path = self.project_root / ".venv" / "bin" / "mojo"
        mojo_available = mojo_path.exists() or len(mojo_binaries) > 0

        return BackendStatus(
            available=mojo_available,
            path=str(mojo_path) if mojo_path.exists() else str(bin_dir),
            functions=mojo_binaries,
        )

    def _detect_zig(self) -> BackendStatus:
        """Detect Zig backend (via Rust FFI)."""
        if self._rust_status and self._rust_status.available:
            zig_functions = [
                f for f in self._rust_status.functions
                if f.startswith("zig_")
            ]
            if zig_functions:
                return BackendStatus(
                    available=True,
                    path="via Rust FFI",
                    functions=zig_functions,
                )
        return BackendStatus(available=False)

    # === Status Methods ===

    def get_status(self) -> dict[str, BackendStatus]:
        """Get status of all backends."""
        return {
            "rust": self._rust_status,
            "mojo": self._mojo_status,
            "zig": self._zig_status,
            "python": BackendStatus(available=True, path="builtin", functions=["all"]),
        }

    def print_status(self) -> None:
        """Print status of all backends."""
        print("=" * 50)
        print("MansionBridge Polyglot Status")
        print("=" * 50)

        for name, status in self.get_status().items():
            symbol = "✅" if status.available else "❌"
            print(f"{symbol} {name.upper()}: ", end="")
            if status.available:
                print(f"{len(status.functions)} functions")
                if status.path:
                    print(f"   └─ Path: {status.path}")
            else:
                print("Not available")

    # === Core Operations ===

    def similarity(
        self,
        text1: str,
        text2: str,
        backend: str | None = None,
    ) -> float:
        """Calculate similarity between two texts.

        Uses Rust if available (~300x faster), else Python.
        """
        # Force specific backend
        if backend == "python":
            return self._similarity_python(text1, text2)

        # Try Rust first
        if self._rust_status.available:
            try:
                import whitemagic_rs
                similarity = whitemagic_rs.fast_similarity(text1, text2)
                return float(similarity)
            except Exception as e:
                logger.warning(f"Rust similarity failed: {e}")

        # Fallback to Python
        return self._similarity_python(text1, text2)

    def _similarity_python(self, text1: str, text2: str) -> float:
        """Python fallback for similarity."""
        from difflib import SequenceMatcher
        return SequenceMatcher(None, text1, text2).ratio()

    def extract_patterns(
        self,
        content: str,
        backend: str | None = None,
    ) -> list[tuple[str, str, str, str]]:
        """Extract patterns from content.

        Uses Rust if available (~23x faster).
        """
        if self._rust_status.available and backend != "python":
            try:
                import whitemagic_rs
                raw_patterns = whitemagic_rs.extract_patterns_from_content(content)
                if isinstance(raw_patterns, list):
                    normalized: list[tuple[str, str, str, str]] = []
                    for item in raw_patterns:
                        if isinstance(item, (tuple, list)) and len(item) >= 4:
                            normalized.append((str(item[0]), str(item[1]), str(item[2]), str(item[3])))
                    return normalized
            except Exception as e:
                logger.warning(f"Rust pattern extraction failed: {e}")

        # Python fallback
        return self._extract_patterns_python(content)

    def _extract_patterns_python(self, content: str) -> list[tuple[str, str, str, str]]:
        """Python fallback for pattern extraction."""
        patterns = []
        lines = content.split("\n")

        for line in lines:
            lower = line.lower()
            if "solution" in lower:
                patterns.append(("solution", line, "", ""))
            elif "error" in lower:
                patterns.append(("anti_pattern", line, "", ""))
            elif "should" in lower or "must" in lower:
                patterns.append(("heuristic", line, "", ""))

        return patterns

    # === Mojo Operations ===

    def run_mojo_binary(
        self,
        binary_name: str,
        args: list[str] = [],
        stdin: str | None = None,
    ) -> str | None:
        """Run a compiled Mojo binary.
        """
        if not self._mojo_status.available:
            logger.warning("Mojo not available")
            return None

        bin_path = self.project_root / "whitemagic-mojo" / "bin" / binary_name

        if not bin_path.exists():
            logger.error(f"Mojo binary not found: {bin_path}")
            return None

        try:
            result = subprocess.run(
                [str(bin_path)] + args,
                capture_output=True,
                text=True,
                input=stdin,
                timeout=30,
            )

            if result.returncode != 0:
                logger.error(f"Mojo binary failed: {result.stderr}")
                return None

            return result.stdout.strip()
        except Exception as e:
            logger.error(f"Mojo execution error: {e}")
            return None

    def neuro_score(self, data: str) -> str | None:
        """Run Mojo neuro_score binary."""
        return self.run_mojo_binary("neuro_score_mojo", stdin=data)

    def zodiac_engine(self, data: str) -> str | None:
        """Run Mojo zodiac_engine binary."""
        return self.run_mojo_binary("zodiac_engine_mojo", stdin=data)

    def coordinate_encoder(self, data: str) -> str | None:
        """Run Mojo coordinate_encoder binary."""
        return self.run_mojo_binary("coordinate_encoder_mojo", stdin=data)

    # === Zig Operations (via Rust) ===

    def iching_cast(self) -> dict[str, Any] | None:
        """Cast I Ching hexagram using Zig backend."""
        if not self._zig_status.available:
            logger.warning("Zig not available")
            return None

        try:
            import whitemagic_rs
            result = whitemagic_rs.zig_py_iching_cast()
            if isinstance(result, dict):
                return result
        except Exception as e:
            logger.error(f"Zig I Ching failed: {e}")
            return None
        return None

    def holographic_project(self, *args: Any) -> Any | None:
        """Holographic projection using Zig backend."""
        if not self._zig_status.available:
            return None

        try:
            import whitemagic_rs
            return whitemagic_rs.zig_py_holographic_project(*args)
        except Exception as e:
            logger.error(f"Zig holographic failed: {e}")
            return None

    # === Rust Holographic Operations ===

    def create_holographic_index(self) -> Any:
        """Create a 4D holographic index."""
        if not self._rust_status.available:
            logger.warning("Rust not available for holographic index")
            return None

        try:
            import whitemagic_rs
            # Note: We updated this in previous work to SpatialIndex if needed,
            # but let's check what whitemagic_rs actually has.
            if hasattr(whitemagic_rs, "SpatialIndex"):
                return whitemagic_rs.SpatialIndex()
            elif hasattr(whitemagic_rs, "HolographicIndex"):
                return whitemagic_rs.HolographicIndex()
            return None
        except Exception as e:
            logger.error(f"Failed to create holographic index: {e}")
            return None

    # === Heaven's Net (Archaeology) ===

    def cast_heavens_net(
        self,
        directory: str,
        limit: int = 1000,
    ) -> Any | None:
        """Cast Heaven's Net for concept extraction."""
        if not self._rust_status.available:
            logger.warning("Rust not available for Heaven's Net")
            return None

        try:
            import whitemagic_rs
            return whitemagic_rs.cast_heavens_net(directory, limit)
        except Exception as e:
            logger.error(f"Heaven's Net failed: {e}")
            return None


# === Singleton ===

_bridge: MansionBridge | None = None


def get_mansion_bridge() -> MansionBridge:
    """Get the singleton MansionBridge instance."""
    global _bridge
    if _bridge is None:
        _bridge = MansionBridge()
    return _bridge
