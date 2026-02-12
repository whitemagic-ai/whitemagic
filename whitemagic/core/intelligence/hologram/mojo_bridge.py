"""Mojo Bridge for Holographic Encoding.
====================================

Bridges Python orchestration with Mojo performance core.
Provides 100-1000x speedup for 4D coordinate encoding.

Fallback: Standard Python CoordinateEncoder.
"""

import logging
import os
import time
from pathlib import Path
from typing import Any

# Local imports
from whitemagic.core.intelligence.hologram.encoder import (
    CoordinateEncoder as PythonEncoder,
)
from whitemagic.core.intelligence.hologram.encoder import HolographicCoordinate

logger = logging.getLogger(__name__)

class MojoEncoderBridge:
    """Bridges Python memory data to Mojo coordinate encoding logic.
    """

    def __init__(self) -> None:
        self.python_fallback = PythonEncoder()
        self.mojo_available = False
        self.mojo_bin: str | None = None
        self._check_mojo()

    def _check_mojo(self) -> Any:
        """Check if Mojo compiler/runtime is available."""
        # Check environment variable first
        if os.getenv("WHITEMAGIC_USE_MOJO", "0") == "0":
            self.mojo_available = False
            return

        # Check for mojo binary
        import subprocess
        try:
            mojo_bin = os.getenv("WHITEMAGIC_MOJO_BIN")
            if mojo_bin:
                candidate = Path(mojo_bin).expanduser()
                if candidate.is_file():
                    self.mojo_bin = str(candidate)

            if not self.mojo_bin:
                repo_root = Path(__file__).resolve()
                for _ in range(5):
                    repo_root = repo_root.parent
                candidates = [
                    repo_root / "whitemagic-mojo" / "mojo-env" / ".pixi" / "envs" / "default" / "bin" / "mojo",
                    repo_root / "whitemagic-mojo" / "mojo-pip-env" / "bin" / "mojo",
                ]
                for candidate in candidates:
                    if candidate.is_file():
                        self.mojo_bin = str(candidate)
                        break

            cmd = [self.mojo_bin or "mojo", "--version"]
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode == 0:
                self.mojo_available = True
                logger.info(f"🔥 Mojo detected: {result.stdout.strip()}")
            else:
                self.mojo_available = False
        except FileNotFoundError:
            self.mojo_available = False

        if not self.mojo_available:
            logger.debug("Mojo not found, using Python fallback for encoding")

    def encode(self, memory: dict[str, Any]) -> HolographicCoordinate:
        """Encode memory using Mojo if available, else Python.
        """
        if self.mojo_available:
            return self._encode_mojo(memory)
        return self.python_fallback.encode(memory)

    def _encode_mojo(self, memory: dict[str, Any]) -> HolographicCoordinate:
        """Call Mojo implementation.
        """
        import subprocess

        # Prepare arguments for Mojo CLI
        # <id> <content> <title> <tags_comma> <type> <garden> <importance> <neuro> <valence> <joy> <resonance> <timestamp> <current_time>
        tags_comma = ",".join(memory.get("tags", []))

        # Try to find the compiled binary first for speed
        repo_root = Path(__file__).resolve()
        for _ in range(5):
            repo_root = repo_root.parent
        binary_path = repo_root / "whitemagic-mojo" / "bin" / "coordinate_encoder_mojo"

        if binary_path.is_file():
            cmd = [str(binary_path)]
        else:
            # Fallback to 'mojo run source.mojo'
            source_path = repo_root / "whitemagic-mojo" / "src" / "coordinate_encoder.mojo"
            cmd = [self.mojo_bin or "mojo", "run", str(source_path)]

        cmd.extend([
            str(memory.get("id", "none")),
            str(memory.get("content", "")),
            str(memory.get("title", "")),
            tags_comma,
            str(memory.get("memory_type", "short_term")),
            str(memory.get("garden", "none")),
            str(memory.get("importance", 0.5)),
            str(memory.get("neuro_score", 0.5)),
            str(memory.get("emotional_valence", 0.0)),
            str(memory.get("joy_score", 0.0)),
            str(memory.get("resonance_score", 0.0)),
            str(memory.get("created_timestamp", int(time.time()))),
            str(int(time.time())), # current_time
        ])

        try:
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)

            # Parse output
            x, y, z, w = 0.0, 0.0, 0.0, 1.0
            for line in result.stdout.splitlines():
                if line.startswith("RESULT_X:"):
                    x = float(line.split(":")[1].strip())
                elif line.startswith("RESULT_Y:"):
                    y = float(line.split(":")[1].strip())
                elif line.startswith("RESULT_Z:"):
                    z = float(line.split(":")[1].strip())
                elif line.startswith("RESULT_W:"):
                    w = float(line.split(":")[1].strip())

            return HolographicCoordinate(x=x, y=y, z=z, w=w)

        except Exception as e:
            logger.error(f"❌ Mojo encoding failed, falling back: {e}")
            return self.python_fallback.encode(memory)

def get_mojo_encoder() -> Any:
    """Singleton getter for the Mojo bridge."""
    global _mojo_bridge
    if "_mojo_bridge" not in globals():
        globals()["_mojo_bridge"] = MojoEncoderBridge()
    return globals()["_mojo_bridge"]
