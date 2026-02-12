"""Neuro-Score Bridge — Python interface to Mojo neuro_score.
===========================================================
Tries the compiled Mojo binary first ($WM_MOJO_NEURO_SCORE or auto-detect),
falls back to a pure-Python reimplementation of the same algorithm.

Algorithm:
  1. Exponential decay: score * e^(-decay_rate * days_since_access)
  2. Reinforcement: score + importance * 0.1 (capped at 1.0)
  3. Novelty blend: 80% decayed score + 20% novelty
"""

from __future__ import annotations

import logging
import math
import os
import subprocess
from dataclasses import dataclass

logger = logging.getLogger(__name__)

# Look for compiled Mojo binary
_MOJO_BINARY: str | None = None


def _find_mojo_binary() -> str | None:
    """Locate the compiled neuro_score binary."""
    global _MOJO_BINARY
    if _MOJO_BINARY is not None:
        return _MOJO_BINARY if _MOJO_BINARY != "" else None

    # Check env var first
    env_path = os.environ.get("WM_MOJO_NEURO_SCORE")
    if env_path and os.path.isfile(env_path) and os.access(env_path, os.X_OK):
        _MOJO_BINARY = env_path
        return _MOJO_BINARY

    # Auto-detect in project
    project_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__),
    ))))
    candidates = [
        os.path.join(project_root, "whitemagic-mojo", "build", "neuro_score"),
        os.path.join(project_root, "whitemagic-mojo", "neuro_score"),
    ]
    for c in candidates:
        if os.path.isfile(c) and os.access(c, os.X_OK):
            _MOJO_BINARY = c
            return _MOJO_BINARY

    _MOJO_BINARY = ""  # Cache miss
    return None


@dataclass
class NeuroScoreInput:
    """Input for neuro-scoring a memory."""

    current_score: float
    access_count: int
    total_memories: int
    days_since_access: float
    importance: float
    decay_rate: float = 0.05


@dataclass
class NeuroScoreResult:
    """Result from neuro-scoring."""

    final_score: float
    used_mojo: bool
    decay_component: float
    novelty_component: float


def _python_neuro_score(inp: NeuroScoreInput) -> NeuroScoreResult:
    """Pure-Python reimplementation of the Mojo NeuroScorer algorithm."""
    # 1. Decay
    decay_factor = math.exp(-inp.decay_rate * inp.days_since_access)
    score = inp.current_score * decay_factor

    # 2. Reinforce
    if inp.importance > 0:
        score = min(1.0, score + inp.importance * 0.1)

    # 3. Novelty
    if inp.access_count == 0:
        novelty = 1.0
    else:
        freq = inp.access_count / max(inp.total_memories, 1)
        novelty = 1.0 - freq

    # 4. Blend
    final = score * 0.8 + novelty * 0.2

    return NeuroScoreResult(
        final_score=round(final, 6),
        used_mojo=False,
        decay_component=round(score, 6),
        novelty_component=round(novelty, 6),
    )


def _mojo_neuro_score(inp: NeuroScoreInput, binary: str) -> NeuroScoreResult | None:
    """Call the compiled Mojo binary."""
    try:
        result = subprocess.run(
            [binary, str(inp.current_score), str(inp.access_count),
             str(inp.total_memories), str(inp.days_since_access), str(inp.importance)],
            capture_output=True, text=True, timeout=5,
        )
        if result.returncode == 0:
            for line in result.stdout.strip().split("\n"):
                if "RESULT_SCORE:" in line:
                    score = float(line.split(":")[-1].strip())
                    return NeuroScoreResult(
                        final_score=round(score, 6),
                        used_mojo=True,
                        decay_component=0.0,  # Mojo doesn't expose components
                        novelty_component=0.0,
                    )
    except Exception as e:
        logger.debug("Mojo neuro_score failed: %s", e)
    return None


def neuro_score(inp: NeuroScoreInput) -> NeuroScoreResult:
    """Compute neuro-score for a memory.
    Uses Mojo binary if available, otherwise pure Python.
    """
    binary = _find_mojo_binary()
    if binary:
        result = _mojo_neuro_score(inp, binary)
        if result is not None:
            return result

    return _python_neuro_score(inp)


def batch_neuro_score(inputs: list[NeuroScoreInput]) -> list[NeuroScoreResult]:
    """Score multiple memories."""
    return [neuro_score(inp) for inp in inputs]
