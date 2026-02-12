"""Mojo Accelerator Bridge — Python ↔ Mojo via Subprocess.
========================================================
Bridges to compiled Mojo executables for batch encoding, embedding
quantization, and neuro scoring. Falls back to pure Python when
Mojo binaries are not available.

Mojo modules:
- batch_encoder.mojo — 5D holographic coordinate encoding from text signals
- embedding_quantize.mojo — Int8 + binary quantization with Hamming distance
- neuro_batch.mojo — 7-signal neuro importance scoring engine

Communication: JSON over stdin/stdout subprocess calls.
This avoids the need for a persistent Mojo server while keeping
the interface clean. Each call spawns a short-lived process.

Usage:
    from whitemagic.core.acceleration.mojo_bridge import (
        mojo_batch_encode, mojo_quantize, mojo_neuro_score, mojo_status
    )
"""
from __future__ import annotations

import json
import logging
import os
import shutil
import subprocess
import threading
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

_mojo_bin: str | None = None
_mojo_lock = threading.Lock()
_HAS_MOJO = False
_MOJO_DIR: Path | None = None


def _find_mojo() -> tuple[str | None, Path | None]:
    """Locate the Mojo binary and source directory."""
    base = Path(__file__).resolve().parent.parent.parent.parent
    mojo_dir = base / "whitemagic-mojo"

    # Find mojo binary
    candidates = [
        os.environ.get("MOJO_PATH", ""),
        str(base / ".venv" / "bin" / "mojo"),
        shutil.which("mojo") or "",
    ]
    for path in candidates:
        if path and os.path.isfile(path) and os.access(path, os.X_OK):
            if mojo_dir.is_dir():
                return path, mojo_dir
            return path, None

    return None, mojo_dir if mojo_dir.is_dir() else None


def _init_mojo() -> Any:
    """Initialize Mojo bridge (lazy, thread-safe)."""
    global _mojo_bin, _HAS_MOJO, _MOJO_DIR
    if _mojo_bin is not None or _HAS_MOJO:
        return
    with _mojo_lock:
        if _mojo_bin is not None or _HAS_MOJO:
            return
        _mojo_bin, _MOJO_DIR = _find_mojo()
        if _mojo_bin:
            _HAS_MOJO = True
            logger.info("Mojo bridge initialized: bin=%s, dir=%s", _mojo_bin, _MOJO_DIR)
        else:
            logger.debug("Mojo binary not found — using Python fallback")


# ---------------------------------------------------------------------------
# Batch 5D Holographic Encoding
# ---------------------------------------------------------------------------

def mojo_batch_encode(
    memories: list[dict[str, Any]],
) -> list[tuple[float, float, float, float, float]] | None:
    """Encode a batch of memories into 5D holographic coordinates using Mojo SIMD.

    Each memory dict should have keys: content, memory_type, importance,
    emotional_valence, recall_count, tags, garden.

    Returns:
        List of (x, y, z, w, v) coordinate tuples, or None if Mojo unavailable.

    """
    _init_mojo()
    if not _HAS_MOJO or not _MOJO_DIR:
        return None

    try:
        request = {
            "command": "batch_encode",
            "memories": memories,
        }
        result = _call_mojo("batch_encoder", request)
        if result and "coordinates" in result:
            return [tuple(c) for c in result["coordinates"]]
    except Exception as e:
        logger.debug("Mojo batch_encode failed: %s", e)
    return None


# ---------------------------------------------------------------------------
# Embedding Quantization
# ---------------------------------------------------------------------------

def mojo_quantize(
    vectors: list[list[float]],
    mode: str = "int8",
) -> dict[str, Any] | None:
    """Quantize embedding vectors using Mojo SIMD.

    Args:
        vectors: List of float vectors to quantize.
        mode: "int8" for 8-bit or "binary" for 1-bit quantization.

    Returns:
        Dict with quantized data and metadata, or None.

    """
    _init_mojo()
    if not _HAS_MOJO or not _MOJO_DIR:
        return None

    try:
        request = {
            "command": "quantize",
            "vectors": vectors,
            "mode": mode,
        }
        return _call_mojo("embedding_quantize", request)
    except Exception as e:
        logger.debug("Mojo quantize failed: %s", e)
    return None


# ---------------------------------------------------------------------------
# Neuro Importance Scoring
# ---------------------------------------------------------------------------

def mojo_neuro_score(
    memories: list[dict[str, Any]],
) -> list[dict[str, Any]] | None:
    """Score a batch of memories using Mojo's 7-signal neuro scoring engine.

    Each memory dict should have keys: importance, emotional_valence,
    recall_count, is_protected, memory_type, tags, content.

    Returns:
        List of dicts with {id, neuro_score, signals} or None.

    """
    _init_mojo()
    if not _HAS_MOJO or not _MOJO_DIR:
        return None

    try:
        request = {
            "command": "neuro_score",
            "memories": memories,
        }
        result = _call_mojo("neuro_batch", request)
        if result and "scores" in result:
            scores = result.get("scores")
            if isinstance(scores, list):
                return [item for item in scores if isinstance(item, dict)]
    except Exception as e:
        logger.debug("Mojo neuro_score failed: %s", e)
    return None


# ---------------------------------------------------------------------------
# Internal: subprocess call to Mojo
# ---------------------------------------------------------------------------

def _call_mojo(module_name: str, request: dict[str, Any]) -> dict[str, Any] | None:
    """Call a Mojo module via subprocess with JSON stdin/stdout."""
    if not _mojo_bin or not _MOJO_DIR:
        return None

    src_path = _MOJO_DIR / "src" / f"{module_name}.mojo"
    if not src_path.exists():
        logger.debug("Mojo source not found: %s", src_path)
        return None

    try:
        proc = subprocess.run(
            [_mojo_bin, "run", str(src_path)],
            input=json.dumps(request),
            capture_output=True,
            text=True,
            timeout=30,
            cwd=str(_MOJO_DIR),
        )
        if proc.returncode == 0 and proc.stdout.strip():
            parsed = json.loads(proc.stdout.strip())
            if isinstance(parsed, dict):
                return parsed
        elif proc.stderr:
            logger.debug("Mojo %s stderr: %s", module_name, proc.stderr[:200])
    except subprocess.TimeoutExpired:
        logger.warning("Mojo %s timed out after 30s", module_name)
    except Exception as e:
        logger.debug("Mojo %s call failed: %s", module_name, e)

    return None


# ---------------------------------------------------------------------------
# Status
# ---------------------------------------------------------------------------

def mojo_status() -> dict[str, Any]:
    """Get Mojo bridge status."""
    _init_mojo()
    return {
        "has_mojo": _HAS_MOJO,
        "mojo_bin": _mojo_bin or "not found",
        "mojo_dir": str(_MOJO_DIR) if _MOJO_DIR else "not found",
        "modules": {
            "batch_encoder": (_MOJO_DIR / "src" / "batch_encoder.mojo").exists() if _MOJO_DIR else False,
            "embedding_quantize": (_MOJO_DIR / "src" / "embedding_quantize.mojo").exists() if _MOJO_DIR else False,
            "neuro_batch": (_MOJO_DIR / "src" / "neuro_batch.mojo").exists() if _MOJO_DIR else False,
        },
        "backend": "mojo_simd" if _HAS_MOJO else "python_fallback",
    }
