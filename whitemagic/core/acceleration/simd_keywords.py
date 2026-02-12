"""Zig SIMD Keyword Extraction — Python Bridge.
=============================================
Loads the compiled Zig shared library and exposes SIMD-accelerated
keyword extraction for text processing. Falls back to pure Python
when the Zig library is not available.

The Zig implementation uses:
- 16-byte SIMD lanes for tokenization
- Vectorized ASCII lowercase
- Comptime bloom filter for stopword checking
- Deduped keyword extraction

Usage:
    from whitemagic.core.acceleration.simd_keywords import extract_keywords, simd_keywords_status
    keywords = extract_keywords("some text content here", max_keywords=50)
"""
from __future__ import annotations

import ctypes
import logging
import re
import threading
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

_lib = None
_lib_lock = threading.Lock()
_HAS_ZIG = False

# Python fallback stopwords
_STOP_WORDS = frozenset({
    "the", "a", "an", "is", "are", "was", "were", "be", "been", "being",
    "have", "has", "had", "do", "does", "did", "will", "would", "shall",
    "should", "may", "might", "can", "could", "must", "to", "of", "in",
    "for", "on", "with", "at", "by", "from", "as", "into", "through",
    "during", "before", "after", "above", "below", "between", "under",
    "again", "further", "then", "once", "here", "there", "when", "where",
    "why", "how", "all", "each", "every", "both", "few", "more", "most",
    "other", "some", "such", "no", "nor", "not", "only", "own", "same",
    "so", "than", "too", "very", "just", "because", "but", "and", "or",
    "if", "while", "about", "up", "out", "off", "over", "this", "that",
    "these", "those", "it", "its", "my", "your", "his", "her", "our",
    "their", "what", "which", "who", "whom", "me", "him", "them", "we",
    "you", "they", "i", "he", "she", "us",
})
_WORD_RE = re.compile(r"[a-z_][a-z0-9_]{2,}")


def _find_zig_lib() -> str | None:
    """Locate the compiled Zig shared library."""
    import os
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
    """Load the Zig shared library and set up keyword extraction FFI."""
    global _lib, _HAS_ZIG
    if _lib is not None:
        return _lib
    with _lib_lock:
        if _lib is not None:
            return _lib
        path = _find_zig_lib()
        if not path:
            logger.debug("Zig SIMD library not found — using Python fallback for keywords")
            return None
        try:
            lib = ctypes.CDLL(path)

            # wm_extract_keywords(text_ptr, text_len, out_ptr, out_capacity, max_keywords) -> count
            lib.wm_extract_keywords.argtypes = [
                ctypes.POINTER(ctypes.c_ubyte),
                ctypes.c_size_t,
                ctypes.POINTER(ctypes.c_ubyte),
                ctypes.c_size_t,
                ctypes.c_size_t,
            ]
            lib.wm_extract_keywords.restype = ctypes.c_size_t

            _lib = lib
            _HAS_ZIG = True
            logger.info("Zig SIMD keyword extraction loaded: path=%s", path)
            return lib
        except Exception as e:
            logger.debug("Failed to load Zig SIMD library for keywords: %s", e)
            return None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def extract_keywords(text: str, max_keywords: int = 50) -> set[str]:
    """Extract keywords from text using SIMD acceleration if available.

    Args:
        text: Input text to extract keywords from.
        max_keywords: Maximum number of keywords to return.

    Returns:
        Set of keyword strings.

    """
    if not text:
        return set()

    lib = _load_lib()
    if lib is not None:
        try:
            text_bytes = text.encode("utf-8", errors="replace")
            text_arr = (ctypes.c_ubyte * len(text_bytes))(*text_bytes)

            # Output buffer: generous allocation (keywords are null-separated)
            out_capacity = min(max_keywords * 64, 16384)
            out_arr = (ctypes.c_ubyte * out_capacity)()

            count = lib.wm_extract_keywords(
                text_arr, len(text_bytes),
                out_arr, out_capacity,
                max_keywords,
            )

            if count > 0:
                # Parse null-separated keywords from output buffer
                raw = bytes(out_arr[:out_capacity])
                keywords = set()
                for kw_bytes in raw.split(b"\x00"):
                    kw = kw_bytes.decode("utf-8", errors="replace").strip()
                    if kw and len(kw) > 2:
                        keywords.add(kw)
                    if len(keywords) >= max_keywords:
                        break
                if keywords:
                    return keywords
        except Exception as e:
            logger.debug("Zig keyword extraction failed, using Python: %s", e)

    # Python fallback
    return _py_extract_keywords(text, max_keywords)


def _py_extract_keywords(text: str, max_keywords: int) -> set[str]:
    """Pure Python keyword extraction fallback."""
    text_lower = text.lower()
    words = _WORD_RE.findall(text_lower)
    keywords = {w for w in words if w not in _STOP_WORDS and len(w) > 2}

    if len(keywords) > max_keywords:
        from collections import defaultdict
        freq: defaultdict[str, int] = defaultdict(int)
        for w in words:
            if w in keywords:
                freq[w] += 1
        sorted_kw = sorted(keywords, key=lambda k: freq[k], reverse=True)
        keywords = set(sorted_kw[:max_keywords])

    return keywords


def simd_keywords_status() -> dict[str, Any]:
    """Get SIMD keyword extraction status."""
    _load_lib()
    return {
        "has_zig_simd": _HAS_ZIG,
        "lib_path": _find_zig_lib() or "not found",
        "backend": "zig_simd" if _HAS_ZIG else "python_fallback",
    }
