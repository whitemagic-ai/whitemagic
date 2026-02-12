#!/usr/bin/env python3
"""Batch Holographic Encoder v2.0.
=============================
Optimized Python batch encoder for mass memory ingestion.
Used as fallback when Mojo is unavailable, or as primary for robustness.

Handles None values gracefully and processes batches efficiently.
"""

import json
import sys
import time
from dataclasses import dataclass
from typing import Any


@dataclass
class HolographicCoord:
    x: float
    y: float
    z: float
    w: float

    def to_dict(self) -> dict[str, float]:
        return {"x": self.x, "y": self.y, "z": self.z, "w": self.w}


class FastBatchEncoder:
    """Optimized encoder for batch processing with null safety."""

    # Pre-computed tag sets for O(1) lookup
    LOGIC_TAGS = frozenset({"logic", "strategy", "code", "architecture", "plan", "analysis",
                            "algorithm", "proof", "calculation", "optimization", "debug"})
    EMOTION_TAGS = frozenset({"joy", "love", "wonder", "beauty", "truth", "grief", "hope",
                              "compassion", "gratitude", "fear", "anger", "serenity"})
    MICRO_TAGS = frozenset({"detail", "bug", "typo", "line", "character", "atom", "bit",
                            "byte", "function", "variable", "specific"})
    MACRO_TAGS = frozenset({"architecture", "system", "design", "overview", "strategy",
                            "roadmap", "vision", "ecosystem", "universe"})
    IMPORTANT_TAGS = frozenset({"critical", "important", "key", "core", "essential",
                                 "milestone", "breakthrough", "wisdom", "principle"})

    def __init__(self) -> None:
        self._base_time = int(time.time())

    def safe_float(self, value: Any, default: float = 0.0) -> float:
        """Convert to float with None safety."""
        if value is None:
            return default
        try:
            return float(value)
        except (ValueError, TypeError):
            return default

    def safe_int(self, value: Any, default: int = 0) -> int:
        """Convert to int with None safety."""
        if value is None:
            return default
        try:
            return int(value)
        except (ValueError, TypeError):
            return default

    def encode_batch(self, memories: list[dict[str, Any]]) -> list[HolographicCoord]:
        """Encode a batch of memories efficiently."""
        results = []
        for mem in memories:
            try:
                coord = self._encode_single(mem)
                results.append(coord)
            except Exception:
                # Fallback to neutral position
                results.append(HolographicCoord(0.0, 0.0, 0.0, 0.5))
        return results

    def _encode_single(self, mem: dict[str, Any]) -> HolographicCoord:
        """Encode a single memory with null safety."""
        tags = set(t.lower() for t in (mem.get("tags") or []))
        content = str(mem.get("content") or "").lower()

        x = self._calc_x(tags, content, mem)
        y = self._calc_y(tags, content, mem)
        z = self._calc_z(tags, content, mem)
        w = self._calc_w(tags, mem)

        return HolographicCoord(x, y, z, w)

    def _calc_x(self, tags: set, content: str, mem: dict) -> float:
        """X-axis: Logic (-1) to Emotion (+1)."""
        # Start with emotional valence if available
        valence = self.safe_float(mem.get("emotional_valence"), 0.0)
        score = -0.5 * valence

        # Tag-based adjustments
        logic_count = len(tags.intersection(self.LOGIC_TAGS))
        emotion_count = len(tags.intersection(self.EMOTION_TAGS))
        score += (logic_count - emotion_count) * 0.15

        return max(-1.0, min(1.0, score))

    def _calc_y(self, tags: set, content: str, mem: dict) -> float:
        """Y-axis: Micro (-1) to Macro (+1)."""
        micro_count = len(tags.intersection(self.MICRO_TAGS))
        macro_count = len(tags.intersection(self.MACRO_TAGS))
        score = (macro_count - micro_count) * 0.2

        # Content length as signal
        content_len = len(content)
        if content_len > 1000:
            score += 0.2
        elif content_len < 100:
            score -= 0.2

        return max(-1.0, min(1.0, score))

    def _calc_z(self, tags: set, content: str, mem: dict) -> float:
        """Z-axis: Time (Past -1 to Future +1)."""
        created = self.safe_int(mem.get("created_timestamp"), self._base_time)
        self.safe_int(mem.get("updated_timestamp"), created)

        # Normalize to days from now
        now = self._base_time
        days_old = (now - created) / 86400

        # Scale: very old = -1, today = 0, future planning = +1
        if days_old > 365:
            score = -0.8
        elif days_old > 30:
            score = -0.4
        elif days_old > 7:
            score = -0.2
        else:
            score = 0.0

        # Future keywords
        if any(w in content for w in ["will ", "plan to", "next step", "future", "goal"]):
            score += 0.3

        return max(-1.0, min(1.0, score))

    def _calc_w(self, tags: set, mem: dict) -> float:
        """W-axis: Importance/Gravity (0.1 to 2.0+)."""
        # Get base values with null safety
        importance = self.safe_float(mem.get("importance"), 0.5)
        neuro_score = self.safe_float(mem.get("neuro_score"), 1.0)
        joy_score = self.safe_float(mem.get("joy_score"), 0.0)
        resonance_score = self.safe_float(mem.get("resonance_score"), 0.0)

        # Base gravity
        base = (importance * 0.4) + (neuro_score * 0.6)

        # Memory type boost
        mem_type = str(mem.get("memory_type") or mem.get("type") or "").lower()
        if mem_type == "long_term":
            base += 0.3
        elif mem_type == "short_term":
            base -= 0.1

        # Important tags
        base += 0.1 * len(tags.intersection(self.IMPORTANT_TAGS))

        # Joy/resonance
        w = base + (joy_score * 0.5) + (resonance_score * 0.5)

        return max(0.1, w)


def main() -> Any:
    """CLI interface: read JSON from stdin, output coordinates."""
    if len(sys.argv) > 1 and sys.argv[1] == "--batch":
        # Read JSON from stdin
        data = json.loads(sys.stdin.read())
        items = data.get("items", [])

        encoder = FastBatchEncoder()
        results = encoder.encode_batch(items)

        for i, item in enumerate(items):
            mem_id = item.get("id", f"unknown_{i}")
            coord = results[i]
            print(f"ID:{mem_id}|RES:{coord.x},{coord.y},{coord.z},{coord.w}")
    else:
        print("Usage: echo '{\"items\": [...]}' | python batch_encoder.py --batch")
        sys.exit(1)


if __name__ == "__main__":
    main()
