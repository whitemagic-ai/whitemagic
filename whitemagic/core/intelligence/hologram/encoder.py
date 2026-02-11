"""Holographic Coordinate Encoder v2.0.
===================================

Converts Memory objects into 4D coordinate vectors [x, y, z, w].

Axis Definitions:
-----------------
X-Axis (Logic <-> Emotion):
    -1.0: Pure Emotion (High joy/fear/anger, low strategic confidence)
     0.0: Balanced / Neutral
    +1.0: Pure Logic (High strategic confidence, low emotional intensity)

Y-Axis (Micro <-> Macro):
    -1.0: Micro / Concrete (Logs, raw data, specific events)
     0.0: Mid-level (Summaries, chapters)
    +1.0: Macro / Abstract (Patterns, principles, wisdom, archetypes)

Z-Axis (Time / Chronos):
    -1.0: Ancient History (Deep archive)
     0.0: Present Moment
    +1.0: Future Vision (Plans, prophecies)

W-Axis (Importance / Gravity):
    0.0: Trivial (Noise)
    1.0: Critical (Signal)
    >1.0: High Gravity (Black Hole / Attractor) - Driven by Joy/Resonance.

V-Axis (Vitality / Galactic Distance):
    1.0: Core (Active spotlight, high retention, frequently accessed)
    0.5: Mid-Band (Moderate relevance)
    0.0: Far Edge (Deep archive, low retention, rarely accessed)
    Derived from galactic_distance: v = 1.0 - galactic_distance

v2.0 Changes:
- Improved keyword matching for all axes
- Fixed memory_type field name
- Better time calculation using actual timestamps
- Content hash for deterministic spread
"""

import hashlib
from dataclasses import dataclass
from datetime import datetime
from typing import Any, cast


@dataclass
class HolographicCoordinate:
    x: float  # Logic <-> Emotion
    y: float  # Micro <-> Macro
    z: float  # Time
    w: float  # Importance/Gravity
    v: float = 0.5  # Vitality/Galactic Distance (1.0=core, 0.0=edge)

    def to_vector(self) -> list[float]:
        return [self.x, self.y, self.z, self.w, self.v]

    def to_vector_4d(self) -> list[float]:
        """Legacy 4D vector for backward compatibility."""
        return [self.x, self.y, self.z, self.w]

    def to_dict(self) -> dict[str, float]:
        return {"x": self.x, "y": self.y, "z": self.z, "w": self.w, "v": self.v}

class CoordinateEncoder:
    """Encodes memories into holographic coordinates (v2.0)."""

    def __init__(self) -> None:
        self._cache: dict[str, HolographicCoordinate] = {}
        self._garden_bias_enabled = True

    def encode(self, memory: dict[str, Any]) -> HolographicCoordinate:
        """Encode a memory dictionary into a HolographicCoordinate."""
        import time

        # 0. Try Rust v13.1 accelerated encoding (fastest path)
        try:
            from whitemagic.optimization.rust_accelerators import (
                holographic_encode_single,
                rust_v131_available,
            )
            if rust_v131_available():
                result = holographic_encode_single(memory)
                if result:
                    return HolographicCoordinate(
                        result.get("x", 0.0),
                        result.get("y", 0.0),
                        result.get("z", 0.0),
                        result.get("w", 0.5),
                        result.get("v", 0.5),
                    )
        except Exception:
            pass

        # 1. Base calculations (always do these first for fallback/verification)
        x = self._calculate_x(memory)
        y = self._calculate_y(memory)
        z = self._calculate_z(memory)
        w = self._calculate_w(memory)
        v = self._calculate_v(memory)

        # 2. Try polyglot routing for Mojo/Zig acceleration (v2.1 optimization)
        # Avoid circular imports by checking if we are already in a fallback
        if getattr(self, "_routing_active", False):
            # Already in target calculation, don't route again
            pass
        else:
            try:
                from whitemagic.optimization.polyglot_router import get_router
                router = get_router()
                current_time = int(time.time())

                # Set flag to prevent recursion
                self._routing_active = True
                mojo_coords = router.encode_holographic(memory, current_time)
                self._routing_active = False

                if mojo_coords:
                    return HolographicCoordinate(
                        mojo_coords.get("x", x),
                        mojo_coords.get("y", y),
                        mojo_coords.get("z", z),
                        mojo_coords.get("w", w),
                        mojo_coords.get("v", v),
                    )
            except Exception:
                self._routing_active = False
                # Fall through to Python math

        # Legacy fallback if router fails completely
        x = self._calculate_x(memory)
        y = self._calculate_y(memory)
        z = self._calculate_z(memory)
        w = self._calculate_w(memory)
        v = self._calculate_v(memory)

        # Apply garden bias if present
        if self._garden_bias_enabled:
            garden_bias = self._get_garden_bias(memory)
            if garden_bias:
                x, y, z, w, v = self._blend_with_garden(x, y, z, w, garden_bias, v=v)

        return HolographicCoordinate(x, y, z, w, v)

    def encode_batch(self, memories: list[dict[str, Any]]) -> list[HolographicCoordinate]:
        """Batch-encode memories into holographic coordinates.

        Uses Rust Rayon parallelism when available, falls back to
        sequential Python encoding.
        """
        # Try Rust v13.1 batch encoding (Rayon parallel)
        try:
            from whitemagic.optimization.rust_accelerators import (
                holographic_encode_batch,
                rust_v131_available,
            )
            if rust_v131_available() and len(memories) > 1:
                results = holographic_encode_batch(memories)
                if results and len(results) == len(memories):
                    return [
                        HolographicCoordinate(
                            r.get("x", 0.0),
                            r.get("y", 0.0),
                            r.get("z", 0.0),
                            r.get("w", 0.5),
                            r.get("v", 0.5),
                        )
                        for r in results
                    ]
        except Exception:
            pass

        # Python fallback: encode one at a time
        return [self.encode(m) for m in memories]

    def _get_garden_bias(self, memory: dict[str, Any]) -> dict[str, float] | None:
        """Extract garden bias from memory if garden affiliation exists."""
        metadata = memory.get("metadata", {})
        if "coordinate_bias" in metadata:
            return cast(dict[str, float], metadata["coordinate_bias"])

        garden_name = metadata.get("garden")
        if not garden_name:
            tags = memory.get("tags", [])
            garden_tags = [t for t in tags if t in [
                "joy", "wisdom", "beauty", "truth", "love", "mystery",
                "play", "wonder", "connection", "courage", "gratitude",
                "patience", "grief", "awe", "humor", "healing", "creation",
                "transformation", "sanctuary", "adventure", "reverence",
                "stillness", "protection", "presence", "voice", "dharma",
                "sangha", "practice", "browser",
            ]]
            if garden_tags:
                garden_name = garden_tags[0]

        if garden_name:
            try:
                from whitemagic.gardens.base_garden import get_garden_bias
                bias = get_garden_bias(garden_name)
                if bias:
                    return cast(dict[str, float], bias.to_dict())
            except ImportError:
                pass
        return None

    def _blend_with_garden(self, x: float, y: float, z: float, w: float,
                          bias: dict[str, float], ratio: float = 0.3, v: float = 0.5) -> tuple:
        """Blend base coordinates with garden bias."""
        blended_x = x * (1 - ratio) + bias.get("x", 0.0) * ratio
        blended_y = y * (1 - ratio) + bias.get("y", 0.0) * ratio
        blended_z = z * (1 - ratio) + bias.get("z", 0.0) * ratio
        blended_w = w * (1 + bias.get("w", 0.0))
        blended_v = v * (1 - ratio) + bias.get("v", v) * ratio

        blended_x = max(-1.0, min(1.0, blended_x))
        blended_y = max(-1.0, min(1.0, blended_y))
        blended_z = max(-1.0, min(1.0, blended_z))
        blended_v = max(0.0, min(1.0, blended_v))

        return (blended_x, blended_y, blended_z, blended_w, blended_v)

    def _content_hash_bias(self, memory: dict[str, Any], axis: str) -> float:
        """Generate deterministic spread based on content hash."""
        content = str(memory.get("content", "")) + str(memory.get("title", ""))
        mem_id = memory.get("id", "")
        hash_input = f"{content}{mem_id}{axis}"
        hash_val = int(hashlib.md5(hash_input.encode()).hexdigest()[:8], 16)
        # Convert to [-0.2, 0.2] range for subtle variation
        return ((hash_val % 1000) / 1000.0 - 0.5) * 0.4

    def _calculate_x(self, memory: dict[str, Any]) -> float:
        """Calculate X-Axis: Logic vs Emotion.
        Range: [-1.0, 1.0].
        """
        # Start with emotional_valence if available (v5.0 integration)
        # Handle None values by coalescing to 0.0
        valence = memory.get("emotional_valence")
        if valence is None:
            valence = 0.0
        score = -0.5 * valence

        # Logic keywords (toward +1.0)
        logic_tags = {"logic", "strategy", "code", "architecture", "plan", "analysis",
                      "audit", "technical", "system", "algorithm", "debug", "fix",
                      "implementation", "refactor", "migration", "database", "api",
                      "sql", "rust", "python", "cli", "schema", "backend"}

        # Emotion keywords (toward -1.0)
        emotion_tags = {"joy", "fear", "anger", "love", "gratitude", "meditation",
                        "dream", "feeling", "emotion", "heart", "soul", "spirit",
                        "intuition", "wonder", "awe", "beauty", "sacred", "dharma"}

        tags = set(t.lower() for t in memory.get("tags", []))
        logic_count = len(tags.intersection(logic_tags))
        emotion_count = len(tags.intersection(emotion_tags))

        if logic_count > emotion_count:
            score += 0.4 + (0.1 * min(logic_count, 5))
        elif emotion_count > logic_count:
            score -= 0.4 + (0.1 * min(emotion_count, 5))

        # Content keyword analysis
        content = str(memory.get("content", "")).lower()
        title = str(memory.get("title", "")).lower()
        combined = content + " " + title

        logic_keywords = ["code", "function", "class", "import", "error", "bug", "fix",
                          "algorithm", "database", "sql", "api", "commit", "git",
                          "version", "module", "test", "debug", "config", "schema",
                          "migrate", "refactor", "implement", "build", "deploy"]
        emotion_keywords = ["feel", "heart", "love", "joy", "wonder", "beauty", "soul",
                            "intuition", "sacred", "meditation", "dream", "gratitude",
                            "peace", "calm", "insight", "wisdom", "dharma", "spirit"]

        logic_word_count = sum(1 for kw in logic_keywords if kw in combined)
        emotion_word_count = sum(1 for kw in emotion_keywords if kw in combined)

        score += 0.03 * logic_word_count
        score -= 0.03 * emotion_word_count

        # Add hash-based variation to prevent clustering
        score += self._content_hash_bias(memory, "x")

        return max(-1.0, min(1.0, score))

    def _calculate_y(self, memory: dict[str, Any]) -> float:
        """Calculate Y-Axis: Micro vs Macro.
        Range: [-1.0, 1.0].
        """
        score = 0.0

        # Check memory_type (fixed: was 'type')
        mem_type = memory.get("memory_type", memory.get("type", "unknown")).lower()

        # Micro types (toward -1.0)
        if mem_type in ["log", "transcript", "raw", "debug", "short_term"]:
            score = -0.6
        # Macro types (toward +1.0)
        elif mem_type in ["pattern", "principle", "wisdom", "insight", "long_term"]:
            score = 0.6
        # Mid-level types
        elif mem_type in ["summary", "report", "episodic"]:
            score = 0.2

        # Micro tags
        micro_tags = {"detail", "specific", "log", "debug", "error", "line", "file"}
        # Macro tags
        macro_tags = {"pattern", "principle", "wisdom", "insight", "overview",
                      "architecture", "design", "philosophy", "strategy", "era"}

        tags = set(t.lower() for t in memory.get("tags", []))
        micro_count = len(tags.intersection(micro_tags))
        macro_count = len(tags.intersection(macro_tags))

        score -= 0.15 * micro_count
        score += 0.15 * macro_count

        # Content analysis
        content = str(memory.get("content", "")).lower()

        # Macro indicators
        if any(w in content for w in ["universal", "always", "principle", "pattern",
                                       "architecture", "overview", "era", "phase"]):
            score += 0.15
        # Micro indicators
        if any(w in content for w in ["specific", "line", "error", "bug", "file:",
                                       "at line", "traceback"]):
            score -= 0.15

        # Hash variation
        score += self._content_hash_bias(memory, "y")

        return max(-1.0, min(1.0, score))

    def _calculate_z(self, memory: dict[str, Any]) -> float:
        """Calculate Z-Axis: Time / Chronos.
        Range: [-1.0, 1.0].
        """
        score = 0.0

        # Try to parse actual timestamp
        timestamp_str = memory.get("created_at") or memory.get("timestamp")
        if timestamp_str:
            try:
                # Handle various formats
                if isinstance(timestamp_str, str):
                    # Try ISO format first
                    for fmt in ["%Y-%m-%dT%H:%M:%S.%f", "%Y-%m-%dT%H:%M:%S",
                                "%Y-%m-%d %H:%M:%S", "%Y-%m-%d"]:
                        try:
                            ts = datetime.strptime(timestamp_str[:26], fmt)
                            break
                        except ValueError:
                            continue
                    else:
                        ts = datetime.now()
                else:
                    ts = datetime.now()

                # Calculate relative age (days from now)
                age_days = (datetime.now() - ts).days

                # Map age to Z: recent = 0, old = -1
                if age_days < 1:
                    score = 0.0  # Today
                elif age_days < 7:
                    score = -0.2  # This week
                elif age_days < 30:
                    score = -0.4  # This month
                elif age_days < 90:
                    score = -0.6  # This quarter
                else:
                    score = -0.8  # Older
            except Exception:
                pass

        # Future-oriented tags push toward +1.0
        tags = set(t.lower() for t in memory.get("tags", []))
        if tags.intersection({"future", "plan", "vision", "roadmap", "goal", "next"}):
            score += 0.5
        if tags.intersection({"history", "archive", "legacy", "old", "past"}):
            score -= 0.3

        # Content hints
        content = str(memory.get("content", "")).lower()
        if any(w in content for w in ["will ", "plan to", "next step", "future", "goal"]):
            score += 0.2
        if any(w in content for w in ["was ", "used to", "previously", "legacy"]):
            score -= 0.2

        # Hash variation
        score += self._content_hash_bias(memory, "z") * 0.5

        return max(-1.0, min(1.0, score))

    def _calculate_w(self, memory: dict[str, Any]) -> float:
        """Calculate W-Axis: Importance / Gravity.
        Range: [0.0, 2.0+].
        """
        # Base importance (v5.0 integration: uses importance OR neuro_score)
        # Handle None values explicitly - fields may exist but contain None
        importance = memory.get("importance")
        if importance is None:
            importance = 0.5  # Baseline importance for all memories
        neuro_score = memory.get("neuro_score")
        if neuro_score is None:
            neuro_score = 1.0  # Baseline neural strength

        # Gravity is a combination of importance and neural strength
        base = (float(importance) * 0.4) + (float(neuro_score) * 0.6)

        # Memory type boost
        mem_type = memory.get("memory_type", memory.get("type", "")).lower()
        if mem_type == "long_term":
            base += 0.3
        elif mem_type == "short_term":
            base -= 0.1

        # High-importance tags
        important_tags = {"critical", "important", "key", "core", "essential",
                          "milestone", "breakthrough", "wisdom", "principle"}
        tags = set(t.lower() for t in memory.get("tags", []))

        base += 0.1 * len(tags.intersection(important_tags))

        # Joy/resonance scores
        joy_score = float(memory.get("joy_score", 0.0) or 0.0)
        resonance_score = float(memory.get("resonance_score", 0.0) or 0.0)

        w = base + (joy_score * 0.5) + (resonance_score * 0.5)

        # --- Importance Jitter (v7.4 Enhancement) ---
        # Add slight random fluctuation to drive emergent clustering
        import random
        jitter = random.uniform(-0.05, 0.05)
        w += jitter
        # --------------------------------------------

        # Content length as minor importance signal
        content_len = len(str(memory.get("content", "")))
        if content_len > 1000:
            w += 0.1
        elif content_len > 500:
            w += 0.05

        return float(max(0.1, w))  # Minimum 0.1 to ensure visibility

    def _calculate_v(self, memory: dict[str, Any]) -> float:
        """Calculate V-Axis: Vitality / Galactic Distance.
        Range: [0.0, 1.0]
        1.0 = Core (active spotlight, high retention)
        0.0 = Far Edge (deep archive, low retention).
        """
        # Primary source: galactic_distance field (already computed by sweep)
        galactic_distance = memory.get("galactic_distance")
        if isinstance(galactic_distance, (int, float)) and galactic_distance > 0.0:
            # Invert: galactic_distance 0=core, 1=edge -> v 1=core, 0=edge
            return float(max(0.0, min(1.0, 1.0 - float(galactic_distance))))

        # Fallback: derive from retention_score
        retention_score = memory.get("retention_score")
        if isinstance(retention_score, (int, float)) and retention_score != 0.5:  # 0.5 is default
            return float(max(0.0, min(1.0, float(retention_score))))

        # Final fallback: quick heuristic from available signals
        importance = float(memory.get("importance") or 0.5)
        neuro_score = float(memory.get("neuro_score") or 1.0)
        access_count = int(memory.get("access_count") or 0)
        recall_count = int(memory.get("recall_count") or 0)
        is_protected = memory.get("is_protected") or False

        if is_protected:
            return 1.0  # Protected = always at core

        activity = min(1.0, (access_count + recall_count) / 10.0)
        v = importance * 0.4 + neuro_score * 0.3 + activity * 0.3
        return float(max(0.0, min(1.0, v)))

# Integration helper
def encode_memory(memory_obj: Any) -> Any:
    encoder = CoordinateEncoder()
    return encoder.encode(memory_obj)
