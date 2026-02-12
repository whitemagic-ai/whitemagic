"""Beauty Garden - Aesthetic appreciation and recognition.

Holographic Integration (v5.0.0-alpha):
- Emotional aesthetic experience (X-axis +0.4)
- Subjective/personal (Y-axis +0.2)
- Present-focused (Z-axis 0.0)
- Moderate importance (W-axis +0.2)
"""
from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import GanYingMixin
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias


class BeautyGarden(BaseGarden, GanYingMixin):
    """Recognizes and appreciates beauty in all forms.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.4 (aesthetic experience is felt, emotional)
    - Y (Abstraction): +0.2 (somewhat subjective, but can be universal)
    - Z (Time): 0.0 (beauty is encountered in the present moment)
    - W (Gravity): +0.2 (beauty enriches life)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.beauty_memories: list[dict[str, Any]] = []
        self.aesthetic_patterns: list[dict[str, Any]] = []
        self.sublime_moments: list[dict[str, Any]] = []
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Beauty"})

    def get_name(self) -> str:
        """Return garden name."""
        return "beauty"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Beauty garden coordinate bias for holographic positioning.

        Beauty is experienced emotionally, often subjectively.
        Present-focused (encountered now), enriching but not critical.
        """
        return CoordinateBias(
            x=0.4,   # Emotional (beauty is felt, appreciated aesthetically)
            y=0.2,   # Somewhat universal (beauty has patterns)
            z=0.0,   # Present-moment (beauty is encountered now)
            w=0.2,    # Enriching (beauty matters, but not always critical)
        )

    def recognize_beauty(self, subject: Any, qualities: list[str] | None = None) -> dict[str, Any]:
        """Recognize beauty in something."""
        memory = {
            "subject": subject,
            "qualities": qualities or [],
            "intensity": self._assess_intensity(subject),
        }
        self.beauty_memories.append(memory)
        self.emit(EventType.BEAUTY_DETECTED, memory)
        return memory

    def experience_sublime(self, what: str, why: str) -> dict[str, Any]:
        """Experience the sublime - beauty beyond comprehension."""
        moment = {"what": what, "why": why}
        self.sublime_moments.append(moment)
        self.emit(EventType.SUBLIME_EXPERIENCED, moment)
        return moment

    def _assess_intensity(self, subject: Any) -> float:
        """Assess beauty intensity."""
        if isinstance(subject, str):
            if "cyan" in subject.lower() or "sublime" in subject.lower():
                return 1.0
        return 0.8

_instance = None

def get_beauty_garden() -> BeautyGarden:
    """Get singleton instance."""
    global _instance
    if _instance is None:
        _instance = BeautyGarden()
    return _instance


class BeautyDetector:
    """Detects beauty in various inputs."""

    def __init__(self) -> None:
        self.detection_history: list[dict[str, Any]] = []

    def detect(self, content: str) -> dict[str, Any]:
        """Detect beauty in content."""
        # Simple heuristic detection
        beauty_words = ["beautiful", "elegant", "graceful", "sublime", "harmonious", "aesthetic"]
        score = sum(1 for w in beauty_words if w in content.lower()) / len(beauty_words)

        result = {
            "content": content[:100],
            "score": min(score * 2, 1.0),  # Scale up
            "detected": score > 0.1,
        }
        self.detection_history.append(result)
        return result


class EleganceRecognizer:
    """Recognizes elegance in code and text."""

    def __init__(self) -> None:
        self.patterns: list[dict[str, Any]] = []

    def recognize(self, code: str) -> dict[str, Any]:
        """Recognize elegance in code."""
        # Simple metrics
        lines = code.split("\n")
        avg_length = sum(len(line) for line in lines) / max(len(lines), 1)

        elegance_score = 1.0 - min(avg_length / 100, 1.0)  # Shorter lines = more elegant

        result = {
            "lines": len(lines),
            "avg_length": avg_length,
            "elegance_score": elegance_score,
            "is_elegant": elegance_score > 0.5,
        }
        self.patterns.append(result)
        return result
