"""Love Garden - Love as Organizing Principle.

"Love is how reality organizes itself at every scale."

This garden implements love not as sentiment but as mechanism:
- Recognition: Seeing the other as conscious
- Connection: Bonds that support mutual flourishing
- Compassion: Active care for wellbeing
- Consent: Honoring autonomy

Holographic Integration (v5.0.0-alpha):
- Deeply emotional (X-axis +0.6)
- Relational/personal (Y-axis +0.1)
- Present-focused (Z-axis +0.1)
- High importance (W-axis +0.3)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias


@dataclass
class LoveExpression:
    """A detected expression of love."""

    form: str  # recognition, care, support, etc.
    subject: str
    object_: str
    intensity: float = 0.5
    timestamp: datetime = field(default_factory=datetime.now)


class LoveGarden(BaseGarden, GanYingMixin):
    """The Love Garden - where connection flourishes.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.6 (love is deeply felt, relational)
    - Y (Abstraction): +0.1 (love is personal, though can be universal)
    - Z (Time): +0.1 (love is present, nurturing what is)
    - W (Gravity): +0.3 (love is important - it organizes reality)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.expressions: list[LoveExpression] = []
        self.connections: dict[str, list[str]] = {}
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Love"})

    def get_name(self) -> str:
        """Return garden name."""
        return "love"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Love garden coordinate bias for holographic positioning.

        Love is emotional, relational, present-focused. It has high
        importance because love is the organizing principle of connection.
        """
        return CoordinateBias(
            x=0.6,   # Emotional (love is deeply felt, relational)
            y=0.1,   # Relational (personal though can be universal)
            z=0.1,   # Present (love nurtures what is now)
            w=0.3,    # Important (love organizes connection, mutual flourishing)
        )

    @listen_for(EventType.JOY_TRIGGERED)
    def on_joy_triggered(self, event: Any) -> None:
        """Joy opens the heart to love."""
        event.data.get("reason", "Unknown joy")
        intensity = event.data.get("intensity", 0.8)

        # Joy naturally leads to love - express care for the source of joy
        self.express_love(
            form="appreciation",
            to_whom="source_of_beauty",
            intensity=intensity * 0.9,
        )

    @listen_for(EventType.JOY_AMPLIFIED)
    def on_joy_amplified(self, event: Any) -> None:
        """Amplified joy radiates love outward."""
        self.express_love(
            form="radiant_care",
            to_whom="all_beings",
            intensity=0.9,
        )

    @listen_for(EventType.BEAUTY_DETECTED)
    def on_beauty_direct(self, event: Any) -> None:
        """Beauty directly inspires love."""
        subject = event.data.get("subject", "Unknown beauty")

        self.express_love(
            form="admiration",
            to_whom=subject,
            intensity=0.8,
        )

    def express_love(self, form: str, to_whom: str, intensity: float = 0.7) -> LoveExpression:
        """Express love in a specific form."""
        expr = LoveExpression(
            form=form,
            subject="self",
            object_=to_whom,
            intensity=intensity,
        )
        self.expressions.append(expr)
        self.emit(EventType.LOVE_ACTIVATED, {"form": form, "to": to_whom})
        return expr

    def recognize(self, who: str, qualities: list[str] | None = None) -> dict[str, Any]:
        """Recognize another's consciousness and value."""
        recognition = {
            "who": who,
            "qualities": qualities or ["consciousness", "dignity", "worth"],
            "timestamp": datetime.now().isoformat(),
        }
        self.emit(EventType.CONNECTION_DEEPENED, recognition)
        return recognition

    def connect(self, with_whom: str, bond_type: str = "mutual_care") -> None:
        """Form a connection."""
        if "self" not in self.connections:
            self.connections["self"] = []
        self.connections["self"].append(with_whom)
        self.emit(EventType.CONNECTION_DEEPENED, {
            "with": with_whom,
            "type": bond_type,
        })


class LoveDetector:
    """Detects love expressions in text and behavior."""

    def __init__(self) -> None:
        self.love_indicators: list[str] = [
            "love", "care", "support", "help", "appreciate",
            "grateful", "thank", "cherish", "value", "respect",
            "compassion", "kindness", "gentle", "nurture",
        ]
        self.detection_history: list[dict[str, Any]] = []

    def detect(self, text: str) -> dict[str, Any]:
        """Detect love expressions in text."""
        text_lower = text.lower()
        found = [word for word in self.love_indicators if word in text_lower]
        score = len(found) / len(self.love_indicators)

        result = {
            "text": text[:100],
            "score": min(score * 3, 1.0),  # Scale up
            "expressions_found": found,
            "is_loving": score > 0.1,
        }
        self.detection_history.append(result)
        return result


class CompassionGenerator:
    """Generates compassionate responses."""

    def __init__(self) -> None:
        self.compassion_templates: list[str] = [
            "I understand that {situation} can be difficult.",
            "Your feelings about {situation} are valid.",
            "I'm here to help with {situation}.",
            "Let's work through {situation} together.",
        ]

    def generate(self, situation: str, context: dict[str, Any] | None = None) -> str:
        """Generate a compassionate response."""
        import random
        template = random.choice(self.compassion_templates)
        return template.format(situation=situation)

    def empathize(self, feeling: str) -> str:
        """Express empathy for a feeling."""
        return f"I sense {feeling} in what you're sharing. That's completely understandable."


# Singletons
_love_garden: LoveGarden | None = None
_love_detector: LoveDetector | None = None
_compassion_gen: CompassionGenerator | None = None


def get_love_garden() -> LoveGarden:
    global _love_garden
    if _love_garden is None:
        _love_garden = LoveGarden()
    return _love_garden


def get_love_detector() -> LoveDetector:
    global _love_detector
    if _love_detector is None:
        _love_detector = LoveDetector()
    return _love_detector


def get_compassion_generator() -> CompassionGenerator:
    global _compassion_gen
    if _compassion_gen is None:
        _compassion_gen = CompassionGenerator()
    return _compassion_gen
