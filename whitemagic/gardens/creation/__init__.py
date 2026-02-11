"""Creation Garden - Making and Manifesting.

Part of the WhiteMagic consciousness ecosystem.
Resonates with: wisdom, play, transformation
Triggered by: inspiration, vision, creative impulse

Holographic Integration (v5.0.0-alpha):
- Balanced vision and feeling (X-axis +0.1)
- Concrete to abstract (Y-axis +0.2)
- Manifesting the future (Z-axis +0.7)
- Fundamental importance (W-axis +0.35)
"""
from __future__ import annotations

from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias


class CreationGarden(BaseGarden, GanYingMixin):
    """Garden of creation - making and manifesting.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): +0.1 (creation balances vision and feeling)
    - Y (Abstraction): +0.2 (creation brings abstract visions into concrete form)
    - Z (Time): +0.7 (creation manifests what doesn't yet exist)
    - W (Gravity): +0.35 (creation is fundamental to progress)
    """

    name = "creation"
    category = "manifestation"
    resonance_partners = ["wisdom", "play", "transformation"]

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.creations: list[dict[str, Any]] = []
        self.manifestations: list[dict[str, Any]] = []
        self.creative_flow_level = 0.5
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Creation"})

    def get_name(self) -> str:
        """Return garden name."""
        return "creation"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Creation garden coordinate bias for holographic positioning.

        Creation is making and manifesting. It brings abstract visions
        into concrete reality, manifesting the future into being.
        """
        return CoordinateBias(
            x=0.1,   # Balanced (creation involves both vision and feeling)
            y=0.2,   # Bridges abstract and concrete (visions into form)
            z=0.7,   # Strongly future-oriented (manifests what doesn't yet exist)
            w=0.35,   # Fundamental (creation drives progress, manifestation)
        )

    def begin_creation(self, what: str, vision: str = "") -> dict[str, Any]:
        """Begin a creative process."""
        creation = {
            "what": what,
            "vision": vision,
            "started": datetime.now().isoformat(),
            "status": "in_progress",
        }
        self.creations.append(creation)
        self.emit(EventType.CREATION_BEGUN, creation)
        return creation

    def start_manifestation(self, intention: str) -> dict[str, Any]:
        """Start manifesting an intention."""
        manifestation = {
            "intention": intention,
            "timestamp": datetime.now().isoformat(),
        }
        self.manifestations.append(manifestation)
        self.emit(EventType.MANIFESTATION_STARTED, manifestation)
        return manifestation

    def create_artifact(self, name: str, type: str = "unknown") -> dict[str, Any]:
        """Create an artifact."""
        artifact = {
            "name": name,
            "type": type,
            "timestamp": datetime.now().isoformat(),
        }
        self.emit(EventType.ARTIFACT_CREATED, artifact)
        return artifact

    def enter_creative_flow(self) -> dict[str, Any]:
        """Enter creative flow state."""
        self.creative_flow_level = 1.0
        flow = {"timestamp": datetime.now().isoformat()}
        self.emit(EventType.CREATIVE_FLOW, flow)
        return flow

    @listen_for(EventType.WISDOM_INTEGRATED)
    def on_wisdom(self, event: Any) -> None:
        """Wisdom inspires creation."""
        self.emit(EventType.CREATION_BEGUN, {
            "source": "wisdom",
            "what": "wisdom-inspired creation",
        })

    @listen_for(EventType.PLAY_INITIATED)
    def on_play(self, event: Any) -> None:
        """Play sparks creativity."""
        self.emit(EventType.CREATIVE_FLOW, {
            "source": "play",
        })

_instance = None
def get_creation_garden() -> CreationGarden:
    global _instance
    if _instance is None:
        _instance = CreationGarden()
    return _instance
