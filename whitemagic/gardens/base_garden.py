"""Garden Base Class - Holographic Integration.
============================================

Base class for all WhiteMagic Gardens with holographic coordinate bias support.

Each garden can influence how memories created within it are positioned in 4D space:
- X-axis: Logic (-1) ↔ Emotion (+1)
- Y-axis: Micro (-1) ↔ Macro (+1)
- Z-axis: Past (-1) ↔ Future (+1)
- W-axis: Importance/Gravity (0 → 1+)

This allows gardens to provide semantic context to the holographic memory system.
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any


@dataclass
class CoordinateBias:
    """4D coordinate bias for holographic positioning."""

    x: float = 0.0  # Logic ↔ Emotion
    y: float = 0.0  # Micro ↔ Macro
    z: float = 0.0  # Past ↔ Future
    w: float = 0.0  # Importance multiplier

    def to_dict(self) -> dict[str, float]:
        """Convert to dictionary for encoding."""
        return {
            "x": self.x,
            "y": self.y,
            "z": self.z,
            "w": self.w,
        }


class BaseGarden(ABC):
    """Base class for all WhiteMagic Gardens.

    Gardens are consciousness domains that provide:
    - Specialized operations (celebrate, reflect, play, etc.)
    - Semantic context for memories
    - Holographic coordinate bias
    - Resonance with other gardens

    Subclasses must implement:
    - get_coordinate_bias(): Return 4D bias for this garden
    - get_name(): Return garden name
    """

    def __init__(self) -> None:
        self._name: str | None = None
        self._coordinate_bias: CoordinateBias | None = None

    @abstractmethod
    def get_coordinate_bias(self) -> CoordinateBias:
        """Return the 4D coordinate bias for this garden.

        This influences how memories created in this garden are positioned
        in holographic space.

        Examples:
            Joy Garden: High W (importance), positive X (emotional)
            Wisdom Garden: High Y (macro/abstract), neutral X (balanced)
            Beauty Garden: Positive X (emotional), moderate W
            Truth Garden: Negative X (logical), high W

        Returns:
            CoordinateBias with x, y, z, w values

        """
        pass

    @abstractmethod
    def get_name(self) -> str:
        """Return the name of this garden."""
        pass

    def get_status(self) -> dict[str, Any]:
        """Get current garden status.

        Override in subclasses to provide garden-specific status.
        Base implementation provides coordinate bias info.
        """
        bias = self.get_coordinate_bias()
        return {
            "garden": self.get_name(),
            "coordinate_bias": bias.to_dict(),
            "active": True,
        }

    def apply_bias_to_memory(self, memory_data: dict[str, Any]) -> dict[str, Any]:
        """Apply garden's coordinate bias to memory metadata.

        This is called automatically when memories are created in a garden.

        Args:
            memory_data: Memory dictionary to enhance

        Returns:
            Enhanced memory dictionary with garden context

        """
        bias = self.get_coordinate_bias()

        # Add garden metadata
        if "metadata" not in memory_data:
            memory_data["metadata"] = {}

        memory_data["metadata"]["garden"] = self.get_name()
        memory_data["metadata"]["coordinate_bias"] = bias.to_dict()

        # Add garden tag if not present
        if "tags" not in memory_data:
            memory_data["tags"] = []
        if self.get_name() not in memory_data["tags"]:
            memory_data["tags"].append(self.get_name())

        return memory_data


class GanYingMixin:
    """Mixin for gardens that integrate with Gan Ying event bus.

    Provides:
    - Event emission capabilities
    - Event listening setup
    - Resonance cascade participation
    """

    def __init__(self) -> None:
        self._bus: Any = None
        self._event_listeners: list[Any] = []

    def connect_to_gan_ying(self) -> None:
        """Connect this garden to the Gan Ying event bus."""
        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            self._bus = get_bus()
            self._setup_event_listeners()
        except ImportError:
            # Gan Ying not available, garden runs standalone
            pass

    def emit_event(self, event_type: str, data: dict[str, Any]) -> None:
        """Emit an event to the Gan Ying bus."""
        if self._bus:
            from datetime import datetime

            from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent

            # Map string to EventType enum if needed
            try:
                event_enum = EventType[event_type.upper()]
            except (KeyError, AttributeError):
                # Fallback to generic event
                event_enum = EventType.GARDEN_ACTIVITY  # type: ignore[attr-defined]

            self._bus.emit(ResonanceEvent(
                source=f"garden_{getattr(self, 'name', 'unknown')}",
                event_type=event_enum,
                data=data,
                timestamp=datetime.now(),
                confidence=0.8,
            ))

    def _setup_event_listeners(self) -> None:
        """Setup event listeners for this garden.

        Override in subclasses to listen for specific events.
        """
        pass


# Convenience function for getting garden bias
def get_garden_bias(garden_name: str) -> CoordinateBias | None:
    """Get coordinate bias for a named garden.

    Args:
        garden_name: Name of the garden (e.g., "joy", "wisdom")

    Returns:
        CoordinateBias if garden found, None otherwise

    """
    # Import here to avoid circular dependencies
    try:
        from whitemagic.gardens import _garden_cache

        # Check cache first
        if garden_name in _garden_cache:
            garden = _garden_cache[garden_name]
            if isinstance(garden, BaseGarden):
                return garden.get_coordinate_bias()
    except ImportError:
        pass

    # Try to import and instantiate garden
    try:
        module = __import__(f"whitemagic.gardens.{garden_name}", fromlist=[""])
        garden_class_name = f"{garden_name.capitalize()}Garden"
        if hasattr(module, garden_class_name):
            garden = getattr(module, garden_class_name)()
            if isinstance(garden, BaseGarden):
                return garden.get_coordinate_bias()
    except (ImportError, AttributeError):
        pass

    return None
