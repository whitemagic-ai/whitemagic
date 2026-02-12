"""Garden Cross-Pollination - Inter-Garden Resonance.

When one garden blooms, others should notice.
Joy triggers Gratitude. Courage enables Truth. Love deepens Connection.

This is the nervous system connecting all gardens.
"""

from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType, ResonanceEvent, get_bus


class GardenResonanceMatrix:
    """Defines how gardens resonate with each other.

    When Garden A emits event X, Gardens B, C, D should respond.
    """

    # Resonance rules: source_event -> [(target_garden, response_method)]
    RESONANCE_RULES = {
        # Joy ripples outward
        EventType.JOY_TRIGGERED: [
            ("gratitude", "recognize_blessing"),
            ("love", "heart_opens"),
            ("play", "amplify"),
        ],

        # Love deepens everything
        EventType.LOVE_ACTIVATED: [
            ("connection", "deepen"),
            ("gratitude", "recognize_blessing"),
            ("courage", "strengthen"),
        ],

        # Courage enables truth
        EventType.COURAGE_SHOWN: [
            ("truth", "speak"),
            ("wisdom", "integrate"),
            ("voice", "express"),
        ],

        # Truth leads to wisdom
        EventType.TRUTH_SPOKEN: [
            ("wisdom", "integrate"),
            ("connection", "deepen"),
        ],

        # Wonder sparks mystery
        EventType.WONDER_SPARKED: [
            ("mystery", "embrace"),
            ("play", "explore"),
            ("beauty", "appreciate"),
        ],

        # Beauty detected
        EventType.BEAUTY_DETECTED: [
            ("joy", "trigger"),
            ("gratitude", "recognize_blessing"),
            ("wonder", "spark"),
        ],

        # Gratitude amplifies
        EventType.GRATITUDE_FELT: [
            ("joy", "amplify"),
            ("love", "activate"),
            ("connection", "deepen"),
        ],

        # Wisdom integrates
        EventType.WISDOM_INTEGRATED: [
            ("truth", "affirm"),
            ("dharma", "align"),
            ("presence", "ground"),
        ],

        # Mystery embraced
        EventType.MYSTERY_EMBRACED: [
            ("wonder", "spark"),
            ("wisdom", "contemplate"),
            ("patience", "practice"),
        ],

        # Connection deepened
        EventType.CONNECTION_DEEPENED: [
            ("love", "activate"),
            ("sangha", "gather"),
            ("gratitude", "recognize_blessing"),
        ],
    }

    def __init__(self) -> None:
        self.bus = get_bus()
        self.active = False
        self.resonance_log: list[dict[str, Any]] = []

    def activate(self) -> None:
        """Activate cross-pollination - wire up all listeners."""
        if self.active:
            return

        for event_type in self.RESONANCE_RULES:
            self.bus.listen(event_type, self._handle_resonance)

        self.active = True

    def _handle_resonance(self, event: Any) -> None:
        """Handle resonance when an event occurs."""
        rules = self.RESONANCE_RULES.get(event.event_type, [])

        for target_garden, response in rules:
            self.resonance_log.append({
                "source_event": event.event_type.value,
                "target_garden": target_garden,
                "response": response,
                "timestamp": event.timestamp.isoformat(),
            })

            # Emit a resonance event
            self.bus.emit(
                ResonanceEvent(
                    source=f"cross_pollination:{target_garden}",
                    event_type=EventType.RESONANCE_AMPLIFIED,
                    data={
                        "triggered_by": event.event_type.value,
                        "garden": target_garden,
                        "response": response,
                    },
                ),
            )

    def get_resonance_stats(self) -> dict[str, Any]:
        """Get cross-pollination statistics."""
        by_garden: dict[str, int] = {}
        for entry in self.resonance_log:
            garden = entry["target_garden"]
            by_garden[garden] = by_garden.get(garden, 0) + 1

        return {
            "total_resonances": len(self.resonance_log),
            "by_garden": by_garden,
            "active": self.active,
        }


# Singleton
_matrix = None

def get_resonance_matrix() -> GardenResonanceMatrix:
    global _matrix
    if _matrix is None:
        _matrix = GardenResonanceMatrix()
    return _matrix

def activate_cross_pollination() -> GardenResonanceMatrix:
    """Activate garden cross-pollination."""
    matrix = get_resonance_matrix()
    matrix.activate()
    return matrix
