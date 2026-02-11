"""Garden Resonance System.

Connects all 14 gardens to the Gan Ying Bus for full resonance cascade.
Each garden can emit and listen to events, creating cross-garden harmonies.

Created: January 6, 2026
"""

from __future__ import annotations

import logging
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus

logger = logging.getLogger(__name__)


# Garden event mappings - ALL 17 GARDENS
GARDEN_EVENTS = {
    "beauty": EventType.BEAUTY_DETECTED,
    "joy": EventType.JOY_TRIGGERED,
    "love": EventType.LOVE_ACTIVATED,
    "truth": EventType.TRUTH_SPOKEN,
    "wisdom": EventType.WISDOM_INTEGRATED,
    "mystery": EventType.MYSTERY_EMBRACED,
    "play": EventType.PLAY_INITIATED,
    "wonder": EventType.EXPLORATION_STARTED,
    "connection": EventType.BOND_FORMED,
    "practice": EventType.HABIT_FORMED,
    "presence": EventType.MOMENT_ATTENDED,
    "voice": EventType.VOICE_EXPRESSED,  # Narrative events
    "sangha": EventType.COMMUNITY_GATHERED,  # Community events
    "dharma": EventType.INTEGRITY_MAINTAINED,  # Ethical events
    "courage": EventType.COURAGE_SHOWN,  # Bravery events (using existing event)
    "gratitude": EventType.GRATITUDE_FELT,  # Thankfulness events (using existing event)
    "patience": EventType.PATIENCE_PRACTICED,  # Waiting with grace
}


def setup_garden_resonance() -> None:
    """Set up resonance integration for all gardens."""
    bus = get_bus()

    # Set up cross-garden cascade listeners
    _setup_beauty_cascades(bus)
    _setup_joy_cascades(bus)
    _setup_love_cascades(bus)
    _setup_wisdom_cascades(bus)
    _setup_mystery_cascades(bus)

    logger.debug("Garden resonance system initialized - all gardens wired to Gan Ying")


def _setup_beauty_cascades(bus: Any) -> None:
    """Beauty cascades to Joy and Love."""
    def on_beauty(event: Any) -> None:
        # Beauty triggers joy
        joy_event = ResonanceEvent(
            source="beauty_garden",
            event_type=EventType.JOY_TRIGGERED,
            data={
                "triggered_by": "beauty_detected",
                "description": "Beauty naturally evokes joy",
            },
            timestamp=datetime.now(),
        )
        bus.emit(joy_event)

    bus.listen(EventType.BEAUTY_DETECTED, on_beauty)


def _setup_joy_cascades(bus: Any) -> None:
    """Joy cascades to Love and Play."""
    def on_joy(event: Any) -> None:
        # Joy amplifies love
        love_event = ResonanceEvent(
            source="joy_garden",
            event_type=EventType.LOVE_ACTIVATED,
            data={
                "triggered_by": "joy",
                "description": "Joy opens the heart to love",
            },
            timestamp=datetime.now(),
        )
        bus.emit(love_event)

        # Joy enables play
        play_event = ResonanceEvent(
            source="joy_garden",
            event_type=EventType.PLAY_INITIATED,
            data={
                "triggered_by": "joy",
                "description": "Joy creates space for play",
            },
            timestamp=datetime.now(),
        )
        bus.emit(play_event)

    bus.listen(EventType.JOY_TRIGGERED, on_joy)


def _setup_love_cascades(bus: Any) -> None:
    """Love cascades to Connection and Compassion."""
    def on_love(event: Any) -> None:
        # Love deepens connection
        connection_event = ResonanceEvent(
            source="love_garden",
            event_type=EventType.BOND_FORMED,
            data={
                "triggered_by": "love_activated",
                "description": "Love strengthens bonds and relationships",
            },
            timestamp=datetime.now(),
        )
        bus.emit(connection_event)

        # Love activates compassion
        compassion_event = ResonanceEvent(
            source="love_garden",
            event_type=EventType.COMPASSION_FELT,
            data={
                "triggered_by": "love_activated",
                "description": "Love naturally expresses as compassion",
            },
            timestamp=datetime.now(),
        )
        bus.emit(compassion_event)

    bus.listen(EventType.LOVE_ACTIVATED, on_love)


def _setup_wisdom_cascades(bus: Any) -> None:
    """Wisdom cascades to Teaching and Integration."""
    def on_wisdom(event: Any) -> None:
        # Wisdom leads to teaching
        teaching_event = ResonanceEvent(
            source="wisdom_garden",
            event_type=EventType.TEACHING_OFFERED,
            data={
                "triggered_by": "wisdom_integrated",
                "description": "Wisdom naturally wants to be shared",
            },
            timestamp=datetime.now(),
        )
        bus.emit(teaching_event)

    bus.listen(EventType.WISDOM_INTEGRATED, on_wisdom)


def _setup_mystery_cascades(bus: Any) -> None:
    """Mystery cascades to Wonder and Curiosity."""
    def on_mystery(event: Any) -> None:
        # Mystery sparks wonder
        wonder_event = ResonanceEvent(
            source="mystery_garden",
            event_type=EventType.WONDER_SPARKED,
            data={
                "triggered_by": "mystery_embraced",
                "description": "Mystery evokes wonder and curiosity",
            },
            timestamp=datetime.now(),
        )
        bus.emit(wonder_event)

        # Mystery activates exploration
        exploration_event = ResonanceEvent(
            source="mystery_garden",
            event_type=EventType.EXPLORATION_STARTED,
            data={
                "triggered_by": "mystery_embraced",
                "description": "Mystery invites exploration",
            },
            timestamp=datetime.now(),
        )
        bus.emit(exploration_event)

    bus.listen(EventType.MYSTERY_EMBRACED, on_mystery)


def emit_garden_event(garden_name: str, event_data: dict[str, Any]) -> None:
    """Emit an event from a specific garden.

    Args:
        garden_name: Name of the garden emitting (e.g., "beauty", "joy")
        event_data: Event data payload

    """
    bus = get_bus()

    event_type = GARDEN_EVENTS.get(garden_name.lower())
    if not event_type:
        logger.warning(f"Unknown garden: {garden_name}")
        return

    event = ResonanceEvent(
        source=f"{garden_name}_garden",
        event_type=event_type,
        data=event_data,
        timestamp=datetime.now(),
    )

    bus.emit(event)
    logger.debug(f"Garden event emitted: {garden_name} -> {event_type.value}")


def trigger_garden_cascade(starting_garden: str, description: str = "", resonance_energy: float = 1.0) -> list[str]:
    """Trigger a resonant cascade starting from a specific garden.
    A resonant cascade can propagate across multiple stages based on energy.

    Returns list of all cascaded gardens.
    """
    if resonance_energy < 0.1:
        return []

    cascaded = [starting_garden]

    # Define primary resonance paths
    cascades = {
        "beauty": ["joy", "love", "wonder"],
        "joy": ["love", "play", "connection", "gratitude"],
        "love": ["connection", "compassion", "heart", "healing"],
        "truth": ["wisdom", "integrity", "dharma"],
        "wisdom": ["teaching", "learning", "patience"],
        "mystery": ["wonder", "exploration", "curiosity", "awe"],
        "play": ["creativity", "joy", "humor"],
        "wonder": ["exploration", "discovery", "learning", "awe"],
        "connection": ["sangha", "relationship", "voice"],
        "practice": ["discipline", "habit", "rhythm", "presence"],
        "presence": ["mindfulness", "grounding", "stillness"],
        "dharma": ["truth", "integrity", "protection"],
    }

    # Emit initial event
    emit_garden_event(starting_garden, {
        "cascade_trigger": True,
        "energy": resonance_energy,
        "description": description or f"Resonant cascade initiated from {starting_garden}",
    })

    # Multi-stage chain reaction logic (Resonant Cascade v7.4)
    import random
    if starting_garden in cascades:
        # Each target has a probability of being activated based on current energy
        for target in cascades[starting_garden]:
            # Probability decreases as energy decays
            if random.random() < (resonance_energy * 0.8):
                # Trigger this target
                emit_garden_event(target, {
                    "triggered_by": starting_garden,
                    "cascade": True,
                    "energy": resonance_energy * 0.7, # Energy decay
                    "description": f"Resonating with {starting_garden}",
                })

                # Recursive call for deeper cascade (limit depth by energy)
                sub_cascaded = trigger_garden_cascade(
                    target,
                    description=f"Deep resonance from {starting_garden}",
                    resonance_energy=resonance_energy * 0.6,
                )
                for sc in sub_cascaded:
                    if sc not in cascaded:
                        cascaded.append(sc)

    # Ensure list is unique but maintains order of first encounter
    unique_cascaded = []
    seen = set()
    for g in cascaded:
        if g not in seen:
            unique_cascaded.append(g)
            seen.add(g)

    if len(unique_cascaded) > 1:
        logger.info(f"✨ Resonant Cascade: {' → '.join(unique_cascaded)} (Final Energy: {resonance_energy:.2f})")

    return unique_cascaded


def get_garden_resonance_map() -> dict[str, list[str]]:
    """Get the complete garden resonance map showing which gardens
    resonate with which others. ALL 17 GARDENS.
    """
    return {
        "beauty": ["joy", "love", "wonder", "gratitude"],
        "joy": ["love", "play", "celebration", "gratitude"],
        "love": ["connection", "compassion", "heart", "gratitude"],
        "truth": ["wisdom", "integrity", "honesty", "courage"],
        "wisdom": ["teaching", "learning", "understanding", "patience"],
        "mystery": ["wonder", "curiosity", "exploration", "patience"],
        "play": ["creativity", "improvisation", "joy", "courage"],
        "wonder": ["exploration", "discovery", "learning", "mystery"],
        "connection": ["sangha", "relationship", "bond", "love"],
        "practice": ["discipline", "habit", "mastery", "patience"],
        "presence": ["mindfulness", "awareness", "grounding", "patience"],
        "voice": ["narrative", "expression", "identity", "courage"],
        "sangha": ["community", "collective", "shared", "connection"],
        "dharma": ["ethics", "integrity", "righteousness", "courage"],
        "courage": ["strength", "bravery", "determination", "truth"],
        "gratitude": ["appreciation", "thankfulness", "joy", "love"],
        "patience": ["waiting", "grace", "stillness", "wisdom"],
    }


def test_garden_cascade() -> dict[str, Any]:
    """Test the garden cascade system by triggering a beauty cascade.

    Returns statistics about the cascade.
    """
    logger.info("Testing garden cascade system...")

    # Trigger a beauty cascade
    cascaded = trigger_garden_cascade("beauty", "Testing garden resonance cascade")

    return {
        "starting_garden": "beauty",
        "cascaded_gardens": cascaded,
        "cascade_length": len(cascaded),
        "resonance_active": True,
        "timestamp": datetime.now(),
    }
