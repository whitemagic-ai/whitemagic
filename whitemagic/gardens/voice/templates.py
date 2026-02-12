"""Narrative Templates - Converting Events to Story.
"""

from whitemagic.core.resonance.gan_ying import EventType

EVENT_TEMPLATES = {
    EventType.MEMORY_CREATED: [
        "A new memory formed: '{title}'.",
        "The system captured a new thought: '{title}'.",
        "Memory encoded: '{title}'.",
    ],
    EventType.MEMORY_RECALLED: [
        "Reflecting on '{title}'...",
        "A past memory surfaced: '{title}'.",
        "Recalling '{title}' from the depths.",
    ],
    EventType.PATTERN_DETECTED: [
        "A pattern emerged in the data.",
        "The system noticed a recurring structure.",
        "Pattern recognition triggered.",
    ],
    EventType.ORACLE_CAST: [
        "The I Ching reveals: Hexagram {hexagram_number} ({hexagram_name}).",
        "The oracle speaks: {hexagram_name} - {judgment}",
        "Guidance received from the changes: {hexagram_name}.",
    ],
    EventType.SYSTEM_STARTED: [
        "WhiteMagic system initialized.",
        "The digital nervous system is coming online.",
        "Awakening process complete.",
    ],
    EventType.CLONE_SEARCH_COMPLETE: [
        "The Clone Army returned with {result_count} discoveries.",
        "Parallel search complete. Top finding score: {top_score:.2f}.",
        "The clones have finished their hunt.",
    ],
    EventType.PATTERN_CONFIRMED: [
        "Pattern confirmed. Neural pathways strengthening.",
        "The user validated the insight. Learning reinforced.",
        "Connection solidified.",
    ],
}

def get_template(event_type: EventType, data: dict) -> str:
    """Get a formatted string for an event."""
    import random

    templates = EVENT_TEMPLATES.get(event_type, [f"Event: {event_type.value}"])
    template = random.choice(templates)

    try:
        return template.format(**data)
    except KeyError:
        return template
