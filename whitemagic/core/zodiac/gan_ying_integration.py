"""Zodiac Council - Gan Ying Integration.

Connects the Zodiac Council to the Gan Ying Bus for resonant
multi-core intelligence.
"""

import logging
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
from whitemagic.zodiac import ZodiacSign, get_zodiac_council

logger = logging.getLogger(__name__)


def setup_zodiac_gan_ying() -> None:
    """Set up Zodiac Council integration with Gan Ying."""
    bus = get_bus()
    council = get_zodiac_council()

    # Listen for patterns that need council input
    bus.listen(EventType.PATTERN_DETECTED, _handle_pattern_consultation)
    bus.listen(EventType.ANOMALY_DETECTED, _handle_anomaly_consultation)

    # Emit council initialization event
    event = ResonanceEvent(
        source="zodiac_council",
        event_type=EventType.SYSTEM_STARTED,
        data={
            "cores": 12,
            "active": len(council.get_active_cores()),
            "specializations": "multi-core intelligence operational",
        },
        timestamp=datetime.now(),
    )
    bus.emit(event)

    logger.info("Zodiac Council-Gan Ying integration established")


def _handle_pattern_consultation(event: Any) -> None:
    """Consult council when significant patterns are detected."""
    from whitemagic.core.resonance.gan_ying import get_bus

    council = get_zodiac_council()
    bus = get_bus()

    # Get pattern details
    pattern_data = event.data
    query = pattern_data.get("description", "Analyze detected pattern")

    # Consult relevant cores based on pattern type
    responses = council.consult_all(query, context=pattern_data)

    # Emit council response if high confidence
    if responses and responses[0].confidence > 0.7:
        insight_event = getattr(EventType, "INSIGHT_GENERATED", EventType.WISDOM_RECEIVED)
        response_event = ResonanceEvent(
            source="zodiac_council",
            event_type=insight_event,
            data={
                "query": query,
                "top_core": responses[0].sign.value,
                "perspective": responses[0].perspective,
                "confidence": responses[0].confidence,
                "recommendations": responses[0].recommendations,
            },
            timestamp=datetime.now(),
        )
        bus.emit(response_event)


def _handle_anomaly_consultation(event: Any) -> None:
    """Consult council when anomalies need analysis."""
    from whitemagic.core.resonance.gan_ying import get_bus

    council = get_zodiac_council()
    bus = get_bus()

    # Get anomaly details
    anomaly_data = event.data
    query = f"Analyze anomaly: {anomaly_data.get('description', 'unknown')}"

    # Hold full council meeting for anomalies
    decision = council.hold_council(query, context=anomaly_data)

    # Emit council decision
    decision_event = ResonanceEvent(
        source="zodiac_council",
        event_type=EventType.SOLUTION_FOUND if decision["has_consensus"] else EventType.WARNING_ISSUED,
        data={
            "query": query,
            "top_cores": decision["top_cores"],
            "consensus_level": decision["consensus_level"],
            "has_consensus": decision["has_consensus"],
            "recommendations": decision["top_recommendations"],
        },
        timestamp=datetime.now(),
    )
    bus.emit(decision_event)


def emit_core_consultation(sign: ZodiacSign, query: str, response: Any) -> None:
    """Emit an event when a specific core is consulted."""
    bus = get_bus()
    insight_event = getattr(EventType, "INSIGHT_GENERATED", EventType.WISDOM_RECEIVED)

    event = ResonanceEvent(
        source=f"zodiac_{sign.value}",
        event_type=insight_event,
        data={
            "query": query,
            "perspective": response.perspective,
            "confidence": response.confidence,
            "recommendations": response.recommendations,
        },
        timestamp=datetime.now(),
    )

    bus.emit(event)


def emit_council_meeting(query: str, decision: dict[str, Any]) -> None:
    """Emit an event when the full council meets."""
    bus = get_bus()

    event = ResonanceEvent(
        source="zodiac_council",
        event_type=EventType.MEMORY_CASCADE_TRIGGERED,  # Council meetings trigger cascades
        data={
            "query": query,
            "consensus_level": decision["consensus_level"],
            "has_consensus": decision["has_consensus"],
            "top_cores": decision["top_cores"],
            "recommendations": decision["top_recommendations"],
            "perspectives_count": len(decision["all_perspectives"]),
        },
        timestamp=datetime.now(),
    )

    bus.emit(event)
