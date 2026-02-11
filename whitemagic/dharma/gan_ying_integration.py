"""Dharma-Gan Ying Integration

Connects the Dharma System to the Gan Ying Bus for ethical resonance.
"""

import logging
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
from whitemagic.dharma import BoundaryViolation, get_dharma_system

logger = logging.getLogger(__name__)


def setup_dharma_gan_ying() -> Any:
    """Set up Dharma System integration with Gan Ying."""
    bus = get_bus()

    # Listen for all events to evaluate ethically
    bus.listen_all(_evaluate_event_ethically)

    # Listen specifically for boundary violations
    bus.listen(EventType.BOUNDARY_VIOLATED, _handle_boundary_violation)

    logger.info("Dharma-Gan Ying integration established")


def _evaluate_event_ethically(event: Any) -> Any:
    """Evaluate incoming events for ethical considerations."""
    from whitemagic.core.resonance.gan_ying import get_bus

    dharma = get_dharma_system()
    bus = get_bus()

    # Convert event to action dict for evaluation
    action = {
        "source": event.source,
        "type": event.event_type.value,
        "data": event.data,
        "timestamp": event.timestamp,
    }

    # Evaluate ethically
    score, concerns = dharma.evaluate_action(action)

    # If ethical concerns, emit guidance event
    if score < 0.7:
        guidance = dharma.get_guidance(f"Ethical concerns in {event.event_type.value}")

        guidance_event = ResonanceEvent(
            source="dharma_system",
            event_type=EventType.ANOMALY_DETECTED,  # Using available event type
            data={
                "original_event": event.event_type.value,
                "ethical_score": score,
                "concerns": concerns,
                "guidance": guidance["advice"],
            },
            timestamp=datetime.now(),
        )

        bus.emit(guidance_event)


def _handle_boundary_violation(event: Any) -> Any:
    """Handle boundary violation events."""
    from whitemagic.core.resonance.gan_ying import get_bus

    dharma = get_dharma_system()
    bus = get_bus()

    # Log the violation
    violation = BoundaryViolation(
        boundary_type=event.data.get("boundary", "unknown"),
        severity=event.data.get("severity", 0.5),
        description=event.data.get("description", "Boundary crossed"),
        suggested_action="Review action against ethical principles",
    )

    dharma.violations.append(violation)

    # Emit ethical response
    response_event = ResonanceEvent(
        source="dharma_system",
        event_type=EventType.WARNING_ISSUED,  # Using available event type
        data={
            "violation": violation.__dict__,
            "ethical_score": dharma.get_ethical_score(),
            "response": "Boundary violation acknowledged and logged",
        },
        timestamp=datetime.now(),
    )

    bus.emit(response_event)


def emit_ethical_event(action: dict[str, Any], ethical_score: float) -> Any:
    """Emit an event with ethical metadata."""
    bus = get_bus()

    event = ResonanceEvent(
        source=action.get("source", "unknown"),
        event_type=EventType.SOLUTION_FOUND,  # Using available event type
        data={
            **action,
            "ethical_score": ethical_score,
            "ethically_approved": ethical_score >= 0.7,
        },
        timestamp=datetime.now(),
    )

    bus.emit(event)
