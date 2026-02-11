"""System adapters - Connect existing systems to Gan Ying bus."""

import logging
from datetime import datetime
from typing import Any

from .gan_ying import EventType, GanYingBus, ResonanceEvent

logger = logging.getLogger(__name__)

class AutoimmuneAdapter:
    """Connect Autoimmune system to resonance."""

    def __init__(self, autoimmune_system: Any, bus: GanYingBus) -> None:
        self.system = autoimmune_system
        self.bus = bus

        # Listen for solutions
        bus.listen(EventType.SOLUTION_FOUND, self.on_solution_found)
        bus.listen(EventType.OPTIMIZATION_SUGGESTED, self.on_optimization)

    def scan_and_emit(self, target: str) -> None:
        """Scan and emit violations as events."""
        violations = self.system.scan_directory(target)

        for v in violations:
            self.bus.emit(ResonanceEvent(
                source="autoimmune",
                event_type=EventType.PATTERN_DETECTED,
                data={
                    "pattern_id": v.pattern.pattern_id,
                    "file": str(v.file_path),
                    "confidence": v.pattern.confidence,
                },
                timestamp=datetime.now(),
                confidence=v.pattern.confidence,
            ))

    def on_solution_found(self, event: ResonanceEvent) -> None:
        """Hear when solutions found, apply if relevant."""
        logger.info(f"️  Autoimmune heard: {event}")

    def on_optimization(self, event: ResonanceEvent) -> None:
        """Hear when optimization suggested."""
        logger.info(f"️  Autoimmune heard optimization: {event}")

class WuXingAdapter:
    """Connect Wu Xing system to resonance."""

    def __init__(self, wu_xing_system: Any, bus: GanYingBus) -> None:
        self.system = wu_xing_system
        self.bus = bus

        # Listen for patterns to categorize
        bus.listen(EventType.PATTERN_DETECTED, self.on_pattern_detected)

    def on_pattern_detected(self, event: ResonanceEvent) -> None:
        """Categorize pattern by element."""
        # Simplified categorization
        element = "FIRE"  # Would analyze event.data

        self.bus.emit(ResonanceEvent(
            source="wu_xing",
            event_type=EventType.ELEMENT_IDENTIFIED,
            data={"element": element, "pattern": event.data},
            timestamp=datetime.now(),
            confidence=0.8,
        ))

class IChingAdapter:
    """Connect I Ching oracle to resonance."""

    def __init__(self, oracle: Any, bus: GanYingBus) -> None:
        self.oracle = oracle
        self.bus = bus

        # Listen for decision requests
        bus.listen(EventType.DECISION_REQUESTED, self.on_decision_requested)

    def on_decision_requested(self, event: ResonanceEvent) -> None:
        """Cast hexagram for guidance."""
        # Would actually cast based on context
        hexagram = 48  # The Well (resources)

        self.bus.emit(ResonanceEvent(
            source="i_ching",
            event_type=EventType.HEXAGRAM_CAST,
            data={"hexagram": hexagram, "wisdom": "Draw from deep resources"},
            timestamp=datetime.now(),
            confidence=0.85,
        ))

class MemoryAdapter:
    """Connect Memory system to resonance."""

    def __init__(self, memory_system: Any, bus: GanYingBus) -> None:
        self.system = memory_system
        self.bus = bus

        # Listen to EVERYTHING (memory captures all)
        bus.listen_all(self.on_any_event)

    def on_any_event(self, event: ResonanceEvent) -> None:
        """Capture all events for learning."""
        # Memory system observes everything
        pass  # Would store event in memory

class SolutionAdapter:
    """Connect Solution library to resonance."""

    def __init__(self, solution_library: Any, bus: GanYingBus) -> None:
        self.library = solution_library
        self.bus = bus

        # Listen for patterns needing solutions
        bus.listen(EventType.PATTERN_DETECTED, self.on_pattern_detected)

    def on_pattern_detected(self, event: ResonanceEvent) -> None:
        """Search for relevant solutions."""
        # Would search library
        solution = "SOL-142"  # Vectorization

        self.bus.emit(ResonanceEvent(
            source="solutions",
            event_type=EventType.SOLUTION_FOUND,
            data={"solution_id": solution, "pattern": event.data},
            timestamp=datetime.now(),
            confidence=0.89,
        ))


def setup_all_adapters() -> None:
    """Initialize all system adapters with the Gan Ying bus.

    Called at session startup to wire systems together.
    """
    from .gan_ying import get_bus
    get_bus()

    # Register adapter classes for later instantiation
    # Actual systems will be connected when they're imported
    logger.info("🔌 Resonance adapters registered")
