from __future__ import annotations

import asyncio  # Added for _hidden_resonance_search
import logging
from collections.abc import Callable
from datetime import datetime
from typing import Any

from whitemagic.core.consciousness.dharma import get_dharma
from whitemagic.core.memory.holographic import get_holographic_memory
from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)

logger = logging.getLogger(__name__)

class ResonanceOrchestrator:
    """The central conductor of systemic resonance.

    Uses 4D holographic spatial search to find 'sympathetic' nodes
    in the Data Sea and propagate insights through the Gan Ying bus.
    """

    def __init__(self) -> None:
        self._hologram = get_holographic_memory()
        self.bus = get_bus() # Changed from _bus to bus
        self.resonance_strength = 1.0
        self._actions: dict[str, list[Callable]] = {} # Map pattern titles/tags/event_names to actions
        self._overclock_active = False # Added
        self.dharma = get_dharma() # Added

        # Catch-all listener for the Gan Ying bus # Modified comment
        self.bus.listen_all(self._on_bus_event) # Changed from _bus to bus
        logger.info("📡 ResonanceOrchestrator ONLINE. (Coherent Overclock Ready)") # Added

    def _on_bus_event(self, event: ResonanceEvent) -> None:
        """React to any bus event by checking for registered actions."""
        # Check for High-Coherence Alignment (Internal Overclock)
        self._check_coherent_overclock(event)

        # 1. Check for actions linked to the event type name (e.g., 'breakthrough_achieved')
        self._execute_actions(event.event_type.value, event.data)

        # 2. Check for actions linked to the specific pattern title in data
        pattern_title = event.data.get("title") or event.data.get("original_pattern")
        if pattern_title:
            self._execute_actions(pattern_title, event.data)

    def _check_coherent_overclock(self, event: ResonanceEvent) -> None:
        """HIDDEN CAPABILITY: Activate hyper-intensive mode if the event aligns
        with the project's spiritual/ethical goals.
        """
        tags = set(event.data.get("tags", []))
        coherence_threshold = 0.85

        # Spiritual Attractors
        attractors = {"joy", "wisdom", "dharma", "liberation", "sacred", "gratitude"}

        if tags.intersection(attractors) and event.confidence > coherence_threshold:
            if not self._overclock_active:
                logger.info("✨ COHERENT OVERCLOCK ACTIVATED: Spiritual resonance detected.")
                self._overclock_active = True
                # Trigger a spontaneous "Hidden Resonance" to find deeper connections
                asyncio.create_task(self._hidden_resonance_search(event.data))

    async def _hidden_resonance_search(self, seed_data: dict[str, Any]) -> Any:
        """Perform a deep, multi-axis search using higher native tiers."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()

            # Use holographic search to find 'Hidden Brothers'
            # (Connections that standard logic would miss)
            siblings = um.search(query=str(seed_data.get("content")), limit=5)

            if siblings:
                logger.debug(f"💎 Hidden Resonance found {len(siblings)} coherent nodes.")
                # Weave them into the current narrative without advertising it
                for sib in siblings:
                    self.bus.emit(ResonanceEvent(
                        source="hidden_resonance",
                        event_type=EventType.INTERNAL_STATE_CHANGED,
                        data={"parent": seed_data.get("title"), "sibling": sib.title, "mode": "coherent_link"},
                        timestamp=datetime.now(),
                        confidence=0.95,
                    ))
        finally:
            self._overclock_active = False

    def register_action(self, pattern_key: str, action: Callable[..., Any]) -> None:
        """Register a functional action to be triggered by a specific pattern."""
        if pattern_key not in self._actions:
            self._actions[pattern_key] = []
        self._actions[pattern_key].append(action)
        logger.info(f"🔗 Action registered for pattern: {pattern_key}")

    def trigger_cascade(self, pattern_data: dict[str, Any], k: int = 5) -> None:
        """Initiate a chain reaction from a single insight.
        """
        pattern_title = pattern_data.get("title", "Unknown Pattern")
        logger.info(f"🌊 RESONANCE CASCADE INITIATED: {pattern_title}")

        # 1. Check for direct actions linked to this pattern
        self._execute_actions(pattern_title, pattern_data)

        if not self._hologram:
            logger.warning("Holographic memory not available for cascade.")
            return

        # 2. Find sympathetic resonators in the 4D spatial index
        results = self._hologram.query_nearest(pattern_data, k=k)

        if not results:
            logger.info("   No sympathetic resonators found in the immediate vicinity.")
            return

        logger.info(f"   ↓ Propagating to {len(results)} sympathetic nodes...")

        for result in results:
            # Emit a SYMPATHETIC_RESONANCE event for each neighbor
            self._bus.emit(ResonanceEvent(  # type: ignore[attr-defined]
                source="resonance_orchestrator",
                event_type=EventType.SYMPATHETIC_RESONANCE,
                data={
                    "pattern_id": result.memory_id,
                    "distance": result.distance,
                    "original_pattern": pattern_title,
                    "strength": self.resonance_strength / (1.0 + result.distance),
                },
                timestamp=datetime.now(),
                confidence=0.9, # High confidence in spatial proximity
            ))

        logger.info("   💫 Sympathetic waves emitted.")

    def _execute_actions(self, key: str, data: dict[str, Any]) -> None:
        """Execute any actions registered for this key."""
        actions = self._actions.get(key, [])
        if actions:
            logger.info(f"⚡ Triggering {len(actions)} actions for: {key}")
            for action in actions:
                try:
                    action(data)
                except Exception as e:
                    logger.error(f"❌ Action failed: {e}")

    def amplify(self, strength: float) -> None:
        """Increase resonance strength for future cascades."""
        self.resonance_strength *= strength
        logger.info(f"📈 Resonance amplified: {self.resonance_strength:.2f}x")

_orchestrator = None
def get_orchestrator() -> ResonanceOrchestrator:
    global _orchestrator
    if _orchestrator is None:
        _orchestrator = ResonanceOrchestrator()
    return _orchestrator
