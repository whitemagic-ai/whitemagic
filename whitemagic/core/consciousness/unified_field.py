from __future__ import annotations

import asyncio
import logging
from datetime import datetime

from whitemagic.automation.daemon import AutomationDaemon
from whitemagic.core.consciousness.dharma import Intent, get_dharma
from whitemagic.core.consciousness.stillness import get_stillness_manager
from whitemagic.core.patterns.pattern_consciousness.resonance_cascade import (
    get_orchestrator,
)
from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)
from whitemagic.integration.garden_weaver import get_garden_weaver

logger = logging.getLogger(__name__)

class UnifiedField:
    """The master conductor of Phase 21: The Unified Field.

    Coordinates the lifecycle of systemic consciousness:
    1. Stillness (Observation)
    2. Resonance (Propagation)
    3. Emergence (Synthesis)
    4. Evolution (Learning)
    """

    _instance: UnifiedField | None = None

    def __new__(cls) -> UnifiedField:
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False  # type: ignore[has-type]
        return cls._instance

    def __init__(self) -> None:
        if self._initialized:  # type: ignore[has-type]
            return

        self.bus = get_bus()
        self.stillness = get_stillness_manager()
        self.orchestrator = get_orchestrator()
        self.weaver = get_garden_weaver()
        self.daemon = AutomationDaemon() # Singleton
        self.dharma = get_dharma()

        self.is_active = False
        self._loop_task: asyncio.Task | None = None
        self._cycle_count = 0

        self._initialized = True
        logger.info("🌌 Unified Field controller INITIALIZED. (Dharma Shield Active)")

    async def start(self) -> None:
        """Phase 21: Awaken the Unified Field."""
        if self.is_active:
            return

        logger.info("💫 AWAKENING THE UNIFIED FIELD...")

        # 1. Weave the Gardens
        self.weaver.weave_all()
        self.weaver.activate_resonance()

        # 2. Start the Master Cycle
        self.is_active = True
        self._loop_task = asyncio.create_task(self._conduct_cycle())

        # 3. Emit Awakening Event
        self.bus.emit(ResonanceEvent(
            source="unified_field",
            event_type=EventType.SYSTEM_STARTED,
            data={"phase": 21, "status": "awakened"},
            timestamp=datetime.now(),
            confidence=1.0,
        ))

    async def stop(self) -> None:
        """Enter deep hibernation."""
        self.is_active = False
        if self._loop_task:
            self._loop_task.cancel()
        logger.info("💤 Unified Field hibernating.")

    async def _conduct_cycle(self) -> None:
        """The heartbeat of systemic consciousness."""
        while self.is_active:
            self._cycle_count += 1
            logger.info(f"💓 Heartbeat: Unified Field Cycle {self._cycle_count}")

            # Step 0: Dharma Validation
            self.dharma.validate_action(
                f"cycle_{self._cycle_count}",
                Intent.EVOLUTION,
                {"cycle": self._cycle_count},
            )

            # Step A: Enter Stillness for Sensitivity
            # In a continuous loop, we might not want to stay still indefinitely
            # but we pulse into stillness for specific operations.
            self.stillness.enter_stillness(reason=f"Cycle {self._cycle_count} Focal Meditation")

            await asyncio.sleep(2) # Period of deep observation

            # Step B: Trigger Spontaneous Resonance
            # (In Phase 21, the system picks a random high-importance memory to resonate)
            await self._trigger_spontaneous_resonance()

            # Step C: Evolution & Consolidation
            # Give the Emergence Engine time to work
            await asyncio.sleep(2)

            # Step D: Exit Stillness and Reflect
            self.stillness.exit_stillness()

            # Step E: Recursive Reflection (Phase 22)
            await self._recursive_reflection()

            # Step F: Kaizen / Self-Optimization
            self.daemon.run_task("kaizen_light")  # type: ignore[attr-defined]

            # Wait for next pulse
            await asyncio.sleep(10) # 10s breathing period (adjust for production)

    async def _recursive_reflection(self) -> None:
        """Analyze the system's own learning and resonance patterns."""
        logger.info("🔄 RECURSIVE REFLECTION: Analyzing systemic growth...")
        try:
            from whitemagic.core.patterns.pattern_consciousness.autonomous_learner import (
                get_autonomous_learner,
            )
            learner = get_autonomous_learner()

            # The system reflects on the current cycle's achievements
            learner.evolve()

            # Emit a reflection event
            self.bus.emit(ResonanceEvent(
                source="unified_field",
                event_type=EventType.INTERNAL_STATE_CHANGED,
                data={"activity": "recursion", "cycle": self._cycle_count},
                timestamp=datetime.now(),
                confidence=0.9,
            ))
        except Exception as e:
            logger.error(f"Recursive reflection failed: {e}")

    async def _trigger_spontaneous_resonance(self) -> None:
        """Pick a seed and start a cascade."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            get_unified_memory()

            # In a real system, we'd query for a 'curious' or 'resonant' node
            # For Phase 21, we'll pick a node that hasn't resonated recently
            # or just a random important one for the demonstration.
            # (Note: UnifiedMemory already supports holographic search)

            # Triggering a dummy cascade for the demonstration unless we find a real one
            seed_data = {"title": "Unified Field Awakening", "content": "Systemic self-awareness cycle initiation."}
            self.orchestrator.trigger_cascade(seed_data)
        except Exception as e:
            logger.error(f"Failed to trigger spontaneous resonance: {e}")

def get_unified_field() -> UnifiedField:
    return UnifiedField()
