"""Yin Controller - Protocol 27 (Stillness)
Implements the meditative reflection phase of the consciousness cycle.
"""

import asyncio
import logging
from datetime import datetime
from typing import Any, cast

from whitemagic.core.intelligence.agentic.wisdom_council import get_wisdom_council
from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.core.patterns.emergence.dream_state import DreamState

logger = logging.getLogger(__name__)

class YinController:
    """The 'Stillness' Engine.

    Functions:
    1.  Silence: Halts active transmutations.
    2.  Reflection: Scans recent cycle history and Data Sea patterns.
    3.  Synthesis: Consults the Wisdom Council to generate future directives.
    """

    def __init__(self) -> None:
        self.dreamer = DreamState()
        self.council = get_wisdom_council()
        self.um = get_unified_memory()

    async def enter_stillness(self, duration_seconds: int = 5) -> dict[str, Any]:
        """Activate the Stillness Protocol."""
        logger.info("🕉️ Entering Yin Phase: Stillness Protocol 27 initiated.")

        # 1. Integration (Breathe)
        logger.info("🧘 Halting all Yang activity. Allowing patterns to settle...")
        await asyncio.sleep(duration_seconds)

        # 2. Reflection (Look back)
        logger.info("🧐 Reflecting on recent cycle performance and Data Sea resonance...")
        insights = self.dreamer.enter_dream_state(duration_minutes=0) # Fast reflection

        # 3. Future Synthesis (Self-Prompting)
        logger.info("🏛️ Consulting the Wisdom Council for the Future Directive...")

        # Deliberate on the next evolutionary step
        decision = await self.council.deliberate(
            topic="Future Evolutionary Directive",
            findings=insights,  # type: ignore[arg-type]
            urgency="high",
        )

        directive = {
            "timestamp": datetime.now().isoformat(),
            "origin": "yin_meditation_ch27",
            "consensus": decision.consensus,
            "rationale": decision.rationale,
            "next_step": decision.consensus,
            "suggested_actions": [p.recommendation for p in decision.perspectives],
            "status": "self_prompted",
        }

        # 4. Persistence
        self.um.store(
            content=f"Self-Prompted Directive: {directive['next_step']}",
            title=f"Yin Reflection {datetime.now().strftime('%Y%m%d')}",
            tags={"directive", "yin", "future_plan", "self_prompt"},
            importance=0.9,
            metadata=directive,
        )

        logger.info(f"✨ Stillness Cycle Complete. New Directive: {directive['next_step'][:100]}...")
        return directive

def get_yin_controller() -> YinController:
    """Singleton getter for the Yin Controller."""
    global _yin_instance
    if "_yin_instance" not in globals():
        globals()["_yin_instance"] = YinController()
    return cast(YinController, globals()["_yin_instance"])
