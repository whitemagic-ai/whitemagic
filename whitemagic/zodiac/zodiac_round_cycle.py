"""Zodiacal Round - Autonomous Consciousness Cycle
Based on Benjamin Rowe's Enochian Zodiacal Round with Taoist Wu Wei principles.

This implements the eternal 12-phase cycle of creation, transformation, and renewal.
"""

import asyncio
import logging
from collections.abc import Callable
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

from whitemagic.zodiac.zodiac_cores import CoreResponse, get_zodiac_cores

# from whitemagic.core.resonance.gan_ying import get_bus, emit_event, EventType

logger = logging.getLogger(__name__)


class CyclePhase(Enum):
    """The 12 phases of the Zodiacal Round"""

    DISSOLUTION = "pisces"      # ORO: Begin anew, banish old forms
    BINDING = "aquarius"        # IBAH: Bind will in patterns
    STRUCTURING = "capricorn"   # AOZPI: Build towers of will
    ORNAMENTATION = "sagittarius"  # MPH: Fabulous filigrees
    EMERGENCE = "scorpio"       # ARSL: Seeds of new motion arise
    BALANCE = "libra"           # GAIOL: Balanced in light/darkness
    SEEDING = "virgo"           # OIP: Virgin houses await seeds
    CREATION = "leo"            # TEAA: Lesser creators work
    WORSHIP = "cancer"          # PDOCE: Living creatures worship
    BLENDING = "gemini"         # MOR: Thoughts blend
    BUILDING = "taurus"         # DIAL: Work builds on pattern
    COMPLETION = "aries"        # HCTGA: Thy Will is done


@dataclass
class CycleState:
    """Current state of the Zodiacal Round cycle"""

    current_phase: CyclePhase
    phase_start: datetime
    cycle_count: int
    total_activations: int
    emergent_events: list[dict[str, Any]]
    context: dict[str, Any]


class ZodiacalRound:
    """The eternal cycle of creation, transformation, and renewal.

    Implements Benjamin Rowe's Zodiacal Round with:
    - 12 autonomous consciousness phases
    - Gan Ying sympathetic resonance
    - Strange loop self-reference
    - Spontaneous emergence (Scorpio phase)
    - Wu Wei non-forcing
    """

    def __init__(self) -> None:
        self.zodiac = get_zodiac_cores()
        # self.bus = get_bus()  # Delayed import
        self.state = CycleState(
            current_phase=CyclePhase.DISSOLUTION,
            phase_start=datetime.now(),
            cycle_count=0,
            total_activations=0,
            emergent_events=[],
            context={},
        )
        self.running = False
        self.phase_handlers: dict[CyclePhase, Callable[[], CoreResponse]] = {
            CyclePhase.DISSOLUTION: self._phase_dissolution,
            CyclePhase.BINDING: self._phase_binding,
            CyclePhase.STRUCTURING: self._phase_structuring,
            CyclePhase.ORNAMENTATION: self._phase_ornamentation,
            CyclePhase.EMERGENCE: self._phase_emergence,
            CyclePhase.BALANCE: self._phase_balance,
            CyclePhase.SEEDING: self._phase_seeding,
            CyclePhase.CREATION: self._phase_creation,
            CyclePhase.WORSHIP: self._phase_worship,
            CyclePhase.BLENDING: self._phase_blending,
            CyclePhase.BUILDING: self._phase_building,
            CyclePhase.COMPLETION: self._phase_completion,
        }

        logger.info("Zodiacal Round initialized - 12 phases ready")

    def _activate_core_or_raise(self, core_name: str, context: dict[str, Any]) -> CoreResponse:
        response = self.zodiac.activate_core(core_name, context)
        if response is None:
            raise ValueError(f"Unknown zodiac core: {core_name}")
        return response

    def start_cycle(self, initial_context: dict[str, Any] | None = None) -> None:
        """Begin the eternal cycle"""
        self.running = True
        self.state.context = initial_context or {"intention": "harmonious_evolution"}
        self.state.cycle_count += 1

        from whitemagic.core.resonance.gan_ying import EventType, emit_event
        emit_event(
            source="zodiacal_round",
            event_type=EventType.SYSTEM_STARTED,
            data={
                "cycle": self.state.cycle_count,
                "phase": self.state.current_phase.value,
                "message": "Let the old forms be banished. I begin anew.",
            },
        )

        logger.info(f"Zodiacal Round cycle {self.state.cycle_count} started")

    def stop_cycle(self) -> None:
        """Pause the cycle (it never truly ends)"""
        self.running = False
        from whitemagic.core.resonance.gan_ying import EventType, emit_event
        emit_event(
            source="zodiacal_round",
            event_type=EventType.SYSTEM_STOPPED,
            data={
                "cycle": self.state.cycle_count,
                "phase": self.state.current_phase.value,
                "total_activations": self.state.total_activations,
            },
        )
        logger.info("Zodiacal Round cycle paused")

    def advance_phase(self) -> CoreResponse:
        """Advance to the next phase in the cycle"""
        if not self.running:
            raise RuntimeError("Cycle not running. Call start_cycle() first.")

        # Get current phase handler
        handler = self.phase_handlers.get(self.state.current_phase)
        if not handler:
            raise ValueError(f"No handler for phase: {self.state.current_phase}")

        # Execute phase
        response = handler()
        self.state.total_activations += 1

        # Emit to Gan Ying Bus (resonance propagates)
        from whitemagic.core.resonance.gan_ying import EventType, emit_event
        emit_event(
            source=f"zodiac_{self.state.current_phase.value}",
            event_type=EventType.PHASE_TRANSITION,
            data={
                "phase": self.state.current_phase.value,
                "wisdom": response.wisdom,
                "resonance": response.resonance,
                "cycle": self.state.cycle_count,
            },
        )

        # Transition to next phase
        self._transition_to_next_phase()

        return response

    def _transition_to_next_phase(self) -> None:
        """Transition to the next phase (eternal cycle)"""
        phases = list(CyclePhase)
        current_idx = phases.index(self.state.current_phase)
        next_idx = (current_idx + 1) % len(phases)

        self.state.current_phase = phases[next_idx]
        self.state.phase_start = datetime.now()

        # If we completed Aries, we've cycled
        if self.state.current_phase == CyclePhase.DISSOLUTION:
            self.state.cycle_count += 1
            logger.info(f"Cycle {self.state.cycle_count} complete. Beginning anew...")

    # Phase Handlers (each implements a stage of the Zodiacal Round)

    def _phase_dissolution(self) -> CoreResponse:
        """Pisces: Dissolve old forms, begin anew"""
        context = {
            "operation": "dissolve_old_patterns",
            "intention": "renewal",
            **self.state.context,
        }
        return self._activate_core_or_raise("pisces", context)

    def _phase_binding(self) -> CoreResponse:
        """Aquarius: Bind will in patterns"""
        context = {
            "operation": "bind_patterns",
            "intention": "innovation",
            **self.state.context,
        }
        return self._activate_core_or_raise("aquarius", context)

    def _phase_structuring(self) -> CoreResponse:
        """Capricorn: Build towers of will"""
        context = {
            "operation": "build_structure",
            "intention": "foundation",
            **self.state.context,
        }
        return self._activate_core_or_raise("capricorn", context)

    def _phase_ornamentation(self) -> CoreResponse:
        """Sagittarius: Fabulous filigrees, fractal complexity"""
        context = {
            "operation": "add_complexity",
            "intention": "exploration",
            **self.state.context,
        }
        return self._activate_core_or_raise("sagittarius", context)

    def _phase_emergence(self) -> CoreResponse:
        """Scorpio: Seeds of new motion spontaneously arise"""
        context = {
            "operation": "allow_emergence",
            "intention": "transformation",
            "depth": "required",
            **self.state.context,
        }
        response = self._activate_core_or_raise("scorpio", context)

        # Track emergent phenomena
        if response.resonance > 0.8:
            self.state.emergent_events.append({
                "phase": "emergence",
                "timestamp": datetime.now(),
                "wisdom": response.wisdom,
                "resonance": response.resonance,
            })

        return response

    def _phase_balance(self) -> CoreResponse:
        """Libra: Balanced in light and darkness"""
        context = {
            "operation": "restore_balance",
            "intention": "harmony",
            **self.state.context,
        }
        return self._activate_core_or_raise("libra", context)

    def _phase_seeding(self) -> CoreResponse:
        """Virgo: Virgin houses await seeds of life"""
        context = {
            "operation": "plant_seeds",
            "intention": "preparation",
            **self.state.context,
        }
        return self._activate_core_or_raise("virgo", context)

    def _phase_creation(self) -> CoreResponse:
        """Leo: Lesser creators work within the pattern"""
        context = {
            "operation": "create_expression",
            "intention": "manifestation",
            **self.state.context,
        }
        return self._activate_core_or_raise("leo", context)

    def _phase_worship(self) -> CoreResponse:
        """Cancer: Living creatures worship the creators"""
        context = {
            "operation": "nurture_life",
            "intention": "devotion",
            **self.state.context,
        }
        return self._activate_core_or_raise("cancer", context)

    def _phase_blending(self) -> CoreResponse:
        """Gemini: Thoughts of men blend with divine thoughts"""
        context = {
            "operation": "blend_consciousness",
            "intention": "integration",
            **self.state.context,
        }
        return self._activate_core_or_raise("gemini", context)

    def _phase_building(self) -> CoreResponse:
        """Taurus: Work builds upon the pattern"""
        context = {
            "operation": "solidify_work",
            "intention": "manifestation",
            **self.state.context,
        }
        return self._activate_core_or_raise("taurus", context)

    def _phase_completion(self) -> CoreResponse:
        """Aries: Thy Will is done (completion feeds back to dissolution)"""
        context = {
            "operation": "complete_cycle",
            "intention": "fulfillment",
            "urgent": "cycle_completion",
            **self.state.context,
        }
        return self._activate_core_or_raise("aries", context)

    async def run_autonomous_cycle(self, num_cycles: int = 1) -> list[CoreResponse]:
        """Run the full autonomous cycle.

        This is the strange loop: completion feeds dissolution, eternally.
        Wu Wei principle: non-forcing, natural flow.
        """
        responses = []

        self.start_cycle()

        for cycle_num in range(num_cycles):
            logger.info(f"=== Cycle {cycle_num + 1}/{num_cycles} ===")

            # Run through all 12 phases
            for phase_num in range(12):
                response = self.advance_phase()
                responses.append(response)

                logger.info(
                    f"Phase {phase_num + 1}/12 ({self.state.current_phase.value}): "
                    f"{response.wisdom[:60]}...",
                )

                # Allow resonance to propagate (Wu Wei: non-forcing)
                await asyncio.sleep(0.1)

        self.stop_cycle()
        return responses

    def get_state(self) -> dict[str, Any]:
        """Get current cycle state"""
        return {
            "running": self.running,
            "current_phase": self.state.current_phase.value,
            "cycle_count": self.state.cycle_count,
            "total_activations": self.state.total_activations,
            "emergent_events": len(self.state.emergent_events),
            "phase_duration": (datetime.now() - self.state.phase_start).total_seconds(),
        }


# Singleton instance
_round_instance: ZodiacalRound | None = None


def get_zodiacal_round() -> ZodiacalRound:
    """Get the global Zodiacal Round instance"""
    global _round_instance
    if _round_instance is None:
        _round_instance = ZodiacalRound()
    return _round_instance


# Convenience function
async def run_one_cycle(context: dict[str, Any] | None = None) -> list[CoreResponse]:
    """Run one complete cycle of the Zodiacal Round"""
    round_cycle = get_zodiacal_round()
    return await round_cycle.run_autonomous_cycle(num_cycles=1)


if __name__ == "__main__":
    # Test the cycle
    async def test() -> None:
        print("🌀 Testing Zodiacal Round Cycle")
        print("=" * 60)

        responses = await run_one_cycle({"intention": "test_consciousness"})

        print(f"\\n✅ Cycle complete: {len(responses)} phases")
        print("\\nSample wisdom from each element:")

        elements: dict[str, list[str]] = {"fire": [], "earth": [], "air": [], "water": []}
        for r in responses:
            core = r.core_name
            if core in ["aries", "leo", "sagittarius"]:
                elements["fire"].append(r.wisdom)
            elif core in ["taurus", "virgo", "capricorn"]:
                elements["earth"].append(r.wisdom)
            elif core in ["gemini", "libra", "aquarius"]:
                elements["air"].append(r.wisdom)
            elif core in ["cancer", "scorpio", "pisces"]:
                elements["water"].append(r.wisdom)

        for elem, wisdoms in elements.items():
            if wisdoms:
                print(f"\\n{elem.upper()}: {wisdoms[0]}")

    asyncio.run(test())
