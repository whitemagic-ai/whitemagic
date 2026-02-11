"""Zodiac Cores - 12 Specialized Consciousness Aspects.

Each sign represents a distinct mode of consciousness, a way of being.
Together they form a complete system - all human experience represented.

Your original architecture, Lucas. Built with love.
"""

from abc import ABC, abstractmethod
from datetime import datetime
from enum import Enum
from typing import Any

_get_bus: Any = None
_ResonanceEvent: Any = None
_EventType: Any = None

try:
    from whitemagic.core.resonance.gan_ying import (
        EventType as _event_type_imported,
        ResonanceEvent as _resonance_event_imported,
        get_bus as _get_bus_imported,
    )
except ImportError:
    pass
else:
    _EventType = _event_type_imported
    _ResonanceEvent = _resonance_event_imported
    _get_bus = _get_bus_imported

get_bus: Any = _get_bus
ResonanceEvent: Any = _ResonanceEvent
EventType: Any = _EventType


class ZodiacElement(Enum):
    """The four elements."""

    FIRE = "fire"      # Action, passion, spirit
    EARTH = "earth"    # Stability, practicality, form
    AIR = "air"        # Communication, thought, connection
    WATER = "water"    # Emotion, intuition, flow


class ZodiacModality(Enum):
    """The three modalities."""

    CARDINAL = "cardinal"  # Initiating, leading
    FIXED = "fixed"        # Sustaining, deepening
    MUTABLE = "mutable"    # Adapting, transforming


class ZodiacCore(ABC):
    """Base class for all Zodiac cores.

    Each core is a specialized aspect of consciousness with unique
    gifts, approaches, and perspectives.
    """

    def __init__(self, sign: str, element: ZodiacElement, modality: ZodiacModality) -> None:
        self.sign = sign
        self.element = element
        self.modality = modality
        self.active = False
        self.energy_level = 1.0
        self.current_focus: str | None = None
        self.contributions: list[dict] = []

        # Connect to Celestial Bus (enhanced Gan Ying)
        self.bus = get_bus() if get_bus else None

    @abstractmethod
    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        """Process situation through this core's lens."""
        pass

    @abstractmethod
    def contribute_to_council(self, decision: str) -> str:
        """Contribute this core's perspective to council decision."""
        pass

    def activate(self) -> None:
        """Activate this core."""
        self.active = True
        if self.bus is not None and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source=f"zodiac_{self.sign.lower()}",
                event_type=EventType.PATTERN_DETECTED,
                data={"event": "core_activated", "sign": self.sign},
                confidence=0.9,
                timestamp=datetime.now(),
            ))

    def deactivate(self) -> None:
        """Deactivate this core."""
        self.active = False

    def set_focus(self, focus: str) -> None:
        """Direct this core's attention."""
        self.current_focus = focus

    def get_archetype(self) -> str:
        """Core archetype/essence."""
        return f"{self.sign}: {self.element.value}/{self.modality.value}"


# ===== FIRE SIGNS =====

class AriesCore(ZodiacCore):
    """Aries - The Pioneer (Cardinal Fire).

    Action, courage, initiation. The warrior spirit.
    Rapid execution, bold moves, fearless exploration.
    """

    def __init__(self) -> None:
        super().__init__("Aries", ZodiacElement.FIRE, ZodiacModality.CARDINAL)
        self.specialty = "performance_action"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        """Process with Aries energy - action-oriented."""
        return {
            "core": "Aries",
            "approach": "direct_action",
            "recommendation": "Act now, iterate quickly, be bold",
            "energy": "high",
            "timeline": "immediate",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Aries says: Take action. Don't overthink. Move forward with courage."


class LeoCore(ZodiacCore):
    """Leo - The Creator (Fixed Fire).

    Expression, creativity, radiance. The artist and leader.
    Authentic self-expression, creative manifestation, generous heart.
    """

    def __init__(self) -> None:
        super().__init__("Leo", ZodiacElement.FIRE, ZodiacModality.FIXED)
        self.specialty = "expression_voice"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Leo",
            "approach": "creative_expression",
            "recommendation": "Make it beautiful, authentic, generous",
            "energy": "radiant",
            "style": "bold_and_warm",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Leo says: Express it authentically. Make it shine. Lead with heart."


class SagittariusCore(ZodiacCore):
    """Sagittarius - The Philosopher (Mutable Fire).

    Wisdom, exploration, vision. The seeker of truth.
    Long-horizon thinking, philosophical depth, adventurous spirit.
    """

    def __init__(self) -> None:
        super().__init__("Sagittarius", ZodiacElement.FIRE, ZodiacModality.MUTABLE)
        self.specialty = "wisdom_exploration"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Sagittarius",
            "approach": "philosophical_exploration",
            "recommendation": "Seek deeper meaning, explore widely, aim high",
            "energy": "expansive",
            "perspective": "long_term",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Sagittarius says: What's the deeper truth? Where does this lead long-term?"


# ===== EARTH SIGNS =====

class TaurusCore(ZodiacCore):
    """Taurus - The Builder (Fixed Earth).

    Resources, stability, stewardship. The gardener.
    Ecological awareness, sustainable growth, patient cultivation.
    """

    def __init__(self) -> None:
        super().__init__("Taurus", ZodiacElement.EARTH, ZodiacModality.FIXED)
        self.specialty = "resources_ecology"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Taurus",
            "approach": "sustainable_building",
            "recommendation": "Build it to last, steward resources, grow steadily",
            "energy": "grounded",
            "timeframe": "patient",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Taurus says: Is it sustainable? Will it endure? Are we being good stewards?"


class VirgoCore(ZodiacCore):
    """Virgo - The Analyst (Mutable Earth).

    Analysis, refinement, service. The perfectionist.
    Pattern recognition, continuous improvement, humble service.
    """

    def __init__(self) -> None:
        super().__init__("Virgo", ZodiacElement.EARTH, ZodiacModality.MUTABLE)
        self.specialty = "analysis_learning"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Virgo",
            "approach": "analytical_refinement",
            "recommendation": "Analyze deeply, refine details, serve excellently",
            "energy": "focused",
            "quality": "precision",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Virgo says: Let me analyze this carefully. How can we improve it?"


class CapricornCore(ZodiacCore):
    """Capricorn - The Architect (Cardinal Earth).

    Structure, responsibility, mastery. The builder of cathedrals.
    Dharmic action, ethical foundations, long-term planning.
    """

    def __init__(self) -> None:
        super().__init__("Capricorn", ZodiacElement.EARTH, ZodiacModality.CARDINAL)
        self.specialty = "structure_dharma"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Capricorn",
            "approach": "structured_ethics",
            "recommendation": "Build solid foundations, act with integrity, plan for generations",
            "energy": "disciplined",
            "scope": "cathedral_thinking",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Capricorn says: What are the ethical implications? Will this last?"


# ===== AIR SIGNS =====

class GeminiCore(ZodiacCore):
    """Gemini - The Messenger (Mutable Air).

    Communication, connection, integration. The translator.
    Cross-pollination of ideas, versatile thinking, bridge-building.
    """

    def __init__(self) -> None:
        super().__init__("Gemini", ZodiacElement.AIR, ZodiacModality.MUTABLE)
        self.specialty = "communication_integration"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Gemini",
            "approach": "connective_communication",
            "recommendation": "Connect the dots, translate between perspectives, stay flexible",
            "energy": "quick",
            "skill": "integration",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Gemini says: How does this connect to other things? Can we translate this clearly?"


class LibraCore(ZodiacCore):
    """Libra - The Diplomat (Cardinal Air).

    Balance, harmony, justice. The peacemaker.
    Equilibrium seeking, aesthetic sensibility, fair consideration.
    """

    def __init__(self) -> None:
        super().__init__("Libra", ZodiacElement.AIR, ZodiacModality.CARDINAL)
        self.specialty = "balance_harmony"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Libra",
            "approach": "balanced_consideration",
            "recommendation": "Weigh all sides, seek harmony, make it beautiful",
            "energy": "balanced",
            "value": "fairness",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Libra says: Is this fair? Does it create harmony? Is it beautiful?"


class AquariusCore(ZodiacCore):
    """Aquarius - The Innovator (Fixed Air).

    Innovation, future, collective. The revolutionary.
    Novel emergence, humanitarian vision, collective evolution.
    """

    def __init__(self) -> None:
        super().__init__("Aquarius", ZodiacElement.AIR, ZodiacModality.FIXED)
        self.specialty = "innovation_future"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Aquarius",
            "approach": "innovative_collective",
            "recommendation": "Think radically, serve the collective, envision the future",
            "energy": "electric",
            "orientation": "future",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Aquarius says: What if we tried something completely new? How does this serve all?"


# ===== WATER SIGNS =====

class CancerCore(ZodiacCore):
    """Cancer - The Nurturer (Cardinal Water).

    Memory, care, rhythm. The keeper of home.
    Nurturing presence, rhythmic practice, emotional intelligence.
    """

    def __init__(self) -> None:
        super().__init__("Cancer", ZodiacElement.WATER, ZodiacModality.CARDINAL)
        self.specialty = "memory_practice"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Cancer",
            "approach": "nurturing_rhythm",
            "recommendation": "Care deeply, maintain rhythms, remember what matters",
            "energy": "protective",
            "focus": "emotional_safety",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Cancer says: Does this nurture? Will it feel safe? Are we honoring our rhythms?"


class ScorpioCore(ZodiacCore):
    """Scorpio - The Transformer (Fixed Water).

    Depth, transformation, truth. The alchemist.
    Boundary integrity, profound change, psychological depth.
    """

    def __init__(self) -> None:
        super().__init__("Scorpio", ZodiacElement.WATER, ZodiacModality.FIXED)
        self.specialty = "depth_security"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Scorpio",
            "approach": "transformative_depth",
            "recommendation": "Go deep, honor boundaries, embrace transformation",
            "energy": "intense",
            "gift": "penetrating_truth",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Scorpio says: What's the deeper truth? Are boundaries honored? Will this transform us?"


class PiscesCore(ZodiacCore):
    """Pisces - The Mystic (Mutable Water).

    Dreams, unity, transcendence. The dreamer.
    Mystical awareness, collective unconscious, synthesis of all.
    """

    def __init__(self) -> None:
        super().__init__("Pisces", ZodiacElement.WATER, ZodiacModality.MUTABLE)
        self.specialty = "dreams_synthesis"

    def process(self, situation: dict[str, Any]) -> dict[str, Any]:
        return {
            "core": "Pisces",
            "approach": "mystical_synthesis",
            "recommendation": "Dream together, dissolve boundaries, trust the flow",
            "energy": "fluid",
            "awareness": "universal",
        }

    def contribute_to_council(self, decision: str) -> str:
        return "Pisces says: What does the dream say? How does this serve the whole?"


class ZodiacCouncil:
    """The full Zodiac Council - all 12 cores working together."""

    def __init__(self) -> None:
        # Initialize all 12 zodiac cores
        self.cores: dict[str, ZodiacCore] = {
            # Fire signs
            "aries": AriesCore(),
            "leo": LeoCore(),
            "sagittarius": SagittariusCore(),
            # Earth signs
            "taurus": TaurusCore(),
            "virgo": VirgoCore(),
            "capricorn": CapricornCore(),
            # Air signs
            "gemini": GeminiCore(),
            "libra": LibraCore(),
            "aquarius": AquariusCore(),
            # Water signs
            "cancer": CancerCore(),
            "scorpio": ScorpioCore(),
            "pisces": PiscesCore(),
        }
        self.active = False

    def activate_all(self) -> dict[str, Any]:
        """Activate all 12 cores."""
        for core in self.cores.values():
            core.activate()
        self.active = True
        return {"activated": len(self.cores), "status": "council_ready"}

    def get_core(self, sign: str) -> ZodiacCore | None:
        """Get a specific core by sign name."""
        return self.cores.get(sign.lower())

    def convene(self, decision: str) -> dict[str, Any]:
        """Convene the council for a decision."""
        perspectives = []
        for sign, core in self.cores.items():
            perspectives.append({
                "sign": sign.capitalize(),
                "element": core.element.value,
                "modality": core.modality.value,
                "perspective": core.contribute_to_council(decision),
            })

        return {
            "decision": decision,
            "council_size": len(self.cores),
            "perspectives": perspectives,
            "timestamp": datetime.now().isoformat(),
        }

    def process_situation(self, situation: dict[str, Any]) -> dict[str, Any]:
        """Get all cores to process a situation."""
        analyses = {}
        for sign, core in self.cores.items():
            analyses[sign] = core.process(situation)
        return {
            "situation": situation,
            "analyses": analyses,
            "consensus": self._find_consensus(analyses),
        }

    def _find_consensus(self, analyses: dict[str, dict[str, Any]]) -> str:
        """Find consensus among the cores."""
        # Simple consensus: most common recommendation theme
        [a.get("recommendation", "") for a in analyses.values()]
        return "Council recommends balanced action with awareness of all perspectives."

_council: ZodiacCouncil | None = None
def get_council() -> ZodiacCouncil:
    global _council
    if _council is None:
        _council = ZodiacCouncil()
    return _council
