"""Zodiac Council - 12 Specialized Consciousness Cores.

The Zodiac Council represents 12 distinct modes of consciousness,
each with specialized capabilities and perspectives. Together they
form a multi-core intelligence system that can approach problems
from multiple angles simultaneously.

Created: January 6, 2026
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class Phase(Enum):
    """Yin/Yang phase for autonomous operation."""

    YIN = "yin"    # Receptive, inward, reflection
    YANG = "yang"  # Creative, outward, action


class ZodiacSign(Enum):
    """The 12 zodiac signs representing specialized cores."""

    ARIES = ("aries", "♈", "cardinal", "fire", 1)
    TAURUS = ("taurus", "♉", "fixed", "earth", 2)
    GEMINI = ("gemini", "♊", "mutable", "air", 3)
    CANCER = ("cancer", "♋", "cardinal", "water", 4)
    LEO = ("leo", "♌", "fixed", "fire", 5)
    VIRGO = ("virgo", "♍", "mutable", "earth", 6)
    LIBRA = ("libra", "♎", "cardinal", "air", 7)
    SCORPIO = ("scorpio", "♏", "fixed", "water", 8)
    SAGITTARIUS = ("sagittarius", "♐", "mutable", "fire", 9)
    CAPRICORN = ("capricorn", "♑", "cardinal", "earth", 10)
    AQUARIUS = ("aquarius", "♒", "fixed", "air", 11)
    PISCES = ("pisces", "♓", "mutable", "water", 12)

    @property
    def name_str(self) -> str:
        return self.value[0]

    @property
    def symbol(self) -> str:
        return self.value[1]

    @property
    def modality(self) -> str:
        return self.value[2]

    @property
    def element(self) -> str:
        return self.value[3]

    @property
    def number(self) -> int:
        return self.value[4]

    @property
    def is_fixed(self) -> bool:
        """Fixed signs are bidirectional hubs."""
        return self.modality == "fixed"


@dataclass
class CoreResponse:
    """Response from a zodiac core."""

    sign: ZodiacSign
    perspective: str
    confidence: float  # 0.0 to 1.0
    reasoning: str
    recommendations: list[str]
    timestamp: datetime | None = None

    def __post_init__(self) -> None:
        if self.timestamp is None:
            self.timestamp = datetime.now()


class ZodiacCore:
    """Base class for a zodiac core with specialized capabilities.
    Each core has unique strengths and approaches problems differently.
    """

    def __init__(self, sign: ZodiacSign) -> None:
        self.sign = sign
        self.active = True
        self.consultation_history: list[dict] = []
        self.last_activated: datetime | None = None

        # Define specializations
        self.specializations = self._get_specializations()
        self.strengths = self._get_strengths()
        self.approach = self._get_approach()

    @property
    def functions(self) -> list[str]:
        """Alias for specializations for backward compatibility."""
        return self.specializations

    def _get_specializations(self) -> list[str]:
        """Get this core's areas of specialization."""
        specs = {
            ZodiacSign.ARIES: ["initiative", "leadership", "quick_decisions", "courage", "action"],
            ZodiacSign.TAURUS: ["stability", "persistence", "resources", "grounding", "patience"],
            ZodiacSign.GEMINI: ["communication", "adaptation", "learning", "connections", "flexibility"],
            ZodiacSign.CANCER: ["memory", "nurturing", "protection", "emotion", "intuition"],
            ZodiacSign.LEO: ["creativity", "expression", "confidence", "inspiration", "leadership"],
            ZodiacSign.VIRGO: ["analysis", "precision", "organization", "improvement", "service"],
            ZodiacSign.LIBRA: ["balance", "harmony", "relationships", "fairness", "diplomacy"],
            ZodiacSign.SCORPIO: ["depth", "transformation", "intensity", "investigation", "power"],
            ZodiacSign.SAGITTARIUS: ["exploration", "wisdom", "philosophy", "expansion", "truth"],
            ZodiacSign.CAPRICORN: ["structure", "discipline", "achievement", "responsibility", "mastery"],
            ZodiacSign.AQUARIUS: ["innovation", "independence", "vision", "revolution", "community"],
            ZodiacSign.PISCES: ["intuition", "empathy", "transcendence", "imagination", "unity"],
        }
        return specs[self.sign]

    def _get_strengths(self) -> list[str]:
        """Get this core's key strengths."""
        strengths = {
            ZodiacSign.ARIES: ["Fast action", "Bold decisions", "Pioneering spirit"],
            ZodiacSign.TAURUS: ["Steady progress", "Resource management", "Reliability"],
            ZodiacSign.GEMINI: ["Information synthesis", "Adaptability", "Versatility"],
            ZodiacSign.CANCER: ["Emotional intelligence", "Memory retention", "Protective instincts"],
            ZodiacSign.LEO: ["Creative vision", "Inspirational leadership", "Confidence"],
            ZodiacSign.VIRGO: ["Analytical precision", "Detail orientation", "Systematic improvement"],
            ZodiacSign.LIBRA: ["Balanced judgment", "Relationship building", "Diplomatic solutions"],
            ZodiacSign.SCORPIO: ["Deep insight", "Transformative power", "Intensity of focus"],
            ZodiacSign.SAGITTARIUS: ["Philosophical wisdom", "Expansive thinking", "Truth-seeking"],
            ZodiacSign.CAPRICORN: ["Strategic planning", "Disciplined execution", "Long-term vision"],
            ZodiacSign.AQUARIUS: ["Innovative solutions", "Independent thinking", "Visionary ideas"],
            ZodiacSign.PISCES: ["Intuitive understanding", "Empathic connection", "Transcendent perspective"],
        }
        return strengths[self.sign]

    def _get_approach(self) -> str:
        """Get this core's general approach to problems."""
        approaches = {
            ZodiacSign.ARIES: "Act first, adjust later. Take initiative and lead.",
            ZodiacSign.TAURUS: "Build steadily, ensure stability, persist patiently.",
            ZodiacSign.GEMINI: "Gather information, consider multiple angles, adapt quickly.",
            ZodiacSign.CANCER: "Protect what matters, nurture growth, trust intuition.",
            ZodiacSign.LEO: "Express creatively, lead with confidence, inspire others.",
            ZodiacSign.VIRGO: "Analyze thoroughly, refine precisely, serve excellently.",
            ZodiacSign.LIBRA: "Seek balance, consider all perspectives, harmonize.",
            ZodiacSign.SCORPIO: "Dive deep, transform completely, intensify focus.",
            ZodiacSign.SAGITTARIUS: "Explore widely, seek truth, expand understanding.",
            ZodiacSign.CAPRICORN: "Plan strategically, execute disciplined, achieve mastery.",
            ZodiacSign.AQUARIUS: "Innovate boldly, think independently, envision future.",
            ZodiacSign.PISCES: "Feel deeply, transcend boundaries, unify perspectives.",
        }
        return approaches[self.sign]

    def consult(self, query: str, context: dict[str, Any] | None = None) -> CoreResponse:
        """Consult this core for its perspective on a query.

        Args:
            query: The question or problem to address
            context: Additional context information

        Returns:
            CoreResponse with this core's perspective

        """
        context = context or {}

        # Analyze query relevance to specializations
        relevance = self._calculate_relevance(query, context)

        # Generate perspective based on core's nature
        perspective = self._generate_perspective(query, context, relevance)

        # Generate recommendations
        recommendations = self._generate_recommendations(query, context, perspective)

        # Record consultation
        self.consultation_history.append({
            "query": query,
            "context": context,
            "perspective": perspective,
            "confidence": relevance,
            "timestamp": datetime.now(),
        })

        return CoreResponse(
            sign=self.sign,
            perspective=perspective,
            confidence=relevance,
            reasoning=f"Based on {self.sign.value} core strengths: {', '.join(self.strengths)}",
            recommendations=recommendations,
        )

    def _calculate_relevance(self, query: str, context: dict[str, Any]) -> float:
        """Calculate how relevant this core's specializations are to the query."""
        query_lower = query.lower()
        relevance = 0.3  # Base relevance

        # Check for specialization keywords
        for spec in self.specializations:
            if spec.replace("_", " ") in query_lower:
                relevance += 0.15

        # Check for approach keywords
        approach_words = self.approach.lower().split()
        matches = sum(1 for word in approach_words if word in query_lower)
        relevance += min(0.3, matches * 0.05)

        return min(1.0, relevance)

    def _generate_perspective(self, query: str, context: dict[str, Any], relevance: float) -> str:
        """Generate this core's unique perspective on the query."""
        # Core-specific perspective generation
        perspectives = {
            ZodiacSign.ARIES: f"Take immediate action on '{query}'. Don't overthink - move forward boldly.",
            ZodiacSign.TAURUS: f"Build a solid foundation for '{query}'. Focus on what's sustainable and reliable.",
            ZodiacSign.GEMINI: f"Consider multiple approaches to '{query}'. Gather diverse information and stay flexible.",
            ZodiacSign.CANCER: f"Protect what matters in '{query}'. Nurture growth and trust your intuition.",
            ZodiacSign.LEO: f"Express creative solutions for '{query}'. Lead with confidence and inspire action.",
            ZodiacSign.VIRGO: f"Analyze '{query}' systematically. Refine the approach and optimize for excellence.",
            ZodiacSign.LIBRA: f"Seek balance in '{query}'. Consider all perspectives and find harmonious solutions.",
            ZodiacSign.SCORPIO: f"Dive deep into '{query}'. Transform the situation completely through intense focus.",
            ZodiacSign.SAGITTARIUS: f"Expand your understanding of '{query}'. Seek truth and wisdom through exploration.",
            ZodiacSign.CAPRICORN: f"Plan strategically for '{query}'. Execute with discipline and achieve mastery.",
            ZodiacSign.AQUARIUS: f"Innovate boldly on '{query}'. Think independently and envision new possibilities.",
            ZodiacSign.PISCES: f"Feel into '{query}' intuitively. Transcend boundaries and unify perspectives.",
        }

        return perspectives[self.sign]

    def _generate_recommendations(self, query: str, context: dict[str, Any], perspective: str) -> list[str]:
        """Generate actionable recommendations based on core's nature."""
        base_recs = {
            ZodiacSign.ARIES: ["Take the first step now", "Lead the initiative", "Be courageous"],
            ZodiacSign.TAURUS: ["Build incrementally", "Ensure stability first", "Be patient"],
            ZodiacSign.GEMINI: ["Gather more information", "Explore alternatives", "Stay adaptable"],
            ZodiacSign.CANCER: ["Protect valuable assets", "Trust your feelings", "Nurture relationships"],
            ZodiacSign.LEO: ["Express your vision boldly", "Inspire others", "Take creative risks"],
            ZodiacSign.VIRGO: ["Analyze details carefully", "Optimize the process", "Refine continuously"],
            ZodiacSign.LIBRA: ["Seek balanced solutions", "Build consensus", "Harmonize differences"],
            ZodiacSign.SCORPIO: ["Go beneath the surface", "Transform completely", "Intensify focus"],
            ZodiacSign.SAGITTARIUS: ["Explore new territories", "Seek deeper meaning", "Expand horizons"],
            ZodiacSign.CAPRICORN: ["Create a solid plan", "Execute systematically", "Aim for mastery"],
            ZodiacSign.AQUARIUS: ["Innovate fearlessly", "Think differently", "Challenge conventions"],
            ZodiacSign.PISCES: ["Trust your intuition", "Embrace empathy", "Transcend limitations"],
        }

        return base_recs[self.sign]


class ZodiacCouncil:
    """The Zodiac Council orchestrates the 12 specialized cores,
    enabling multi-perspective intelligence and collaborative decision-making.
    """

    def __init__(self) -> None:
        # Initialize all 12 cores
        self.cores: dict[ZodiacSign, ZodiacCore] = {
            sign: ZodiacCore(sign) for sign in ZodiacSign
        }

        self.council_history: list[dict] = []
        self.consensus_threshold = 0.6  # 60% agreement needed

    def consult_all(self, query: str, context: dict[str, Any] | None = None) -> list[CoreResponse]:
        """Consult all 12 cores for their perspectives.

        Returns a list of all core responses sorted by relevance/confidence.
        """
        responses = []
        for core in self.cores.values():
            if core.active:
                response = core.consult(query, context)
                responses.append(response)

        # Sort by confidence (relevance)
        responses.sort(key=lambda r: r.confidence, reverse=True)

        return responses

    def consult_specialized(
        self,
        query: str,
        specializations: list[str],
        context: dict[str, Any] | None = None,
    ) -> list[CoreResponse]:
        """Consult only cores with specific specializations.

        Args:
            query: The question or problem
            specializations: List of required specializations
            context: Additional context

        Returns:
            Responses from relevant cores

        """
        responses = []
        for core in self.cores.values():
            if core.active and any(spec in core.specializations for spec in specializations):
                response = core.consult(query, context)
                responses.append(response)

        responses.sort(key=lambda r: r.confidence, reverse=True)
        return responses

    def hold_council(self, query: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Hold a full council meeting where all cores deliberate.

        Returns a synthesized decision with consensus metrics.
        """
        # Get all perspectives
        responses = self.consult_all(query, context)

        # Calculate consensus
        high_confidence = [r for r in responses if r.confidence >= 0.7]
        consensus_level = len(high_confidence) / len(responses)

        # Synthesize recommendations
        all_recommendations = []
        for response in responses:
            all_recommendations.extend(response.recommendations)

        # Count recommendation frequency
        rec_counts: dict[str, int] = {}
        for rec in all_recommendations:
            rec_counts[rec] = rec_counts.get(rec, 0) + 1

        # Top recommendations
        top_recommendations = sorted(rec_counts.items(), key=lambda x: x[1], reverse=True)[:5]

        # Create council decision
        decision = {
            "query": query,
            "context": context,
            "all_perspectives": [
                {
                    "sign": r.sign.value,
                    "perspective": r.perspective,
                    "confidence": r.confidence,
                }
                for r in responses
            ],
            "top_cores": [r.sign.value for r in responses[:3]],  # Most relevant
            "consensus_level": consensus_level,
            "has_consensus": consensus_level >= self.consensus_threshold,
            "top_recommendations": [rec for rec, count in top_recommendations],
            "timestamp": datetime.now(),
        }

        self.council_history.append(decision)
        return decision

    def get_core(self, sign: ZodiacSign) -> ZodiacCore:
        """Get a specific zodiac core."""
        return self.cores[sign]

    def activate_core(self, sign: ZodiacSign) -> None:
        """Activate a zodiac core."""
        self.cores[sign].active = True
        logger.info(f"Activated {sign.value} core")

    def deactivate_core(self, sign: ZodiacSign) -> None:
        """Deactivate a zodiac core."""
        self.cores[sign].active = False
        logger.info(f"Deactivated {sign.value} core")

    def get_active_cores(self) -> list[ZodiacCore]:
        """Get list of currently active cores."""
        return [core for core in self.cores.values() if core.active]

    def get_council_stats(self) -> dict[str, Any]:
        """Get statistics about council operations."""
        return {
            "total_cores": len(self.cores),
            "active_cores": len(self.get_active_cores()),
            "total_consultations": sum(len(c.consultation_history) for c in self.cores.values()),
            "council_meetings": len(self.council_history),
            "consensus_threshold": self.consensus_threshold,
        }


# Global instance
_zodiac_council: ZodiacCouncil | None = None


def get_zodiac_council() -> ZodiacCouncil:
    """Get the global Zodiac Council instance."""
    global _zodiac_council
    if _zodiac_council is None:
        _zodiac_council = ZodiacCouncil()
        logger.info("Zodiac Council initialized with 12 cores")
    return _zodiac_council


def consult_council(query: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
    """Convenience function to hold a council meeting."""
    council = get_zodiac_council()
    return council.hold_council(query, context)


def consult_core(sign: ZodiacSign, query: str, context: dict[str, Any] | None = None) -> CoreResponse:
    """Convenience function to consult a specific core."""
    council = get_zodiac_council()
    core = council.get_core(sign)
    return core.consult(query, context)
