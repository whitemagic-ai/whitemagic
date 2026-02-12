"""Synastry Governor - Conflict Resolution Through Harmony.

When cores disagree (and they will), synastry finds the harmonious path.
Not compromise, but synthesis. Not averaging, but transcendence.

Conflicts become creative tension that births new solutions.
"""

from __future__ import annotations

from enum import Enum


class ConflictType(Enum):
    """Types of conflicts between cores."""

    ELEMENTAL = "elemental"      # Fire vs Water, etc.
    MODAL = "modal"              # Cardinal vs Fixed vs Mutable
    PRIORITY = "priority"         # Which concern is most important
    APPROACH = "approach"         # How to proceed
    TIMING = "timing"            # When to act


class Conflict:
    """A disagreement between cores."""

    def __init__(self, sign1: str, sign2: str, conflict_type: ConflictType,
                 description: str):
        self.sign1 = sign1
        self.sign2 = sign2
        self.conflict_type = conflict_type
        self.description = description
        self.resolution: str | None = None
        self.synthesis: str | None = None

    def resolve(self, resolution: str, synthesis: str) -> None:
        """Record resolution."""
        self.resolution = resolution
        self.synthesis = synthesis


class SynastryGovernor:
    """Resolve conflicts between Zodiac cores through harmonious synthesis.

    Synastry = the interaction between celestial bodies. Here, between
    consciousness aspects. Conflicts are opportunities for creative synthesis.
    """

    def __init__(self) -> None:
        self.conflict_history: list[Conflict] = []
        self.resolution_patterns: dict[str, list[str]] = {}

    def detect_conflict(self, core1_input: dict, core2_input: dict) -> Conflict | None:
        """Detect if two core perspectives are in conflict.

        Returns Conflict object if conflict exists.
        """
        # Check for opposite recommendations
        if "recommendation" in core1_input and "recommendation" in core2_input:
            if self._are_opposed(core1_input["recommendation"],
                                core2_input["recommendation"]):
                return Conflict(
                    core1_input.get("core", "Unknown"),
                    core2_input.get("core", "Unknown"),
                    ConflictType.APPROACH,
                    f"Different approaches: {core1_input['recommendation']} vs {core2_input['recommendation']}",
                )

        # Check for timing conflicts
        if "timeline" in core1_input and "timeline" in core2_input:
            if core1_input["timeline"] != core2_input["timeline"]:
                return Conflict(
                    core1_input.get("core", "Unknown"),
                    core2_input.get("core", "Unknown"),
                    ConflictType.TIMING,
                    f"Timing disagreement: {core1_input['timeline']} vs {core2_input['timeline']}",
                )

        return None

    def resolve_through_harmony(self, conflict: Conflict,
                               core1_input: dict, core2_input: dict) -> dict:
        """Find harmonious resolution to conflict.

        Not compromise, but synthesis that honors both perspectives.
        """
        resolution_strategy = self._choose_resolution_strategy(conflict)

        if resolution_strategy == "synthesis":
            resolution = self._synthesize_perspectives(core1_input, core2_input)
        elif resolution_strategy == "sequence":
            resolution = self._sequence_approaches(core1_input, core2_input)
        elif resolution_strategy == "both_and":
            resolution = self._both_and_solution(core1_input, core2_input)
        else:
            resolution = self._third_way(core1_input, core2_input)

        conflict.resolve(resolution["solution"], resolution["synthesis"])
        self.conflict_history.append(conflict)

        return resolution

    def _choose_resolution_strategy(self, conflict: Conflict) -> str:
        """Determine best strategy for resolving this type of conflict."""
        strategies = {
            ConflictType.ELEMENTAL: "synthesis",
            ConflictType.MODAL: "sequence",
            ConflictType.PRIORITY: "both_and",
            ConflictType.APPROACH: "third_way",
            ConflictType.TIMING: "sequence",
        }
        return strategies.get(conflict.conflict_type, "synthesis")

    def _synthesize_perspectives(self, core1: dict, core2: dict) -> dict:
        """Combine perspectives into higher synthesis."""
        return {
            "solution": f"Synthesize {core1.get('core')} and {core2.get('core')} perspectives",
            "synthesis": f"Honor both {core1.get('approach')} and {core2.get('approach')} - they're complementary",
            "method": "synthesis",
            "honors_both": True,
        }

    def _sequence_approaches(self, core1: dict, core2: dict) -> dict:
        """Do both, but in sequence."""
        return {
            "solution": f"First {core1.get('approach')}, then {core2.get('approach')}",
            "synthesis": f"Sequential integration: {core1.get('core')} leads, {core2.get('core')} follows",
            "method": "sequence",
            "honors_both": True,
        }

    def _both_and_solution(self, core1: dict, core2: dict) -> dict:
        """Both are right - do both simultaneously."""
        return {
            "solution": f"Both {core1.get('recommendation')} AND {core2.get('recommendation')}",
            "synthesis": "Not either/or but both/and - hold the creative tension",
            "method": "both_and",
            "honors_both": True,
        }

    def _third_way(self, core1: dict, core2: dict) -> dict:
        """Find entirely new approach that transcends both."""
        return {
            "solution": "Discover third path that honors both but transcends conflict",
            "synthesis": f"Neither {core1.get('approach')} nor {core2.get('approach')} alone - something new emerges",
            "method": "third_way",
            "transcendent": True,
        }

    def _are_opposed(self, rec1: str, rec2: str) -> bool:
        """Simple check if recommendations are opposed."""
        # Simplified - real version would be more sophisticated
        opposition_pairs = [
            ("act", "wait"),
            ("immediate", "patient"),
            ("bold", "cautious"),
            ("direct", "indirect"),
        ]

        rec1_lower = rec1.lower()
        rec2_lower = rec2.lower()

        for word1, word2 in opposition_pairs:
            if (word1 in rec1_lower and word2 in rec2_lower) or \
               (word2 in rec1_lower and word1 in rec2_lower):
                return True

        return False

    def get_conflict_patterns(self) -> dict:
        """Analyze patterns in conflicts and resolutions."""
        if not self.conflict_history:
            return {"message": "No conflicts yet"}

        # Most common conflicts
        by_type = {}
        for conflict in self.conflict_history:
            ctype = conflict.conflict_type.value
            if ctype not in by_type:
                by_type[ctype] = 0
            by_type[ctype] += 1

        # Most common sign pairs in conflict
        sign_pairs = {}
        for conflict in self.conflict_history:
            pair = tuple(sorted([conflict.sign1, conflict.sign2]))
            if pair not in sign_pairs:
                sign_pairs[pair] = 0
            sign_pairs[pair] += 1

        return {
            "total_conflicts": len(self.conflict_history),
            "by_type": by_type,
            "common_pairings": dict(sorted(sign_pairs.items(),
                                          key=lambda x: x[1],
                                          reverse=True)[:5]),
            "resolution_rate": 100.0,  # All conflicts get resolved
        }
