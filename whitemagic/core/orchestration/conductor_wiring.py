"""Conductor ↔ Gana Wiring.

Connects the ConductorOrchestrator to all 28 Ganas, enabling:
1. Gana-aware task routing (match tasks to appropriate Ganas)
2. Quadrant-based orchestration (seasonal cycling)
3. Resonance-aware chaining (predecessor → successor flow)
4. Gan Ying event emission for all Gana activations
"""

import logging
from dataclasses import dataclass, field
from typing import Any

from whitemagic.core.ganas.base import (
    BaseGana,
    GanaOperation,
    GanaResult,
    ResonanceHints,
)
from whitemagic.core.ganas.registry import get_all_ganas, get_gana_for_tool
from whitemagic.core.orchestration.conductor import (
    ConductorConfig,
    ConductorOrchestrator,
)
from whitemagic.core.resonance.gan_ying_enhanced import EventType, emit_event

logger = logging.getLogger(__name__)

# === Quadrant Definitions ===
QUADRANTS = {
    "east": ["gana_horn", "gana_neck", "gana_root", "gana_room", "gana_heart", "gana_tail", "gana_winnowing_basket"],
    "south": ["gana_ghost", "gana_willow", "gana_star", "gana_extended_net", "gana_wings", "gana_chariot", "gana_abundance"],
    "west": ["gana_straddling_legs", "gana_mound", "gana_stomach", "gana_hairy_head", "gana_net", "gana_turtle_beak", "gana_three_stars"],
    "north": ["gana_dipper", "gana_ox", "gana_girl", "gana_void", "gana_roof", "gana_encampment", "gana_wall"],
}

# === Task → Gana Routing Heuristics ===
TASK_KEYWORDS = {
    # Eastern (Spring - Initiation)
    "session": "gana_horn",
    "start": "gana_horn",
    "init": "gana_horn",
    "memory": "gana_neck",
    "remember": "gana_neck",
    "health": "gana_root",
    "status": "gana_root",
    "lock": "gana_room",
    "privacy": "gana_room",
    "context": "gana_heart",
    "pulse": "gana_heart",
    "performance": "gana_tail",
    "accelerate": "gana_tail",
    "wisdom": "gana_winnowing_basket",
    "filter": "gana_winnowing_basket",

    # Southern (Summer - Expansion)
    "introspect": "gana_ghost",
    "metric": "gana_ghost",
    "resilience": "gana_willow",
    "ui": "gana_willow",
    "govern": "gana_star",
    "prat": "gana_star",
    "pattern": "gana_extended_net",
    "connect": "gana_extended_net",
    "deploy": "gana_wings",
    "scale": "gana_wings",
    "explore": "gana_chariot",
    "archaeology": "gana_chariot",
    "regenerate": "gana_abundance",
    "surplus": "gana_abundance",

    # Western (Autumn - Harvest)
    "balance": "gana_straddling_legs",
    "equilibrium": "gana_straddling_legs",
    "cache": "gana_mound",
    "accumulate": "gana_mound",
    "resource": "gana_stomach",
    "digest": "gana_stomach",
    "debug": "gana_hairy_head",
    "detail": "gana_hairy_head",
    "capture": "gana_net",
    "search": "gana_net",
    "protect": "gana_turtle_beak",
    "precision": "gana_turtle_beak",
    "judge": "gana_three_stars",
    "rejuvenate": "gana_three_stars",

    # Northern (Winter - Contemplation)
    "strategy": "gana_dipper",
    "governance": "gana_dipper",
    "endure": "gana_ox",
    "watchdog": "gana_ox",
    "nurture": "gana_girl",
    "profile": "gana_girl",
    "defrag": "gana_void",
    "empty": "gana_void",
    "zodiac": "gana_roof",
    "shelter": "gana_roof",
    "handoff": "gana_encampment",
    "transition": "gana_encampment",
    "boundary": "gana_wall",
    "notify": "gana_wall",
}


@dataclass
class WiredConductor:
    """ConductorOrchestrator wired to all 28 Ganas.

    Features:
    - Automatic Gana selection based on task keywords
    - Quadrant-based cycling for comprehensive coverage
    - Resonance passing between Ganas in a chain
    - Full Gan Ying event emission
    """

    conductor: ConductorOrchestrator = field(default_factory=ConductorOrchestrator)
    active_ganas: list[BaseGana] = field(default_factory=get_all_ganas)
    resonance_chain: list[GanaResult] = field(default_factory=list)

    def __post_init__(self) -> None:
        """Emit wiring-complete event."""
        emit_event(
            "conductor_wiring",
            EventType.SYSTEM_STARTED,
            {"component": "WiredConductor", "ganas_loaded": len(self.active_ganas)},
        )
        logger.info(f"WiredConductor initialized with {len(self.active_ganas)} Ganas")

    def route_task(self, task: str) -> BaseGana | None:
        """Route a task to the most appropriate Gana based on keywords.

        Args:
            task: Task description

        Returns:
            Best matching Gana, or None if no match

        """
        task_lower = task.lower()

        # Score each keyword match
        best_match = None
        best_score = 0

        for keyword, gana_name in TASK_KEYWORDS.items():
            if keyword in task_lower:
                score = len(keyword)  # Longer matches = higher priority
                if score > best_score:
                    best_score = score
                    best_match = gana_name

        if best_match:
            gana = get_gana_for_tool(best_match)
            if gana:
                emit_event("conductor_wiring", EventType.PATTERN_DETECTED, {
                    "routing": "task_to_gana",
                    "task": task[:100],
                    "gana": best_match,
                    "score": best_score,
                })
                return gana

        # Default to Horn (initiation) if no match
        return get_gana_for_tool("gana_horn")

    def invoke_gana(
        self,
        gana_name: str,
        operation: GanaOperation,
        **kwargs: Any,
    ) -> GanaResult | None:
        """Invoke a specific Gana with resonance from previous calls.

        Args:
            gana_name: Tool name (e.g., 'gana_heart')
            operation: GanaOperation to perform
            **kwargs: Arguments for the operation

        Returns:
            GanaResult or None

        """
        gana = get_gana_for_tool(gana_name)
        if not gana:
            logger.warning(f"Unknown Gana: {gana_name}")
            return None

        # Build resonance from chain
        hints = ResonanceHints()
        if self.resonance_chain:
            last_result = self.resonance_chain[-1]
            hints.predecessor_output = last_result.output
            hints.successor_context = last_result.successor_hint

        # Execute
        result = gana.dispatch_operation(operation, resonance_hints=hints, **kwargs)

        # Emit event
        emit_event("conductor_wiring", EventType.GARDEN_ACTIVATED, {
            "gana": gana_name,
            "operation": operation.value,
            "success": result is not None,
        })

        # Chain
        if result:
            self.resonance_chain.append(result)  # type: ignore[arg-type]

        return result  # type: ignore[return-value]

    def cycle_quadrant(
        self,
        quadrant: str,
        operation: GanaOperation,
        **kwargs: Any,
    ) -> list[GanaResult]:
        """Cycle through all Ganas in a quadrant.

        Args:
            quadrant: 'east', 'south', 'west', or 'north'
            operation: Operation to perform on each
            **kwargs: Shared arguments

        Returns:
            List of results from each Gana

        """
        gana_names = QUADRANTS.get(quadrant, [])
        results = []

        emit_event("conductor_wiring", EventType.EXPLORATION_STARTED, {
            "cycle_type": "quadrant",
            "quadrant": quadrant,
            "gana_count": len(gana_names),
        })

        for name in gana_names:
            result = self.invoke_gana(name, operation, **kwargs)
            if result:
                results.append(result)

        emit_event("conductor_wiring", EventType.CASCADE_COMPLETED, {
            "cycle_type": "quadrant",
            "quadrant": quadrant,
            "success_count": len(results),
        })

        return results

    def full_cycle(
        self,
        operation: GanaOperation,
        **kwargs: Any,
    ) -> dict[str, list[GanaResult]]:
        """Cycle through all 28 Ganas in quadrant order.

        Returns:
            Dict mapping quadrant name to results

        """
        return {
            quadrant: self.cycle_quadrant(quadrant, operation, **kwargs)
            for quadrant in ["east", "south", "west", "north"]
        }

    def conduct_with_ganas(
        self,
        prompt: str,
        primary_gana: str | None = None,
        max_iterations: int = 30,
    ) -> dict[str, Any]:
        """Conduct autonomous iterations with Gana integration.

        Args:
            prompt: Task prompt
            primary_gana: Optional specific Gana to use
            max_iterations: Max conductor iterations

        Returns:
            Combined results from Conductor and Ganas

        """
        # Route to Gana if not specified
        if primary_gana:
            gana = get_gana_for_tool(primary_gana)
        else:
            gana = self.route_task(prompt)

        # Configure conductor
        config = ConductorConfig(
            max_iterations=max_iterations,
            garden=gana.mansion.quadrant if gana else "practice",  # type: ignore[operator]
        )
        self.conductor = ConductorOrchestrator(config)

        # Conduct
        try:
            result = self.conductor.conduct(prompt)

            # Get final stats
            progress = self.conductor.get_progress_report()

            return {
                "conductor_result": result,
                "progress": progress,
                "primary_gana": primary_gana or (gana.mansion.pinyin if gana else None),
                "resonance_depth": len(self.resonance_chain),
            }
        except Exception as e:
            logger.error(f"Conducted task failed: {e}")
            return {"error": str(e)}

    def clear_resonance(self) -> None:
        """Clear the resonance chain for fresh start."""
        self.resonance_chain = []
        emit_event("conductor_wiring", EventType.MEMORY_UPDATED, {"action": "resonance_cleared"})


# === Convenience Functions ===

def get_wired_conductor() -> WiredConductor:
    """Get a fully wired conductor instance."""
    return WiredConductor()


def conduct_gana_aware(
    prompt: str,
    gana: str | None = None,
    max_iterations: int = 30,
) -> dict[str, Any]:
    """Quick Gana-aware task conduction.

    Args:
        prompt: Task description
        gana: Optional specific Gana
        max_iterations: Max iterations

    Returns:
        Results dict

    """
    wc = get_wired_conductor()
    return wc.conduct_with_ganas(prompt, primary_gana=gana, max_iterations=max_iterations)


def cycle_all_ganas(operation: str = "analyze", **kwargs: Any) -> dict[str, list[GanaResult]]:
    """Cycle all 28 Ganas with specified operation.

    Args:
        operation: 'search', 'analyze', 'transform', or 'consolidate'
        **kwargs: Operation arguments

    Returns:
        Results by quadrant

    """
    wc = get_wired_conductor()
    op = GanaOperation(operation) if isinstance(operation, str) else operation
    return wc.full_cycle(op, **kwargs)
