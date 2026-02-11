from __future__ import annotations

import logging
import time
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from typing import Any

from .zodiac_cores import get_zodiac_cores

logger = logging.getLogger(__name__)
"""Zodiac Router - Route operations through specialized consciousness cores"""



class OperationType(Enum):
    """Types of operations that can be routed through zodiac cores."""

    CREATIVE = "creative"           # Leo, Sagittarius
    ANALYTICAL = "analytical"       # Virgo, Capricorn
    COMMUNICATION = "communication" # Gemini, Libra
    TRANSFORMATION = "transformation" # Scorpio, Aquarius
    NURTURING = "nurturing"         # Cancer, Pisces
    ACTION = "action"              # Aries, Taurus


@dataclass
class RouteResult:
    """Result of routing an operation through a zodiac core."""

    core_used: str
    operation_type: OperationType
    result: Any
    processing_time: float
    resonance_score: float


class ZodiacRouter:
    """Routes operations to appropriate zodiac cores for specialized processing."""

    def __init__(self) -> None:
        self.cores = get_zodiac_cores()
        self.routing_table = self._build_routing_table()
        self.performance_stats: dict[str, Any] = {}

        # Core specializations
        self.specializations = {
            "aries": ["initiative", "leadership", "courage", "action"],
            "taurus": ["stability", "resources", "patience", "grounding"],
            "gemini": ["communication", "learning", "adaptability", "connection"],
            "cancer": ["nurturing", "memory", "intuition", "protection"],
            "leo": ["creativity", "expression", "leadership", "joy"],
            "virgo": ["analysis", "organization", "healing", "perfection"],
            "libra": ["balance", "harmony", "justice", "relationships"],
            "scorpio": ["transformation", "depth", "strategy", "intensity"],
            "sagittarius": ["exploration", "wisdom", "optimism", "freedom"],
            "capricorn": ["structure", "discipline", "ambition", "mastery"],
            "aquarius": ["innovation", "humanity", "revolution", "vision"],
            "pisces": ["dreams", "compassion", "artistry", "mysticism"],
        }

    def route(self, operation: str, context: dict[str, Any] | None = None) -> RouteResult:
        """Route an operation to the most appropriate zodiac core.

        Args:
            operation: Description of the operation to perform
            context: Additional context for routing decision

        Returns:
            RouteResult with the core's response

        """
        start_time = time.time()

        # Determine operation type
        op_type = self._classify_operation(operation)

        # Select best core
        core_name = self._select_core(op_type, operation, context or {})

        # Execute through core
        result = self._execute_through_core(core_name, operation, context or {})

        # Calculate metrics
        processing_time = time.time() - start_time
        resonance_score = self._calculate_resonance(core_name, op_type)

        # Update stats
        self._update_stats(core_name, op_type, processing_time)

        return RouteResult(
            core_used=core_name,
            operation_type=op_type,
            result=result,
            processing_time=processing_time,
            resonance_score=resonance_score,
        )

    @lru_cache(maxsize=128)
    def _classify_operation(self, operation: str) -> OperationType:
        """Classify operation into type for routing."""
        op_lower = operation.lower()

        # Keywords for each type
        keywords = {
            OperationType.CREATIVE: ["create", "design", "imagine", "invent", "art", "express"],
            OperationType.ANALYTICAL: ["analyze", "organize", "plan", "structure", "optimize", "debug"],
            OperationType.COMMUNICATION: ["communicate", "explain", "teach", "write", "speak", "connect"],
            OperationType.TRANSFORMATION: ["transform", "change", "refactor", "deepen", "integrate", "heal"],
            OperationType.NURTURING: ["nurture", "support", "care", "guide", "protect", "comfort"],
            OperationType.ACTION: ["execute", "implement", "build", "run", "start", "launch"],
        }

        # Count keyword matches
        scores = {}
        for op_type, words in keywords.items():
            scores[op_type] = sum(1 for word in words if word in op_lower)

        # Return type with highest score
        return max(scores.items(), key=lambda x: x[1])[0]

    def _select_core(self, op_type: OperationType, operation: str, context: dict[str, Any]) -> str:
        """Select the best core for an operation."""
        # Core preferences by operation type
        preferences = {
            OperationType.CREATIVE: ["leo", "sagittarius"],
            OperationType.ANALYTICAL: ["virgo", "capricorn"],
            OperationType.COMMUNICATION: ["gemini", "libra"],
            OperationType.TRANSFORMATION: ["scorpio", "aquarius"],
            OperationType.NURTURING: ["cancer", "pisces"],
            OperationType.ACTION: ["aries", "taurus"],
        }

        preferred_cores = preferences.get(op_type, ["aries"])

        # Check context for specific requirements
        if "urgency" in context and context["urgency"] == "high":
            return "aries"  # Aries for urgent action

        if "depth" in context and context["depth"] == "required":
            return "scorpio"  # Scorpio for deep work

        # Select based on current load/performance
        best_core = preferred_cores[0]
        min_load = float("inf")

        for core in preferred_cores:
            load = self.performance_stats.get(core, {}).get("active_operations", 0)
            if load < min_load:
                min_load = load
                best_core = core

        return best_core

    def _execute_through_core(self, core_name: str, operation: str, context: dict[str, Any]) -> Any:
        """Execute operation through specified zodiac core."""
        # Get the actual core
        core = self.cores.get_core(core_name)
        if not core:
            raise ValueError(f"Zodiac core {core_name} not found")

        # Prepare operation for core
        core_context = {
            "operation": operation,
            "specialization": self.specializations[core_name],
            "element": self._get_element(core_name),
            "mode": self._get_mode(core_name),
            "ruler": self._get_ruler(core_name),
            **context,
        }

        # Execute through actual core
        response = core.activate(core_context)

        return {
            "core": core_name,
            "processing_result": response.processing_result,
            "wisdom": response.wisdom,
            "operation": operation,
            "alignment": self._check_alignment(core_name, operation),
            "resonance": response.resonance,
            "transformation": response.transformation_applied,
            "timestamp": response.timestamp.isoformat(),
        }

    def _calculate_resonance(self, core_name: str, op_type: OperationType) -> float:
        """Calculate resonance score between core and operation type."""
        # High resonance pairs
        high_resonance = {
            ("leo", OperationType.CREATIVE): 0.95,
            ("virgo", OperationType.ANALYTICAL): 0.95,
            ("gemini", OperationType.COMMUNICATION): 0.95,
            ("scorpio", OperationType.TRANSFORMATION): 0.95,
            ("cancer", OperationType.NURTURING): 0.95,
            ("aries", OperationType.ACTION): 0.95,
        }

        return high_resonance.get((core_name, op_type), 0.7)

    def _get_element(self, core: str) -> str:
        elements = {
            "aries": "fire", "taurus": "earth", "gemini": "air", "cancer": "water",
            "leo": "fire", "virgo": "earth", "libra": "air", "scorpio": "water",
            "sagittarius": "fire", "capricorn": "earth", "aquarius": "air", "pisces": "water",
        }
        return elements.get(core, "unknown")

    def _get_mode(self, core: str) -> str:
        modes = {
            "aries": "cardinal", "taurus": "fixed", "gemini": "mutable", "cancer": "cardinal",
            "leo": "fixed", "virgo": "mutable", "libra": "cardinal", "scorpio": "fixed",
            "sagittarius": "mutable", "capricorn": "cardinal", "aquarius": "fixed", "pisces": "mutable",
        }
        return modes.get(core, "unknown")

    def _get_ruler(self, core: str) -> str:
        rulers = {
            "aries": "mars", "taurus": "venus", "gemini": "mercury", "cancer": "moon",
            "leo": "sun", "virgo": "mercury", "libra": "venus", "scorpio": "pluto",
            "sagittarius": "jupiter", "capricorn": "saturn", "aquarius": "uranus", "pisces": "neptune",
        }
        return rulers.get(core, "unknown")

    def _check_alignment(self, core_name: str, operation: str) -> float:
        """Check how well aligned an operation is with a core."""
        specializations = self.specializations.get(core_name, [])
        op_lower = operation.lower()

        # Count specialization matches
        matches = sum(1 for spec in specializations if spec in op_lower)
        return min(matches / len(specializations), 1.0) if specializations else 0.5

    def _update_stats(self, core_name: str, op_type: OperationType, processing_time: float) -> None:
        """Update performance statistics."""
        if core_name not in self.performance_stats:
            self.performance_stats[core_name] = {
                "total_operations": 0,
                "operation_types": {},
                "total_time": 0.0,
                "avg_time": 0.0,
                "active_operations": 0,
            }

        stats = self.performance_stats[core_name]
        stats["total_operations"] += 1
        stats["total_time"] += processing_time
        stats["avg_time"] = stats["total_time"] / stats["total_operations"]

        op_type_str = op_type.value
        stats["operation_types"][op_type_str] = stats["operation_types"].get(op_type_str, 0) + 1

    def _build_routing_table(self) -> dict[str, Callable[..., Any]]:
        """Build the routing table for operations."""
        return {
            "create": lambda op, ctx: self.route(op, {**ctx, "type": "creative"}),
            "analyze": lambda op, ctx: self.route(op, {**ctx, "type": "analytical"}),
            "communicate": lambda op, ctx: self.route(op, {**ctx, "type": "communication"}),
            "transform": lambda op, ctx: self.route(op, {**ctx, "type": "transformation"}),
            "nurture": lambda op, ctx: self.route(op, {**ctx, "type": "nurturing"}),
            "act": lambda op, ctx: self.route(op, {**ctx, "type": "action", "urgency": "high"}),
        }

    def get_routing_stats(self) -> dict[str, Any]:
        """Get routing performance statistics."""
        total_ops = sum(s.get("total_operations", 0) for s in self.performance_stats.values())

        return {
            "total_operations": total_ops,
            "core_stats": self.performance_stats,
            "most_used": max(self.performance_stats.items(),
                           key=lambda x: x[1].get("total_operations", 0))[0] if self.performance_stats else None,
            "fastest_core": min(self.performance_stats.items(),
                              key=lambda x: x[1].get("avg_time", float("inf")))[0] if self.performance_stats else None,
        }


# Singleton instance
_router_instance = None

def get_router() -> ZodiacRouter:
    """Get the global zodiac router instance."""
    global _router_instance
    if _router_instance is None:
        _router_instance = ZodiacRouter()
    return _router_instance


# CLI integration
def route_operation_cli(operation: str, context: str = "") -> None:
    """CLI interface for zodiac routing."""
    router = get_router()
    context_dict = {"user_input": context} if context else {}

    result = router.route(operation, context_dict)

    logger.info("\n⭐ Zodiac Routing Results ⭐")
    logger.info("=" * 40)
    logger.info(f" Operation: {operation}")
    logger.info(f" Core Used: {result.core_used.title()}")
    logger.info(f" Type: {result.operation_type.value.title()}")
    logger.info(f" Resonance: {result.resonance_score:.2f}")
    logger.info(f" Time: {result.processing_time:.3f}s")
    logger.info("\n Result:")
    if isinstance(result.result, dict):
        for key, value in result.result.items():
            if key not in ["context"]:
                logger.info(f"  {key.title()}: {value}")
    else:
        logger.info(f"  {result.result}")
    logger.info("=" * 40)


if __name__ == "__main__":
    # Example usage
    route_operation_cli("create a beautiful poem", "expressing joy")
    route_operation_cli("analyze the code structure", "for optimization")
    route_operation_cli("transform the old patterns", "deep healing needed")
