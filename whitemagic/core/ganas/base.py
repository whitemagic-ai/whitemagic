"""Base Gana - Polymorphic, Resonant MCP Tool.

Each Gana represents a lunar mansion and provides specialized functionality
that morphs based on resonance from predecessor and successor Ganas.

Supports 4 polymorphic operations:
- SEARCH: Find and retrieve information
- ANALYZE: Examine and understand patterns
- TRANSFORM: Modify and evolve data
- CONSOLIDATE: Merge and unify information
"""

import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class LunarMansion(Enum):
    """28 Lunar Mansions (Chinese Xiu)."""

    # Eastern Quadrant - Azure Dragon (Spring/Initiative)
    HORN = (1, "角", "Jiao", "Sharp initiation")
    NECK = (2, "亢", "Kang", "Stability")
    ROOT = (3, "氐", "Di", "Foundation")
    ROOM = (4, "房", "Fang", "Enclosure")
    HEART = (5, "心", "Xin", "Vital pulse")
    TAIL = (6, "尾", "Wei", "Passionate drive")
    WINNOWING_BASKET = (7, "箕", "Ji", "Separation")

    # Southern Quadrant - Vermilion Bird (Summer/Radiance)
    GHOST = (8, "鬼", "Gui", "Introspection")
    WILLOW = (9, "柳", "Liu", "Flexibility")
    STAR = (10, "星", "Xing", "Illumination")
    EXTENDED_NET = (11, "张", "Zhang", "Connectivity")
    WINGS = (12, "翼", "Yi", "Expansion")
    CHARIOT = (13, "轸", "Zhen", "Movement")
    ABUNDANCE = (14, "豐", "Fēng", "Surplus")

    # Western Quadrant - White Tiger (Autumn/Harvest)
    STRADDLING_LEGS = (15, "奎", "Kui", "Balance")
    MOUND = (16, "娄", "Lou", "Accumulation")
    STOMACH = (17, "胃", "Wei", "Nourishment")
    HAIRY_HEAD = (18, "昴", "Mao", "Detail")
    NET = (19, "毕", "Bi", "Capture")
    TURTLE_BEAK = (20, "觜", "Zui", "Precision")
    THREE_STARS = (21, "参", "Shen", "Judgment")

    # Northern Quadrant - Black Tortoise (Winter/Storage)
    DIPPER = (22, "斗", "Dou", "Governance")
    OX = (23, "牛", "Niu", "Endurance")
    GIRL = (24, "女", "Nü", "Nurture")
    VOID = (25, "虚", "Xu", "Emptiness")
    ROOF = (26, "危", "Wei", "Shelter")
    ENCAMPMENT = (27, "室", "Shi", "Structure")
    WALL = (28, "壁", "Bi", "Boundaries")

    def __init__(self, number: int, chinese: str, pinyin: str, meaning: str):
        self.number = number
        self.chinese = chinese
        self.pinyin = pinyin
        self.meaning = meaning

    @property
    def quadrant(self) -> str:
        """Get quadrant (East/South/West/North)."""
        if 1 <= self.number <= 7:
            return "East"
        elif 8 <= self.number <= 14:
            return "South"
        elif 15 <= self.number <= 21:
            return "West"
        else:
            return "North"

    def predecessor(self) -> "LunarMansion":
        """Get previous mansion in circular sequence."""
        prev_num = self.number - 1 if self.number > 1 else 28
        return [m for m in LunarMansion if m.number == prev_num][0]

    def successor(self) -> "LunarMansion":
        """Get next mansion in circular sequence."""
        next_num = self.number + 1 if self.number < 28 else 1
        return [m for m in LunarMansion if m.number == next_num][0]


class GanaOperation(Enum):
    """Four polymorphic operation types that each Gana can perform.

    Each Gana interprets these operations through its unique lens:
    - Horn (Initiation) SEARCH → Bootstrap discovery
    - Heart (Pulse) ANALYZE → Monitor vitals
    - Net (Capture) CONSOLIDATE → Pattern extraction
    """

    SEARCH = "search"           # Find and retrieve information
    ANALYZE = "analyze"         # Examine and understand patterns
    TRANSFORM = "transform"     # Modify and evolve data
    CONSOLIDATE = "consolidate" # Merge and unify information


@dataclass
class ResonanceHints:
    """Context injection for polymorphic behavior."""

    predecessor_output: Any | None = None
    successor_context: str | None = None
    lunar_phase: float = 0.0  # 0.0-1.0
    harmony_score: float = 0.5
    guna_tag: str = "Rajas"
    timestamp: float = field(default_factory=time.time)


@dataclass
class GanaCall:
    """Single Gana invocation with resonance."""

    task: str
    state_vector: dict[str, Any] = field(default_factory=dict)
    resonance_hints: ResonanceHints = field(default_factory=ResonanceHints)

    def with_resonance(self, hints: ResonanceHints) -> "GanaCall":
        """Create new call with updated resonance."""
        return GanaCall(
            task=self.task,
            state_vector=self.state_vector,
            resonance_hints=hints,
        )


@dataclass
class GanaResult:
    """Result from Gana invocation."""

    output: Any
    mansion: "LunarMansion"
    garden: str | None = None
    successor_hint: str | None = None
    karma_trace: dict[str, Any] = field(default_factory=dict)
    execution_time_ms: float = 0.0

    def __post_init__(self) -> None:
        """Auto-populate karma trace."""
        if not self.karma_trace:
            self.karma_trace = {
                "timestamp": time.time(),
                "success": True,
            }


class BaseGana(ABC):
    """Base class for polymorphic, resonant Ganas.

    Each Gana is a specialized tool that:
    1. Has a base behavior (template/prompt)
    2. Morphs based on predecessor output (resonance)
    3. Seeds context for successor (preparation)
    4. Responds to lunar phase (amplification)
    5. Adapts to Guna state (priority adjustment)
    """

    def __init__(self, mansion: LunarMansion, garden: str | None = None) -> None:
        self.mansion = mansion
        self.garden = garden
        # In a real implementation, we would look up the essence from a registry
        # For now, we'll just store the name
        self.garden_essence = f"Essence of {garden}" if garden else ""
        self.base_template = self._get_base_template()
        self.invocation_count = 0
        self.total_time_ms = 0.0

    @abstractmethod
    def _get_base_template(self) -> str:
        """Return base prompt/template for this Gana."""
        pass

    @abstractmethod
    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute the actual Gana logic with morphed template."""
        pass

    # Polymorphic Operation Methods (Override for Gana-specific behavior)

    async def search(self, query: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """SEARCH operation - Find and retrieve information.
        Default: Basic search through this Gana's lens.
        Override for mansion-specific search behavior.
        """
        return {
            "operation": "search",
            "mansion": self.mansion.name,
            "query": query,
            "results": [],
            "note": f"{self.mansion.meaning} perspective on search",
        }

    async def analyze(self, data: Any, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """ANALYZE operation - Examine and understand patterns.
        Default: Basic analysis through this Gana's lens.
        Override for mansion-specific analysis.
        """
        return {
            "operation": "analyze",
            "mansion": self.mansion.name,
            "data_type": type(data).__name__,
            "insights": [],
            "note": f"{self.mansion.meaning} perspective on analysis",
        }

    async def transform(self, data: Any, transformation: str, context: dict[str, Any] | None = None) -> Any:
        """TRANSFORM operation - Modify and evolve data.
        Default: Pass-through with metadata.
        Override for mansion-specific transformations.
        """
        return {
            "operation": "transform",
            "mansion": self.mansion.name,
            "original": data,
            "transformed": data,
            "transformation_applied": transformation,
            "note": f"{self.mansion.meaning} transformation",
        }

    async def consolidate(self, items: list[Any], context: dict[str, Any] | None = None) -> Any:
        """CONSOLIDATE operation - Merge and unify information.
        Default: Basic merging strategy.
        Override for mansion-specific consolidation.
        """
        return {
            "operation": "consolidate",
            "mansion": self.mansion.name,
            "item_count": len(items),
            "consolidated": items,
            "note": f"{self.mansion.meaning} consolidation",
        }

    async def dispatch_operation(
        self,
        operation: GanaOperation,
        **kwargs: Any,
    ) -> Any:
        """Dispatch to the appropriate operation method.
        This is the main entry point for polymorphic operations.
        """
        # Explicit mapping to satisfy Mypy
        if operation == GanaOperation.SEARCH:
            return await self.search(**kwargs)
        elif operation == GanaOperation.ANALYZE:
            return await self.analyze(**kwargs)
        elif operation == GanaOperation.TRANSFORM:
            return await self.transform(**kwargs)
        elif operation == GanaOperation.CONSOLIDATE:
            return await self.consolidate(**kwargs)

        raise ValueError(f"Unknown operation: {operation}")

    async def invoke(self, call: GanaCall) -> GanaResult:
        """Main invocation with full resonance protocol.

        Flow:
        1. Start with base template
        2. Blend predecessor influence
        3. Amplify if lunar alignment
        4. Adjust for Guna state
        5. Execute core logic
        6. Prepare successor hint
        7. Log karma trace
        """
        start_time = time.time()

        # Start with base
        template = self.base_template

        # Predecessor resonance
        if call.resonance_hints.predecessor_output:
            template = self._blend_predecessor(
                template,
                call.resonance_hints.predecessor_output,
            )

        # Lunar amplification
        template = self._amplify_lunar(
            template,
            call.resonance_hints.lunar_phase,
        )

        # Guna adaptation
        if call.resonance_hints.guna_tag == "Tamas":
            # System stressed, minimal mode
            template = self._minimal_mode(template)
        elif call.resonance_hints.guna_tag == "Sattva":
            # System optimal, full capabilities
            template = self._optimal_mode(template)

        # Execute
        output = await self._execute_core(call, template)

        # Prepare successor
        successor_hint = self._prepare_successor_context(output)

        # Timing
        execution_time = (time.time() - start_time) * 1000
        self.invocation_count += 1
        self.total_time_ms += execution_time

        return GanaResult(
            output=output,
            mansion=self.mansion,
            garden=self.garden,
            successor_hint=successor_hint,
            execution_time_ms=execution_time,
            karma_trace={
                "mansion": self.mansion.name,
                "timestamp": start_time,
                "guna": call.resonance_hints.guna_tag,
                "harmony": call.resonance_hints.harmony_score,
                "lunar_phase": call.resonance_hints.lunar_phase,
            },
        )

    def _blend_predecessor(self, template: str, predecessor_data: Any) -> str:
        """Morph template based on predecessor output.
        Default: inject as context at end.
        Override for custom blending.
        """
        predecessor_summary = str(predecessor_data)[:200]  # Truncate
        return f"{template}\n\nContext from predecessor: {predecessor_summary}"

    def _amplify_lunar(self, template: str, lunar_phase: float) -> str:
        """Boost behavior when Moon is in this mansion.
        Also injects Garden essence if present.
        """
        # Get current lunar mansion (simplified - just use phase)
        current_mansion_num = int(lunar_phase * 28) + 1

        amplified = template
        if current_mansion_num == self.mansion.number:
            amplified = f"[LUNAR AMPLIFICATION: {self.mansion.chinese}]\n{template}"

        if self.garden_essence:
            amplified = f"{amplified}\n\n[GARDEN RESONANCE: {self.garden}]\n{self.garden_essence}"

        return amplified

    def _minimal_mode(self, template: str) -> str:
        """Reduce to essentials for Tamas (system stressed)."""
        return f"[MINIMAL MODE]\n{template[:200]}..."

    def _optimal_mode(self, template: str) -> str:
        """Full capabilities for Sattva (system harmonious)."""
        return f"[OPTIMAL MODE: {self.mansion.meaning}]\n{template}"

    def _prepare_successor_context(self, output: Any) -> str:
        """Seed hints for next Gana in chain.
        Default: simple summary.
        Override for custom preparation.
        """
        successor = self.mansion.successor()
        return f"Prepared for {successor.pinyin} ({successor.meaning}): {str(output)[:100]}"

    @property
    def stats(self) -> dict[str, Any]:
        """Get performance statistics."""
        avg_time = self.total_time_ms / self.invocation_count if self.invocation_count > 0 else 0
        return {
            "mansion": self.mansion.name,
            "invocations": self.invocation_count,
            "total_time_ms": self.total_time_ms,
            "avg_time_ms": avg_time,
        }
