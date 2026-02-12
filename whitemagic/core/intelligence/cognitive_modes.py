"""Cognitive Modes — Switchable Agent Behavior Profiles via Guna Classification.
===============================================================================
Wires the existing Guna classifier (sattvic/rajasic/tamasic) from the
Harmony Vector into switchable behavior profiles that modulate how the
agent operates:

  - **EXPLORER** (Sattvic-dominant): Curiosity-driven, prefers novel
    searches, graph walks, serendipity. Broadens context windows.
    Best for research, discovery, creative tasks.

  - **EXECUTOR** (Rajasic-dominant): Action-oriented, prefers tool
    execution, pipeline creation, task completion. Narrows focus.
    Best for structured work, code generation, task lists.

  - **REFLECTOR** (Tamasic-dominant): Contemplative, prefers
    introspection, consolidation, dream cycles. Slows down.
    Best for review, synthesis, memory organization.

  - **BALANCED** (No dominant guna): Adaptive, lets the Harmony
    Vector and Drive Core naturally guide behavior.

  - **GUARDIAN** (Override): Activated by Hermit Crab's guarded
    state. Restricts to safe/read-only operations.

Modes can be:
  1. Auto-detected from Harmony Vector guna percentages
  2. Manually set by the user/agent
  3. Overridden by safety systems (Hermit Crab → GUARDIAN)

The active mode influences:
  - Tool selection hints (which tools are preferred)
  - Context window sizing
  - Dream cycle phase ordering
  - Resonance chain weighting

Gana: gana_dipper (Strategy)

Usage:
    from whitemagic.core.intelligence.cognitive_modes import get_cognitive_modes
    cm = get_cognitive_modes()
    cm.set_mode("explorer")
    mode = cm.current_mode()
    hints = cm.get_tool_hints()
"""

from __future__ import annotations

import logging
import threading
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class CognitiveMode(Enum):
    """Available cognitive modes."""

    EXPLORER = "explorer"     # Sattvic: curiosity, discovery
    EXECUTOR = "executor"     # Rajasic: action, completion
    REFLECTOR = "reflector"   # Tamasic: contemplation, synthesis
    BALANCED = "balanced"     # Adaptive, no dominant guna
    GUARDIAN = "guardian"      # Safety override


@dataclass
class ModeProfile:
    """Configuration for a cognitive mode."""

    mode: CognitiveMode
    preferred_tools: list[str]
    avoided_tools: list[str]
    context_window_multiplier: float  # 1.0 = normal, >1 = broader, <1 = narrower
    dream_phase_priority: list[str]   # preferred dream phase order
    description: str

    def to_dict(self) -> dict[str, Any]:
        return {
            "mode": self.mode.value,
            "preferred_tools": self.preferred_tools,
            "avoided_tools": self.avoided_tools,
            "context_window_multiplier": self.context_window_multiplier,
            "dream_phase_priority": self.dream_phase_priority,
            "description": self.description,
        }


# Mode profiles
MODE_PROFILES: dict[CognitiveMode, ModeProfile] = {
    CognitiveMode.EXPLORER: ModeProfile(
        mode=CognitiveMode.EXPLORER,
        preferred_tools=[
            "search_memories", "hybrid_recall", "graph_walk",
            "serendipity_surface", "pattern_search", "research_topic",
            "archaeology", "kg.query", "web_search_and_read",
        ],
        avoided_tools=["delete_memory"],
        context_window_multiplier=1.5,
        dream_phase_priority=["serendipity", "consolidation", "kaizen", "oracle", "governance", "decay"],
        description="Curiosity-driven exploration. Broadens searches, favors discovery.",
    ),
    CognitiveMode.EXECUTOR: ModeProfile(
        mode=CognitiveMode.EXECUTOR,
        preferred_tools=[
            "create_memory", "pipeline.create", "task.distribute",
            "swarm.decompose", "execute_cascade", "kaizen_apply_fixes",
            "ollama.agent",
        ],
        avoided_tools=[],
        context_window_multiplier=0.8,
        dream_phase_priority=["kaizen", "consolidation", "oracle", "governance", "serendipity", "decay"],
        description="Action-oriented execution. Focuses on task completion.",
    ),
    CognitiveMode.REFLECTOR: ModeProfile(
        mode=CognitiveMode.REFLECTOR,
        preferred_tools=[
            "gnosis", "reasoning.bicameral", "memory.consolidate",
            "graph_topology", "selfmodel.forecast", "harmony_vector",
            "salience.spotlight",
        ],
        avoided_tools=["swarm.decompose", "execute_cascade"],
        context_window_multiplier=1.2,
        dream_phase_priority=["governance", "consolidation", "decay", "serendipity", "kaizen", "oracle"],
        description="Contemplative reflection. Favors introspection and synthesis.",
    ),
    CognitiveMode.BALANCED: ModeProfile(
        mode=CognitiveMode.BALANCED,
        preferred_tools=[],
        avoided_tools=[],
        context_window_multiplier=1.0,
        dream_phase_priority=["consolidation", "serendipity", "governance", "kaizen", "oracle", "decay"],
        description="Adaptive balance. Lets natural signals guide behavior.",
    ),
    CognitiveMode.GUARDIAN: ModeProfile(
        mode=CognitiveMode.GUARDIAN,
        preferred_tools=[
            "gnosis", "health_report", "harmony_vector",
            "search_memories", "read_memory", "capabilities",
        ],
        avoided_tools=[
            "delete_memory", "execute_cascade", "swarm.decompose",
            "pipeline.create", "sandbox.set_limits", "ollama.agent",
        ],
        context_window_multiplier=0.5,
        dream_phase_priority=["governance", "decay", "consolidation", "oracle", "kaizen", "serendipity"],
        description="Safety mode. Restricts to read-only and introspection tools.",
    ),
}


class CognitiveModes:
    """Manages switchable cognitive behavior profiles.

    Integrates with the Harmony Vector's guna classification to
    auto-detect the appropriate mode, or allows manual override.
    """

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._mode = CognitiveMode.BALANCED
        self._manual_override: CognitiveMode | None = None
        self._safety_override: CognitiveMode | None = None
        self._mode_history: list[dict[str, Any]] = []
        self._auto_detect_enabled = True

    # ------------------------------------------------------------------
    # Mode management
    # ------------------------------------------------------------------

    def current_mode(self) -> dict[str, Any]:
        """Get the current cognitive mode and its profile."""
        mode = self._effective_mode()
        profile = MODE_PROFILES.get(mode, MODE_PROFILES[CognitiveMode.BALANCED])

        result = profile.to_dict()
        result["is_manual_override"] = self._manual_override is not None
        result["is_safety_override"] = self._safety_override is not None
        result["auto_detect_enabled"] = self._auto_detect_enabled
        return result

    def set_mode(self, mode_name: str) -> dict[str, Any]:
        """Manually set the cognitive mode.

        Args:
            mode_name: One of "explorer", "executor", "reflector", "balanced", "auto".
                       "auto" clears manual override and re-enables auto-detection.
        """
        if mode_name == "auto":
            with self._lock:
                self._manual_override = None
                self._auto_detect_enabled = True
            self._record_transition("auto", "Manual override cleared")
            return {"status": "ok", "mode": self._effective_mode().value, "auto_detect": True}

        try:
            mode = CognitiveMode(mode_name.lower())
        except ValueError:
            valid = [m.value for m in CognitiveMode if m != CognitiveMode.GUARDIAN]
            return {
                "status": "error",
                "message": f"Unknown mode '{mode_name}'. Valid: {valid + ['auto']}",
            }

        if mode == CognitiveMode.GUARDIAN:
            return {
                "status": "error",
                "message": "GUARDIAN mode can only be activated by safety systems.",
            }

        with self._lock:
            self._manual_override = mode
            self._auto_detect_enabled = False

        self._record_transition(mode.value, f"Manual set to {mode.value}")
        return {"status": "ok", "mode": mode.value, "auto_detect": False}

    def activate_guardian(self, reason: str = "hermit_crab") -> dict[str, Any]:
        """Activate GUARDIAN mode (safety override — takes precedence)."""
        with self._lock:
            self._safety_override = CognitiveMode.GUARDIAN

        self._record_transition("guardian", f"Safety override: {reason}")
        logger.warning("🛡️ Cognitive mode: GUARDIAN activated — %s", reason)
        return {"status": "ok", "mode": "guardian", "reason": reason}

    def deactivate_guardian(self) -> dict[str, Any]:
        """Deactivate GUARDIAN safety override."""
        with self._lock:
            self._safety_override = None

        self._record_transition(
            self._effective_mode().value, "Guardian deactivated",
        )
        return {"status": "ok", "mode": self._effective_mode().value}

    # ------------------------------------------------------------------
    # Auto-detection from Harmony Vector
    # ------------------------------------------------------------------

    def auto_detect(self) -> CognitiveMode:
        """Auto-detect mode from Harmony Vector guna percentages."""
        try:
            from whitemagic.harmony.vector import get_harmony_vector
            hv = get_harmony_vector()
            snap = hv.snapshot()

            sattvic = snap.guna_sattvic_pct
            rajasic = snap.guna_rajasic_pct
            tamasic = snap.guna_tamasic_pct

            # Determine dominant guna (>40% threshold)
            threshold = 0.4
            if sattvic > threshold and sattvic > rajasic and sattvic > tamasic:
                return CognitiveMode.EXPLORER
            elif rajasic > threshold and rajasic > sattvic and rajasic > tamasic:
                return CognitiveMode.EXECUTOR
            elif tamasic > threshold and tamasic > sattvic and tamasic > rajasic:
                return CognitiveMode.REFLECTOR
            else:
                return CognitiveMode.BALANCED

        except Exception:
            return CognitiveMode.BALANCED

    def _effective_mode(self) -> CognitiveMode:
        """Get the effective mode considering all overrides."""
        # Safety override takes precedence
        if self._safety_override is not None:
            return self._safety_override

        # Manual override next
        if self._manual_override is not None:
            return self._manual_override

        # Auto-detect if enabled
        if self._auto_detect_enabled:
            detected = self.auto_detect()
            with self._lock:
                self._mode = detected
            return detected

        return self._mode

    # ------------------------------------------------------------------
    # Tool hints
    # ------------------------------------------------------------------

    def get_tool_hints(self) -> dict[str, Any]:
        """Get tool selection hints for the current mode.

        Returns preferred/avoided tools and context sizing hints.
        """
        mode = self._effective_mode()
        profile = MODE_PROFILES.get(mode, MODE_PROFILES[CognitiveMode.BALANCED])

        return {
            "mode": mode.value,
            "preferred_tools": profile.preferred_tools,
            "avoided_tools": profile.avoided_tools,
            "context_multiplier": profile.context_window_multiplier,
            "dream_phase_priority": profile.dream_phase_priority,
        }

    def is_tool_preferred(self, tool_name: str) -> bool:
        """Check if a tool is preferred in the current mode."""
        mode = self._effective_mode()
        profile = MODE_PROFILES.get(mode, MODE_PROFILES[CognitiveMode.BALANCED])
        return tool_name in profile.preferred_tools

    def is_tool_avoided(self, tool_name: str) -> bool:
        """Check if a tool should be avoided in the current mode."""
        mode = self._effective_mode()
        profile = MODE_PROFILES.get(mode, MODE_PROFILES[CognitiveMode.BALANCED])
        return tool_name in profile.avoided_tools

    # ------------------------------------------------------------------
    # History & introspection
    # ------------------------------------------------------------------

    def _record_transition(self, new_mode: str, reason: str) -> None:
        """Record a mode transition."""
        with self._lock:
            self._mode_history.append({
                "mode": new_mode,
                "reason": reason,
                "timestamp": datetime.now().isoformat(),
            })
            if len(self._mode_history) > 50:
                self._mode_history = self._mode_history[-50:]

    def get_stats(self) -> dict[str, Any]:
        """Get cognitive modes statistics."""
        with self._lock:
            return {
                "current_mode": self._effective_mode().value,
                "manual_override": self._manual_override.value if self._manual_override else None,
                "safety_override": self._safety_override.value if self._safety_override else None,
                "auto_detect_enabled": self._auto_detect_enabled,
                "mode_transitions": len(self._mode_history),
                "recent_transitions": self._mode_history[-5:],
                "available_modes": [m.value for m in CognitiveMode],
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_cognitive_modes: CognitiveModes | None = None
_cm_lock = threading.Lock()


def get_cognitive_modes() -> CognitiveModes:
    """Get the global Cognitive Modes singleton."""
    global _cognitive_modes
    if _cognitive_modes is None:
        with _cm_lock:
            if _cognitive_modes is None:
                _cognitive_modes = CognitiveModes()
    return _cognitive_modes
