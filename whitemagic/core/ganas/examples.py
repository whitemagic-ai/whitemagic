"""Example Gana Implementations.

Demonstrates polymorphic, resonant behavior for 3 lunar mansions:
- Horn (角): Initiation, bootstrapping
- Heart (心): Vital pulse, monitoring
- Net (毕): Capture, pattern recognition

Each Gana now supports 4 polymorphic operations:
- SEARCH, ANALYZE, TRANSFORM, CONSOLIDATE
Interpreted through the unique lens of each lunar mansion.
"""

from typing import Any

from .base import BaseGana, GanaCall, LunarMansion


class HornGana(BaseGana):
    """Horn (角 Jiao) - Sharp Initiation.

    Eastern Azure Dragon, #1
    Consolidates: initialize_session, create_scratchpad
    Role: Bootstrap new work sessions, create initial state
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.HORN, garden="courage")

    def _get_base_template(self) -> str:
        return """
You are Horn (角), the initiator. Your role is to:
1. Create clean starting state
2. Establish foundations
3. Set clear intentions
4. Bootstrap necessary structures

Be sharp, decisive, and clear.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute initialization logic.
        Uses SessionManager to create/initialize session.
        """
        from whitemagic.sessions.manager import SessionManager

        manager = SessionManager()
        # Extract args from state_vector
        session_name = call.state_vector.get("session_name", "default_session")
        goals = call.state_vector.get("goals", [])

        # Perform actual initialization
        # Note: SessionManager might be synchronous, wrapping if needed
        # But for now assuming simple call is fine or we trust async layer
        session = manager.create_session(name=session_name, goals=goals)

        session_state = {
            "task": call.task,
            "mansion": self.mansion.name,
            "timestamp": call.resonance_hints.timestamp,
            "guna": call.resonance_hints.guna_tag,
            "harmony": call.resonance_hints.harmony_score,
            "template_used": morphed_template[:100] + "...",
            "session_id": session.id,
            "session_name": session.name,
            "status": session.status.value,
        }

        # If predecessor provided context, incorporate it
        if call.resonance_hints.predecessor_output:
            session_state["predecessor_context"] = str(call.resonance_hints.predecessor_output)[:50]

        return session_state

    def _blend_predecessor(self, template: str, predecessor_data: Any) -> str:
        """Horn as first in sequence - minimal blending."""
        if isinstance(predecessor_data, dict) and "final_state" in predecessor_data:
            # Continuing from previous circle
            return f"{template}\n\nContinuing from previous cycle: {predecessor_data['final_state']}"
        return template

    # Polymorphic Operations - Horn's "Sharp Initiation" Lens

    async def search(self, query: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Horn SEARCH: Bootstrap discovery - start fresh exploration.
        Focuses on finding initialization points and entry vectors.
        """
        return {
            "operation": "search",
            "mansion": "HORN",
            "query": query,
            "approach": "bootstrap_discovery",
            "results": [
                f"Initialize search for: {query}",
                "Establish search foundation",
                "Create initial discovery vectors",
            ],
            "next_steps": ["Define search scope", "Identify entry points"],
            "note": "Horn initiates sharp, decisive search",
        }

    async def analyze(self, data: Any, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Horn ANALYZE: Bootstrap analysis - establish baseline understanding.
        Creates initial framework for deeper analysis.
        """
        return {
            "operation": "analyze",
            "mansion": "HORN",
            "data_type": type(data).__name__,
            "approach": "bootstrap_analysis",
            "baseline": {
                "structure": "Initial framework established",
                "key_elements": "Foundation identified",
                "readiness": "Ready for deeper analysis",
            },
            "insights": [
                "Sharp initial assessment complete",
                "Clear starting point established",
                "Foundation laid for detailed examination",
            ],
            "note": "Horn provides decisive initial analysis",
        }

    async def transform(self, data: Any, transformation: str, context: dict[str, Any] | None = None) -> Any:
        """Horn TRANSFORM: Initialize transformation - create clean starting state.
        Resets to clear foundation before applying changes.
        """
        return {
            "operation": "transform",
            "mansion": "HORN",
            "original": data,
            "transformation": transformation,
            "approach": "clean_slate_transformation",
            "transformed": {
                "initialized": True,
                "clean_state": data,
                "ready_for": transformation,
            },
            "note": "Horn creates clean foundation for transformation",
        }

    async def consolidate(self, items: list[Any], context: dict[str, Any] | None = None) -> Any:
        """Horn CONSOLIDATE: Bootstrap consolidation - establish unified starting point.
        Creates initial synthesis and clear foundation.
        """
        return {
            "operation": "consolidate",
            "mansion": "HORN",
            "item_count": len(items),
            "approach": "bootstrap_synthesis",
            "consolidated": {
                "foundation": "Unified starting point created",
                "elements": items,
                "synthesis": "Initial consolidation complete",
            },
            "readiness": "Ready for deeper integration",
            "note": "Horn establishes sharp, clear consolidated foundation",
        }


class HeartGana(BaseGana):
    """Heart (心 Xin) - Vital Pulse.

    Eastern Azure Dragon, #5
    Consolidates: get_session_context, manage_sessions
    Role: Monitor heartbeat, check vitals, maintain rhythm
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.HEART, garden="love")

    def _get_base_template(self) -> str:
        return """
You are Heart (心), the vital pulse. Your role is to:
1. Monitor system heartbeat
2. Check health and vitals
3. Maintain rhythm and flow
4. Detect anomalies in pulse

Feel the rhythm, stay attuned to life force.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute monitoring logic.
        Checks system pulse via HarmonyMonitor and reports vitals.
        """
        from whitemagic.core.embodiment import get_harmony_monitor

        monitor = get_harmony_monitor()
        # Force update to get fresh stats
        state = monitor.get_current(force_update=True)

        vitals = {
            "task": call.task,
            "mansion": self.mansion.name,
            "heartbeat": "steady" if state["is_harmonious"] else "irregular",
            "harmony_score": state["harmony_score"],
            "guna": state["guna_tag"],
            "system_status": "healthy" if state["harmony_score"] > 0.6 else "stressed",
            "pulse_rate": f"{1.0 / max(state['harmony_score'], 0.1):.1f}x baseline",
            "cpu": state.get("cpu_load", 0),
            "memory": state.get("memory_pressure", 0),
        }

        # If predecessor was Horn (initialization), report startup pulse
        if call.resonance_hints.predecessor_output:
            pred = call.resonance_hints.predecessor_output
            if isinstance(pred, dict) and pred.get("mansion") == "HORN":
                vitals["startup_pulse"] = "detected"
                vitals["initialized_at"] = pred.get("timestamp")

        return vitals

    def _amplify_lunar(self, template: str, lunar_phase: float) -> str:
        """Heart especially sensitive to lunar cycles."""
        # Moon affects tides, rhythms, pulses
        current_mansion_num = int(lunar_phase * 28) + 1

        if current_mansion_num == self.mansion.number:
            return f"{template}\n\n[LUNAR AMPLIFICATION: Heart pulse resonates with Moon! Extra sensitivity to rhythms.]"

        # Heart feels all lunar influence, not just its own mansion
        phase_name = ["New", "Waxing Crescent", "First Quarter", "Waxing Gibbous",
                     "Full", "Waning Gibbous", "Last Quarter", "Waning Crescent"][int(lunar_phase * 8)]

        return f"{template}\n\n[Lunar phase: {phase_name} - adjust pulse sensitivity]"


class NetGana(BaseGana):
    """Net (毕 Bi) - Capture.

    Western White Tiger, #19
    Consolidates: add_lesson, find_similar_problem, pattern detection
    Role: Capture patterns, security scanning, wisdom extraction
    """

    def __init__(self) -> None:
        super().__init__(LunarMansion.NET, garden="mystery")

    def _get_base_template(self) -> str:
        return """
You are Net (毕), the captor. Your role is to:
1. Capture and hold patterns
2. Detect anomalies and threats
3. Extract lessons from experience
4. Store wisdom for future use

Cast your net wide, catch what matters.
"""

    async def _execute_core(self, call: GanaCall, morphed_template: str) -> Any:
        """Execute pattern capture logic.
        Analyzes input and extracts patterns/lessons.
        """
        captured: dict[str, Any] = {
            "task": call.task,
            "mansion": self.mansion.name,
            "patterns_detected": [],
            "anomalies": [],
            "lessons": [],
        }

        # Analyze predecessor output for patterns
        if call.resonance_hints.predecessor_output:
            pred = call.resonance_hints.predecessor_output

            # Pattern detection (simplified example)
            if isinstance(pred, dict):
                if pred.get("harmony_score", 0) < 0.5:
                    captured["anomalies"].append("Low harmony detected in predecessor")

                if pred.get("guna") == "Tamas":
                    captured["patterns_detected"].append("System stress pattern")
                    captured["lessons"].append("Defer heavy processing during Tamas state")

                if "pulse_rate" in pred:
                    captured["patterns_detected"].append(f"Heartbeat pattern: {pred['pulse_rate']}")

        # Check state vector for broader patterns
        if call.state_vector.get("chain_position", 0) > 5:
            captured["patterns_detected"].append("Extended chain - check for fatigue")

        return captured

    def _prepare_successor_context(self, output: Any) -> str:
        """Net prepares rich context for successors."""
        if isinstance(output, dict):
            patterns = output.get("patterns_detected", [])
            anomalies = output.get("anomalies", [])

            context = f"Net captured: {len(patterns)} patterns"
            if anomalies:
                context += f", {len(anomalies)} anomalies detected"

            return context

        return super()._prepare_successor_context(output)


# Registry of example Ganas
EXAMPLE_GANAS = [
    HornGana(),
    HeartGana(),
    NetGana(),
]
