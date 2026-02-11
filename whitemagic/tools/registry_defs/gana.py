"""GANA Tools — 28 Lunar Mansions.
================================
Each Gana maps to a specific operational domain through the lens
of its corresponding lunar mansion.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

_GANA_SCHEMA_BASE = {
    "type": "object",
    "properties": {
        "operation": {"type": "string", "enum": ["search", "analyze", "transform", "consolidate"]},
        "context": {"type": "object"},
    },
    "required": ["operation"],
}


def _gana(name: str, desc: str, safety: ToolSafety, extra_props: dict | None = None) -> ToolDefinition:
    """Helper to build a Gana tool with the standard schema."""
    schema = {
        "type": "object",
        "properties": {
            "operation": {"type": "string", "enum": ["search", "analyze", "transform", "consolidate"]},
            **(extra_props or {}),
            "context": {"type": "object"},
        },
        "required": ["operation"],
    }
    return ToolDefinition(name=name, description=desc, category=ToolCategory.GANA, safety=safety, input_schema=schema)


TOOLS: list[ToolDefinition] = [
    _gana("gana_horn", "[HORN] Session Initialization & Setup. Lens: Sharp initiation.",
          ToolSafety.WRITE, {"session_name": {"type": "string"}, "goals": {"type": "array", "items": {"type": "string"}}}),
    _gana("gana_neck", "[NECK] Core Memory Creation & Stability. Lens: Structural integrity.",
          ToolSafety.WRITE, {"title": {"type": "string"}, "content": {"type": "string"}, "tags": {"type": "array", "items": {"type": "string"}}}),
    _gana("gana_root", "[ROOT] System Health & Foundations. Lens: Root checking.",
          ToolSafety.READ, {"component": {"type": "string"}}),
    _gana("gana_room", "[ROOM] Resource Locks & Privacy. Lens: Secure enclosure.",
          ToolSafety.WRITE, {"resource": {"type": "string"}, "action": {"type": "string", "enum": ["acquire", "release", "list"]}}),
    _gana("gana_heart", "[HEART] Session Context & Pulse. Lens: Vital resonance.",
          ToolSafety.READ),
    _gana("gana_tail", "[TAIL] Performance & Acceleration. Lens: Passionate drive/acceleration.",
          ToolSafety.WRITE, {"acceleration_mode": {"type": "string"}}),
    _gana("gana_winnowing_basket", "[WINNOWING BASKET] Wisdom & Tag Filtering. Lens: Separation of essence.",
          ToolSafety.READ, {"tags": {"type": "array", "items": {"type": "string"}}}),
    _gana("gana_ghost", "[GHOST] Introspection & Metric Tracking. Lens: Internal observation.",
          ToolSafety.READ, {"metrics": {"type": "array", "items": {"type": "string"}}}),
    _gana("gana_willow", "[WILLOW] Resilience & UI Flexibility. Lens: Adaptive bending.",
          ToolSafety.WRITE, {"ui_state": {"type": "object"}}),
    _gana("gana_star", "[STAR] Governance & PRAT Invocation. Lens: Guiding light.",
          ToolSafety.WRITE, {"target_tool": {"type": "string"}, "query": {"type": "string"}}),
    _gana("gana_extended_net", "[EXTENDED NET] Pattern Connectivity. Lens: Woven connections.",
          ToolSafety.READ, {"pattern": {"type": "string"}}),
    _gana("gana_wings", "[WINGS] Deployment & Scalability. Lens: Expansive flight.",
          ToolSafety.WRITE),
    _gana("gana_chariot", "[CHARIOT] Mobility & Archaeology. Lens: Fast exploration.",
          ToolSafety.READ, {"directory": {"type": "string"}}),
    _gana("gana_abundance", "[ABUNDANCE] Regeneration & Surplus. Lens: Harvest of wisdom.",
          ToolSafety.READ),
    _gana("gana_straddling_legs", "[STRADDLING LEGS] Balance & Equilibrium. Lens: Steady harvest.",
          ToolSafety.READ),
    _gana("gana_mound", "[MOUND] Accumulation & Caching. Lens: Stored potential.",
          ToolSafety.WRITE, {"cache_key": {"type": "string"}}),
    _gana("gana_stomach", "[STOMACH] Digestion & Resource Management. Lens: Nutrient integration.",
          ToolSafety.WRITE, {"resource_data": {"type": "object"}}),
    _gana("gana_hairy_head", "[HAIRY HEAD] Detail & Focus (Debug). Lens: Microscopic scrutiny.",
          ToolSafety.READ, {"debug_target": {"type": "string"}}),
    _gana("gana_net", "[NET] Capture & Filtering. Lens: Selective retrieval.",
          ToolSafety.READ, {"filter_criteria": {"type": "object"}}),
    _gana("gana_turtle_beak", "[TURTLE BEAK] Precision & Protection. Lens: Sharp defense.",
          ToolSafety.WRITE, {"target_id": {"type": "string"}}),
    _gana("gana_three_stars", "[THREE STARS] Judgment & Synthesis. Lens: Final appraisal.",
          ToolSafety.READ, {"question": {"type": "string"}}),
    _gana("gana_dipper", "[DIPPER] Governance & Strategy. Lens: Navigational control.",
          ToolSafety.WRITE, {"strategy_goal": {"type": "string"}}),
    _gana("gana_ox", "[OX] Endurance & Watchdog. Lens: Steady vigilance.",
          ToolSafety.READ),
    _gana("gana_girl", "[GIRL] Nurture & User Profile. Lens: Personable care.",
          ToolSafety.WRITE, {"preferences": {"type": "object"}}),
    _gana("gana_void", "[VOID] Emptiness & Defrag. Lens: Sacred space.",
          ToolSafety.WRITE),
    _gana("gana_roof", "[ROOF] Shelter & Zodiac Cores. Lens: Enclosing wisdom.",
          ToolSafety.READ, {"core_name": {"type": "string"}}),
    _gana("gana_encampment", "[ENCAMPMENT] Transition & Handoff. Lens: Secure staging.",
          ToolSafety.WRITE, {"handoff_data": {"type": "object"}}),
    _gana("gana_wall", "[WALL] Boundaries & Notifications. Lens: Defining limits.",
          ToolSafety.WRITE, {"boundary_type": {"type": "string"}}),
]
