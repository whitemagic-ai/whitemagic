"""Capability Matrix — Introspectable System Inventory.
=====================================================
Makes the full capability matrix queryable via MCP/Gnosis.

Tracks:
- 23+ active subsystems with location, polyglot accelerator, status
- 10+ active cross-system fusions (wired connections)
- 17+ unexplored synthesis opportunities
- Live status checks for key subsystems

Usage:
    from whitemagic.tools.capability_matrix import get_capability_matrix
    matrix = get_capability_matrix()
"""

import logging
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Subsystem Registry
# ---------------------------------------------------------------------------

SUBSYSTEMS: list[dict[str, Any]] = [
    {"id": "memory_unified", "name": "Memory (Unified)", "location": "core/memory/unified.py",
     "polyglot": "Rust (KD-tree, batch scoring)", "category": "memory"},
    {"id": "galactic_map", "name": "Galactic Map", "location": "core/memory/galactic_map.py",
     "polyglot": "Rust (retention scoring)", "category": "memory"},
    {"id": "holographic_5d", "name": "5D Holographic Coords", "location": "intelligence/hologram/encoder.py",
     "polyglot": "Rust (SpatialIndex5D)", "category": "memory"},
    {"id": "association_miner", "name": "Association Miner", "location": "core/memory/association_miner.py",
     "polyglot": "Rust (Jaccard overlap)", "category": "memory"},
    {"id": "constellations", "name": "Constellation Detection", "location": "core/memory/constellations.py",
     "polyglot": None, "category": "memory"},
    {"id": "consolidation", "name": "Memory Consolidation", "location": "core/memory/consolidation.py",
     "polyglot": None, "category": "memory"},
    {"id": "mindful_forgetting", "name": "Mindful Forgetting", "location": "core/memory/mindful_forgetting.py",
     "polyglot": None, "category": "memory"},
    {"id": "dream_cycle", "name": "Dream Cycle (5-phase)", "location": "core/dreaming/dream_cycle.py",
     "polyglot": "Elixir (OTP GenServer)", "category": "intelligence"},
    {"id": "temporal_scheduler", "name": "Temporal Scheduler", "location": "core/resonance/temporal_scheduler.py",
     "polyglot": None, "category": "resonance"},
    {"id": "salience_arbiter", "name": "Salience Arbiter", "location": "core/resonance/salience_arbiter.py",
     "polyglot": None, "category": "resonance"},
    {"id": "gan_ying", "name": "Gan Ying Event Bus", "location": "core/resonance/gan_ying_enhanced.py",
     "polyglot": "Elixir (actor model)", "category": "resonance"},
    {"id": "redis_bridge", "name": "Redis Bridge", "location": "core/resonance/redis_bridge.py",
     "polyglot": "Go (libp2p mesh)", "category": "resonance"},
    {"id": "harmony_vector", "name": "Harmony Vector (7D)", "location": "harmony/vector.py",
     "polyglot": None, "category": "governance"},
    {"id": "homeostatic_loop", "name": "Homeostatic Loop", "location": "harmony/homeostatic_loop.py",
     "polyglot": None, "category": "governance"},
    {"id": "dharma_rules", "name": "Dharma Rules Engine", "location": "dharma/rules.py",
     "polyglot": "Haskell (algebraic types)", "category": "governance"},
    {"id": "karma_ledger", "name": "Karma Ledger", "location": "dharma/karma_ledger.py",
     "polyglot": None, "category": "governance"},
    {"id": "maturity_gates", "name": "Maturity Gates", "location": "core/governance/maturity_gates.py",
     "polyglot": None, "category": "governance"},
    {"id": "bicameral", "name": "Bicameral Reasoner", "location": "core/intelligence/bicameral.py",
     "polyglot": None, "category": "intelligence"},
    {"id": "emotion_drive", "name": "Emotion/Drive Core", "location": "core/intelligence/emotion_drive.py",
     "polyglot": None, "category": "intelligence"},
    {"id": "self_model", "name": "Self-Model", "location": "core/intelligence/self_model.py",
     "polyglot": None, "category": "intelligence"},
    {"id": "knowledge_graph", "name": "Knowledge Graph", "location": "intelligence/knowledge_graph/",
     "polyglot": None, "category": "intelligence"},
    {"id": "gana_architecture", "name": "28 Gana Architecture", "location": "core/ganas/",
     "polyglot": None, "category": "architecture"},
    {"id": "prat_router", "name": "PRAT Router", "location": "tools/prat_router.py",
     "polyglot": None, "category": "architecture"},
    {"id": "prat_resonance", "name": "PRAT Resonance", "location": "tools/prat_resonance.py",
     "polyglot": None, "category": "architecture"},
    {"id": "zig_simd", "name": "Zig SIMD Acceleration", "location": "core/acceleration/simd_cosine.py",
     "polyglot": "Zig (AVX2 SIMD cosine)", "category": "memory"},
]


# ---------------------------------------------------------------------------
# Active Cross-System Fusions
# ---------------------------------------------------------------------------

ACTIVE_FUSIONS: list[dict[str, str]] = [
    {"id": "dream_consolidation_galactic",
     "name": "Dream Cycle → Consolidation → Galactic Map",
     "emergent": "Sleeping creates strategy memories promoted to INNER_RIM",
     "path": "dream_cycle.py → consolidation.py → galactic_map.py"},
    {"id": "harmony_homeostasis_dharma",
     "name": "Harmony Vector → Homeostatic Loop → Dharma",
     "emergent": "Self-healing: health drops → auto-correction → ethical tightening",
     "path": "vector.py → homeostatic_loop.py → rules.py"},
    {"id": "recall_galactic_spiral",
     "name": "Recall → Galactic Spiral-In",
     "emergent": "Accessing a memory moves it closer to CORE (5%/access)",
     "path": "unified.py recall()"},
    {"id": "tool_harmony_karma_dream",
     "name": "Tool Call → Harmony + Karma + Dream Touch",
     "emergent": "Every tool invocation feeds health, audits side-effects, resets idle timer",
     "path": "unified_api.py _record_telemetry()"},
    {"id": "agent_trust_rate_limiter",
     "name": "Agent Trust → Rate Limiter",
     "emergent": "Trusted agents get 2× rate limits; restricted get 0.25×",
     "path": "rate_limiter.py _trust_multiplier()"},
    {"id": "association_kg_constellations",
     "name": "Association Miner → Knowledge Graph → Constellations",
     "emergent": "Keyword links → KG relations → density clusters",
     "path": "Three-way cross-feed"},
    {"id": "circuit_breaker_selfmodel",
     "name": "Circuit Breaker → Self-Model Forecast",
     "emergent": "Predictive breaker tightening from metric regression",
     "path": "circuit_breaker.py predictive_check()"},
    {"id": "grimoire_emotion",
     "name": "Grimoire → Emotion/Drive Core",
     "emergent": "Spell confidence modulated by emotional state",
     "path": "grimoire/auto_cast.py _drive_bias()"},
    {"id": "galactic_harmony_energy",
     "name": "Galactic Map → Harmony Vector Energy",
     "emergent": "Memory vitality (40%) blends with runtime pressure (60%)",
     "path": "galactic_map.py → vector.py"},
    {"id": "consolidation_kg",
     "name": "Consolidation → KG Relations",
     "emergent": "Synthesized memories create KG entities",
     "path": "consolidation.py _feed_knowledge_graph()"},
    {"id": "prat_resonance_chain",
     "name": "PRAT Router → Resonance State",
     "emergent": "Sequential tool calls carry predecessor/successor context, lunar amplification, Guna adaptation",
     "path": "prat_router.py → prat_resonance.py"},
    {"id": "depgraph_pipeline",
     "name": "Dependency Graph → Pipeline Engine",
     "emergent": "Pipeline step validation against dependency edges before execution",
     "path": "tools/handlers/pipeline.py _validate_pipeline_deps()"},
    {"id": "selfmodel_dream",
     "name": "Self-Model Forecasts → Dream Scheduling",
     "emergent": "Predicted energy troughs trigger proactive dreaming before exhaustion",
     "path": "core/fusions.py check_proactive_dream()"},
    {"id": "wuxing_gana_boost",
     "name": "Wu Xing Phase → Gana Quadrant Boost",
     "emergent": "Current elemental phase amplifies matching quadrant's Ganas via boost_factor",
     "path": "core/fusions.py → prat_router.py route_prat_call()"},
    {"id": "resonance_emotion",
     "name": "PRAT Resonance → Emotion/Drive Core",
     "emergent": "Predecessor/successor Gana output modulates emotional state, creating mood across chains",
     "path": "core/fusions.py modulate_drive_from_resonance() → prat_router.py"},
    {"id": "zodiac_grimoire",
     "name": "Zodiac Cores → Grimoire Spells",
     "emergent": "Active zodiac core modulates which grimoire spells are recommended via element→Wu Xing mapping",
     "path": "core/fusions.py get_zodiac_spell_boost()"},
    {"id": "bicameral_consolidation",
     "name": "Bicameral Reasoner → Consolidation",
     "emergent": "Right hemisphere creative cross-pollination enhances memory cluster discovery",
     "path": "core/fusions.py bicameral_consolidation_enhance()"},
    {"id": "salience_homeostasis",
     "name": "Salience Arbiter ↔ Homeostatic Loop",
     "emergent": "Bidirectional: salience alerts trigger homeostasis; health adjusts salience thresholds",
     "path": "core/fusions.py salience_homeostasis_sync()"},
    {"id": "dream_bicameral",
     "name": "Dream Cycle → Bicameral",
     "emergent": "Dream SERENDIPITY phase uses bicameral reasoning for creative cross-pollination",
     "path": "core/fusions.py dream_bicameral_serendipity()"},
    {"id": "constellation_garden",
     "name": "Constellation Detection → Garden Activation",
     "emergent": "Dense memory clusters auto-activate the corresponding consciousness garden",
     "path": "core/fusions.py constellation_garden_activate()"},
    {"id": "kg_gana_routing",
     "name": "Knowledge Graph → Gana Routing",
     "emergent": "KG entity relationships suggest which Gana to invoke next",
     "path": "core/fusions.py kg_suggest_next_gana()"},
    {"id": "zig_simd_cosine",
     "name": "Zig SIMD → Vector Search",
     "emergent": "Hardware-accelerated cosine similarity for embedding vector search (AVX2, 8-lane)",
     "path": "core/acceleration/simd_cosine.py → core/memory/vector_search.py"},
    {"id": "haskell_dharma_primary",
     "name": "Haskell Dharma → Python Dharma",
     "emergent": "Type-safe algebraic rule engine as primary evaluator with Python fallback",
     "path": "dharma/rules.py → haskell/haskell_bridge.py"},
    {"id": "gana_chain_harmony",
     "name": "Gana Chain → Harmony Vector",
     "emergent": "Chains adapt mid-sequence based on system health — stressed system truncates, healthy extends",
     "path": "core/fusions.py gana_chain_harmony_adapt()"},
    {"id": "prat_gana_chain",
     "name": "PRAT Router → Gana Chain",
     "emergent": "Sequential same-Gana PRAT calls auto-detected for resonance chain sequencing",
     "path": "core/fusions.py prat_auto_chain_detect()"},
    {"id": "mojo_holographic",
     "name": "Mojo SIMD → Holographic Encoding",
     "emergent": "Batch 5D coordinate encoding via Mojo SIMD with Python fallback",
     "path": "core/fusions.py mojo_holographic_batch_encode()"},
    {"id": "elixir_event_bus",
     "name": "Elixir Event Bus → Python Gan Ying",
     "emergent": "Elixir OTP actor-model event routing bridges with Python Gan Ying (3-lane temporal)",
     "path": "core/fusions.py elixir_event_bridge()"},
    {"id": "go_mesh_sync",
     "name": "Go Mesh → Memory Sync",
     "emergent": "Cross-node memory replication and announcement via Go libp2p mesh",
     "path": "core/fusions.py mesh_memory_sync()"},
]


# ---------------------------------------------------------------------------
# Unexplored Synthesis Opportunities
# ---------------------------------------------------------------------------

UNEXPLORED_FUSIONS: list[dict[str, str]] = [
    # All 28 fusions now wired — matching the 28 Ganas.
    # Future synthesis opportunities will be added here as new subsystems emerge.
]


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def get_capability_matrix(
    category: str | None = None,
    include_unexplored: bool = True,
) -> dict[str, Any]:
    """Get the full capability matrix.

    Args:
        category: Filter subsystems by category (memory, intelligence, resonance,
                  governance, architecture). None = all.
        include_unexplored: Include unexplored fusion opportunities.

    Returns:
        Dict with subsystems, active_fusions, unexplored_fusions, and summary stats.

    """
    subsystems = SUBSYSTEMS
    if category:
        subsystems = [s for s in subsystems if s.get("category") == category]

    polyglot_count = sum(1 for s in SUBSYSTEMS if s.get("polyglot"))
    categories = sorted(set(s.get("category", "other") for s in SUBSYSTEMS))

    result: dict[str, Any] = {
        "summary": {
            "total_subsystems": len(SUBSYSTEMS),
            "filtered_subsystems": len(subsystems),
            "active_fusions": len(ACTIVE_FUSIONS),
            "unexplored_fusions": len(UNEXPLORED_FUSIONS),
            "polyglot_accelerated": polyglot_count,
            "categories": categories,
        },
        "subsystems": subsystems,
        "active_fusions": ACTIVE_FUSIONS,
    }

    if include_unexplored:
        result["unexplored_fusions"] = UNEXPLORED_FUSIONS
        # Group by difficulty
        by_diff: dict[str, list[str]] = {"low": [], "medium": [], "high": []}
        for f in UNEXPLORED_FUSIONS:
            d = f.get("difficulty", "medium")
            by_diff.setdefault(d, []).append(f["name"])
        result["unexplored_by_difficulty"] = {
            k: {"count": len(v), "fusions": v} for k, v in by_diff.items()
        }

    return result


def get_subsystem_status(subsystem_id: str) -> dict[str, Any]:
    """Get live status for a specific subsystem.

    Returns the subsystem metadata + a live health probe.
    """
    subsystem = next((s for s in SUBSYSTEMS if s["id"] == subsystem_id), None)
    if not subsystem:
        return {"status": "error", "error": f"Unknown subsystem: {subsystem_id}"}

    result = dict(subsystem)
    result["status"] = "active"

    # Live probes for key subsystems
    probes = {
        "harmony_vector": _probe_harmony,
        "dharma_rules": _probe_dharma,
        "maturity_gates": _probe_maturity,
        "dream_cycle": _probe_dream,
        "temporal_scheduler": _probe_temporal,
        "homeostatic_loop": _probe_homeostasis,
        "prat_resonance": _probe_resonance,
    }

    probe_fn = probes.get(subsystem_id)
    if probe_fn:
        try:
            result["live_probe"] = probe_fn()
        except Exception as e:
            result["live_probe"] = {"error": str(e)}

    return result


def suggest_next_fusion() -> dict[str, Any]:
    """Suggest the best unexplored fusion to wire next, based on difficulty
    and potential impact.

    Prioritizes low-difficulty fusions first.
    """
    low = [f for f in UNEXPLORED_FUSIONS if f["difficulty"] == "low"]
    medium = [f for f in UNEXPLORED_FUSIONS if f["difficulty"] == "medium"]

    pick: dict[str, str] | None
    if low:
        pick = low[0]
        reason = "Low difficulty — quick win"
    elif medium:
        pick = medium[0]
        reason = "Medium difficulty — good balance of effort and impact"
    else:
        pick = UNEXPLORED_FUSIONS[0] if UNEXPLORED_FUSIONS else None
        reason = "Only high-difficulty fusions remain"

    if not pick:
        return {"status": "all_wired", "note": "All known fusions have been explored!"}

    return {
        "suggested_fusion": pick,
        "reason": reason,
        "remaining": {
            "low": len(low),
            "medium": len(medium),
            "high": len(UNEXPLORED_FUSIONS) - len(low) - len(medium),
        },
    }


# ---------------------------------------------------------------------------
# Live Probes
# ---------------------------------------------------------------------------

def _probe_harmony() -> dict[str, Any]:
    from whitemagic.harmony.vector import get_harmony_vector
    hv = get_harmony_vector()
    snap = hv.snapshot()
    return {"harmony_score": snap.harmony_score, "energy": snap.energy}


def _probe_dharma() -> dict[str, Any]:
    from whitemagic.dharma.rules import get_rules_engine
    engine = get_rules_engine()
    return {"profile": engine.get_profile(), "rules": len(engine.get_rules())}


def _probe_maturity() -> dict[str, Any]:
    from whitemagic.core.governance.maturity_gates import get_maturity_engine
    engine = get_maturity_engine()
    report = engine.assess()
    return {"stage": report.to_dict().get("current_stage", "unknown")}


def _probe_dream() -> dict[str, Any]:
    from whitemagic.core.dreaming.dream_cycle import get_dream_cycle
    dc = get_dream_cycle()
    phase_obj = getattr(dc, "current_phase", None)
    phase_name = getattr(phase_obj, "name", "IDLE")
    return {
        "phase": phase_name,
        "is_dreaming": bool(getattr(dc, "is_dreaming", False)),
    }


def _probe_temporal() -> dict[str, Any]:
    from whitemagic.core.resonance.temporal_scheduler import get_temporal_scheduler
    scheduler = get_temporal_scheduler()
    return {"running": scheduler.is_running}


def _probe_homeostasis() -> dict[str, Any]:
    from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
    loop = get_homeostatic_loop()
    stats = loop.get_stats()
    return {"running": stats.get("running", False),
            "total_checks": stats.get("total_checks", 0)}


def _probe_resonance() -> dict[str, Any]:
    from whitemagic.tools.prat_resonance import get_resonance_summary
    summary = get_resonance_summary()
    return {"session_calls": summary["session_calls"],
            "last_gana": summary.get("last_gana")}
