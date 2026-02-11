"""PRAT Router — Polymorphic Resonant Adaptive Tools.
====================================================
Collapses 175+ MCP tools into 28 Gana meta-tools.

Each Gana accepts a `tool` parameter naming the specific sub-tool,
plus `args` dict that passes through to the underlying dispatch pipeline.

When WM_MCP_PRAT=1, only the 28 Gana tools are registered with MCP.
AI clients call e.g.:
    gana_ghost(tool="gnosis", args={"compact": true})
instead of:
    gnosis(compact=true)

The Gana handler routes through the existing call_tool() pipeline,
preserving all middleware (circuit breaker, rate limiter, RBAC, etc.).
"""

import logging
from typing import Any

logger = logging.getLogger(__name__)

# ──────────────────────────────────────────────────────────
# Tool → Gana mapping by domain affinity
# ──────────────────────────────────────────────────────────

TOOL_TO_GANA: dict[str, str] = {
    # ═══ HORN — Session Initialization & Setup ═══
    "session_bootstrap":       "gana_horn",
    "create_session":          "gana_horn",
    "resume_session":          "gana_horn",
    "checkpoint_session":      "gana_horn",

    # ═══ NECK — Core Memory Creation & Stability ═══
    "create_memory":           "gana_neck",
    "update_memory":           "gana_neck",
    "import_memories":         "gana_neck",
    "delete_memory":           "gana_neck",

    # ═══ ROOT — System Health & Foundations ═══
    "health_report":           "gana_root",
    "rust_status":             "gana_root",
    "rust_similarity":         "gana_root",
    "ship.check":              "gana_root",
    "state.paths":             "gana_root",
    "state.summary":           "gana_root",

    # ═══ ROOM — Resource Locks & Privacy ═══
    "sangha_lock":             "gana_room",
    "sandbox.set_limits":      "gana_room",
    "sandbox.status":          "gana_room",
    "sandbox.violations":      "gana_room",

    # ═══ HEART — Session Context & Pulse ═══
    "scratchpad":              "gana_heart",
    "session.handoff":         "gana_heart",
    "context.pack":            "gana_heart",
    "context.status":          "gana_heart",

    # ═══ TAIL — Performance & Acceleration ═══
    "simd.cosine":             "gana_tail",
    "simd.batch":              "gana_tail",
    "simd.status":             "gana_tail",
    "execute_cascade":         "gana_tail",
    "list_cascade_patterns":   "gana_tail",

    # ═══ WINNOWING BASKET — Wisdom & Tag Filtering ═══
    "search_memories":         "gana_winnowing_basket",
    "vector.search":           "gana_winnowing_basket",
    "vector.index":            "gana_winnowing_basket",
    "vector.status":           "gana_winnowing_basket",
    "fast_read_memory":        "gana_winnowing_basket",
    "batch_read_memories":     "gana_winnowing_basket",
    "read_memory":             "gana_winnowing_basket",
    "list_memories":           "gana_winnowing_basket",
    "hybrid_recall":           "gana_winnowing_basket",
    "graph_walk":              "gana_winnowing_basket",

    # ═══ GHOST — Introspection & Metric Tracking ═══
    "gnosis":                  "gana_ghost",
    "capabilities":            "gana_ghost",
    "manifest":                "gana_ghost",
    "get_telemetry_summary":   "gana_ghost",
    "repo.summary":            "gana_ghost",
    "explain_this":            "gana_ghost",
    "drive.snapshot":          "gana_ghost",
    "drive.event":             "gana_ghost",
    "selfmodel.forecast":      "gana_ghost",
    "selfmodel.alerts":        "gana_ghost",
    "capability.matrix":       "gana_ghost",
    "capability.status":       "gana_ghost",
    "capability.suggest":      "gana_ghost",
    "graph_topology":          "gana_ghost",
    "surprise_stats":          "gana_ghost",

    # ═══ WILLOW — Resilience & Flexibility ═══
    "rate_limiter.stats":      "gana_willow",
    "grimoire_suggest":        "gana_willow",
    "grimoire_cast":           "gana_willow",
    "grimoire_recommend":      "gana_willow",
    "grimoire_auto_status":    "gana_willow",
    "grimoire_walkthrough":    "gana_willow",

    # ═══ STAR — Governance & PRAT Invocation ═══
    "governor_validate":       "gana_star",
    "governor_set_goal":       "gana_star",
    "governor_check_drift":    "gana_star",
    "dharma.reload":           "gana_star",
    "set_dharma_profile":      "gana_star",

    # ═══ EXTENDED NET — Pattern Connectivity ═══
    "pattern_search":          "gana_extended_net",
    "cluster_stats":           "gana_extended_net",
    "tool.graph":              "gana_extended_net",
    "learning.patterns":       "gana_extended_net",
    "learning.suggest":        "gana_extended_net",
    "learning.status":         "gana_extended_net",

    # ═══ WINGS — Deployment & Export ═══
    "export_memories":         "gana_wings",
    "audit.export":            "gana_wings",
    "mesh.broadcast":          "gana_wings",
    "mesh.status":             "gana_wings",

    # ═══ CHARIOT — Mobility & Archaeology ═══
    "archaeology":             "gana_chariot",
    "kg.extract":              "gana_chariot",
    "kg.query":                "gana_chariot",
    "kg.top":                  "gana_chariot",
    "kg.status":               "gana_chariot",

    # ═══ ABUNDANCE — Regeneration & Dream Cycle ═══
    "dream":                   "gana_abundance",
    "memory.lifecycle":        "gana_abundance",
    "memory.retention_sweep":  "gana_abundance",
    "serendipity_surface":     "gana_abundance",
    "serendipity_mark_accessed": "gana_abundance",
    "entity_resolve":          "gana_abundance",
    "whitemagic.tip":          "gana_abundance",
    "gratitude.stats":         "gana_abundance",
    "gratitude.benefits":      "gana_abundance",

    # ═══ STRADDLING LEGS — Balance & Equilibrium ═══
    "evaluate_ethics":         "gana_straddling_legs",
    "check_boundaries":        "gana_straddling_legs",
    "verify_consent":          "gana_straddling_legs",
    "get_ethical_score":        "gana_straddling_legs",
    "get_dharma_guidance":     "gana_straddling_legs",
    "harmony_vector":          "gana_straddling_legs",

    # ═══ MOUND — Accumulation & Caching ═══
    "view_hologram":           "gana_mound",
    "track_metric":            "gana_mound",
    "get_metrics_summary":     "gana_mound",
    "record_yin_yang_activity": "gana_mound",
    "get_yin_yang_balance":    "gana_mound",

    # ═══ STOMACH — Digestion & Resource Management ═══
    "pipeline":                "gana_stomach",
    "task.distribute":         "gana_stomach",
    "task.status":             "gana_stomach",
    "task.list":               "gana_stomach",
    "task.complete":           "gana_stomach",
    "task.route_smart":        "gana_stomach",

    # ═══ HAIRY HEAD — Detail & Debug ═══
    "salience.spotlight":      "gana_hairy_head",
    "anomaly":                 "gana_hairy_head",
    "otel":                    "gana_hairy_head",
    "karma_report":            "gana_hairy_head",
    "karmic_trace":            "gana_hairy_head",
    "dharma_rules":            "gana_hairy_head",

    # ═══ NET — Capture & Filtering ═══
    "prompt.render":           "gana_net",
    "prompt.list":             "gana_net",
    "prompt.reload":           "gana_net",
    "karma.verify_chain":      "gana_net",

    # ═══ TURTLE BEAK — Precision & Protection ═══
    "edge_infer":              "gana_turtle_beak",
    "edge_batch_infer":        "gana_turtle_beak",
    "edge_stats":              "gana_turtle_beak",
    "bitnet_infer":            "gana_turtle_beak",
    "bitnet_status":           "gana_turtle_beak",

    # ═══ THREE STARS — Judgment & Synthesis ═══
    "reasoning.bicameral":     "gana_three_stars",
    "ensemble":                "gana_three_stars",
    "solve_optimization":      "gana_three_stars",
    "kaizen_analyze":          "gana_three_stars",
    "kaizen_apply_fixes":      "gana_three_stars",

    # ═══ DIPPER — Governance & Strategy ═══
    "homeostasis":             "gana_dipper",
    "maturity.assess":         "gana_dipper",
    "starter_packs":           "gana_dipper",

    # ═══ OX — Endurance & Watchdog ═══
    "swarm.decompose":         "gana_ox",
    "swarm.route":             "gana_ox",
    "swarm.complete":          "gana_ox",
    "swarm.vote":              "gana_ox",
    "swarm.resolve":           "gana_ox",
    "swarm.plan":              "gana_ox",
    "swarm.status":            "gana_ox",
    "worker.status":           "gana_ox",

    # ═══ GIRL — Nurture & User Profile ═══
    "agent.register":          "gana_girl",
    "agent.heartbeat":         "gana_girl",
    "agent.list":              "gana_girl",
    "agent.capabilities":      "gana_girl",
    "agent.deregister":        "gana_girl",
    "agent.trust":             "gana_girl",

    # ═══ VOID — Emptiness & Defrag ═══
    "galactic.dashboard":      "gana_void",
    "garden_activate":         "gana_void",
    "garden_status":           "gana_void",
    "garden_health":           "gana_void",
    "galaxy.create":           "gana_void",
    "galaxy.switch":           "gana_void",
    "galaxy.list":             "gana_void",
    "galaxy.status":           "gana_void",
    "galaxy.ingest":           "gana_void",
    "galaxy.delete":           "gana_void",

    # ═══ SIMPLIFIED ALIASES ═══
    "remember":                "gana_neck",
    "recall":                  "gana_winnowing_basket",
    "think":                   "gana_three_stars",
    "check":                   "gana_root",

    # ═══ ROOF — Shelter & Zodiac Cores ═══
    "ollama.models":           "gana_roof",
    "ollama.generate":         "gana_roof",
    "ollama.chat":             "gana_roof",
    "ollama.agent":            "gana_roof",

    # ═══ ENCAMPMENT — Transition & Handoff ═══
    "sangha_chat_send":        "gana_encampment",
    "sangha_chat_read":        "gana_encampment",
    "broker.publish":          "gana_encampment",
    "broker.history":          "gana_encampment",
    "broker.status":           "gana_encampment",

    # ═══ WALL — Boundaries & Notifications ═══
    "vote.create":             "gana_wall",
    "vote.cast":               "gana_wall",
    "vote.analyze":            "gana_wall",
    "vote.list":               "gana_wall",
    "vote.record_outcome":     "gana_wall",
    "engagement.issue":        "gana_wall",
    "engagement.validate":     "gana_wall",
    "engagement.revoke":       "gana_wall",
    "engagement.list":         "gana_wall",
    "engagement.status":       "gana_wall",

    # ═══ ROOM — Edgerunner Violet: MCP Integrity & Security Monitor ═══
    "mcp_integrity.snapshot":  "gana_room",
    "mcp_integrity.verify":    "gana_room",
    "mcp_integrity.status":    "gana_room",
    "security.alerts":         "gana_room",
    "security.monitor_status": "gana_room",

    # ═══ ROOF — Edgerunner Violet: Model Signing ═══
    "model.register":          "gana_roof",
    "model.verify":            "gana_roof",
    "model.list":              "gana_roof",
    "model.hash":              "gana_roof",
    "model.signing_status":    "gana_roof",
}

# Reverse: Gana → list of nested tools
GANA_TO_TOOLS: dict[str, list[str]] = {}
for _tool, _gana in TOOL_TO_GANA.items():
    GANA_TO_TOOLS.setdefault(_gana, []).append(_tool)


def get_gana_for_tool(tool_name: str) -> str | None:
    """Look up which Gana a tool belongs to."""
    return TOOL_TO_GANA.get(tool_name)


def get_tools_for_gana(gana_name: str) -> list[str]:
    """Get all tools nested under a Gana."""
    return GANA_TO_TOOLS.get(gana_name, [])


def build_prat_description(gana_name: str, base_desc: str) -> str:
    """Build a rich description for a PRAT Gana tool listing its nested tools."""
    tools = get_tools_for_gana(gana_name)
    if not tools:
        return base_desc

    tool_list = ", ".join(sorted(tools))
    return f"{base_desc}\n\nNested tools ({len(tools)}): {tool_list}\n\nPass tool='<name>' and args={{...}} to invoke a specific tool."


def build_prat_schema(gana_name: str, tool_registry: list) -> dict:
    """Build a PRAT schema for a Gana with its nested tools enumerated."""
    tools = get_tools_for_gana(gana_name)

    # Build tool descriptions for the enum
    tool_descs = {}
    for td in tool_registry:
        if td.name in tools:
            tool_descs[td.name] = td.description

    tool_enum = sorted(tools) if tools else []

    # Build description lines for each nested tool
    tool_desc_lines = []
    for t in tool_enum:
        desc = tool_descs.get(t, "")
        short = desc[:80] + "..." if len(desc) > 80 else desc
        tool_desc_lines.append(f"  - {t}: {short}")

    tool_help = "\n".join(tool_desc_lines)

    return {
        "type": "object",
        "properties": {
            "tool": {
                "type": "string",
                "enum": tool_enum,
                "description": f"Which tool to invoke within this Gana.\n{tool_help}",
            },
            "args": {
                "type": "object",
                "description": "Arguments to pass to the selected tool. See individual tool schemas.",
                "default": {},
            },
            "operation": {
                "type": "string",
                "enum": ["search", "analyze", "transform", "consolidate"],
                "description": "Polymorphic operation (used when no specific tool is specified).",
            },
            "context": {
                "type": "object",
                "description": "Optional resonance context.",
            },
        },
    }


def route_prat_call(gana_name: str, tool: str | None = None,
                    args: dict | None = None, **kwargs: Any) -> Any:
    """Route a PRAT call through the existing dispatch pipeline
    **with full Gana resonance**.

    Resonance protocol:
    1. Build resonance context (predecessor output, lunar phase, harmony, guna)
    2. Execute the tool call through call_tool()
    3. Record resonance state for the next call
    4. Inject _resonance metadata into the response envelope

    If `tool` is specified, delegates to call_tool(tool, **args).
    Otherwise, falls back to the Gana's native polymorphic operation.
    """
    from whitemagic.tools.prat_resonance import (
        _GANA_META,
        build_resonance_context,
        record_resonance,
    )
    from whitemagic.tools.unified_api import call_tool

    # ── Step 1: Build resonance context before execution ──
    resonance_ctx = build_resonance_context(gana_name)
    mode_hint = resonance_ctx.get("mode_hint", "normal")

    # ── Wu Xing quadrant boost (Fusion: Wu Xing → Gana) ──
    try:
        from whitemagic.core.fusions import get_wuxing_quadrant_boost
        wuxing_boost = get_wuxing_quadrant_boost(gana_name)
        resonance_ctx["wuxing_boost"] = wuxing_boost.get("boost_factor", 1.0)
        resonance_ctx["wuxing_boosted"] = wuxing_boost.get("boosted", False)
    except Exception:
        pass

    # ── Garden integration: resolve the Gana's garden ──
    _garden_instance = None
    try:
        meta = _GANA_META.get(gana_name)
        if meta:
            garden_name = meta[3].lower()  # index 3 = garden name
            from whitemagic.gardens import get_garden
            _garden_instance = get_garden(garden_name)
            if _garden_instance and hasattr(_garden_instance, "get_status"):
                resonance_ctx["garden"] = garden_name
                resonance_ctx["garden_status"] = _garden_instance.get_status()
    except Exception as exc:
        logger.debug(f"Garden lookup for {gana_name}: {exc}")

    if tool:
        # Validate that this tool belongs to this Gana
        expected_gana = TOOL_TO_GANA.get(tool)
        if expected_gana and expected_gana != gana_name:
            return {
                "status": "error",
                "error": f"Tool '{tool}' belongs to {expected_gana}, not {gana_name}.",
                "hint": f"Call {expected_gana}(tool='{tool}', args=...) instead.",
            }

        # ── Leap 7: Zig dispatch pre-check (rate limit, circuit breaker, maturity) ──
        try:
            from whitemagic.core.acceleration.dispatch_bridge import get_dispatch, DispatchResult
            dispatch = get_dispatch()
            meta = _GANA_META.get(gana_name)
            engine_slot = (meta[0] - 1) if meta else None  # mansion_num is 1-indexed, slots are 0-indexed
            if engine_slot is not None and 0 <= engine_slot < 28:
                check = dispatch.check(engine_slot)
                if check != DispatchResult.ALLOW:
                    return {
                        "status": "error",
                        "error_code": f"dispatch_{check.name.lower()}",
                        "message": f"Tool '{tool}' blocked by Zig dispatch: {check.name}",
                        "gana": gana_name,
                        "retryable": check != DispatchResult.IMMATURE,
                    }
        except Exception:
            pass  # Dispatch pre-check is optional

        # ── Step 2: Route through existing dispatch pipeline ──
        tool_args = args or {}

        # Inject resonance context into args if the tool accepts it
        if kwargs.get("context"):
            tool_args.setdefault("_resonance_context", kwargs["context"])

        try:
            result = call_tool(tool, **tool_args)
        except Exception as e:
            return {"status": "error", "error": str(e), "tool": tool}

        # ── Step 3: Record resonance state ──
        resonance_meta = record_resonance(gana_name, tool, None, result)

        # ── Fusion: Resonance → Emotion/Drive ──
        try:
            from whitemagic.core.fusions import modulate_drive_from_resonance
            modulate_drive_from_resonance(gana_name, tool)
        except Exception:
            pass

        # ── Garden notification: record the tool call ──
        if _garden_instance:
            try:
                if hasattr(_garden_instance, "record_tool_call"):
                    _garden_instance.record_tool_call(tool, tool_args, result)
                if hasattr(_garden_instance, "emit"):
                    from whitemagic.core.resonance.gan_ying_enhanced import EventType
                    _garden_instance.emit(EventType.GARDEN_ACTIVITY, {  # type: ignore[attr-defined]
                        "action": "prat_tool_call",
                        "gana": gana_name,
                        "tool": tool,
                    })
            except Exception:
                pass

        # ── Step 4: Inject resonance into response ──
        if isinstance(result, dict):
            result["_resonance"] = resonance_meta
            if _garden_instance:
                result["_garden"] = resonance_ctx.get("garden", "unknown")
        else:
            result = {
                "result": result,
                "_resonance": resonance_meta,
            }
            if _garden_instance:
                result["_garden"] = resonance_ctx.get("garden", "unknown")

        # ── v14: Speculative prefetch — record transition, predict next ──
        try:
            from whitemagic.tools.speculative_prefetch import get_prefetcher
            get_prefetcher().on_call_complete(gana_name)
        except Exception:
            pass

        return result

    # No specific tool — use native Gana operation with resonance
    operation = kwargs.get("operation", "search")

    native_result = {
        "status": "ok",
        "gana": gana_name,
        "operation": operation,
        "mode": mode_hint,
        "note": f"Native {gana_name} {operation} operation. Specify tool='<name>' for a specific sub-tool.",
        "available_tools": get_tools_for_gana(gana_name),
    }

    # Add garden context to native operations
    if _garden_instance:
        native_result["garden"] = resonance_ctx.get("garden", "unknown")
        if hasattr(_garden_instance, "get_status"):
            try:
                native_result["garden_status"] = _garden_instance.get_status()
            except Exception:
                pass

    # Add predecessor context to native operations
    if resonance_ctx.get("predecessor"):
        native_result["predecessor_context"] = resonance_ctx["predecessor"]

    # Lunar amplification note
    if resonance_ctx.get("lunar_amplification"):
        native_result["lunar_amplification"] = resonance_ctx["lunar_amplification"]

    # Record resonance for native operations too
    resonance_meta = record_resonance(gana_name, None, operation, native_result)
    native_result["_resonance"] = resonance_meta

    # ── Fusion: Resonance → Emotion/Drive (native ops too) ──
    try:
        from whitemagic.core.fusions import modulate_drive_from_resonance
        modulate_drive_from_resonance(gana_name, None)
    except Exception:
        pass

    return native_result


def validate_mapping(tool_registry: list) -> dict[str, Any]:
    """Check that all non-Gana tools are mapped to a Gana."""
    mapped = set(TOOL_TO_GANA.keys())
    all_tools = set()
    gana_tools = set()

    for td in tool_registry:
        all_tools.add(td.name)
        if td.name.startswith("gana_"):
            gana_tools.add(td.name)

    non_gana = all_tools - gana_tools
    unmapped = non_gana - mapped
    orphaned = mapped - all_tools  # Tools in mapping but not in registry

    return {
        "total_tools": len(all_tools),
        "gana_tools": len(gana_tools),
        "non_gana_tools": len(non_gana),
        "mapped": len(mapped & non_gana),
        "unmapped": sorted(unmapped),
        "orphaned": sorted(orphaned),
    }
