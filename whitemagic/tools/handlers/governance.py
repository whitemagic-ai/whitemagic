"""Governance & lifecycle tool handlers — lifecycle, consolidation, homeostasis, maturity, dharma reload, tool graph."""
from typing import Any, cast


def handle_memory_lifecycle(**kwargs: Any) -> dict[str, Any]:
    """Unified memory lifecycle handler — routes by action parameter."""
    action = kwargs.get("action", "stats")
    dispatch = {
        "sweep": handle_lifecycle_sweep,
        "stats": handle_lifecycle_stats,
        "consolidate": handle_consolidate_memories,
        "consolidation_stats": handle_consolidation_stats,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def handle_lifecycle_sweep(**kwargs: Any) -> dict[str, Any]:
    """Trigger a memory lifecycle retention sweep (mindful forgetting)."""
    from whitemagic.core.memory.lifecycle import get_lifecycle_manager
    mgr = get_lifecycle_manager()
    return cast("dict[str, Any]", mgr.run_sweep())


def handle_lifecycle_stats(**kwargs: Any) -> dict[str, Any]:
    """Get memory lifecycle manager statistics."""
    from whitemagic.core.memory.lifecycle import get_lifecycle_manager
    mgr = get_lifecycle_manager()
    return {"status": "success", "lifecycle": mgr.get_stats()}


def handle_homeostasis(**kwargs: Any) -> dict[str, Any]:
    """Unified homeostasis handler — routes by action parameter."""
    action = kwargs.get("action", "status")
    if action == "check":
        return handle_homeostasis_check(**kwargs)
    return handle_homeostasis_status(**kwargs)


def handle_homeostasis_status(**kwargs: Any) -> dict[str, Any]:
    """Get the homeostatic loop's status and recent corrective actions."""
    from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
    loop = get_homeostatic_loop()
    return {"status": "success", "homeostasis": loop.get_stats()}


def handle_homeostasis_check(**kwargs: Any) -> dict[str, Any]:
    """Manually trigger a homeostatic harmony check."""
    from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
    loop = get_homeostatic_loop()
    actions = loop.check()
    return {
        "status": "success",
        "actions_taken": len(actions),
        "actions": [a.to_dict() for a in actions],
    }


def handle_maturity_assess(**kwargs: Any) -> dict[str, Any]:
    """Assess the system's current developmental maturity stage."""
    from whitemagic.core.governance.maturity_gates import (
        MaturityStage,
        get_maturity_engine,
    )
    engine = get_maturity_engine()
    report = engine.assess()
    result: dict[str, Any] = {"status": "success", "maturity": report.to_dict()}

    # Enrich with locked/unlocked tool info from maturity_check
    try:
        from whitemagic.tools.maturity_check import _MATURITY_REQUIREMENTS
        current_level = report.current_stage.value
        stage_names = {s.value: s.name for s in MaturityStage}
        locked = []
        unlocked = []
        for tool_name, req_level in sorted(_MATURITY_REQUIREMENTS.items()):
            entry = {"tool": tool_name, "required_level": req_level,
                     "required_stage": stage_names.get(req_level, str(req_level))}
            if current_level >= req_level:
                unlocked.append(entry)
            else:
                locked.append(entry)
        result["locked_tools"] = locked
        result["unlocked_tools"] = unlocked
    except Exception:
        pass

    return result


def handle_consolidate_memories(**kwargs: Any) -> dict[str, Any]:
    """Run hippocampal-replay memory consolidation (cluster, synthesize, promote)."""
    from whitemagic.core.memory.consolidation import get_consolidator
    consolidator = get_consolidator()
    report = consolidator.consolidate()
    return {"status": "success", "consolidation": report.to_dict()}


def handle_consolidation_stats(**kwargs: Any) -> dict[str, Any]:
    """Get memory consolidation engine statistics."""
    from whitemagic.core.memory.consolidation import get_consolidator
    consolidator = get_consolidator()
    return {"status": "success", "consolidation": consolidator.get_stats()}


def handle_tool_graph(**kwargs: Any) -> dict[str, Any]:
    """Get the tool dependency graph — relationships between tools for AI planning."""
    from whitemagic.tools.dependency_graph import get_tool_graph
    graph = get_tool_graph()
    tool = kwargs.get("tool")
    if tool:
        return {
            "status": "success",
            "tool": tool,
            "next_steps": graph.next_steps(tool),
            "prerequisites": graph.prerequisites(tool),
            "plan": graph.plan(tool),
        }
    return {"status": "success", "graph": graph.get_graph_summary()}


def handle_tool_graph_full(**kwargs: Any) -> dict[str, Any]:
    """Get the full tool dependency graph as a list of edges."""
    from whitemagic.tools.dependency_graph import get_tool_graph
    graph = get_tool_graph()
    return {"status": "success", "edges": graph.get_full_graph()}


def handle_dharma_reload(**kwargs: Any) -> dict[str, Any]:
    """Hot-reload Dharma rules from disk (YAML files)."""
    from whitemagic.dharma.rules import get_rules_engine
    engine = get_rules_engine()
    count = engine.reload()
    return {
        "status": "success",
        "rules_loaded": count,
        "active_profile": engine.get_profile(),
    }
