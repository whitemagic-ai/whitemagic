"""War Room MCP Tool Handlers
==============================
Handlers for shadow clone army deployment, Art of War strategy,
Imperial Doctrine, and Fool's Guard (Ralph Wiggum) tools.

Tools registered:
  - war_room.status          → War Room status summary
  - war_room.plan            → Plan a campaign for an objective
  - war_room.execute         → Execute a named tactic
  - war_room.hierarchy       → View command hierarchy
  - war_room.campaigns       → List recent campaigns
  - war_room.phase           → Detect optimal Wu Xing phase
  - doctrine.summary         → Get doctrine campaign summary
  - doctrine.stratagems      → Select applicable stratagems
  - doctrine.force           → Recommend force composition
  - art_of_war.wisdom        → Get Art of War wisdom for situation
  - art_of_war.terrain       → Assess terrain for objective
  - art_of_war.campaign      → Plan Art of War campaign
  - art_of_war.chapter       → Consult a specific chapter (1-13)
  - fool_guard.status        → Get Fool's Guard / Ralph Wiggum stats
  - fool_guard.dare_to_die   → Deploy Dare-to-Die Corps
  - fool_guard.ralph         → Single Ralph Wiggum maneuver
"""

from typing import Any

import asyncio


def _run_async(coro: Any) -> Any:
    """Run an async coroutine from sync handler context."""
    try:
        loop = asyncio.get_running_loop()
        import concurrent.futures
        with concurrent.futures.ThreadPoolExecutor() as pool:
            return loop.run_in_executor(pool, asyncio.run, coro)
    except RuntimeError:
        return asyncio.run(coro)


# ---------------------------------------------------------------------------
# War Room Handlers
# ---------------------------------------------------------------------------

def handle_war_room_status(**kwargs: Any) -> dict[str, Any]:
    """Get War Room status summary."""
    from whitemagic.agents.war_room import get_war_room
    return {"status": "ok", **get_war_room().get_status()}


def handle_war_room_plan(**kwargs: Any) -> dict[str, Any]:
    """Plan a campaign for an objective."""
    from whitemagic.agents.war_room import get_war_room
    objective = kwargs.get("objective", "")
    if not objective:
        return {"status": "error", "error": "objective is required"}
    intent = kwargs.get("emperor_intent", "")
    campaign = get_war_room().plan_campaign(objective, intent)
    return {"status": "ok", **campaign.to_dict()}


def handle_war_room_execute(**kwargs: Any) -> dict[str, Any]:
    """Execute a named tactic."""
    from whitemagic.agents.war_room import get_war_room
    tactic = kwargs.get("tactic", "Chain of Stratagems")
    objective = kwargs.get("objective", "")
    if not objective:
        return {"status": "error", "error": "objective is required"}
    context = kwargs.get("context", {})
    result = _run_async(get_war_room().execute_tactic(tactic, objective, context))
    return {"status": "ok", **result} if isinstance(result, dict) else {"status": "ok", "result": result}


def handle_war_room_hierarchy(**kwargs: Any) -> dict[str, Any]:
    """View command hierarchy."""
    from whitemagic.agents.war_room import get_war_room
    return {"status": "ok", "hierarchy": get_war_room().get_hierarchy()}


def handle_war_room_campaigns(**kwargs: Any) -> dict[str, Any]:
    """List recent campaigns."""
    from whitemagic.agents.war_room import get_war_room
    limit = kwargs.get("limit", 10)
    return {"status": "ok", "campaigns": get_war_room().get_campaign_history(limit)}


def handle_war_room_phase(**kwargs: Any) -> dict[str, Any]:
    """Detect optimal Wu Xing phase for current conditions."""
    from whitemagic.agents.war_room import get_war_room
    return {"status": "ok", **get_war_room().detect_optimal_phase()}


# ---------------------------------------------------------------------------
# Imperial Doctrine Handlers
# ---------------------------------------------------------------------------

def handle_doctrine_summary(**kwargs: Any) -> dict[str, Any]:
    """Get full doctrine campaign summary for an objective."""
    from whitemagic.agents.doctrine import get_doctrine
    objective = kwargs.get("objective", "")
    if not objective:
        return {"status": "error", "error": "objective is required"}
    return {"status": "ok", **get_doctrine().campaign_summary(objective)}


def handle_doctrine_stratagems(**kwargs: Any) -> dict[str, Any]:
    """Select applicable stratagems for a situation."""
    from whitemagic.agents.doctrine import get_doctrine
    situation = kwargs.get("situation", "")
    if not situation:
        return {"status": "error", "error": "situation is required"}
    max_results = kwargs.get("max_results", 5)
    stratagems = get_doctrine().select_stratagems(situation, max_results)
    return {
        "status": "ok",
        "stratagems": [
            {
                "number": s.number,
                "name": s.name,
                "chinese": s.chinese,
                "category": s.category,
                "principle": s.principle,
                "application": s.application,
                "force_nature": s.force_nature.value,
                "wu_xing_phase": s.wu_xing_phase.value if s.wu_xing_phase else None,
            }
            for s in stratagems
        ],
    }


def handle_doctrine_force(**kwargs: Any) -> dict[str, Any]:
    """Recommend force composition for an objective."""
    from whitemagic.agents.doctrine import get_doctrine
    objective = kwargs.get("objective", "")
    if not objective:
        return {"status": "error", "error": "objective is required"}
    constraints = kwargs.get("constraints", {})
    specs = get_doctrine().recommend_force(objective, constraints)
    return {
        "status": "ok",
        "force_composition": [
            {
                "type": s.force_type.value,
                "count": s.clone_count,
                "strategies": s.strategies,
                "nature": s.nature.value,
                "phase": s.wu_xing_phase.value,
                "stateless": s.stateless,
            }
            for s in specs
        ],
        "total_clones": sum(s.clone_count for s in specs),
    }


# ---------------------------------------------------------------------------
# Art of War Handlers
# ---------------------------------------------------------------------------

def handle_art_of_war_wisdom(**kwargs: Any) -> dict[str, Any]:
    """Get Art of War wisdom for a situation."""
    from whitemagic.core.intelligence.wisdom.art_of_war import get_war_engine
    situation = kwargs.get("situation", "")
    if not situation:
        return {"status": "error", "error": "situation is required"}
    p = get_war_engine().get_war_wisdom(situation)
    return {
        "status": "ok",
        "chapter": p.chapter,
        "chapter_number": p.chapter_number,
        "principle": p.principle,
        "application": p.application,
    }


def handle_art_of_war_terrain(**kwargs: Any) -> dict[str, Any]:
    """Assess terrain for an objective."""
    from whitemagic.core.intelligence.wisdom.art_of_war import get_war_engine
    objective = kwargs.get("objective", "")
    if not objective:
        return {"status": "error", "error": "objective is required"}
    t = get_war_engine().assess_terrain(objective)
    return {
        "status": "ok",
        "terrain_type": t.terrain_type.value,
        "file_count": t.file_count,
        "complexity": t.complexity,
        "test_coverage": t.test_coverage,
        "risk_level": t.risk_level,
        "recommended_phase": t.recommended_phase.value,
        "description": t.description,
        "principles": [
            {"chapter": p.chapter, "principle": p.principle, "application": p.application}
            for p in t.principles
        ],
    }


def handle_art_of_war_campaign(**kwargs: Any) -> dict[str, Any]:
    """Plan an Art of War campaign."""
    from whitemagic.core.intelligence.wisdom.art_of_war import get_war_engine
    objective = kwargs.get("objective", "")
    if not objective:
        return {"status": "error", "error": "objective is required"}
    plan = get_war_engine().plan_campaign(objective)
    return {"status": "ok", **plan.to_dict()}


def handle_art_of_war_chapter(**kwargs: Any) -> dict[str, Any]:
    """Consult a specific Art of War chapter (1-13)."""
    from whitemagic.core.intelligence.wisdom.art_of_war import get_war_engine
    chapter = kwargs.get("chapter", 1)
    principles = get_war_engine().consult_chapter(chapter)
    if not principles:
        return {"status": "error", "error": f"No principles found for chapter {chapter}"}
    return {
        "status": "ok",
        "chapter": chapter,
        "principles": [
            {"chapter": p.chapter, "principle": p.principle, "application": p.application}
            for p in principles
        ],
    }


# ---------------------------------------------------------------------------
# Fool's Guard / Ralph Wiggum Handlers
# ---------------------------------------------------------------------------

def handle_fool_guard_status(**kwargs: Any) -> dict[str, Any]:
    """Get Fool's Guard / Ralph Wiggum stats."""
    from whitemagic.core.intelligence.agentic.fool_guard import get_fool_guard
    return {"status": "ok", **get_fool_guard().get_stats()}


def handle_fool_guard_dare_to_die(**kwargs: Any) -> dict[str, Any]:
    """Deploy Dare-to-Die Corps against a problem."""
    from whitemagic.core.intelligence.agentic.fool_guard import get_fool_guard
    mission = kwargs.get("mission", "")
    if not mission:
        return {"status": "error", "error": "mission is required"}
    target_file = kwargs.get("target_file")
    error_log = kwargs.get("error_log")
    max_attempts = kwargs.get("max_attempts", 10)
    result = _run_async(get_fool_guard().deploy_dare_to_die(
        mission=mission,
        target_file=target_file,
        error_log=error_log,
        max_attempts=max_attempts,
    ))
    if hasattr(result, 'to_dict'):
        return {"status": "ok", **result.to_dict()}
    return {"status": "ok", "result": str(result)}


def handle_fool_guard_ralph(**kwargs: Any) -> dict[str, Any]:
    """Execute a single Ralph Wiggum maneuver."""
    from whitemagic.core.intelligence.agentic.fool_guard import ralph_wiggum_maneuver
    mission = kwargs.get("mission", "")
    if not mission:
        return {"status": "error", "error": "mission is required"}
    target_file = kwargs.get("target_file")
    result = _run_async(ralph_wiggum_maneuver(mission=mission, target_file=target_file))
    return {
        "status": "ok",
        "clone_id": result.clone_id,
        "success": result.success,
        "output": result.output[:500],
        "duration_ms": result.duration_ms,
        "strategy": result.strategy,
    }
