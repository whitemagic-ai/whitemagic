"""MCP handlers for Grimoire auto-cast and recommendation."""

from typing import Any


def handle_grimoire_suggest(**kwargs: Any) -> dict[str, Any]:
    """Suggest spells for a given task context."""
    from whitemagic.grimoire.auto_cast import AutoCaster, CastContext, CastMode

    task = kwargs.get("task", "")
    if not task:
        return {"status": "error", "message": "task is required"}

    emotional_state = kwargs.get("emotional_state", "neutral")
    wu_xing = kwargs.get("wu_xing", "earth")
    urgency = float(kwargs.get("urgency", 0.5))

    caster = AutoCaster(mode=CastMode.SUGGEST_ONLY)
    caster.activate()

    ctx = CastContext(
        task=task,
        emotional_state=emotional_state,
        wu_xing=wu_xing,
        urgency=urgency,
        keywords=task.lower().split(),
    )
    results = caster.process_context(ctx)
    caster.deactivate()

    suggestions: list[dict[str, Any]] = []
    for r in results:
        if r.spell:
            suggestions.append({
                "spell": r.spell.name,
                "description": r.spell.description,
                "confidence": round(r.confidence, 3),
                "wu_xing": r.spell.wu_xing_affinity,
                "keywords": r.spell.keywords[:5],
            })

    # Fusion: Zodiac Cores → Grimoire Spells boost
    zodiac_boost = {}
    try:
        from whitemagic.core.fusions import get_zodiac_spell_boost
        zodiac_boost = get_zodiac_spell_boost(task)
        boosted_names = {s["spell"] for s in zodiac_boost.get("boosted_spells", [])}
        for s in suggestions:
            if s["spell"] in boosted_names:
                confidence = float(s.get("confidence", 0.0))
                s["confidence"] = round(min(1.0, confidence + 0.2), 3)
                s["zodiac_boosted"] = True
    except Exception:
        pass

    # Re-sort by confidence after boost
    suggestions.sort(key=lambda s: s["confidence"], reverse=True)

    result = {
        "status": "success",
        "task": task,
        "suggestions": suggestions,
        "count": len(suggestions),
    }
    if zodiac_boost.get("active_cores"):
        result["zodiac_context"] = {
            "active_cores": zodiac_boost["active_cores"],
            "boost_affinities": zodiac_boost.get("boost_affinities", []),
        }
    return result


def handle_grimoire_cast(**kwargs: Any) -> dict[str, Any]:
    """Cast a specific spell by name."""
    from whitemagic.grimoire.auto_cast import AutoCaster, CastContext, CastMode

    spell_name = kwargs.get("spell_name", "")
    if not spell_name:
        return {"status": "error", "message": "spell_name is required"}

    task = kwargs.get("task", "manual cast")
    emotional_state = kwargs.get("emotional_state", "neutral")

    caster = AutoCaster(mode=CastMode.ALWAYS)
    caster.activate()

    ctx = CastContext(task=task, emotional_state=emotional_state)
    result = caster.quick_cast(spell_name, ctx)
    caster.deactivate()

    return {
        "status": "success" if result.spell else "error",
        "spell": result.spell.name if result.spell else None,
        "outcome": result.outcome.value,
        "confidence": round(result.confidence, 3),
        "reason": result.reason,
    }


def handle_grimoire_recommend(**kwargs: Any) -> dict[str, Any]:
    """Recommend MCP tools based on Grimoire chapter-tool mapping."""
    from whitemagic.grimoire.chapters import ChapterIndex

    task = kwargs.get("task", "")
    if not task:
        return {"status": "error", "message": "task is required"}

    limit = int(kwargs.get("limit", 5))

    # Chapter → MCP tool mapping (from 00_INDEX.md mandala)
    CHAPTER_TOOLS: dict[int, list[str]] = {
        0:  ["session_bootstrap"],
        1:  ["session_bootstrap", "get_agent_capabilities"],
        2:  ["create_memory", "search_memories"],
        3:  ["garden_activate", "garden_status", "record_yin_yang_activity"],
        4:  ["search_memories", "serendipity_surface"],
        5:  ["broker.publish", "agent.register", "task.distribute"],
        6:  ["edge_infer", "reasoning.bicameral", "search_memories"],
        7:  ["gnosis", "harmony_vector", "homeostasis.check"],
        8:  ["search_memories", "create_memory", "fast_read_memory", "export_memories"],
        9:  ["create_memory", "pipeline.create", "grimoire_cast"],
        10: ["evaluate_ethics", "check_boundaries", "dharma_rules", "karma_report"],
        11: ["session_status", "focus_session", "get_yin_yang_balance"],
        12: ["cast_oracle", "memory.consolidate", "memory.lifecycle_sweep"],
        13: ["wu_xing_balance", "garden_synergy", "harmony_vector"],
        14: ["cast_oracle", "wu_xing_balance"],
        15: ["maturity.assess", "tool.graph", "homeostasis.status"],
        16: ["ganying_emit", "resonance_trace", "ganying_listeners"],
        17: ["edge_infer", "coherence_boost", "solve_optimization"],
        18: ["grimoire_cast", "grimoire_suggest"],
        19: ["export_memories", "session_handoff"],
    }

    index = ChapterIndex()
    matched_chapters = index.find_for_task(task)

    recommendations: list[dict[str, Any]] = []
    seen_tools: set[str] = set()
    for chapter in matched_chapters:
        tools = CHAPTER_TOOLS.get(chapter.number, [])
        for tool_name in tools:
            if tool_name not in seen_tools:
                seen_tools.add(tool_name)
                recommendations.append({
                    "tool": tool_name,
                    "chapter": chapter.number,
                    "chapter_title": chapter.title,
                    "reason": f"Matched via chapter {chapter.number}: {chapter.title}",
                })
                if len(recommendations) >= limit:
                    break
        if len(recommendations) >= limit:
            break

    return {
        "status": "success",
        "task": task,
        "recommendations": recommendations,
        "chapters_matched": [{"number": c.number, "title": c.title} for c in matched_chapters[:5]],
        "count": len(recommendations),
    }


def handle_grimoire_auto_status(**kwargs: Any) -> dict[str, Any]:
    """Get auto-caster status and recent cast history."""
    from whitemagic.grimoire.auto_cast import AutoCaster, CastMode

    caster = AutoCaster(mode=CastMode.SUGGEST_ONLY)

    return {
        "status": "success",
        "active": caster.active,
        "mode": caster.mode.value,
        "spells_available": len(caster.spell_book),
        "cast_history_count": len(caster.cast_history),
    }
