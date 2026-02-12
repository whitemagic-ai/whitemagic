"""Synthesis, Kaizen, Serendipity, and Pattern tool handlers."""
import logging
from typing import Any, cast

logger = logging.getLogger(__name__)


def handle_kaizen_analyze(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.kaizen import run_kaizen_analysis
    return cast("dict[str, Any]", run_kaizen_analysis(auto_fix=False, **kwargs))


def handle_kaizen_apply_fixes(**kwargs: Any) -> dict[str, Any]:
    fix_ids = kwargs.get("fix_ids") or []
    dry_run = bool(kwargs.get("dry_run", True))
    if not isinstance(fix_ids, list) or not all(isinstance(x, str) for x in fix_ids):
        return {"status": "error", "message": "fix_ids must be a list of strings"}
    from whitemagic.core.intelligence.synthesis.kaizen_engine import get_kaizen_engine
    engine = get_kaizen_engine()
    report = engine.analyze()
    proposals_by_id = {p.id: p for p in report.proposals}
    missing = [fid for fid in fix_ids if fid not in proposals_by_id]
    selected = [proposals_by_id[fid] for fid in fix_ids if fid in proposals_by_id]
    not_applicable = [p.id for p in selected if (not p.auto_fixable) or (not p.fix_action)]
    applicable = [p for p in selected if p.auto_fixable and p.fix_action]
    if dry_run:
        return {
            "status": "success", "dry_run": True, "requested": len(fix_ids),
            "missing": missing, "not_applicable": not_applicable, "would_apply": [p.id for p in applicable],
        }
    results = {"applied": 0, "skipped": 0, "errors": 0, "missing": len(missing)}
    applied: list = []
    errors: list = []
    for proposal in applicable:
        try:
            action = proposal.fix_action or ""
            if "title_generator" in action:
                from whitemagic.core.intelligence.synthesis.title_generator import (
                    get_title_generator,
                )
                get_title_generator().fix_all()
            elif "tag_normalizer" in action:
                from whitemagic.core.intelligence.synthesis.tag_normalizer import (
                    get_tag_normalizer,
                )
                get_tag_normalizer().auto_tag_untagged()
            elif "sub_clustering" in action:
                from whitemagic.core.intelligence.synthesis.sub_clustering import (
                    get_sub_clustering_engine,
                )
                get_sub_clustering_engine().subdivide_large_clusters()
            else:
                results["skipped"] += 1
                continue
            results["applied"] += 1
            applied.append(proposal.id)
        except Exception as exc:
            results["errors"] += 1
            errors.append({"id": proposal.id, "error": str(exc)})
    return {"status": "success", "dry_run": False, "results": results, "applied": applied, "errors": errors}


def handle_serendipity_surface(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.memory.unified import get_unified_memory
    get_unified_memory()
    from whitemagic.core.intelligence.synthesis.serendipity_engine import (
        get_serendipity_engine,
    )
    engine = get_serendipity_engine()
    results = engine.surface(**kwargs)
    return {"status": "success", "results": [r.__dict__ for r in results] if results else []}


def handle_serendipity_mark_accessed(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.memory.unified import get_unified_memory
    get_unified_memory()
    from whitemagic.core.intelligence.synthesis.serendipity_engine import (
        get_serendipity_engine,
    )
    get_serendipity_engine().mark_accessed(**kwargs)
    return {"status": "success"}


def handle_pattern_search(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.pattern import detect_patterns
    return cast("dict[str, Any]", detect_patterns(**kwargs))


def handle_cluster_stats(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.memory.unified import get_unified_memory
    get_unified_memory()
    from whitemagic.core.intelligence.synthesis.sub_clustering import (
        get_sub_clustering_engine,
    )
    return cast("dict[str, Any]", get_sub_clustering_engine().get_cluster_stats())


def handle_list_cascade_patterns(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.core.bridge.tools import execute_mcp_tool
    return cast("dict[str, Any]", execute_mcp_tool("list_patterns", **kwargs))
