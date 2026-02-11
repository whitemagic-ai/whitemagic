
from typing import Any

from whitemagic.core.bridge.wisdom import consult_full_council


def run_kaizen_analysis(auto_fix: bool = False, **kwargs: Any) -> dict[str, Any]:
    """Run Kaizen continuous improvement analysis."""
    from whitemagic.core.intelligence.synthesis.kaizen_engine import get_kaizen_engine

    engine = get_kaizen_engine()

    if auto_fix:
        return engine.apply_auto_fixes()

    report = engine.analyze()

    # Serialize report
    return {
        "timestamp": report.timestamp.isoformat(),
        "metrics": report.metrics,
        "proposals": [
            {
                "id": p.id,
                "category": p.category,
                "title": p.title,
                "impact": p.impact,
                "effort": p.effort,
                "auto_fixable": p.auto_fixable,
            }
            for p in report.proposals
        ],
        "summary": {
            cat: len(items) for cat, items in report.by_category.items()
        },
    }


def analyze_wu_xing_phase(
    description: str | None = None,
    question: str | None = None,
    operation: str = "analyze",
    task_type: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Analyze Wu Xing phase or get recommendations."""
    # Wu Xing analysis - route to wisdom council for now
    if question:
        return consult_full_council(question=question, urgency="normal", **kwargs)
    elif description:
        return consult_full_council(question=f"Wu Xing analysis: {description}", urgency="normal", **kwargs)
    elif task_type:
        return consult_full_council(question=f"Wu Xing analysis for task type: {task_type}", urgency="normal", **kwargs)
    else:
        return {"error": "Either description, question, or task_type must be provided"}
