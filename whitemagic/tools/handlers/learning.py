"""MCP handlers for Cross-Session Learning."""

from typing import Any


def handle_learning_patterns(**kwargs: Any) -> dict[str, Any]:
    """Get aggregated cross-session tool usage patterns."""
    from whitemagic.core.learning import get_session_learner
    learner = get_session_learner()
    return {"status": "success", **learner.get_patterns()}


def handle_learning_suggest(**kwargs: Any) -> dict[str, Any]:
    """Suggest next tools based on learned sequences."""
    from whitemagic.core.learning import get_session_learner
    tool = kwargs.get("current_tool", "")
    if not tool:
        return {"status": "error", "error": "current_tool is required"}
    learner = get_session_learner()
    return {"status": "success", "suggestions": learner.suggest_next(tool)}


def handle_learning_status(**kwargs: Any) -> dict[str, Any]:
    """Get cross-session learner status."""
    from whitemagic.core.learning import get_session_learner
    learner = get_session_learner()
    return {"status": "success", **learner.status()}
