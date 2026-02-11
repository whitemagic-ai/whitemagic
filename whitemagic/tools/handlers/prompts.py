"""MCP handlers for the Prompt Template Library."""

from typing import Any


def handle_prompt_render(**kwargs: Any) -> dict[str, Any]:
    """Render a named prompt template with variable substitution."""
    from whitemagic.prompts import get_prompt_engine

    name = kwargs.get("name", "")
    wu_xing = kwargs.get("wu_xing")
    variables = kwargs.get("variables", {})

    if not name:
        return {"status": "error", "error": "name is required"}

    engine = get_prompt_engine()
    template = engine.get_template(name)
    if template is None:
        return {
            "status": "error",
            "error": f"Unknown template: {name}",
            "available": [t["name"] for t in engine.list_templates()],
        }

    rendered = engine.render(name, wu_xing=wu_xing, **variables)
    return {
        "status": "success",
        "template": name,
        "wu_xing": wu_xing or "default",
        "rendered": rendered,
    }


def handle_prompt_list(**kwargs: Any) -> dict[str, Any]:
    """List available prompt templates."""
    from whitemagic.prompts import get_prompt_engine

    tag = kwargs.get("tag")
    engine = get_prompt_engine()

    return {
        "status": "success",
        "templates": engine.list_templates(tag=tag),
        **engine.status(),
    }


def handle_prompt_reload(**kwargs: Any) -> dict[str, Any]:
    """Reload prompt templates from disk."""
    from whitemagic.prompts import get_prompt_engine

    engine = get_prompt_engine()
    engine.reload()

    return {"status": "success", "message": "Prompt templates reloaded", **engine.status()}
