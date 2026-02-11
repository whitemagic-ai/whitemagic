"""Homeostasis & Maturity — status/check, maturity assess, dependency graph, dharma reload.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="homeostasis",
    description=(
        "Unified homeostatic loop control. Actions: status (view loop state + recent "
        "corrective actions), check (manually trigger a harmony check and apply "
        "corrections if needed)."
    ),
    category=ToolCategory.METRICS,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["status", "check"],
                "description": "Action to perform",
                "default": "status",
            },
        },
    },
),
ToolDefinition(
    name="maturity.assess",
    description=(
        "Assess the system's developmental maturity stage. Runs gate checks "
        "for each stage (Seed→Bicameral→Reflective→Radiant→Collective→Logos) "
        "and reports which capabilities are unlocked, the current stage, and "
        "what's blocking advancement to the next stage."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

ToolDefinition(
    name="tool.graph",
    description=(
        "Query the tool dependency graph. Without arguments, returns a summary "
        "(total tools, edges, edge types). With a 'tool' argument, returns "
        "next_steps, prerequisites, and plan. With detail='full', returns all edges."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "tool": {
                "type": "string",
                "description": "Tool name to query relationships for (optional — omit for graph summary)",
            },
            "detail": {
                "type": "string",
                "enum": ["summary", "full"],
                "default": "summary",
                "description": "Level of detail: summary (default) or full (all edges)",
            },
        },
    },
),
ToolDefinition(
    name="dharma.reload",
    description=(
        "Hot-reload Dharma rules from disk. Scans $WM_STATE_ROOT/dharma_rules.yaml "
        "and $WM_STATE_ROOT/dharma/rules.d/*.yaml, merges with built-in defaults "
        "(user rules override by name). Use this after editing rule files."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {}},
),
]
