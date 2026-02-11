"""Introspection / Meta Tools — capabilities, manifest, state, ship, gnosis, telemetry, etc.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="salience.spotlight",
    description=(
        "Attention Spotlight — returns the top-N most salient events the system "
        "should focus on right now. Salience = urgency x novelty x confidence. "
        "Use this to understand what the system considers most important."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "limit": {"type": "integer", "default": 5, "description": "Number of spotlight entries to return"},
        },
    },
),
ToolDefinition(
    name="reasoning.bicameral",
    description=(
        "Bicameral Reasoning — dual-hemisphere analysis of a query. "
        "Left hemisphere (precise, logical, sequential) and right hemisphere "
        "(creative, holistic, divergent) independently analyze the query, then "
        "cross-critique each other through the corpus callosum. Returns synthesis, "
        "tension score, and dominant hemisphere."
    ),
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "query": {"type": "string", "description": "The question or problem to reason about"},
            "context": {"type": "object", "description": "Optional context for the reasoning"},
        },
        "required": ["query"],
    },
),
ToolDefinition(
    name="memory.retention_sweep",
    description=(
        "Mindful Forgetting retention sweep — evaluates memories using 5 signals "
        "(semantic, emotional, recency, connection, protection) to decide what "
        "to keep and what to forget. Defaults to dry_run=true (no actual deletions)."
    ),
    category=ToolCategory.MEMORY,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "dry_run": {"type": "boolean", "default": True, "description": "If true, only report what would be forgotten"},
            "limit": {"type": "integer", "default": 50, "description": "Max memories to evaluate"},
        },
    },
),
ToolDefinition(
    name="gnosis",
    description=(
        "Gnosis Portal — unified read-only introspection across all Whitemagic "
        "subsystems in a single call. Returns: Harmony Vector (7 health "
        "dimensions + composite score), Dharma status (active profile, rules, "
        "last decision), Karma Ledger (debt + mismatch rate), circuit breaker "
        "states, Yin/Yang balance, telemetry summary, state-root sizes, "
        "homeostasis loop, maturity stage, broker/tasks/votes/agents/temporal. "
        "Use compact=true for a token-efficient alerts-only snapshot with auto-suggestions. "
        "Use this as your primary system health check before complex operations."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "compact": {"type": "boolean", "default": False, "description": "If true, return only alerts + suggested next actions (saves tokens)"},
        },
    },
    gana="WinnowingBasket", garden="truth", quadrant="eastern", element="wood",
),
ToolDefinition(
    name="starter_packs",
    description=(
        "Unified starter pack management — curated tool sets for common workflows. "
        "Actions: list (browse packs), get (full pack details), suggest (AI recommendation). "
        "Packs: quickstart, memory, coordination, introspection, reasoning, safety."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "action": {
                "type": "string",
                "enum": ["list", "get", "suggest"],
                "description": "Action to perform",
                "default": "list",
            },
            "name": {"type": "string", "description": "Pack name (for get)"},
            "context": {"type": "string", "description": "What you want to do (for suggest)"},
        },
    },
),
ToolDefinition(
    name="rate_limiter.stats",
    description="Rate limiter statistics — total checks, blocks, block rate, per-agent usage.",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "agent_id": {"type": "string", "description": "Optional agent ID to get per-agent usage"},
        },
    },
),
ToolDefinition(
    name="audit.export",
    description=(
        "Export audit log in MCP-compatible format — karmic trace, telemetry, "
        "circuit breaker events, rate limiter stats. Filterable by time, tool, agent."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "limit": {"type": "integer", "default": 100},
            "since": {"type": "string", "description": "ISO timestamp — only entries after this time"},
            "tool": {"type": "string", "description": "Filter to specific tool name"},
            "agent_id": {"type": "string", "description": "Filter to specific agent ID"},
        },
    },
),
ToolDefinition(
    name="capabilities",
    description="Return contract versions, runtime features, limits, and (optionally) the tool list.",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "include_tools": {"type": "boolean", "default": True},
            "include_schemas": {"type": "boolean", "default": False},
            "include_env": {"type": "boolean", "default": True},
        },
    },
),
ToolDefinition(
    name="manifest",
    description="Return the canonical tool manifest (registry snapshot) in multiple formats.",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "format": {
                "type": "string",
                "enum": ["summary", "whitemagic", "mcp", "openai"],
                "default": "summary",
            },
            "include_schemas": {"type": "boolean", "default": False},
        },
    },
),
ToolDefinition(
    name="state.paths",
    description="Return resolved WM_STATE_ROOT paths used by this process.",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="state.summary",
    description="Summarize local Whitemagic state (counts, sizes) without leaking content.",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "include_sizes": {"type": "boolean", "default": True},
        },
    },
),
ToolDefinition(
    name="repo.summary",
    description="Summarize the repo/worktree (files, sizes, quick hygiene checks).",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "max_files": {"type": "integer", "default": 2500},
            "max_matches": {"type": "integer", "default": 25},
        },
    },
),
ToolDefinition(
    name="ship.check",
    description="Run open-source shipping checks (no secrets, no runtime blobs, no absolute paths).",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "max_files": {"type": "integer", "default": 4000},
            "max_large_files": {"type": "integer", "default": 25},
            "large_file_mb": {"type": "integer", "default": 10},
            "max_matches": {"type": "integer", "default": 50},
        },
    },
),
ToolDefinition(
    name="get_telemetry_summary",
    description="Return summarized performance and error metrics for tool execution.",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="health_report",
    description="Consolidated system health report aggregating Rust, Julia, Haskell bridges, DB, gardens, and archaeology.",
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="capability.matrix",
    description=(
        "Capability Matrix — full inventory of all active subsystems (24+), "
        "wired cross-system fusions (11+), and unexplored synthesis opportunities (17+). "
        "Filter by category: memory, intelligence, resonance, governance, architecture."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "category": {
                "type": "string",
                "enum": ["memory", "intelligence", "resonance", "governance", "architecture"],
                "description": "Filter subsystems by category. Omit for all.",
            },
            "include_unexplored": {
                "type": "boolean",
                "default": True,
                "description": "Include unexplored synthesis opportunities.",
            },
        },
    },
),
ToolDefinition(
    name="capability.status",
    description=(
        "Get live status for a specific subsystem by ID. "
        "Returns metadata plus a live health probe when available."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "subsystem_id": {
                "type": "string",
                "description": "Subsystem ID (e.g. 'harmony_vector', 'dream_cycle', 'prat_resonance').",
            },
        },
        "required": ["subsystem_id"],
    },
),
ToolDefinition(
    name="capability.suggest",
    description=(
        "Suggest the next best unexplored fusion to wire. "
        "Prioritizes low-difficulty fusions for quick wins."
    ),
    category=ToolCategory.INTROSPECTION,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="solve_optimization",
    description="Solve a constrained optimization problem using the Universal Solver (Frank-Wolfe).",
    category=ToolCategory.SYNTHESIS,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "nodes": {"type": "array", "items": {"type": "string"}, "description": "Decision variables."},
            "edges": {"type": "array", "items": {"type": "array", "items": {"type": "string"}}, "description": "Causal dependency edges [parent, child]."},
            "scores": {"type": "object", "description": "Node scores (targets)."},
            "budget": {"type": "integer", "description": "Max nodes to select."},
            "max_iters": {"type": "integer", "description": "Convergence limit.", "default": 50},
        },
        "required": ["nodes", "scores"],
    },
),
]
