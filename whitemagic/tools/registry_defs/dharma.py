"""Dharma Tools — ethics, boundaries, consent, karma, rules, profiles.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
ToolDefinition(
    name="evaluate_ethics",
    description="Evaluate the ethical implications of an action",
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "action": {"type": "string", "description": "Action to evaluate"},
            "context": {"type": "object", "description": "Context for evaluation"},
        },
        "required": ["action"],
    },
    gana="StraddlingLegs", garden="dharma", quadrant="western", element="metal",
),
ToolDefinition(
    name="check_boundaries",
    description="Check if an action respects established boundaries",
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "action": {"type": "string", "description": "Action to check"},
            "boundary_type": {"type": "string", "enum": ["help", "interfere", "neutral"]},
        },
        "required": ["action"],
    },
),
ToolDefinition(
    name="verify_consent",
    description="Verify user consent for an action",
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "action": {"type": "string", "description": "Action requiring consent"},
            "consent_type": {"type": "string", "enum": ["explicit", "implicit", "informed"]},
        },
        "required": ["action"],
    },
),
ToolDefinition(
    name="get_ethical_score",
    description="Get cumulative ethical score over time window",
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "time_window_hours": {"type": "integer", "default": 24},
        },
    },
),
ToolDefinition(
    name="get_dharma_guidance",
    description="Get guidance on how to act according to Dharma",
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "situation": {"type": "string", "description": "Situation to get guidance for"},
        },
        "required": ["situation"],
    },
),
ToolDefinition(
    name="karma_report",
    description=(
        "Get the Karma Ledger report — tracks declared vs actual side-effects "
        "for every tool call. Shows total karma debt, mismatch rate, top "
        "offending tools, and recent entries. Use this to audit whether tools "
        "are behaving as declared."
    ),
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "limit": {"type": "integer", "default": 100, "description": "Max entries to include"},
        },
    },
),
ToolDefinition(
    name="karmic_trace",
    description=(
        "Get the Karmic Trace — an immutable audit trail of every Dharma "
        "rule evaluation. Each entry records the action, which rules fired, "
        "what decision was made, and why. Essential for transparency."
    ),
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "limit": {"type": "integer", "default": 50, "description": "Max trace entries"},
        },
    },
),
ToolDefinition(
    name="dharma_rules",
    description=(
        "List all active Dharma rules and the current profile. Rules are "
        "declarative YAML policies that govern tool behavior (log, tag, warn, "
        "throttle, block). Profiles: default, creative, secure."
    ),
    category=ToolCategory.DHARMA,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "profile": {"type": "string", "description": "Filter by profile name (optional)"},
        },
    },
),
ToolDefinition(
    name="set_dharma_profile",
    description=(
        "Switch the active Dharma profile. Profiles control how strictly "
        "the Dharma engine governs tool usage. Options: 'default' (balanced), "
        "'creative' (relaxed writes), 'secure' (blocks all mutations)."
    ),
    category=ToolCategory.DHARMA,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "profile": {"type": "string", "enum": ["default", "creative", "secure", "violet"]},
        },
        "required": ["profile"],
    },
),
]
