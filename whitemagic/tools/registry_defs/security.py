"""Edgerunner Violet Security Tools — MCP integrity, model signing,
engagement tokens, security monitor.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
# ---------------------------------------------------------------------------
# MCP Integrity
# ---------------------------------------------------------------------------
ToolDefinition(
    name="mcp_integrity.snapshot",
    description=(
        "Capture the current MCP tool registry as an integrity baseline. "
        "Computes SHA-256 fingerprints of every tool's schema, description, "
        "safety level, and category. Use before deploying to production."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {}},
    gana="Room", garden="sanctuary", quadrant="eastern", element="wood",
),
ToolDefinition(
    name="mcp_integrity.verify",
    description=(
        "Verify the current tool registry against the integrity baseline. "
        "Detects added, removed, or modified tools since the last snapshot. "
        "Reports CLEAN or DRIFT_DETECTED."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
    gana="Room", garden="sanctuary", quadrant="eastern", element="wood",
),
ToolDefinition(
    name="mcp_integrity.status",
    description="Return MCP integrity subsystem status — baseline info, verification count, drift events.",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
    gana="Room", garden="sanctuary", quadrant="eastern", element="wood",
),

# ---------------------------------------------------------------------------
# Model Signing
# ---------------------------------------------------------------------------
ToolDefinition(
    name="model.register",
    description=(
        "Register an AI model manifest with its SHA-256 hash and trust level. "
        "Supports OpenSSF OMS-compatible fields: signer, license, training "
        "disclosure, and safety profile."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "model_name": {"type": "string", "description": "Model name (e.g., 'phi-3-mini')"},
            "sha256": {"type": "string", "description": "SHA-256 hash of model weights/file"},
            "trust": {
                "type": "string",
                "enum": ["verified", "self_signed", "unsigned", "blocked"],
                "description": "Trust level for the model",
            },
            "signer": {"type": "string", "description": "Who signed (e.g., 'openssf', 'user')"},
            "license": {"type": "string", "description": "Model license (e.g., 'apache-2.0')"},
            "training_disclosure": {"type": "string", "description": "Training data provenance summary"},
            "safety_profile": {"type": "string", "description": "Safety evaluation result"},
        },
        "required": ["model_name", "sha256"],
    },
    gana="Roof", garden="protection", quadrant="northern", element="water",
),
ToolDefinition(
    name="model.verify",
    description=(
        "Verify a model against its registered manifest. Checks trust level "
        "and optionally verifies the current SHA-256 hash against the manifest."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "model_name": {"type": "string", "description": "Model name to verify"},
            "current_sha256": {"type": "string", "description": "Current hash to check against manifest"},
        },
        "required": ["model_name"],
    },
    gana="Roof", garden="protection", quadrant="northern", element="water",
),
ToolDefinition(
    name="model.list",
    description="List all registered model manifests with their trust levels and verification counts.",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
    gana="Roof", garden="protection", quadrant="northern", element="water",
),
ToolDefinition(
    name="model.hash",
    description="Compute SHA-256 hash of a model file on disk for registration or verification.",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "path": {"type": "string", "description": "Absolute path to the model file"},
        },
        "required": ["path"],
    },
    gana="Roof", garden="protection", quadrant="northern", element="water",
),
ToolDefinition(
    name="model.signing_status",
    description="Return model signing subsystem status — registered models, trust distribution, recent verifications.",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
    gana="Roof", garden="protection", quadrant="northern", element="water",
),

# ---------------------------------------------------------------------------
# Engagement Tokens
# ---------------------------------------------------------------------------
ToolDefinition(
    name="engagement.issue",
    description=(
        "Issue a new scope-of-engagement token — a cryptographic certificate "
        "authorizing time-bounded, scope-limited offensive security actions. "
        "Required by the Violet Dharma profile for any red-ops tool."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "scope": {
                "type": "array", "items": {"type": "string"},
                "description": "Target patterns (IP ranges, domains, glob)",
            },
            "tools": {
                "type": "array", "items": {"type": "string"},
                "description": "Authorized tool patterns (fnmatch)",
            },
            "issuer": {"type": "string", "description": "Identity of the authorizing person/system"},
            "duration_minutes": {"type": "integer", "default": 60, "description": "Token validity in minutes"},
            "max_uses": {"type": "integer", "default": 0, "description": "Max uses (0 = unlimited)"},
        },
        "required": ["issuer"],
    },
    gana="Wall", garden="air", quadrant="northern", element="water",
),
ToolDefinition(
    name="engagement.validate",
    description=(
        "Validate an engagement token for a specific tool/target combination. "
        "Checks HMAC integrity, expiry, scope, tool authorization, and use count."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "token_id": {"type": "string", "description": "Engagement token ID"},
            "tool": {"type": "string", "description": "Tool being invoked"},
            "target": {"type": "string", "description": "Target being accessed"},
        },
        "required": ["token_id"],
    },
    gana="Wall", garden="air", quadrant="northern", element="water",
),
ToolDefinition(
    name="engagement.revoke",
    description="Immediately revoke an engagement token, blocking all further use.",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.WRITE,
    input_schema={
        "type": "object",
        "properties": {
            "token_id": {"type": "string", "description": "Token ID to revoke"},
        },
        "required": ["token_id"],
    },
    gana="Wall", garden="air", quadrant="northern", element="water",
),
ToolDefinition(
    name="engagement.list",
    description="List all engagement tokens (active by default, or include expired).",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "include_expired": {"type": "boolean", "default": False},
        },
    },
    gana="Wall", garden="air", quadrant="northern", element="water",
),
ToolDefinition(
    name="engagement.status",
    description="Return engagement token subsystem status — active, expired, revoked counts and audit log.",
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
    gana="Wall", garden="air", quadrant="northern", element="water",
),

# ---------------------------------------------------------------------------
# Security Monitor
# ---------------------------------------------------------------------------
ToolDefinition(
    name="security.alerts",
    description=(
        "Return recent security alerts from the anomaly monitor. Detects "
        "rapid-fire calls, lateral movement, privilege escalation, and "
        "mutation bursts."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={
        "type": "object",
        "properties": {
            "limit": {"type": "integer", "default": 50, "description": "Max alerts to return"},
        },
    },
    gana="Room", garden="sanctuary", quadrant="eastern", element="wood",
),
ToolDefinition(
    name="security.monitor_status",
    description=(
        "Return security monitor status — total calls monitored, alert counts "
        "by pattern, blocked count, and configuration."
    ),
    category=ToolCategory.SYSTEM,
    safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
    gana="Room", garden="sanctuary", quadrant="eastern", element="wood",
),
]
