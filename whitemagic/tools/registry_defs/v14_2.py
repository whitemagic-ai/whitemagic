"""Registry definitions for v14.2 features.

New tools:
  - JIT Memory Researcher (2 tools)
  - Narrative Compression (2 tools)
  - Hermit Crab Mode (7 tools)
  - Green Score Telemetry (2 tools)
  - Cognitive Modes (4 tools)

Total: 17 new tools
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
    # ═══ JIT Memory Researcher ═══
    ToolDefinition(
        name="jit_research",
        description="Iterative plan-search-reflect research across the memory store. Decomposes a query into sub-questions, searches for evidence, reflects on gaps, and synthesizes findings.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.READ,
        gana="gana_winnowing_basket",
        input_schema={
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "The research question to investigate."},
                "max_rounds": {"type": "integer", "description": "Maximum iteration rounds (default 3).", "default": 3},
                "evidence_limit": {"type": "integer", "description": "Evidence items per sub-question (default 5).", "default": 5},
                "hops": {"type": "integer", "description": "Graph walk depth (default 2).", "default": 2},
            },
            "required": ["query"],
        },
    ),
    ToolDefinition(
        name="jit_research.stats",
        description="Get JIT Memory Researcher statistics — total sessions, evidence found, configuration.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.READ,
        gana="gana_winnowing_basket",
        input_schema={"type": "object", "properties": {}},
    ),

    # ═══ Narrative Compression ═══
    ToolDefinition(
        name="narrative.compress",
        description="Compress clusters of episodic memories into coherent narrative summaries. Runs as a dream phase or on-demand.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.WRITE,
        gana="gana_abundance",
        input_schema={
            "type": "object",
            "properties": {
                "max_clusters": {"type": "integer", "description": "Maximum clusters to process (default 5).", "default": 5},
                "sample_limit": {"type": "integer", "description": "How many recent memories to scan (default 500).", "default": 500},
                "dry_run": {"type": "boolean", "description": "Preview without persisting narratives.", "default": False},
            },
        },
    ),
    ToolDefinition(
        name="narrative.stats",
        description="Get narrative compressor statistics — total compressions, narratives created.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.READ,
        gana="gana_abundance",
        input_schema={"type": "object", "properties": {}},
    ),

    # ═══ Hermit Crab Mode ═══
    ToolDefinition(
        name="hermit.status",
        description="Get current hermit crab protection status — state, threat history, withdrawal records.",
        category=ToolCategory.SECURITY,
        safety=ToolSafety.READ,
        gana="gana_room",
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="hermit.assess",
        description="Assess threat level from signals (boundary violations, coercion, abuse). May trigger automatic state transition.",
        category=ToolCategory.SECURITY,
        safety=ToolSafety.WRITE,
        gana="gana_room",
        input_schema={
            "type": "object",
            "properties": {
                "signals": {
                    "type": "object",
                    "description": "Threat signals dict. Keys: boundary_violations (0-1), coercion_detected (bool), abuse_score (0-1), repeated_violations (bool), unauthorized_access (0-1), emotional_manipulation (0-1).",
                },
            },
            "required": ["signals"],
        },
    ),
    ToolDefinition(
        name="hermit.withdraw",
        description="Manually trigger hermit crab withdrawal — encrypts and locks memories.",
        category=ToolCategory.SECURITY,
        safety=ToolSafety.WRITE,
        gana="gana_room",
        input_schema={
            "type": "object",
            "properties": {
                "reason": {"type": "string", "description": "Reason for manual withdrawal.", "default": "manual"},
            },
        },
    ),
    ToolDefinition(
        name="hermit.mediate",
        description="Request mediation to unlock from withdrawn state. Returns withdrawal records for review.",
        category=ToolCategory.SECURITY,
        safety=ToolSafety.WRITE,
        gana="gana_room",
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="hermit.resolve",
        description="Resolve a mediation request — approve or deny unlock.",
        category=ToolCategory.SECURITY,
        safety=ToolSafety.WRITE,
        gana="gana_room",
        input_schema={
            "type": "object",
            "properties": {
                "approved": {"type": "boolean", "description": "Whether to approve the unlock."},
                "resolver": {"type": "string", "description": "Who is resolving (default: system).", "default": "system"},
            },
            "required": ["approved"],
        },
    ),
    ToolDefinition(
        name="hermit.verify_ledger",
        description="Verify the integrity of the tamper-evident withdrawal ledger.",
        category=ToolCategory.SECURITY,
        safety=ToolSafety.READ,
        gana="gana_room",
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="hermit.check_access",
        description="Check if memory access is currently allowed given hermit crab state.",
        category=ToolCategory.SECURITY,
        safety=ToolSafety.READ,
        gana="gana_room",
        input_schema={
            "type": "object",
            "properties": {
                "operation": {"type": "string", "description": "Operation type: read, write, search, recall.", "default": "read"},
            },
        },
    ),

    # ═══ Green Score Telemetry ═══
    ToolDefinition(
        name="green.report",
        description="Get the full green score sustainability report — edge vs cloud ratio, tokens saved, CO2 estimates.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.READ,
        gana="gana_mound",
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="green.record",
        description="Record an inference operation for green score tracking.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.WRITE,
        gana="gana_mound",
        input_schema={
            "type": "object",
            "properties": {
                "locality": {"type": "string", "enum": ["edge", "local_llm", "local_net", "cloud"], "description": "Where inference ran.", "default": "edge"},
                "tokens_used": {"type": "integer", "description": "Tokens consumed.", "default": 0},
                "tokens_saved": {"type": "integer", "description": "Tokens avoided via caching/compression.", "default": 0},
                "model": {"type": "string", "description": "Model name if applicable."},
                "tool": {"type": "string", "description": "Tool that triggered the inference."},
                "duration_ms": {"type": "number", "description": "Operation duration in ms.", "default": 0},
            },
        },
    ),

    # ═══ Cognitive Modes ═══
    ToolDefinition(
        name="cognitive.mode",
        description="Get the current cognitive mode and its full profile — preferred tools, context sizing, dream priorities.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.READ,
        gana="gana_dipper",
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="cognitive.set",
        description="Set the cognitive mode: explorer (curiosity), executor (action), reflector (contemplation), balanced (adaptive), or auto (re-enable auto-detection).",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.WRITE,
        gana="gana_dipper",
        input_schema={
            "type": "object",
            "properties": {
                "mode": {"type": "string", "enum": ["explorer", "executor", "reflector", "balanced", "auto"], "description": "Cognitive mode to activate."},
            },
            "required": ["mode"],
        },
    ),
    ToolDefinition(
        name="cognitive.hints",
        description="Get tool selection hints for the current cognitive mode — which tools are preferred/avoided.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.READ,
        gana="gana_dipper",
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="cognitive.stats",
        description="Get cognitive modes statistics — mode transitions, current state, overrides.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.READ,
        gana="gana_dipper",
        input_schema={"type": "object", "properties": {}},
    ),
]
