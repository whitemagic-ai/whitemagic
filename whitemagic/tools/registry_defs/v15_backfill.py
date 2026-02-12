"""v15.1 Backfill — ToolDefinition entries for 104 tools that existed in
dispatch_table but lacked formal registry definitions.

Generated during the v15.1 release audit. Each tool has a minimal but
accurate input_schema so that ``capabilities`` and ``manifest`` tools
report the full 313-tool surface.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

_S = {"type": "object", "properties": {}}  # empty schema shorthand

TOOLS: list[ToolDefinition] = [

# ═══════════════════════════════════════════════════════════════════
# Archaeology (12 individual sub-commands exposed as flat tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="archaeology_mark_read",
    description="Mark a file as read in the archaeology tracker.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string", "description": "File path"},
        "context": {"type": "string"},
        "insight": {"type": "string"},
    }, "required": ["path"]},
),
ToolDefinition(
    name="archaeology_mark_written",
    description="Mark a file as written in the archaeology tracker.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string"}, "context": {"type": "string"},
    }, "required": ["path"]},
),
ToolDefinition(
    name="archaeology_have_read",
    description="Check if a file has been read.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string"},
    }, "required": ["path"]},
),
ToolDefinition(
    name="archaeology_find_unread",
    description="Find files not yet read in a directory.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "directory": {"type": "string"},
        "patterns": {"type": "array", "items": {"type": "string"}},
        "limit": {"type": "integer", "default": 50},
    }, "required": ["directory"]},
),
ToolDefinition(
    name="archaeology_find_changed",
    description="Find files changed since last read.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "directory": {"type": "string"},
        "patterns": {"type": "array", "items": {"type": "string"}},
        "limit": {"type": "integer", "default": 50},
    }, "required": ["directory"]},
),
ToolDefinition(
    name="archaeology_recent_reads",
    description="List recently read files.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 20},
    }},
),
ToolDefinition(
    name="archaeology_stats",
    description="Get archaeology tracker statistics.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="archaeology_scan_directory",
    description="Scan a directory and track all files.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "directory": {"type": "string"},
        "patterns": {"type": "array", "items": {"type": "string"}},
    }, "required": ["directory"]},
),
ToolDefinition(
    name="archaeology_report",
    description="Generate a full archaeology report.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "directory": {"type": "string"},
    }},
),
ToolDefinition(
    name="archaeology_search",
    description="Search archaeology history by query.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string"},
        "limit": {"type": "integer", "default": 20},
    }, "required": ["query"]},
),
ToolDefinition(
    name="archaeology_process_wisdom",
    description="Extract wisdom insights from tracked files.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 10},
    }},
),
ToolDefinition(
    name="archaeology_daily_digest",
    description="Generate a daily archaeology digest.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Browser Automation (7 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="browser_navigate",
    description="Navigate the browser to a URL.",
    category=ToolCategory.BROWSER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "url": {"type": "string", "description": "URL to navigate to"},
    }, "required": ["url"]},
),
ToolDefinition(
    name="browser_click",
    description="Click an element in the browser by selector.",
    category=ToolCategory.BROWSER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "selector": {"type": "string"},
    }, "required": ["selector"]},
),
ToolDefinition(
    name="browser_type",
    description="Type text into a browser element.",
    category=ToolCategory.BROWSER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "selector": {"type": "string"},
        "text": {"type": "string"},
    }, "required": ["selector", "text"]},
),
ToolDefinition(
    name="browser_extract_dom",
    description="Extract the DOM structure of the current page.",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "selector": {"type": "string"},
    }},
),
ToolDefinition(
    name="browser_screenshot",
    description="Take a screenshot of the current browser page.",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="browser_get_interactables",
    description="Get all interactable elements on the current page.",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="browser_session_status",
    description="Get the current browser session status.",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Dream Cycle (4 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="dream_start",
    description="Start the 5-phase dream cycle (consolidation, serendipity, kaizen, oracle, decay).",
    category=ToolCategory.SYSTEM, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "phases": {"type": "array", "items": {"type": "string"}},
    }},
),
ToolDefinition(
    name="dream_stop",
    description="Stop a running dream cycle.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.WRITE,
    input_schema=_S,
),
ToolDefinition(
    name="dream_status",
    description="Get the current dream cycle status.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="dream_now",
    description="Run a single dream cycle synchronously.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "phases": {"type": "array", "items": {"type": "string"}},
    }},
),

# ═══════════════════════════════════════════════════════════════════
# Scratchpad (4 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="scratchpad_create",
    description="Create a new session scratchpad.",
    category=ToolCategory.SESSION, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "title": {"type": "string"},
        "content": {"type": "string"},
    }, "required": ["title"]},
),
ToolDefinition(
    name="scratchpad_update",
    description="Update an existing scratchpad.",
    category=ToolCategory.SESSION, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "id": {"type": "string"},
        "content": {"type": "string"},
    }, "required": ["id", "content"]},
),
ToolDefinition(
    name="scratchpad_finalize",
    description="Finalize a scratchpad and optionally persist as memory.",
    category=ToolCategory.SESSION, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "id": {"type": "string"},
        "persist": {"type": "boolean", "default": False},
    }, "required": ["id"]},
),
ToolDefinition(
    name="analyze_scratchpad",
    description="Analyze scratchpad content for patterns and insights.",
    category=ToolCategory.SESSION, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "id": {"type": "string"},
    }, "required": ["id"]},
),

# ═══════════════════════════════════════════════════════════════════
# Session (5 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="session_status",
    description="Get current session status and metadata.",
    category=ToolCategory.SESSION, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="session_handoff",
    description="Create a session handoff for continuity.",
    category=ToolCategory.SESSION, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "summary": {"type": "string"},
        "pending_tasks": {"type": "array", "items": {"type": "string"}},
    }},
),
ToolDefinition(
    name="session.handoff_transfer",
    description="Transfer session handoff to another agent.",
    category=ToolCategory.SESSION, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "handoff_id": {"type": "string"},
        "target_agent": {"type": "string"},
    }, "required": ["handoff_id"]},
),
ToolDefinition(
    name="session.accept_handoff",
    description="Accept a session handoff from another agent.",
    category=ToolCategory.SESSION, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "handoff_id": {"type": "string"},
    }, "required": ["handoff_id"]},
),
ToolDefinition(
    name="session.list_handoffs",
    description="List available session handoffs.",
    category=ToolCategory.SESSION, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "status": {"type": "string", "enum": ["pending", "accepted", "all"]},
    }},
),
ToolDefinition(
    name="focus_session",
    description="Set the active focus session for context continuity.",
    category=ToolCategory.SESSION, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "session_id": {"type": "string"},
    }, "required": ["session_id"]},
),

# ═══════════════════════════════════════════════════════════════════
# Web Research (4 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="web_search",
    description="Search the web (Brave API preferred, DuckDuckGo fallback).",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string", "description": "Search query"},
        "num_results": {"type": "integer", "default": 8},
    }, "required": ["query"]},
),
ToolDefinition(
    name="web_fetch",
    description="Fetch and extract content from a URL.",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "url": {"type": "string", "description": "URL to fetch"},
        "max_chars": {"type": "integer", "default": 50000},
    }, "required": ["url"]},
),
ToolDefinition(
    name="web_search_and_read",
    description="Search the web and auto-fetch top results.",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string"},
        "num_results": {"type": "integer", "default": 5},
        "max_chars_per_source": {"type": "integer", "default": 15000},
    }, "required": ["query"]},
),
ToolDefinition(
    name="research_topic",
    description="Deep research orchestrator: search → fetch → synthesize.",
    category=ToolCategory.BROWSER, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "topic": {"type": "string", "description": "Research topic or question"},
        "num_search_results": {"type": "integer", "default": 6},
        "max_sources_to_fetch": {"type": "integer", "default": 4},
    }, "required": ["topic"]},
),

# ═══════════════════════════════════════════════════════════════════
# Watcher (8 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="watcher_add",
    description="Add a file or directory watcher.",
    category=ToolCategory.WATCHER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string"}, "patterns": {"type": "array", "items": {"type": "string"}},
    }, "required": ["path"]},
),
ToolDefinition(
    name="watcher_remove",
    description="Remove a file watcher.",
    category=ToolCategory.WATCHER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "watcher_id": {"type": "string"},
    }, "required": ["watcher_id"]},
),
ToolDefinition(
    name="watcher_start",
    description="Start watching for file changes.",
    category=ToolCategory.WATCHER, safety=ToolSafety.WRITE,
    input_schema=_S,
),
ToolDefinition(
    name="watcher_stop",
    description="Stop all file watchers.",
    category=ToolCategory.WATCHER, safety=ToolSafety.WRITE,
    input_schema=_S,
),
ToolDefinition(
    name="watcher_list",
    description="List all active file watchers.",
    category=ToolCategory.WATCHER, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="watcher_status",
    description="Get watcher subsystem status.",
    category=ToolCategory.WATCHER, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="watcher_stats",
    description="Get watcher event statistics.",
    category=ToolCategory.WATCHER, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="watcher_recent_events",
    description="List recent watcher events.",
    category=ToolCategory.WATCHER, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 20},
    }},
),

# ═══════════════════════════════════════════════════════════════════
# Windsurf Conversations (5 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="windsurf_list_conversations",
    description="List Windsurf IDE conversations.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 20},
    }},
),
ToolDefinition(
    name="windsurf_search_conversations",
    description="Search Windsurf conversations by query.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string"},
        "limit": {"type": "integer", "default": 10},
    }, "required": ["query"]},
),
ToolDefinition(
    name="windsurf_read_conversation",
    description="Read a specific Windsurf conversation.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "conversation_id": {"type": "string"},
    }, "required": ["conversation_id"]},
),
ToolDefinition(
    name="windsurf_export_conversation",
    description="Export a Windsurf conversation to memory.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "conversation_id": {"type": "string"},
    }, "required": ["conversation_id"]},
),
ToolDefinition(
    name="windsurf_stats",
    description="Get Windsurf conversation statistics.",
    category=ToolCategory.ARCHAEOLOGY, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Gan Ying Event Bus (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="ganying_emit",
    description="Emit an event on the Gan Ying bus.",
    category=ToolCategory.BROKER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "event_type": {"type": "string"},
        "payload": {"type": "object"},
    }, "required": ["event_type"]},
),
ToolDefinition(
    name="ganying_history",
    description="Get recent Gan Ying event history.",
    category=ToolCategory.BROKER, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 20},
        "event_type": {"type": "string"},
    }},
),
ToolDefinition(
    name="ganying_listeners",
    description="List active Gan Ying event listeners.",
    category=ToolCategory.BROKER, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Governor Extensions (4 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="governor_check_budget",
    description="Check remaining budget for a tool or category.",
    category=ToolCategory.GOVERNOR, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "tool": {"type": "string"},
        "category": {"type": "string"},
    }},
),
ToolDefinition(
    name="governor_check_dharma",
    description="Check Dharma compliance for a proposed action.",
    category=ToolCategory.GOVERNOR, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "tool": {"type": "string"},
        "action": {"type": "string"},
    }, "required": ["tool"]},
),
ToolDefinition(
    name="governor_stats",
    description="Get Governor statistics and rate limit status.",
    category=ToolCategory.GOVERNOR, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="governor_validate_path",
    description="Validate a multi-step tool execution path.",
    category=ToolCategory.GOVERNOR, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "tools": {"type": "array", "items": {"type": "string"}, "description": "Ordered list of tool names"},
    }, "required": ["tools"]},
),

# ═══════════════════════════════════════════════════════════════════
# Grimoire (2 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="grimoire_list",
    description="List available grimoire spells.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "category": {"type": "string"},
    }},
),
ToolDefinition(
    name="grimoire_read",
    description="Read a grimoire spell's details.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "spell": {"type": "string", "description": "Spell name or ID"},
    }, "required": ["spell"]},
),

# ═══════════════════════════════════════════════════════════════════
# Homeostasis (2 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="homeostasis.check",
    description="Manually trigger a homeostatic health check and corrective actions.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.WRITE,
    input_schema=_S,
),
ToolDefinition(
    name="homeostasis.status",
    description="Get homeostatic loop status and recent actions.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Immune System (2 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="immune_scan",
    description="Run an immune system scan for anomalies.",
    category=ToolCategory.SECURITY, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="immune_heal",
    description="Trigger immune system healing for detected issues.",
    category=ToolCategory.SECURITY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "issue_id": {"type": "string"},
    }},
),

# ═══════════════════════════════════════════════════════════════════
# Memory Extensions (4 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="memory.consolidate",
    description="Trigger memory consolidation (merge similar, strengthen important).",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "dry_run": {"type": "boolean", "default": False},
        "limit": {"type": "integer", "default": 100},
    }},
),
ToolDefinition(
    name="memory.consolidation_stats",
    description="Get memory consolidation statistics.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="memory.lifecycle_stats",
    description="Get memory lifecycle statistics (creation, access, decay).",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="memory.lifecycle_sweep",
    description="Run a lifecycle sweep (evaluate retention scores, rotate).",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "dry_run": {"type": "boolean", "default": False},
        "limit": {"type": "integer", "default": 500},
    }},
),

# ═══════════════════════════════════════════════════════════════════
# Anomaly Detection (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="anomaly.check",
    description="Run an anomaly detection check on system metrics.",
    category=ToolCategory.INTROSPECTION, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="anomaly.history",
    description="Get anomaly detection history.",
    category=ToolCategory.INTROSPECTION, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 20},
    }},
),
ToolDefinition(
    name="anomaly.status",
    description="Get anomaly detector status.",
    category=ToolCategory.INTROSPECTION, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Ensemble (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="ensemble.query",
    description="Query the ensemble reasoning system.",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string"},
    }, "required": ["query"]},
),
ToolDefinition(
    name="ensemble.history",
    description="Get ensemble reasoning history.",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 10},
    }},
),
ToolDefinition(
    name="ensemble.status",
    description="Get ensemble system status.",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Galaxy Extensions (2 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="galaxy.backup",
    description="Backup a galaxy database.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "galaxy": {"type": "string"},
    }},
),
ToolDefinition(
    name="galaxy.restore",
    description="Restore a galaxy database from backup.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "backup_path": {"type": "string"},
    }, "required": ["backup_path"]},
),

# ═══════════════════════════════════════════════════════════════════
# OTel Observability (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="otel.status",
    description="Get OpenTelemetry collector status.",
    category=ToolCategory.METRICS, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="otel.metrics",
    description="Get OpenTelemetry metrics summary.",
    category=ToolCategory.METRICS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "metric": {"type": "string"},
        "limit": {"type": "integer", "default": 20},
    }},
),
ToolDefinition(
    name="otel.spans",
    description="Get recent OpenTelemetry spans.",
    category=ToolCategory.METRICS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 20},
    }},
),

# ═══════════════════════════════════════════════════════════════════
# Pipeline Extensions (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="pipeline.create",
    description="Create a multi-step tool pipeline.",
    category=ToolCategory.TASK, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "name": {"type": "string"},
        "steps": {"type": "array", "items": {"type": "object"}},
    }, "required": ["name", "steps"]},
),
ToolDefinition(
    name="pipeline.list",
    description="List available pipelines.",
    category=ToolCategory.TASK, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="pipeline.status",
    description="Get pipeline execution status.",
    category=ToolCategory.TASK, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "pipeline_id": {"type": "string"},
    }, "required": ["pipeline_id"]},
),

# ═══════════════════════════════════════════════════════════════════
# Sangha Lock (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="sangha_lock_acquire",
    description="Acquire a distributed resource lock.",
    category=ToolCategory.BROKER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "resource": {"type": "string"},
        "ttl_seconds": {"type": "integer", "default": 60},
    }, "required": ["resource"]},
),
ToolDefinition(
    name="sangha_lock_release",
    description="Release a distributed resource lock.",
    category=ToolCategory.BROKER, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "resource": {"type": "string"},
        "lock_id": {"type": "string"},
    }, "required": ["resource"]},
),
ToolDefinition(
    name="sangha_lock_list",
    description="List active distributed locks.",
    category=ToolCategory.BROKER, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Rust Bridge Extensions (2 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="rust_audit",
    description="Run a Rust bridge audit (check all accelerator modules).",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="rust_compress",
    description="Compress data using the Rust zstd accelerator.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "data": {"type": "string"},
        "level": {"type": "integer", "default": 3},
    }, "required": ["data"]},
),

# ═══════════════════════════════════════════════════════════════════
# Starter Packs (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="starter_packs.list",
    description="List available starter packs for new installations.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="starter_packs.get",
    description="Get details of a specific starter pack.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "pack_id": {"type": "string"},
    }, "required": ["pack_id"]},
),
ToolDefinition(
    name="starter_packs.suggest",
    description="Suggest a starter pack based on system state.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Verification (3 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="verification.request",
    description="Request verification for a tool output or claim.",
    category=ToolCategory.GOVERNANCE, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "claim": {"type": "string"},
        "evidence": {"type": "string"},
    }, "required": ["claim"]},
),
ToolDefinition(
    name="verification.attest",
    description="Attest to a verification request.",
    category=ToolCategory.GOVERNANCE, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "request_id": {"type": "string"},
        "verdict": {"type": "string", "enum": ["confirmed", "denied", "uncertain"]},
    }, "required": ["request_id", "verdict"]},
),
ToolDefinition(
    name="verification.status",
    description="Get verification request status.",
    category=ToolCategory.GOVERNANCE, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "request_id": {"type": "string"},
    }, "required": ["request_id"]},
),

# ═══════════════════════════════════════════════════════════════════
# Edge (1 tool)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="edge_add_rule",
    description="Add a rule to the edge inference engine.",
    category=ToolCategory.EDGE, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "pattern": {"type": "string"},
        "action": {"type": "string"},
    }, "required": ["pattern", "action"]},
),

# ═══════════════════════════════════════════════════════════════════
# Miscellaneous (12 tools)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="anti_loop_check",
    description="Check for repetitive tool call loops and suggest breaking patterns.",
    category=ToolCategory.GOVERNANCE, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "tool_history": {"type": "array", "items": {"type": "string"}},
    }},
),
ToolDefinition(
    name="capability_harness",
    description="Test a capability by running it in a sandboxed harness.",
    category=ToolCategory.INTROSPECTION, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "capability": {"type": "string"},
    }, "required": ["capability"]},
),
ToolDefinition(
    name="cast_oracle",
    description="Cast an oracle reading (I Ching divination).",
    category=ToolCategory.GARDEN, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "question": {"type": "string"},
    }},
),
ToolDefinition(
    name="coherence_boost",
    description="Boost system coherence by running targeted maintenance.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.WRITE,
    input_schema=_S,
),
ToolDefinition(
    name="garden_synergy",
    description="Check synergy between gardens and suggest activations.",
    category=ToolCategory.GARDEN, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="get_agent_capabilities",
    description="Get capabilities of a registered agent.",
    category=ToolCategory.AGENT, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "agent_id": {"type": "string"},
    }, "required": ["agent_id"]},
),
ToolDefinition(
    name="resonance_trace",
    description="Trace PRAT resonance flow across tool calls.",
    category=ToolCategory.INTROSPECTION, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 10},
    }},
),
ToolDefinition(
    name="thought_clone",
    description="Clone a memory or thought with optional modifications.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "memory_id": {"type": "string"},
        "modifications": {"type": "object"},
    }, "required": ["memory_id"]},
),
ToolDefinition(
    name="token_report",
    description="Estimate token usage for a set of memories or content.",
    category=ToolCategory.METRICS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "content": {"type": "string"},
        "memory_ids": {"type": "array", "items": {"type": "string"}},
    }},
),
ToolDefinition(
    name="tool.graph_full",
    description="Get the full tool dependency graph.",
    category=ToolCategory.INTROSPECTION, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="wu_xing_balance",
    description="Get the current Wu Xing (Five Elements) balance.",
    category=ToolCategory.GARDEN, safety=ToolSafety.READ,
    input_schema=_S,
),

]
