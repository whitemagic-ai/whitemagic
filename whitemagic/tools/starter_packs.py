"""Tool Starter Packs — Curated minimal tool sets for common agent workflows.
===========================================================================
AI agents hitting 130+ tools face a discovery problem. Starter packs solve
this by providing curated, task-focused tool lists with descriptions of
*when* to use each tool and *what order* makes sense.

Usage:
    from whitemagic.tools.starter_packs import get_pack, list_packs

    pack = get_pack("memory")
    # Returns: {"name": "memory", "description": ..., "tools": [...]}
"""

from typing import Any

# ---------------------------------------------------------------------------
# Pack definitions
# ---------------------------------------------------------------------------

_PACKS: dict[str, dict[str, Any]] = {
    "quickstart": {
        "description": (
            "Essential tools for a brand-new agent. Start here to understand "
            "the system, check health, and perform basic operations."
        ),
        "tools": [
            {"name": "gnosis", "when": "First call — get system health (use compact=true to save tokens)"},
            {"name": "capabilities", "when": "Discover all available tools"},
            {"name": "session_bootstrap", "when": "Initialize a working session"},
            {"name": "maturity.assess", "when": "Check what tools you're allowed to use"},
            {"name": "create_memory", "when": "Store something important"},
            {"name": "search_memories", "when": "Find something you stored before"},
        ],
    },
    "memory": {
        "description": (
            "Memory-focused workflow: create, search, consolidate, and manage "
            "memory lifecycle. For agents that primarily store/retrieve knowledge."
        ),
        "tools": [
            {"name": "create_memory", "when": "Store a new memory with tags and importance"},
            {"name": "search_memories", "when": "Semantic search across all memories"},
            {"name": "fast_read_memory", "when": "Read a specific memory by ID"},
            {"name": "batch_read_memories", "when": "Read multiple memories at once"},
            {"name": "memory.consolidate", "when": "Cluster and synthesize related memories"},
            {"name": "memory.retention_sweep", "when": "Evaluate what to keep/forget (dry_run=true first)"},
            {"name": "memory.lifecycle_stats", "when": "Check memory system health"},
        ],
    },
    "coordination": {
        "description": (
            "Multi-agent coordination: register agents, distribute tasks, "
            "run votes, and use pipelines. For orchestrating multi-agent work."
        ),
        "tools": [
            {"name": "agent.register", "when": "Register this agent with identity and capabilities"},
            {"name": "agent.list", "when": "Discover other agents and their capabilities"},
            {"name": "task.distribute", "when": "Create a task and assign to agents"},
            {"name": "task.status", "when": "Check task progress"},
            {"name": "vote.create", "when": "Start a vote session for group decisions"},
            {"name": "vote.cast", "when": "Cast a vote in an active session"},
            {"name": "pipeline.create", "when": "Chain multiple tools together with variable passing"},
            {"name": "broker.publish", "when": "Send messages to other agents via Redis"},
        ],
    },
    "introspection": {
        "description": (
            "System health and debugging: monitor harmony, check breakers, "
            "review karma, inspect the attention spotlight."
        ),
        "tools": [
            {"name": "gnosis", "when": "Full system snapshot (or compact=true for alerts only)"},
            {"name": "harmony_vector", "when": "7-dimensional health pulse"},
            {"name": "karma_report", "when": "Side-effect audit (declared vs actual)"},
            {"name": "homeostasis.status", "when": "Self-regulation loop status"},
            {"name": "salience.spotlight", "when": "What the system considers most important right now"},
            {"name": "tool.graph", "when": "Tool dependency relationships for planning"},
            {"name": "ship.check", "when": "Full health report including infra"},
        ],
    },
    "reasoning": {
        "description": (
            "Deep analysis and synthesis: dual-hemisphere reasoning, pattern "
            "detection, serendipity surfacing."
        ),
        "tools": [
            {"name": "reasoning.bicameral", "when": "Dual-hemisphere analysis (precise vs creative)"},
            {"name": "thought_clone", "when": "Spawn parallel thought clones for a problem"},
            {"name": "pattern_search", "when": "Find patterns across memory and events"},
            {"name": "serendipity_surface", "when": "Surface unexpected connections"},
            {"name": "solve_optimization", "when": "Multi-strategy optimization solver"},
            {"name": "kaizen_analyze", "when": "Continuous improvement analysis"},
        ],
    },
    "safety": {
        "description": (
            "Ethics, governance, and safety: evaluate actions, check boundaries, "
            "manage dharma rules, audit karma."
        ),
        "tools": [
            {"name": "evaluate_ethics", "when": "Dharma evaluation of a proposed action"},
            {"name": "check_boundaries", "when": "Check if an action crosses safety boundaries"},
            {"name": "dharma_rules", "when": "List active rules and profile"},
            {"name": "set_dharma_profile", "when": "Switch between default/creative/secure profiles"},
            {"name": "karmic_trace", "when": "Immutable audit trail of all Dharma evaluations"},
            {"name": "governor_check_drift", "when": "Check if actions are drifting from goals"},
            {"name": "maturity.assess", "when": "Check developmental stage and tool locks"},
        ],
    },
    # ---- Tier-specific packs (mapped to docs/PRODUCT_TIERS.md) ----
    "free_tier": {
        "description": (
            "Essential tools for Free tier users — personal AI that remembers you. "
            "Covers basic memory, health checks, and a handful of galaxies."
        ),
        "tools": [
            {"name": "gnosis", "when": "System health snapshot (use compact=true)"},
            {"name": "create_memory", "when": "Store preferences, recurring topics, important people"},
            {"name": "search_memories", "when": "Find something you stored before"},
            {"name": "galaxy.create", "when": "Create a personal memory space (e.g. 'Life', 'Work')"},
            {"name": "galaxy.switch", "when": "Switch between your memory spaces"},
            {"name": "memory.lifecycle_sweep", "when": "Let the system tidy old memories automatically"},
        ],
    },
    "pro_tier": {
        "description": (
            "Tools for Pro tier — freelancers, consultants, coaches. "
            "Client-aware memory, workflows, and light CRM capabilities."
        ),
        "tools": [
            {"name": "galaxy.create", "when": "Create one galaxy per client for isolated context"},
            {"name": "galaxy.switch", "when": "Switch to a client's galaxy before their session"},
            {"name": "galaxy.ingest", "when": "Bulk-import client documents into their galaxy"},
            {"name": "hybrid_recall", "when": "Cross-session contextual search across client history"},
            {"name": "pipeline.create", "when": "Automate post-session workflows (summarize, update goals)"},
            {"name": "archaeology_search", "when": "Deep-dive into client history across all sessions"},
            {"name": "gratitude.stats", "when": "Track which clients have tipped (for prioritization)"},
        ],
    },
    "dev_tier": {
        "description": (
            "Tools for Dev tier — engineers building on WhiteMagic as a platform. "
            "Full API access, repo-scoped memory, CLI integration, agentic loops."
        ),
        "tools": [
            {"name": "capabilities", "when": "Discover all available tools and feature flags"},
            {"name": "tool.graph", "when": "Understand tool dependencies for planning"},
            {"name": "grimoire_cast", "when": "Execute named tool sequences (reusable workflows)"},
            {"name": "ollama.agent", "when": "Run an agentic loop with a local LLM + WhiteMagic tools"},
            {"name": "starter_packs.list", "when": "Discover all workflow starter packs"},
            {"name": "pipeline.create", "when": "Chain tools together with variable passing"},
            {"name": "explain_this", "when": "Pre-execution impact preview of any tool"},
        ],
    },
    "team_tier": {
        "description": (
            "Tools for Team tier — shared team brain, multi-agent coordination, "
            "role-based access, and process anchors."
        ),
        "tools": [
            {"name": "agent.register", "when": "Register agents with roles and capabilities"},
            {"name": "agent.list", "when": "Discover other agents on the team"},
            {"name": "task.distribute", "when": "Assign tasks to agents based on capabilities"},
            {"name": "vote.create", "when": "Start a team vote for group decisions"},
            {"name": "broker.publish", "when": "Send messages to other agents via Redis pub/sub"},
            {"name": "session.handoff", "when": "Transfer context between agents or to humans"},
            {"name": "swarm.decompose", "when": "Break complex tasks into subtasks for the team"},
        ],
    },
    "enterprise_tier": {
        "description": (
            "Tools for Enterprise tier — governed AI layer with full audit, "
            "custom policies, security monitoring, and compliance features."
        ),
        "tools": [
            {"name": "dharma_rules", "when": "Review and manage ethical policy rules"},
            {"name": "set_dharma_profile", "when": "Switch between default/creative/secure profiles"},
            {"name": "karma_report", "when": "Full side-effect audit (declared vs actual)"},
            {"name": "karmic_trace", "when": "Immutable audit trail for compliance"},
            {"name": "governor_validate", "when": "Strategic oversight and goal alignment"},
            {"name": "security.monitor_status", "when": "Real-time anomaly detection"},
            {"name": "mcp_integrity.verify", "when": "SHA-256 schema fingerprint verification"},
            {"name": "audit.export", "when": "Export audit logs in MCP-compatible format"},
        ],
    },
}


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def list_packs() -> list[dict[str, Any]]:
    """Return a list of available starter packs with descriptions."""
    return [
        {"name": name, "description": pack["description"], "tool_count": len(pack["tools"])}
        for name, pack in _PACKS.items()
    ]


def get_pack(name: str) -> dict[str, Any] | None:
    """Get a specific starter pack by name."""
    pack = _PACKS.get(name)
    if pack is None:
        return None
    return {
        "name": name,
        "description": pack["description"],
        "tool_count": len(pack["tools"]),
        "tools": pack["tools"],
    }


def suggest_pack(context: str) -> dict[str, Any]:
    """Suggest the best starter pack based on a natural language description."""
    context_lower = context.lower()

    # Simple keyword matching for pack suggestion
    scores: dict[str, int] = {}
    keywords = {
        "quickstart": ["start", "begin", "new", "hello", "first", "setup", "init"],
        "memory": ["memory", "remember", "store", "recall", "forget", "knowledge", "learn"],
        "coordination": ["agent", "task", "vote", "pipeline", "coordinate", "team", "distribute", "multi"],
        "introspection": ["health", "status", "debug", "monitor", "check", "inspect", "diagnose"],
        "reasoning": ["think", "reason", "analyze", "solve", "pattern", "creative", "synthesize"],
        "safety": ["safe", "ethic", "dharma", "boundar", "govern", "karma", "rule", "secure"],
        "free_tier": ["personal", "casual", "simple", "basic", "free", "newcomer", "everyday"],
        "pro_tier": ["client", "freelanc", "consult", "coach", "crm", "session", "professional"],
        "dev_tier": ["develop", "sdk", "api", "build", "engineer", "code", "platform", "ollama"],
        "team_tier": ["team", "collaborat", "shared", "org", "startup", "agency", "onboard"],
        "enterprise_tier": ["enterprise", "compliance", "audit", "sso", "govern", "regulated", "on-prem"],
    }

    for pack_name, kws in keywords.items():
        scores[pack_name] = sum(1 for kw in kws if kw in context_lower)

    best = max(scores, key=scores.get)  # type: ignore[arg-type]
    if scores[best] == 0:
        best = "quickstart"

    pack = get_pack(best)
    return {
        "suggested_pack": best,
        "confidence": min(scores[best] / 3.0, 1.0),
        "pack": pack,
        "all_packs": [p["name"] for p in list_packs()],
    }
