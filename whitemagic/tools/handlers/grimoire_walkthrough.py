"""MCP handler for Grimoire Interactive Walkthrough — guided 28-chapter tour."""

from typing import Any

# Full 28-chapter walkthrough data
CHAPTERS: list[dict[str, Any]] = [
    # EASTERN QUADRANT (Spring/Wood) — Yang Rising
    {"ch": 1, "gana": "Horn", "name": "Session Initiation", "garden": "courage",
     "quadrant": "eastern", "element": "wood",
     "when": "Starting new session, receiving handoff, restoring context",
     "tools": ["session_bootstrap", "search_memories"],
     "exercise": "Call session_bootstrap to initialize, then search_memories for recent context."},
    {"ch": 2, "gana": "Neck", "name": "Memory Presence", "garden": "presence",
     "quadrant": "eastern", "element": "wood",
     "when": "Establishing memory foundation, connecting to past work",
     "tools": ["create_memory", "search_memories"],
     "exercise": "Create a memory about your current goal, then search for related memories."},
    {"ch": 3, "gana": "Root", "name": "System Foundation", "garden": "practice",
     "quadrant": "eastern", "element": "wood",
     "when": "Verifying system health, diagnosing issues",
     "tools": ["gnosis", "harmony_vector"],
     "exercise": "Call gnosis(compact=true) to check system health."},
    {"ch": 4, "gana": "Room", "name": "Resource Sanctuary", "garden": "sanctuary",
     "quadrant": "eastern", "element": "wood",
     "when": "Locking resources, preventing conflicts",
     "tools": ["sangha_lock_acquire", "sangha_lock_release"],
     "exercise": "Acquire a lock on a shared resource, then release it."},
    {"ch": 5, "gana": "Heart", "name": "Context Connection", "garden": "love",
     "quadrant": "eastern", "element": "wood",
     "when": "Connecting to work context, understanding relationships",
     "tools": ["search_memories", "tool.graph"],
     "exercise": "Use tool.graph to explore tool dependencies for your current task."},
    {"ch": 6, "gana": "Tail", "name": "Performance Drive", "garden": "adventure",
     "quadrant": "eastern", "element": "wood",
     "when": "Accelerating work, optimizing performance",
     "tools": ["rust_status", "get_metrics_summary"],
     "exercise": "Check Rust bridge status and view metrics summary."},
    {"ch": 7, "gana": "WinnowingBasket", "name": "Consolidation", "garden": "truth",
     "quadrant": "eastern", "element": "wood",
     "when": "Separating signal from noise, consolidating learnings",
     "tools": ["memory.consolidate", "track_metric"],
     "exercise": "Run memory.consolidate to cluster and promote memories."},
    # SOUTHERN QUADRANT (Summer/Fire) — Yang Peak
    {"ch": 8, "gana": "Ghost", "name": "Metrics & Introspection", "garden": "mystery",
     "quadrant": "southern", "element": "fire",
     "when": "Tracking metrics, investigating mysteries",
     "tools": ["track_metric", "get_metrics_summary"],
     "exercise": "Track a custom metric, then view the summary."},
    {"ch": 9, "gana": "Willow", "name": "Adaptive Play", "garden": "play",
     "quadrant": "southern", "element": "fire",
     "when": "Flexibility, creative adaptation, experimentation",
     "tools": ["grimoire_suggest", "serendipity_surface"],
     "exercise": "Call serendipity_surface to discover unexpected memory connections."},
    {"ch": 10, "gana": "Star", "name": "Illumination", "garden": "wisdom",
     "quadrant": "southern", "element": "fire",
     "when": "Context synthesis, wisdom retrieval",
     "tools": ["cast_oracle", "reasoning.bicameral"],
     "exercise": "Use reasoning.bicameral to analyze a question from two perspectives."},
    {"ch": 11, "gana": "ExtendedNet", "name": "Resonance Network", "garden": "connection",
     "quadrant": "southern", "element": "fire",
     "when": "Gan Ying bus, network effects, system resonance",
     "tools": ["ganying_emit", "ganying_history"],
     "exercise": "View ganying_history to see recent system events."},
    {"ch": 12, "gana": "Wings", "name": "Parallel Creation", "garden": "creation",
     "quadrant": "southern", "element": "fire",
     "when": "Expansion, parallel execution, building at scale",
     "tools": ["pipeline.create", "pipeline.status"],
     "exercise": "Create a simple pipeline with two steps."},
    {"ch": 13, "gana": "Chariot", "name": "Codebase Navigation", "garden": "transformation",
     "quadrant": "southern", "element": "fire",
     "when": "Archaeology, codebase exploration",
     "tools": ["archaeology_scan_directory", "archaeology_search"],
     "exercise": "Scan a directory to discover files, then search for patterns."},
    {"ch": 14, "gana": "Abundance", "name": "Resource Sharing", "garden": "gratitude",
     "quadrant": "southern", "element": "fire",
     "when": "Sharing surplus, celebrating achievements",
     "tools": ["export_memories", "broker.publish"],
     "exercise": "Export memories as JSON to share your knowledge."},
    # WESTERN QUADRANT (Autumn/Metal) — Yin Rising
    {"ch": 15, "gana": "StraddlingLegs", "name": "Ethical Balance", "garden": "dharma",
     "quadrant": "western", "element": "metal",
     "when": "Moral reasoning, autoimmune checks",
     "tools": ["evaluate_ethics", "dharma_rules"],
     "exercise": "Evaluate the ethics of a hypothetical action."},
    {"ch": 16, "gana": "Mound", "name": "Strategic Patience", "garden": "patience",
     "quadrant": "western", "element": "metal",
     "when": "Waiting strategically, accumulating strength",
     "tools": ["homeostasis.status", "homeostasis.check"],
     "exercise": "Check homeostasis status — is the system in balance?"},
    {"ch": 17, "gana": "Stomach", "name": "Energy Management", "garden": "vitality",
     "quadrant": "western", "element": "metal",
     "when": "Energy flow, vitality management",
     "tools": ["harmony_vector", "memory.lifecycle_stats"],
     "exercise": "Read the Harmony Vector to assess system energy."},
    {"ch": 18, "gana": "HairyHead", "name": "Detailed Attention", "garden": "excellence",
     "quadrant": "western", "element": "metal",
     "when": "Debugging, microscopic focus, precision",
     "tools": ["salience.spotlight", "pattern_search"],
     "exercise": "Use salience.spotlight to find the most urgent system events."},
    {"ch": 19, "gana": "Net", "name": "Pattern Capture", "garden": "mystery",
     "quadrant": "western", "element": "metal",
     "when": "Detecting patterns, similar problems",
     "tools": ["pattern_search", "cluster_stats"],
     "exercise": "Search for patterns in recent memories."},
    {"ch": 20, "gana": "TurtleBeak", "name": "Precise Validation", "garden": "truth",
     "quadrant": "western", "element": "metal",
     "when": "Rigorous testing, verification",
     "tools": ["ship.check", "governor_validate"],
     "exercise": "Run ship.check to verify deployment readiness."},
    {"ch": 21, "gana": "ThreeStars", "name": "Wisdom Council", "garden": "reverence",
     "quadrant": "western", "element": "metal",
     "when": "Multi-perspective deliberation, I Ching",
     "tools": ["cast_oracle", "reasoning.bicameral"],
     "exercise": "Cast an I Ching oracle about your project's direction."},
    # NORTHERN QUADRANT (Winter/Water) — Yin Peak
    {"ch": 22, "gana": "Dipper", "name": "Governance", "garden": "depth",
     "quadrant": "northern", "element": "water",
     "when": "Managing system state, governing policies",
     "tools": ["maturity.assess", "set_dharma_profile"],
     "exercise": "Assess maturity stage and review which tools are unlocked."},
    {"ch": 23, "gana": "Ox", "name": "Endurance", "garden": "practice",
     "quadrant": "northern", "element": "water",
     "when": "Long-running tasks, persistence",
     "tools": ["gnosis", "health_report"],
     "exercise": "Run a deep gnosis check during a long session."},
    {"ch": 24, "gana": "Girl", "name": "Nurture", "garden": "joy",
     "quadrant": "northern", "element": "water",
     "when": "Building user profiles, personalization",
     "tools": ["create_memory", "garden_activate"],
     "exercise": "Activate the 'joy' garden and create a memory about what brings you energy."},
    {"ch": 25, "gana": "Void", "name": "Emptiness", "garden": "stillness",
     "quadrant": "northern", "element": "water",
     "when": "Meditative pause, clearing context",
     "tools": ["dream_start", "dream_status"],
     "exercise": "Start the Dream Cycle and check its status."},
    {"ch": 26, "gana": "Roof", "name": "Shelter", "garden": "sanctuary",
     "quadrant": "northern", "element": "water",
     "when": "Protecting resources, establishing secure boundaries",
     "tools": ["sangha_lock_acquire", "check_boundaries"],
     "exercise": "Check boundaries on a sensitive operation."},
    {"ch": 27, "gana": "Encampment", "name": "Structure", "garden": "order",
     "quadrant": "northern", "element": "water",
     "when": "Finalizing structures, preparing for deployment",
     "tools": ["checkpoint_session", "ship.check"],
     "exercise": "Checkpoint your session and run a ship check."},
    {"ch": 28, "gana": "Wall", "name": "Boundaries", "garden": "protection",
     "quadrant": "northern", "element": "water",
     "when": "Ending session, creating handoff",
     "tools": ["session_handoff", "checkpoint_session"],
     "exercise": "Create a session handoff for the next agent."},
]


def handle_grimoire_walkthrough(**kwargs: Any) -> dict[str, Any]:
    """Get walkthrough data for a specific chapter or range."""
    chapter = kwargs.get("chapter")
    quadrant = kwargs.get("quadrant")

    if chapter is not None:
        ch = int(chapter)
        if 1 <= ch <= 28:
            data = CHAPTERS[ch - 1]
            # Add navigation
            data["prev"] = ch - 1 if ch > 1 else 28
            data["next"] = ch + 1 if ch < 28 else 1
            return {"status": "success", **data}
        return {"status": "error", "error": f"Chapter must be 1-28, got {ch}"}

    if quadrant:
        q = quadrant.lower()
        filtered = [c for c in CHAPTERS if c["quadrant"] == q]
        return {
            "status": "success",
            "quadrant": q,
            "chapters": filtered,
            "count": len(filtered),
        }

    # Return overview
    return {
        "status": "success",
        "total_chapters": 28,
        "quadrants": {
            "eastern": {"element": "wood", "season": "spring", "chapters": "1-7", "theme": "Foundation"},
            "southern": {"element": "fire", "season": "summer", "chapters": "8-14", "theme": "Expansion"},
            "western": {"element": "metal", "season": "autumn", "chapters": "15-21", "theme": "Refinement"},
            "northern": {"element": "water", "season": "winter", "chapters": "22-28", "theme": "Integration"},
        },
        "usage": "Call with chapter=N for detailed walkthrough, or quadrant=eastern/southern/western/northern",
    }
