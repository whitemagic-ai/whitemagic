"""Living Graph Tools — v14.0 graph topology, hybrid recall, surprise gate.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
    ToolDefinition(
        name="hybrid_recall",
        description="Multi-hop graph-aware memory recall. Combines BM25 + embedding anchor search with graph walk expansion to discover memories connected via the association graph.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "Search query for anchor memories"},
                "hops": {"type": "integer", "description": "Number of graph hops (default: 2)", "default": 2},
                "anchor_limit": {"type": "integer", "description": "Max anchor memories from initial search (default: 5)", "default": 5},
                "final_limit": {"type": "integer", "description": "Max total results to return (default: 10)", "default": 10},
            },
            "required": ["query"],
        },
    ),
    ToolDefinition(
        name="graph_topology",
        description="Graph topology introspection — centrality metrics, community detection, bridge nodes, echo chamber detection. Actions: summary, rebuild, centrality, communities, bridges, echo_chambers.",
        category=ToolCategory.INTROSPECTION,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "action": {
                    "type": "string",
                    "description": "Action to perform: summary, rebuild, centrality, communities, bridges, echo_chambers",
                    "default": "summary",
                    "enum": ["summary", "rebuild", "centrality", "communities", "bridges", "echo_chambers"],
                },
                "top_n": {"type": "integer", "description": "Max results for bridges (default: 10)", "default": 10},
                "sample_limit": {"type": "integer", "description": "Max edges to load for rebuild (default: 50000)", "default": 50000},
                "sigma_threshold": {"type": "number", "description": "Sigma threshold for echo chamber detection (default: 2.0)", "default": 2.0},
            },
        },
    ),
    ToolDefinition(
        name="graph_walk",
        description="Execute a multi-hop weighted random walk from seed memory IDs. Returns traversal paths with edge weights and relation types.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "seed_ids": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "List of memory IDs to start walking from",
                },
                "hops": {"type": "integer", "description": "Number of hops (default: 2)", "default": 2},
                "top_k": {"type": "integer", "description": "Max paths to return (default: 10)", "default": 10},
            },
            "required": ["seed_ids"],
        },
    ),
    ToolDefinition(
        name="surprise_stats",
        description="Surprise gate statistics — shows novelty detection metrics including total evaluations, novel/redundant/normal counts, and thresholds.",
        category=ToolCategory.INTROSPECTION,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {},
        },
    ),
    ToolDefinition(
        name="entity_resolve",
        description="Run embedding-based entity resolution (dedup) on the memory store. Finds near-duplicate memories and merges them by reinforcing the canonical and pushing duplicates to FAR_EDGE.",
        category=ToolCategory.MEMORY,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "similarity_threshold": {"type": "number", "description": "Cosine similarity threshold (default: 0.92)", "default": 0.92},
                "batch_limit": {"type": "integer", "description": "Max pairs to evaluate (default: 500)", "default": 500},
            },
        },
    ),
]
