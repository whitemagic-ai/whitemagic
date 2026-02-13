"""v15.6 Cognitive Extensions — Registry definitions for 11 new tools.

New capabilities:
- Cross-encoder reranking (precision search)
- Working memory (bounded attentional bottleneck)
- Memory reconsolidation (labile state updates on retrieval)
- Incremental community maintenance (label propagation)
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [

# ═══════════════════════════════════════════════════════════════════
# Cross-Encoder Reranking
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="rerank",
    description="Rerank search results using cross-encoder model or BM25 lexical fallback for higher precision.",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string", "description": "The search query"},
        "results": {"type": "array", "description": "List of result dicts with id, title, content, score"},
        "top_k": {"type": "integer", "description": "Number of results to return", "default": 10},
        "strategy": {"type": "string", "enum": ["auto", "cross_encoder", "lexical"], "default": "auto"},
    }, "required": ["query", "results"]},
),
ToolDefinition(
    name="rerank.status",
    description="Get reranker status (cross-encoder availability, fallback mode).",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

# ═══════════════════════════════════════════════════════════════════
# Working Memory (Bounded Attentional Bottleneck)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="working_memory.attend",
    description="Bring a memory into working memory focus. LRU eviction when at capacity (7±2 chunks).",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "memory_id": {"type": "string", "description": "Memory ID to attend to"},
        "content": {"type": "string", "description": "Memory content"},
        "title": {"type": "string", "description": "Optional title"},
        "importance": {"type": "number", "description": "0.0-1.0 importance weight", "default": 0.5},
    }, "required": ["memory_id", "content"]},
),
ToolDefinition(
    name="working_memory.context",
    description="Get current working memory contents sorted by activation, for prompt injection.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "max_tokens": {"type": "integer", "description": "Optional token budget (chars/4)"},
    }},
),
ToolDefinition(
    name="working_memory.status",
    description="Get working memory status: capacity, used slots, chunks, eviction stats.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

# ═══════════════════════════════════════════════════════════════════
# Memory Reconsolidation (Labile State Updates)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="reconsolidation.mark",
    description="Mark a retrieved memory as labile (modifiable). Within the 5-minute window, it can be updated with new context.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "memory_id": {"type": "string", "description": "Memory ID to mark labile"},
        "content": {"type": "string", "description": "Current memory content"},
        "tags": {"type": "array", "items": {"type": "string"}, "description": "Current tags"},
        "query": {"type": "string", "description": "Query that triggered retrieval"},
    }, "required": ["memory_id", "content"]},
),
ToolDefinition(
    name="reconsolidation.update",
    description="Update a labile memory with new context before reconsolidation.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "memory_id": {"type": "string", "description": "Memory ID to update"},
        "new_context": {"type": "string", "description": "Additional context to append"},
        "new_tags": {"type": "array", "items": {"type": "string"}, "description": "New tags to merge"},
        "annotation": {"type": "string", "description": "Note about why the update happened"},
    }, "required": ["memory_id"]},
),
ToolDefinition(
    name="reconsolidation.status",
    description="Get reconsolidation engine status: labile count, stats, pending updates.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

# ═══════════════════════════════════════════════════════════════════
# Incremental Community Maintenance
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="community.propagate",
    description="Propagate community label from neighbors to a new memory via label propagation.",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "memory_id": {"type": "string", "description": "New memory ID"},
        "neighbors": {"type": "array", "description": "List of [neighbor_id, weight] pairs"},
        "memory_tags": {"type": "array", "items": {"type": "string"}, "description": "Tags for labeling"},
    }, "required": ["memory_id"]},
),
ToolDefinition(
    name="community.status",
    description="Get community maintenance status: communities, members, stats.",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),
ToolDefinition(
    name="community.health",
    description="Check community health — detect oversized or orphaned communities.",
    category=ToolCategory.SYNTHESIS, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {}},
),

]
