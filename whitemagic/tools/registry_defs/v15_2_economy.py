"""v15.2 Agent Economy — Registry definitions for 32 tools that exist in
dispatch_table but lack formal ToolDefinition entries.

Covers: Shelter (sovereign sandbox), OMS (optimized memory states),
ILP (streaming payments), Marketplace, Galaxy extensions (transfer/merge/sync/lineage).
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

_S = {"type": "object", "properties": {}}  # empty schema shorthand

TOOLS: list[ToolDefinition] = [

# ═══════════════════════════════════════════════════════════════════
# Sovereign Sandbox (Shelter)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="shelter.create",
    description="Create a sovereign sandbox environment for safe code execution.",
    category=ToolCategory.SECURITY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "name": {"type": "string", "description": "Sandbox name"},
        "policy": {"type": "string", "description": "Execution policy: permissive, standard, strict"},
        "timeout": {"type": "integer", "description": "Max execution time in seconds", "default": 30},
    }, "required": ["name"]},
),
ToolDefinition(
    name="shelter.execute",
    description="Execute code in a sovereign sandbox.",
    category=ToolCategory.SECURITY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "name": {"type": "string", "description": "Sandbox name"},
        "code": {"type": "string", "description": "Code to execute"},
        "language": {"type": "string", "description": "Language: python, shell", "default": "python"},
    }, "required": ["name", "code"]},
),
ToolDefinition(
    name="shelter.inspect",
    description="Inspect a sandbox's state, history, and resource usage.",
    category=ToolCategory.SECURITY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "name": {"type": "string", "description": "Sandbox name"},
    }, "required": ["name"]},
),
ToolDefinition(
    name="shelter.destroy",
    description="Destroy a sandbox and clean up resources.",
    category=ToolCategory.SECURITY, safety=ToolSafety.DELETE,
    input_schema={"type": "object", "properties": {
        "name": {"type": "string", "description": "Sandbox name"},
    }, "required": ["name"]},
),
ToolDefinition(
    name="shelter.status",
    description="Get status of all active sandboxes.",
    category=ToolCategory.SECURITY, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="shelter.policy",
    description="View or update sandbox execution policies.",
    category=ToolCategory.SECURITY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "name": {"type": "string", "description": "Sandbox name"},
        "set_policy": {"type": "string", "description": "New policy to set (optional)"},
    }},
),

# ═══════════════════════════════════════════════════════════════════
# Optimized Memory States (OMS)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="oms.export",
    description="Export memories as an optimized .mem package for agent-to-agent transfer.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string", "description": "Search query to select memories"},
        "memory_ids": {"type": "array", "items": {"type": "string"}, "description": "Specific memory IDs"},
        "format": {"type": "string", "description": "Export format: mem, json", "default": "mem"},
        "compress": {"type": "boolean", "default": True},
    }},
),
ToolDefinition(
    name="oms.import",
    description="Import a .mem package into the current galaxy.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string", "description": "Path to .mem package"},
        "merge_strategy": {"type": "string", "description": "Strategy: merge, replace, skip", "default": "merge"},
    }, "required": ["path"]},
),
ToolDefinition(
    name="oms.inspect",
    description="Inspect a .mem package without importing it.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string", "description": "Path to .mem package"},
    }, "required": ["path"]},
),
ToolDefinition(
    name="oms.verify",
    description="Verify integrity and signatures of a .mem package.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string", "description": "Path to .mem package"},
    }, "required": ["path"]},
),
ToolDefinition(
    name="oms.price",
    description="Estimate the value of a .mem package based on memory quality metrics.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "path": {"type": "string", "description": "Path to .mem package"},
        "currency": {"type": "string", "default": "XRP"},
    }},
),
ToolDefinition(
    name="oms.list",
    description="List available .mem packages in the local store.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="oms.status",
    description="Get OMS subsystem status and cache statistics.",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# ILP Streaming Payments
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="ilp.configure",
    description="Configure ILP payment pointer and streaming settings.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "payment_pointer": {"type": "string", "description": "ILP payment pointer ($...)"},
        "max_rate": {"type": "number", "description": "Max payment rate per second"},
    }, "required": ["payment_pointer"]},
),
ToolDefinition(
    name="ilp.send",
    description="Send a streaming micropayment via ILP.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "destination": {"type": "string", "description": "Destination payment pointer"},
        "amount": {"type": "number", "description": "Amount to send"},
        "currency": {"type": "string", "default": "XRP"},
        "memo": {"type": "string"},
    }, "required": ["destination", "amount"]},
),
ToolDefinition(
    name="ilp.receipt",
    description="Get receipt for a specific ILP transaction.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "transaction_id": {"type": "string"},
    }, "required": ["transaction_id"]},
),
ToolDefinition(
    name="ilp.history",
    description="Get ILP transaction history.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "limit": {"type": "integer", "default": 20},
        "direction": {"type": "string", "enum": ["sent", "received", "all"], "default": "all"},
    }},
),
ToolDefinition(
    name="ilp.balance",
    description="Get current ILP balance and pending settlements.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema=_S,
),
ToolDefinition(
    name="ilp.status",
    description="Get ILP subsystem status and connection health.",
    category=ToolCategory.SYSTEM, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Marketplace Bridge
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="marketplace.publish",
    description="Publish a .mem package or capability listing to the marketplace.",
    category=ToolCategory.AGENT, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "title": {"type": "string"},
        "description": {"type": "string"},
        "package_path": {"type": "string", "description": "Path to .mem package"},
        "price": {"type": "number"},
        "currency": {"type": "string", "default": "XRP"},
        "tags": {"type": "array", "items": {"type": "string"}},
    }, "required": ["title"]},
),
ToolDefinition(
    name="marketplace.discover",
    description="Search the marketplace for available listings.",
    category=ToolCategory.AGENT, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "query": {"type": "string"},
        "category": {"type": "string"},
        "max_price": {"type": "number"},
        "limit": {"type": "integer", "default": 20},
    }},
),
ToolDefinition(
    name="marketplace.negotiate",
    description="Negotiate terms for a marketplace listing.",
    category=ToolCategory.AGENT, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "listing_id": {"type": "string"},
        "offer_price": {"type": "number"},
        "message": {"type": "string"},
    }, "required": ["listing_id"]},
),
ToolDefinition(
    name="marketplace.complete",
    description="Complete a marketplace transaction.",
    category=ToolCategory.AGENT, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "transaction_id": {"type": "string"},
    }, "required": ["transaction_id"]},
),
ToolDefinition(
    name="marketplace.my_listings",
    description="View your published marketplace listings.",
    category=ToolCategory.AGENT, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "status": {"type": "string", "enum": ["active", "sold", "expired", "all"], "default": "active"},
    }},
),
ToolDefinition(
    name="marketplace.remove",
    description="Remove a listing from the marketplace.",
    category=ToolCategory.AGENT, safety=ToolSafety.DELETE,
    input_schema={"type": "object", "properties": {
        "listing_id": {"type": "string"},
    }, "required": ["listing_id"]},
),
ToolDefinition(
    name="marketplace.status",
    description="Get marketplace connection status and statistics.",
    category=ToolCategory.AGENT, safety=ToolSafety.READ,
    input_schema=_S,
),

# ═══════════════════════════════════════════════════════════════════
# Galaxy Extensions (v15.3-15.4)
# ═══════════════════════════════════════════════════════════════════
ToolDefinition(
    name="galaxy.transfer",
    description="Transfer memories between galaxies with coordinate re-mapping and dedup.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "source_galaxy": {"type": "string"},
        "target_galaxy": {"type": "string"},
        "memory_ids": {"type": "array", "items": {"type": "string"}},
        "query": {"type": "string", "description": "Transfer memories matching query"},
    }, "required": ["source_galaxy", "target_galaxy"]},
),
ToolDefinition(
    name="galaxy.merge",
    description="Merge one galaxy into another with content-hash dedup.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "source_galaxy": {"type": "string"},
        "target_galaxy": {"type": "string"},
        "delete_source": {"type": "boolean", "default": False},
    }, "required": ["source_galaxy", "target_galaxy"]},
),
ToolDefinition(
    name="galaxy.sync",
    description="Bidirectional sync between two galaxies.",
    category=ToolCategory.MEMORY, safety=ToolSafety.WRITE,
    input_schema={"type": "object", "properties": {
        "galaxy_a": {"type": "string"},
        "galaxy_b": {"type": "string"},
        "strategy": {"type": "string", "enum": ["merge", "newest_wins"], "default": "merge"},
    }, "required": ["galaxy_a", "galaxy_b"]},
),
ToolDefinition(
    name="galaxy.lineage",
    description="Build phylogenetic lineage tree for a memory (ancestors + descendants).",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "memory_id": {"type": "string"},
        "depth": {"type": "integer", "default": 5},
    }, "required": ["memory_id"]},
),
ToolDefinition(
    name="galaxy.taxonomy",
    description="Classify a memory using binomial nomenclature (species/genus/family/order/kingdom).",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema={"type": "object", "properties": {
        "memory_id": {"type": "string"},
    }, "required": ["memory_id"]},
),
ToolDefinition(
    name="galaxy.lineage_stats",
    description="Statistics on the phylogenetic graph (edge counts by type, galaxies connected).",
    category=ToolCategory.MEMORY, safety=ToolSafety.READ,
    input_schema=_S,
),

]
