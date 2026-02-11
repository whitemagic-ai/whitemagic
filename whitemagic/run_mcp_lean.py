#!/usr/bin/env python3
# mypy: disable-error-code=no-untyped-def
"""
WhiteMagic MCP Server — Lean Edition (v14.1)
==============================================
Uses the standard mcp SDK directly (no FastMCP overhead).
Registers 28 PRAT Gana meta-tools.  All heavy imports are
deferred to first tool invocation so the server handshakes
with clients in < 1 second.

Supports:
- stdio transport (default, for IDE integration)
- Streamable HTTP transport (--http flag, for remote/browser access)
- Server Instructions (auto-injected into AI client context)
- Per-Gana tool icons (lunar mansion symbols)
- Task execution modes (slow tools marked as task-optional)
"""

import asyncio
import json
import logging
import sys
from pathlib import Path
from typing import Any

# ── Ensure project root is on sys.path ──────────────────────────────
ROOT_DIR = Path(__file__).resolve().parent
CORE_SYSTEM_DIR = ROOT_DIR.parent
if str(CORE_SYSTEM_DIR) not in sys.path:
    sys.path.insert(0, str(CORE_SYSTEM_DIR))

# ── Logging (stderr only — stdout is the MCP transport) ─────────────
logging.basicConfig(
    level=logging.WARNING,
    stream=sys.stderr,
    format="%(levelname)s:%(name)s:%(message)s",
)
logger = logging.getLogger("wm_mcp")

# ── Standard MCP SDK imports ─────────────────────────────────────────
from mcp.server import Server
from mcp.server.stdio import stdio_server
import mcp.types as types

# ── Version ──────────────────────────────────────────────────────────
_VERSION_FILE = CORE_SYSTEM_DIR / "VERSION"
_VERSION = _VERSION_FILE.read_text().strip() if _VERSION_FILE.exists() else "14.0.0"


# ══════════════════════════════════════════════════════════════════════
# Lazy subsystem initialiser — runs once on first tool call
# ══════════════════════════════════════════════════════════════════════

_INITIALISED = False


def _ensure_init() -> None:
    """One-shot heavy initialisation (DB, singletons, Rust bridge)."""
    global _INITIALISED
    if _INITIALISED:
        return
    _INITIALISED = True
    logger.info("Lazy-initialising WhiteMagic subsystems …")
    try:
        from importlib.util import find_spec
        if find_spec("whitemagic_rs") is not None:
            logger.info("Rust bridge available")
    except Exception:
        pass


# ══════════════════════════════════════════════════════════════════════
# PRAT Gana tool definitions (static — no heavy imports needed)
# ══════════════════════════════════════════════════════════════════════

# Gana name → (short description, nested tool names)
# Built lazily so we don't need to import the registry at startup.
_GANA_CACHE: dict[str, tuple[str, list[str]]] | None = None


def _load_gana_metadata() -> dict[str, tuple[str, list[str]]]:
    """Load Gana metadata from the registry (heavy import, done once)."""
    global _GANA_CACHE
    if _GANA_CACHE is not None:
        return _GANA_CACHE

    from whitemagic.tools.registry import TOOL_REGISTRY
    from whitemagic.tools.prat_router import GANA_TO_TOOLS

    gana_descs: dict[str, str] = {}
    for td in TOOL_REGISTRY:
        if td.name.startswith("gana_"):
            gana_descs[td.name] = td.description

    _GANA_CACHE = {}
    for gana_name, desc in gana_descs.items():
        nested = GANA_TO_TOOLS.get(gana_name, [])
        _GANA_CACHE[gana_name] = (desc, nested)

    return _GANA_CACHE


# ── Static Gana list for instant tools/list (no heavy imports) ──────
# These are the 28 Gana names — hard-coded for zero-import startup.
_GANA_NAMES: list[str] = [
    "gana_horn", "gana_neck", "gana_root", "gana_room",
    "gana_heart", "gana_tail", "gana_winnowing_basket",
    "gana_ghost", "gana_willow", "gana_star",
    "gana_extended_net", "gana_wings", "gana_chariot",
    "gana_abundance", "gana_straddling_legs", "gana_mound",
    "gana_stomach", "gana_hairy_head", "gana_net",
    "gana_turtle_beak", "gana_three_stars", "gana_dipper",
    "gana_ox", "gana_girl", "gana_void", "gana_roof",
    "gana_encampment", "gana_wall",
]

# Static short descriptions (no imports needed)
_GANA_SHORT_DESC: dict[str, str] = {
    "gana_horn": "Session initialisation — bootstrap, create, resume, checkpoint",
    "gana_neck": "Core memory creation — create, update, import, delete memories",
    "gana_root": "System health — health report, rust status, state summary",
    "gana_room": "Resource locks & privacy — sangha lock, sandbox controls",
    "gana_heart": "Session context — scratchpad, handoff, context pack/status",
    "gana_tail": "Performance & acceleration — SIMD ops, cascade execution",
    "gana_winnowing_basket": "Wisdom & search — search, vector search, hybrid recall, graph walk, read memories",
    "gana_ghost": "Introspection — gnosis, telemetry, capabilities, graph topology, surprise stats",
    "gana_willow": "Resilience — rate limiter, grimoire spells & suggestions",
    "gana_star": "Governance — governor validate/set-goal/drift, dharma reload/profile",
    "gana_extended_net": "Pattern connectivity — pattern search, cluster stats, learning",
    "gana_wings": "Deployment & export — export memories, audit export, mesh",
    "gana_chariot": "Archaeology & knowledge graph — archaeology, KG extract/query/top",
    "gana_abundance": "Regeneration — dream cycle, lifecycle, serendipity, entity resolve",
    "gana_straddling_legs": "Ethics & balance — ethics eval, boundaries, consent, harmony vector",
    "gana_mound": "Metrics & caching — hologram view, metric tracking, yin-yang balance",
    "gana_stomach": "Digestion & tasks — pipeline, task distribute/status/route",
    "gana_hairy_head": "Detail & debug — salience, anomaly, otel, karma report/trace, dharma rules",
    "gana_net": "Capture & filtering — prompt render/list/reload, karma verify",
    "gana_turtle_beak": "Precision — edge/bitnet inference, edge batch, stats",
    "gana_three_stars": "Judgment & synthesis — bicameral reasoning, ensemble, optimization, kaizen",
    "gana_dipper": "Strategy — homeostasis, maturity assess, starter packs",
    "gana_ox": "Endurance — swarm decompose/route/complete/vote/plan/status, worker",
    "gana_girl": "Nurture — agent register/heartbeat/list/capabilities/deregister/trust",
    "gana_void": "Stillness — galactic dashboard, garden activate/status/health",
    "gana_roof": "Shelter — ollama models/generate/chat, model signing/verify",
    "gana_encampment": "Community — sangha chat, broker publish/history/status",
    "gana_wall": "Boundaries — vote create/cast/analyze/list/record_outcome, engagement tokens",
}

# Static per-Gana tool lists (no heavy imports needed at startup).
_GANA_TOOLS: dict[str, list[str]] = {
    "gana_horn": ["checkpoint_session", "create_session", "resume_session", "session_bootstrap"],
    "gana_neck": ["create_memory", "delete_memory", "import_memories", "update_memory"],
    "gana_root": ["health_report", "rust_similarity", "rust_status", "ship.check", "state.paths", "state.summary"],
    "gana_room": ["mcp_integrity.snapshot", "mcp_integrity.status", "mcp_integrity.verify", "sandbox.set_limits", "sandbox.status", "sandbox.violations", "sangha_lock", "security.alerts", "security.monitor_status"],
    "gana_heart": ["context.pack", "context.status", "scratchpad", "session.handoff"],
    "gana_tail": ["execute_cascade", "list_cascade_patterns", "simd.batch", "simd.cosine", "simd.status"],
    "gana_winnowing_basket": ["batch_read_memories", "fast_read_memory", "graph_walk", "hybrid_recall", "list_memories", "read_memory", "search_memories", "vector.index", "vector.search", "vector.status"],
    "gana_ghost": ["capabilities", "capability.matrix", "capability.status", "capability.suggest", "drive.event", "drive.snapshot", "explain_this", "get_telemetry_summary", "gnosis", "graph_topology", "manifest", "repo.summary", "selfmodel.alerts", "selfmodel.forecast", "surprise_stats"],
    "gana_willow": ["grimoire_auto_status", "grimoire_cast", "grimoire_recommend", "grimoire_suggest", "grimoire_walkthrough", "rate_limiter.stats"],
    "gana_star": ["dharma.reload", "governor_check_drift", "governor_set_goal", "governor_validate", "set_dharma_profile"],
    "gana_extended_net": ["cluster_stats", "learning.patterns", "learning.status", "learning.suggest", "pattern_search", "tool.graph"],
    "gana_wings": ["audit.export", "export_memories", "mesh.broadcast", "mesh.status"],
    "gana_chariot": ["archaeology", "kg.extract", "kg.query", "kg.status", "kg.top"],
    "gana_abundance": ["dream", "entity_resolve", "gratitude.benefits", "gratitude.stats", "memory.lifecycle", "memory.retention_sweep", "serendipity_mark_accessed", "serendipity_surface", "whitemagic.tip"],
    "gana_straddling_legs": ["check_boundaries", "evaluate_ethics", "get_dharma_guidance", "get_ethical_score", "harmony_vector", "verify_consent"],
    "gana_mound": ["get_metrics_summary", "get_yin_yang_balance", "record_yin_yang_activity", "track_metric", "view_hologram"],
    "gana_stomach": ["pipeline", "task.complete", "task.distribute", "task.list", "task.route_smart", "task.status"],
    "gana_hairy_head": ["anomaly", "dharma_rules", "karma_report", "karmic_trace", "otel", "salience.spotlight"],
    "gana_net": ["karma.verify_chain", "prompt.list", "prompt.reload", "prompt.render"],
    "gana_turtle_beak": ["bitnet_infer", "bitnet_status", "edge_batch_infer", "edge_infer", "edge_stats"],
    "gana_three_stars": ["ensemble", "kaizen_analyze", "kaizen_apply_fixes", "reasoning.bicameral", "solve_optimization"],
    "gana_dipper": ["homeostasis", "maturity.assess", "starter_packs"],
    "gana_ox": ["swarm.complete", "swarm.decompose", "swarm.plan", "swarm.resolve", "swarm.route", "swarm.status", "swarm.vote", "worker.status"],
    "gana_girl": ["agent.capabilities", "agent.deregister", "agent.heartbeat", "agent.list", "agent.register", "agent.trust"],
    "gana_void": ["galactic.dashboard", "galaxy.create", "galaxy.delete", "galaxy.ingest", "galaxy.list", "galaxy.status", "galaxy.switch", "garden_activate", "garden_health", "garden_status"],
    "gana_roof": ["model.hash", "model.list", "model.register", "model.signing_status", "model.verify", "ollama.agent", "ollama.chat", "ollama.generate", "ollama.models"],
    "gana_encampment": ["broker.history", "broker.publish", "broker.status", "sangha_chat_read", "sangha_chat_send"],
    "gana_wall": ["engagement.issue", "engagement.list", "engagement.revoke", "engagement.status", "engagement.validate", "vote.analyze", "vote.cast", "vote.create", "vote.list", "vote.record_outcome"],
}


def _schema_for_gana(name: str) -> dict:
    """Build the input schema for a Gana with its specific tool enum."""
    tools_list = _GANA_TOOLS.get(name, [])
    tool_prop: dict = {
        "type": "string",
        "description": "Which nested tool to invoke within this Gana.",
    }
    if tools_list:
        tool_prop["enum"] = tools_list
    return {
        "type": "object",
        "properties": {
            "tool": tool_prop,
            "args": {
                "type": "object",
                "description": "Arguments to pass to the selected tool.",
                "default": {},
            },
            "operation": {
                "type": "string",
                "enum": ["search", "analyze", "transform", "consolidate"],
                "description": "Polymorphic operation (when no specific tool is given).",
            },
        },
    }


# ══════════════════════════════════════════════════════════════════════
# Per-Gana icons (lunar mansion Chinese characters as data-URI SVGs)
# ══════════════════════════════════════════════════════════════════════

_GANA_ICONS: dict[str, str] = {
    "gana_horn":             "\u89D2",  # 角
    "gana_neck":             "\u4EA2",  # 亢
    "gana_root":             "\u6C10",  # 氐
    "gana_room":             "\u623F",  # 房
    "gana_heart":            "\u5FC3",  # 心
    "gana_tail":             "\u5C3E",  # 尾
    "gana_winnowing_basket": "\u7B95",  # 箕
    "gana_ghost":            "\u9B3C",  # 鬼
    "gana_willow":           "\u67F3",  # 柳
    "gana_star":             "\u661F",  # 星
    "gana_extended_net":     "\u5F20",  # 张
    "gana_wings":            "\u7FFC",  # 翼
    "gana_chariot":          "\u8F78",  # 轸
    "gana_abundance":        "\u8C50",  # 豐
    "gana_straddling_legs":  "\u594E",  # 奎
    "gana_mound":            "\u5A04",  # 娄
    "gana_stomach":          "\u80C3",  # 胃
    "gana_hairy_head":       "\u6634",  # 昴
    "gana_net":              "\u6BD5",  # 毕
    "gana_turtle_beak":      "\u89DC",  # 觜
    "gana_three_stars":      "\u53C2",  # 参
    "gana_dipper":           "\u6597",  # 斗
    "gana_ox":               "\u725B",  # 牛
    "gana_girl":             "\u5973",  # 女
    "gana_void":             "\u865A",  # 虚
    "gana_roof":             "\u5371",  # 危
    "gana_encampment":       "\u5BA4",  # 室
    "gana_wall":             "\u58C1",  # 壁
}


def _icon_for_gana(name: str) -> list[types.Icon] | None:
    """Generate a data-URI SVG icon for a Gana using its lunar mansion character."""
    char = _GANA_ICONS.get(name)
    if not char:
        return None
    svg = (
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40">'
        f'<circle cx="20" cy="20" r="18" fill="%23334155"/>'
        f'<text x="20" y="27" text-anchor="middle" font-size="20" fill="white">{char}</text>'
        f'</svg>'
    )
    import urllib.parse
    data_uri = "data:image/svg+xml," + urllib.parse.quote(svg)
    return [types.Icon(src=data_uri, mimeType="image/svg+xml")]


# ══════════════════════════════════════════════════════════════════════
# Slow tools — these get execution={mode: "optional"} for async tasks
# ══════════════════════════════════════════════════════════════════════

_SLOW_GANAS: set[str] = {
    "gana_abundance",       # dream cycle, lifecycle (2-7s)
    "gana_three_stars",     # kaizen analysis (3-5s)
    "gana_extended_net",    # pattern detection (1-2s)
    "gana_chariot",         # archaeology, KG operations (1-3s)
    "gana_ghost",           # graph topology rebuild (1s)
}


# ══════════════════════════════════════════════════════════════════════
# Server Instructions (loaded from markdown file)
# ══════════════════════════════════════════════════════════════════════

_INSTRUCTIONS_PATH = ROOT_DIR / "mcp_instructions.md"
_INSTRUCTIONS = ""
try:
    _INSTRUCTIONS = _INSTRUCTIONS_PATH.read_text(encoding="utf-8")
except FileNotFoundError:
    _INSTRUCTIONS = "WhiteMagic cognitive OS. Use gana_winnowing_basket to search, gana_neck to create memories."


# ══════════════════════════════════════════════════════════════════════
# Build the MCP Server
# ══════════════════════════════════════════════════════════════════════

server = Server("WhiteMagic Core")
server.instructions = _INSTRUCTIONS
server.version = _VERSION
server.website_url = "https://github.com/whitemagic-ai/whitemagic"


@server.list_tools()
async def list_tools() -> list[types.Tool]:
    """Return the 28 Gana meta-tools with per-Gana tool enums, icons, and execution modes."""
    tools: list[types.Tool] = []
    for name in _GANA_NAMES:
        desc = _GANA_SHORT_DESC.get(name, f"Gana {name}")
        kwargs: dict[str, Any] = {
            "name": name,
            "description": desc,
            "inputSchema": _schema_for_gana(name),
        }
        icons = _icon_for_gana(name)
        if icons:
            kwargs["icons"] = icons
        if name in _SLOW_GANAS:
            kwargs["execution"] = types.ToolExecution(mode=types.TASK_OPTIONAL)
        tools.append(types.Tool(**kwargs))
    return tools


def _sync_dispatch(gana: str, tool_name: str | None, tool_args: dict, operation: str | None) -> dict:
    """Run the PRAT dispatch synchronously — called from a worker thread."""
    _ensure_init()
    try:
        from whitemagic.tools.prat_router import route_prat_call
        return route_prat_call(  # type: ignore[no-any-return]
            gana,
            tool=tool_name,
            args=tool_args,
            operation=operation,
        )
    except Exception as exc:
        return {"status": "error", "error": str(exc)}


@server.call_tool()
async def call_tool(name: str, arguments: dict[str, Any] | None) -> list[types.TextContent]:
    """Dispatch a PRAT Gana call through the full WhiteMagic pipeline."""
    args = arguments or {}
    tool_name = args.get("tool")
    tool_args = args.get("args") or {}
    operation = args.get("operation")

    # Run in a thread so sync handlers that use asyncio.run() don't
    # collide with the MCP server's own event loop.
    result = await asyncio.to_thread(
        _sync_dispatch, name, tool_name, tool_args, operation,
    )

    text = json.dumps(result, indent=2, default=str)
    return [types.TextContent(type="text", text=text)]


@server.list_resources()
async def list_resources() -> list[types.Resource]:
    """Expose orientation and documentation resources."""
    return [
        types.Resource(
            uri="whitemagic://orientation/ai-primary",
            name="AI Primary",
            description="Primary orientation document for AI runtimes.",
            mimeType="text/markdown",
        ),
        types.Resource(
            uri="whitemagic://orientation/server-instructions",
            name="Server Instructions",
            description="How to use WhiteMagic — tool guide for AI clients.",
            mimeType="text/markdown",
        ),
        types.Resource(
            uri="whitemagic://orientation/system-map",
            name="System Map",
            description="Architecture overview and subsystem map.",
            mimeType="text/markdown",
        ),
    ]


@server.read_resource()
async def read_resource(uri) -> str:
    """Read a resource by URI."""
    uri_str = str(uri)
    if "ai-primary" in uri_str:
        path = CORE_SYSTEM_DIR / "AI_PRIMARY.md"
        try:
            return path.read_text(encoding="utf-8")
        except Exception as exc:
            return f"# Unavailable\n\nerror: {exc}"
    if "server-instructions" in uri_str:
        return _INSTRUCTIONS
    if "system-map" in uri_str:
        path = CORE_SYSTEM_DIR / "SYSTEM_MAP.md"
        try:
            return path.read_text(encoding="utf-8")
        except Exception as exc:
            return f"# Unavailable\n\nerror: {exc}"
    return f"# Unknown resource: {uri}"


# ══════════════════════════════════════════════════════════════════════
# Entry points — stdio (default) or HTTP
# ══════════════════════════════════════════════════════════════════════

async def main_stdio() -> None:
    """Run as stdio MCP server (default, for IDE integration)."""
    async with stdio_server() as (read_stream, write_stream):
        await server.run(read_stream, write_stream, server.create_initialization_options())


async def main_http(host: str = "127.0.0.1", port: int = 8770) -> None:
    """Run as Streamable HTTP MCP server (for remote/browser access)."""
    from starlette.applications import Starlette
    from starlette.routing import Mount
    from starlette.middleware import Middleware
    from starlette.middleware.cors import CORSMiddleware
    from mcp.server.streamable_http import StreamableHTTPServerTransport
    import uvicorn
    import uuid

    transport = StreamableHTTPServerTransport(
        mcp_session_id=str(uuid.uuid4()),
        is_json_response_enabled=True,
    )

    app = Starlette(
        routes=[Mount("/mcp", app=transport.handle_request)],
        middleware=[
            Middleware(
                CORSMiddleware,
                allow_origins=["*"],
                allow_methods=["*"],
                allow_headers=["*"],
            )
        ],
    )

    logger.warning(f"WhiteMagic MCP HTTP server starting on http://{host}:{port}/mcp")
    print(f"\n  WhiteMagic MCP Server v{_VERSION}", file=sys.stderr)
    print(f"  HTTP endpoint: http://{host}:{port}/mcp", file=sys.stderr)
    print(f"  28 Gana tools | 186 nested tools\n", file=sys.stderr)

    config = uvicorn.Config(app, host=host, port=port, log_level="warning")
    uv_server = uvicorn.Server(config)

    async def _run_both() -> None:
        await asyncio.gather(
            server.run(
                transport.read_stream,
                transport.write_stream,
                server.create_initialization_options(),
            ),
            uv_server.serve(),
        )

    await _run_both()


def main() -> None:
    """CLI entry point with --http flag support."""
    if "--http" in sys.argv:
        host = "127.0.0.1"
        port = 8770
        for i, arg in enumerate(sys.argv):
            if arg == "--host" and i + 1 < len(sys.argv):
                host = sys.argv[i + 1]
            if arg == "--port" and i + 1 < len(sys.argv):
                port = int(sys.argv[i + 1])
        asyncio.run(main_http(host, port))
    else:
        asyncio.run(main_stdio())


if __name__ == "__main__":
    main()
