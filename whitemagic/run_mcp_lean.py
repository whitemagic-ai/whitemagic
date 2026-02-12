#!/usr/bin/env python3
# mypy: disable-error-code=no-untyped-def
"""
WhiteMagic MCP Server — Lean Edition (v15.0)
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
import threading
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

# ── Standard MCP SDK imports (after sys.path setup) ──────────────────
from mcp.server import Server  # noqa: E402
import mcp.types as types  # noqa: E402
from mcp.shared.message import SessionMessage  # noqa: E402

# ── Version ──────────────────────────────────────────────────────────
_VERSION_FILE = CORE_SYSTEM_DIR / "VERSION"
_VERSION = _VERSION_FILE.read_text().strip() if _VERSION_FILE.exists() else "15.0.0"


# ══════════════════════════════════════════════════════════════════════
# Lazy subsystem initialiser — runs once on first tool call
# ══════════════════════════════════════════════════════════════════════

_INITIALISED = False
_INIT_LOCK = threading.Lock()


def _ensure_init() -> None:
    """One-shot heavy initialisation (DB, singletons, Rust bridge)."""
    global _INITIALISED
    if _INITIALISED:
        return
    with _INIT_LOCK:
        if _INITIALISED:
            return
        logger.info("Lazy-initialising WhiteMagic subsystems …")
        try:
            from importlib.util import find_spec
            if find_spec("whitemagic_rs") is not None:
                logger.info("Rust bridge available")
        except Exception:
            pass

        # Auto-load Gana Forge extensions (12.108.17 — declarative śāstra)
        try:
            from whitemagic.tools.gana_forge import load_extensions
            ext_result = load_extensions()
            if ext_result.get("loaded", 0) > 0:
                logger.info("Forge: loaded %d extension tool(s)", ext_result["loaded"])
        except Exception:
            pass
        _INITIALISED = True


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
    "gana_ghost": "Introspection & web research — gnosis, telemetry, capabilities, graph topology, surprise stats, web_search, web_fetch, web_search_and_read, research_topic, browser automation",
    "gana_willow": "Resilience — rate limiter, grimoire spells & suggestions",
    "gana_star": "Governance — governor validate/set-goal/drift, dharma reload/profile, forge status/reload/validate",
    "gana_extended_net": "Pattern connectivity — pattern search, cluster stats, learning",
    "gana_wings": "Deployment & export — export memories, audit export, mesh",
    "gana_chariot": "Archaeology & knowledge graph — archaeology, KG extract/query/top",
    "gana_abundance": "Regeneration — dream cycle, lifecycle, serendipity, entity resolve",
    "gana_straddling_legs": "Ethics & balance — ethics eval, boundaries, consent, harmony vector",
    "gana_mound": "Metrics & caching — hologram view, metric tracking, yin-yang balance",
    "gana_stomach": "Digestion & tasks — pipeline, task distribute/status/route",
    "gana_hairy_head": "Detail & debug — salience, anomaly, otel, karma report/trace, dharma rules, karma XRPL anchoring",
    "gana_net": "Capture & filtering — prompt render/list/reload, karma verify",
    "gana_turtle_beak": "Precision — edge/bitnet inference, edge batch, stats",
    "gana_three_stars": "Judgment & synthesis — bicameral reasoning, ensemble, optimization, kaizen, sabha convene/status",
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
    "gana_horn": ["checkpoint_session", "create_session", "resume_session", "session_bootstrap", "session_status", "focus_session"],
    "gana_neck": ["create_memory", "delete_memory", "import_memories", "update_memory", "thought_clone"],
    "gana_root": ["health_report", "rust_audit", "rust_compress", "rust_similarity", "rust_status", "ship.check", "state.paths", "state.summary"],
    "gana_room": ["hermit.assess", "hermit.check_access", "hermit.mediate", "hermit.resolve", "hermit.status", "hermit.verify_ledger", "hermit.withdraw", "immune_heal", "immune_scan", "mcp_integrity.snapshot", "mcp_integrity.status", "mcp_integrity.verify", "sandbox.set_limits", "sandbox.status", "sandbox.violations", "sangha_lock", "security.alerts", "security.monitor_status"],
    "gana_heart": ["analyze_scratchpad", "context.pack", "context.status", "scratchpad", "scratchpad_create", "scratchpad_finalize", "scratchpad_update", "session.handoff"],
    "gana_tail": ["execute_cascade", "list_cascade_patterns", "simd.batch", "simd.cosine", "simd.status"],
    "gana_winnowing_basket": ["batch_read_memories", "fast_read_memory", "graph_walk", "hybrid_recall", "jit_research", "jit_research.stats", "list_memories", "read_memory", "search_memories", "vector.index", "vector.search", "vector.status"],
    "gana_ghost": ["capabilities", "capability.matrix", "capability.status", "capability.suggest", "drive.event", "drive.snapshot", "explain_this", "get_telemetry_summary", "gnosis", "graph_topology", "manifest", "repo.summary", "selfmodel.alerts", "selfmodel.forecast", "surprise_stats", "watcher_add", "watcher_list", "watcher_recent_events", "watcher_status", "web_fetch", "web_search", "web_search_and_read", "research_topic", "browser_navigate", "browser_click", "browser_type", "browser_extract_dom", "browser_screenshot", "browser_get_interactables", "browser_session_status"],
    "gana_willow": ["cast_oracle", "grimoire_auto_status", "grimoire_cast", "grimoire_list", "grimoire_read", "grimoire_recommend", "grimoire_suggest", "grimoire_walkthrough", "rate_limiter.stats"],
    "gana_star": ["dharma.reload", "forge.reload", "forge.status", "forge.validate", "governor_check_budget", "governor_check_dharma", "governor_check_drift", "governor_set_goal", "governor_stats", "governor_validate", "governor_validate_path", "set_dharma_profile"],
    "gana_extended_net": ["cluster_stats", "coherence_boost", "learning.patterns", "learning.status", "learning.suggest", "pattern_search", "resonance_trace", "tool.graph", "tool.graph_full"],
    "gana_wings": ["audit.export", "export_memories", "mesh.broadcast", "mesh.status"],
    "gana_chariot": ["archaeology", "archaeology_daily_digest", "archaeology_search", "archaeology_stats", "kg.extract", "kg.query", "kg.status", "kg.top", "windsurf_list_conversations", "windsurf_search_conversations"],
    "gana_abundance": ["dream", "dream_now", "dream_start", "dream_status", "dream_stop", "entity_resolve", "gratitude.benefits", "gratitude.stats", "memory.consolidate", "memory.consolidation_stats", "memory.lifecycle", "memory.lifecycle_stats", "memory.lifecycle_sweep", "memory.retention_sweep", "narrative.compress", "narrative.stats", "serendipity_mark_accessed", "serendipity_surface", "whitemagic.tip"],
    "gana_straddling_legs": ["check_boundaries", "evaluate_ethics", "get_dharma_guidance", "get_ethical_score", "harmony_vector", "verify_consent", "wu_xing_balance"],
    "gana_mound": ["get_metrics_summary", "get_yin_yang_balance", "green.record", "green.report", "record_yin_yang_activity", "track_metric", "view_hologram"],
    "gana_stomach": ["pipeline", "pipeline.create", "pipeline.list", "pipeline.status", "task.complete", "task.distribute", "task.list", "task.route_smart", "task.status"],
    "gana_hairy_head": ["anomaly", "anomaly.check", "anomaly.history", "anomaly.status", "dharma_rules", "karma.anchor", "karma.anchor_status", "karma.verify_anchor", "karma_report", "karmic_trace", "otel", "otel.metrics", "otel.spans", "otel.status", "salience.spotlight"],
    "gana_net": ["karma.verify_chain", "prompt.list", "prompt.reload", "prompt.render"],
    "gana_turtle_beak": ["bitnet_infer", "bitnet_status", "edge_batch_infer", "edge_infer", "edge_stats"],
    "gana_three_stars": ["ensemble", "ensemble.history", "ensemble.query", "ensemble.status", "kaizen_analyze", "kaizen_apply_fixes", "reasoning.bicameral", "sabha.convene", "sabha.status", "solve_optimization"],
    "gana_dipper": ["cognitive.hints", "cognitive.mode", "cognitive.set", "cognitive.stats", "homeostasis", "homeostasis.check", "homeostasis.status", "maturity.assess", "starter_packs", "starter_packs.get", "starter_packs.list", "starter_packs.suggest"],
    "gana_ox": ["swarm.complete", "swarm.decompose", "swarm.plan", "swarm.resolve", "swarm.route", "swarm.status", "swarm.vote", "worker.status"],
    "gana_girl": ["agent.capabilities", "agent.deregister", "agent.heartbeat", "agent.list", "agent.register", "agent.trust"],
    "gana_void": ["galactic.dashboard", "galaxy.create", "galaxy.delete", "galaxy.ingest", "galaxy.list", "galaxy.status", "galaxy.switch", "galaxy.backup", "galaxy.restore", "garden_activate", "garden_health", "garden_status", "garden_synergy"],
    "gana_roof": ["model.hash", "model.list", "model.register", "model.signing_status", "model.verify", "ollama.agent", "ollama.chat", "ollama.generate", "ollama.models"],
    "gana_encampment": ["broker.history", "broker.publish", "broker.status", "ganying_emit", "ganying_history", "ganying_listeners", "sangha_chat_read", "sangha_chat_send"],
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
            kwargs["execution"] = types.ToolExecution(taskSupport=types.TASK_OPTIONAL)
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

    # Keep dispatch synchronous here; some runtimes intermittently stall when
    # resolving executor-backed futures in this handler path.
    result = _sync_dispatch(name, tool_name, tool_args, operation)

    text = json.dumps(result, indent=2, default=str)
    return [types.TextContent(type="text", text=text)]


# ── Workflow templates (v14.1.1) ─────────────────────────────────────
_WORKFLOW_DIR = ROOT_DIR / "workflows"
_WORKFLOW_META: dict[str, str] = {
    "new_session": "Start every conversation — bootstrap, health, introspect, serendipity",
    "deep_research": "Multi-step research — search, graph walk, KG extract, synthesise",
    "memory_maintenance": "Periodic Data Sea hygiene — sweep, constellations, patterns",
    "ethical_review": "Full ethical governance check — dharma, boundaries, karma, harmony",
    "galaxy_setup": "Create and populate a new galaxy (isolated memory namespace)",
    "local_ai_chat": "Privacy-first local AI reasoning via Ollama integration",
}


@server.list_resources()
async def list_resources() -> list[types.Resource]:
    """Expose orientation docs and workflow templates."""
    resources = [
        types.Resource(
            uri="whitemagic://orientation/ai-primary",  # type: ignore[arg-type]
            name="AI Primary",
            description="Primary orientation document for AI runtimes.",
            mimeType="text/markdown",
        ),
        types.Resource(
            uri="whitemagic://orientation/server-instructions",  # type: ignore[arg-type]
            name="Server Instructions",
            description="How to use WhiteMagic — tool guide for AI clients.",
            mimeType="text/markdown",
        ),
        types.Resource(
            uri="whitemagic://orientation/system-map",  # type: ignore[arg-type]
            name="System Map",
            description="Architecture overview and subsystem map.",
            mimeType="text/markdown",
        ),
    ]
    # v14.1.1: Workflow templates
    for wf_name, wf_desc in _WORKFLOW_META.items():
        resources.append(types.Resource(
            uri=f"whitemagic://workflow/{wf_name}",  # type: ignore[arg-type]
            name=f"Workflow: {wf_name.replace('_', ' ').title()}",
            description=wf_desc,
            mimeType="text/markdown",
        ))
    return resources


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
    # v14.1.1: Workflow templates
    if "workflow/" in uri_str:
        wf_name = uri_str.split("workflow/")[-1].strip("/")
        wf_path = _WORKFLOW_DIR / f"{wf_name}.md"
        try:
            return wf_path.read_text(encoding="utf-8")
        except Exception as exc:
            return f"# Workflow not found: {wf_name}\n\nerror: {exc}"
    return f"# Unknown resource: {uri}"


# ══════════════════════════════════════════════════════════════════════
# Entry points — stdio (default) or HTTP
# ══════════════════════════════════════════════════════════════════════

async def main_stdio() -> None:
    """Run as stdio MCP server (default, for IDE integration)."""
    import anyio

    read_stream_writer, read_stream = anyio.create_memory_object_stream[SessionMessage | Exception](0)
    write_stream, write_stream_reader = anyio.create_memory_object_stream[SessionMessage](0)

    async def stdin_reader() -> None:
        """Read newline-delimited JSON-RPC from stdin and forward to MCP stream."""
        async with read_stream_writer:
            while True:
                raw = await asyncio.to_thread(sys.stdin.buffer.readline)
                if not raw:
                    break
                try:
                    line = raw.decode("utf-8")
                    message = types.JSONRPCMessage.model_validate_json(line)
                except Exception as exc:
                    await read_stream_writer.send(exc)
                    continue
                await read_stream_writer.send(SessionMessage(message))

    async def stdout_writer() -> None:
        """Write MCP session messages as newline-delimited JSON-RPC to stdout."""
        async with write_stream_reader:
            async for session_message in write_stream_reader:
                payload = session_message.message.model_dump_json(by_alias=True, exclude_none=True)
                sys.stdout.write(payload + "\n")
                sys.stdout.flush()

    stdin_task = asyncio.create_task(stdin_reader())
    stdout_task = asyncio.create_task(stdout_writer())
    try:
        await server.run(read_stream, write_stream, server.create_initialization_options())
    finally:
        stdin_task.cancel()
        stdout_task.cancel()
        await asyncio.gather(stdin_task, stdout_task, return_exceptions=True)


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
    print("  28 Gana tools | 311 nested tools\n", file=sys.stderr)

    config = uvicorn.Config(app, host=host, port=port, log_level="warning")
    uv_server = uvicorn.Server(config)

    async with transport.connect() as (read_stream, write_stream):
        await asyncio.gather(
            server.run(
                read_stream,
                write_stream,
                server.create_initialization_options(),
            ),
            uv_server.serve(),
        )


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
