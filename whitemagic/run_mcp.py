#!/usr/bin/env python3
"""
WhiteMagic MCP Server (Hydrated)
================================
Exposes the full Sacred Cycle (28 Ganas + Aux Tools) via FastMCP.
Dynamically loads tools from the hydrated `mcp_api_bridge.py`.
"""

import os
import sys
import inspect
import logging
import re
from importlib.util import find_spec
from pathlib import Path
from typing import Callable, Any

# Add project root to path
ROOT_DIR = Path(__file__).resolve().parent
CORE_SYSTEM_DIR = ROOT_DIR.parent
sys.path.append(str(CORE_SYSTEM_DIR))

try:
    from fastmcp import FastMCP
except ImportError:
    print("❌ Error: fastmcp not installed. Please install it: pip install fastmcp")
    sys.exit(1)

# Server setup

# Initialize Server
mcp = FastMCP("WhiteMagic Core")

# Setup Logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("mcp_server")
_RESOURCES_REGISTERED = False


def _safe_read_text(path: Path) -> str:
    """Read UTF-8 text with a clear fallback payload for MCP resources."""
    try:
        return path.read_text(encoding="utf-8")
    except Exception as exc:
        return (
            f"# Resource Unavailable\n\n"
            f"- path: `{path}`\n"
            f"- error: `{exc}`\n"
        )


def register_resources() -> None:
    """Register orientation resources so new MCP clients can self-bootstrap."""
    global _RESOURCES_REGISTERED
    if _RESOURCES_REGISTERED:
        return

    ai_primary_path = CORE_SYSTEM_DIR / "AI_PRIMARY.md"
    grimoire_index_path = ROOT_DIR / "grimoire" / "00_INDEX.md"

    @mcp.resource(
        "whitemagic://orientation/ai-primary",
        name="orientation_ai_primary",
        title="Whitemagic AI Primary",
        description="Primary orientation document for AI runtimes integrating Whitemagic.",
        mime_type="text/markdown",
    )
    def orientation_ai_primary() -> str:
        return _safe_read_text(ai_primary_path)

    @mcp.resource(
        "whitemagic://grimoire/index",
        name="grimoire_index",
        title="Grimoire Index",
        description="Top-level Grimoire index for chapter navigation and session guidance.",
        mime_type="text/markdown",
    )
    def grimoire_index() -> str:
        return _safe_read_text(grimoire_index_path)

    _RESOURCES_REGISTERED = True

class LifecycleManager:
    """Manages the lifecycle of the WhiteMagic Body (Rust, BitNet, etc)."""

    def __init__(self) -> None:
        self.rust_available = False
        self.bitnet_ready = False

    def check_body(self) -> None:
        """Check status of high-performance components."""
        # 1. Check Rust Bridge
        if find_spec("whitemagic_rs") is not None:
            self.rust_available = True
            logger.info("✅ Body: Rust Bridge (whitemagic-rs) is active.")
        else:
            logger.warning("⚠️  Body: Rust Bridge not found. Run './build_body.sh' to compile.")
        # 2. Local-model inference is archived/disabled by default.
        # External models should call Whitemagic via MCP/REST instead.

    def startup(self) -> None:
        """Run startup tasks."""
        self.check_body()

lifecycle = LifecycleManager()

# Categories always included in lite mode
_LITE_CATEGORIES = {
    "introspection", "memory", "dharma", "session", "garden",
    "governor", "synthesis",
}
# Individual tools always included in lite mode (even if category excluded)
_LITE_TOOLS = {
    "gnosis", "harmony_vector", "capabilities", "manifest",
    "health_report", "state.summary", "state.paths",
    "create_memory", "search_memories", "fast_read_memory",
    "dream_status", "dream_start", "dream_stop", "dream_now",
    "rust_status", "explain_this",
}


def _register_prat_tools(mcp_client: str) -> int:
    """Register 28 PRAT Gana meta-tools (collapses 181 → 28)."""
    from whitemagic.tools.registry import TOOL_REGISTRY
    from whitemagic.tools.schema_adapter import adapt_schema
    from whitemagic.tools.prat_router import (
        build_prat_description, build_prat_schema, route_prat_call,
        get_tools_for_gana,
    )

    count = 0
    for tool_def in TOOL_REGISTRY:
        if not tool_def.name.startswith("gana_"):
            continue

        gana_name = tool_def.name
        get_tools_for_gana(gana_name)
        description = build_prat_description(gana_name, tool_def.description)
        schema = build_prat_schema(gana_name, TOOL_REGISTRY)

        if mcp_client:
            schema = adapt_schema(schema, mcp_client)

        # Build wrapper that routes through PRAT
        def _make_prat_wrapper(gn: str) -> Callable:
            def wrapper(
                tool: str | None = None,
                args: dict | None = None,
                operation: str | None = None,
                context: dict | None = None,
                **kwargs: Any,
            ) -> Any:
                return route_prat_call(
                    gn,
                    tool=tool,
                    args=args,
                    operation=operation,
                    context=context,
                    **kwargs,
                )
            return wrapper

        wrapper = _make_prat_wrapper(gana_name)
        wrapper.__name__ = re.sub(r"[^0-9A-Za-z_]", "_", gana_name)
        wrapper.__doc__ = description

        # Build signature: tool (optional), args (optional), operation (optional), context (optional)
        params = [
            inspect.Parameter("tool", inspect.Parameter.KEYWORD_ONLY, default=None),
            inspect.Parameter("args", inspect.Parameter.KEYWORD_ONLY, default=None),
            inspect.Parameter("operation", inspect.Parameter.KEYWORD_ONLY, default=None),
            inspect.Parameter("context", inspect.Parameter.KEYWORD_ONLY, default=None),
        ]
        wrapper.__signature__ = inspect.Signature(params)  # type: ignore[attr-defined]

        mcp.tool(name=gana_name, description=description)(wrapper)
        count += 1

    return count


def register_tools() -> None:
    """Register all tools from the canonical registry.py with dynamic signatures."""
    try:
        from whitemagic.tools.registry import TOOL_REGISTRY
        from whitemagic.tools.unified_api import call_tool
        from whitemagic.tools.schema_adapter import adapt_schema
    except ImportError as e:
        logger.error(f"Failed to import registry or unified_api: {e}")
        return

    prat_mode = os.environ.get("WM_MCP_PRAT", "").strip().lower() in ("1", "true", "yes")
    lite_mode = os.environ.get("WM_MCP_LITE", "").strip().lower() in ("1", "true", "yes")
    mcp_client = os.environ.get("WM_MCP_CLIENT", "")
    if mcp_client:
        logger.info(f"🔧 Schema adaptation active for client: {mcp_client}")

    # PRAT mode: register only 28 Gana meta-tools
    if prat_mode:
        logger.info("🔮 PRAT mode: registering 28 Gana meta-tools (181 tools nested)")
        count = _register_prat_tools(mcp_client)
        msg = f"🔮 PRAT WhiteMagic MCP Server with {count} Gana tools (all tools nested inside)"
        print(msg, file=sys.stderr)
        logger.info(msg)
        return

    if lite_mode:
        logger.info("🔧 Lite mode: registering core tools only")

    def _make_wrapper(tool_name: str, call_fn: Callable) -> Callable:
        """Create a closure-based wrapper instead of using exec()."""
        def wrapper(**kwargs: Any) -> Any:
            # Filter out None values to let unified_api handle defaults
            filtered = {k: v for k, v in kwargs.items() if v is not None}
            return call_fn(tool_name, **filtered)
        return wrapper

    count = 0
    skipped = 0
    for tool_def in TOOL_REGISTRY:
        try:
            name = tool_def.name
            description = tool_def.description

            # In lite mode, skip non-essential tools
            if lite_mode:
                cat = tool_def.category.value if hasattr(tool_def.category, 'value') else str(tool_def.category)
                if cat not in _LITE_CATEGORIES and name not in _LITE_TOOLS:
                    skipped += 1
                    continue

            schema = adapt_schema(tool_def.input_schema, mcp_client) if mcp_client else tool_def.input_schema

            # Build parameter annotations for FastMCP from the schema
            props = schema.get("properties", {})
            required = schema.get("required", [])

            # Build a clean parameter list for introspection
            params = []
            for p_name in required:
                if p_name in props:
                    params.append(
                        inspect.Parameter(p_name, inspect.Parameter.KEYWORD_ONLY)
                    )
            for p_name, p_info in props.items():
                if p_name not in required:
                    default_val = p_info.get("default", None)
                    params.append(
                        inspect.Parameter(
                            p_name,
                            inspect.Parameter.KEYWORD_ONLY,
                            default=default_val,
                        )
                    )

            wrapper = _make_wrapper(name, call_tool)
            wrapper.__name__ = re.sub(r"[^0-9A-Za-z_]", "_", name)
            wrapper.__doc__ = description
            # Attach a proper signature so FastMCP can introspect parameters
            wrapper.__signature__ = inspect.Signature(params)  # type: ignore[attr-defined]

            # Register with FastMCP
            mcp.tool(name=name, description=description)(wrapper)

            count += 1

        except Exception as e:
            logger.error(f"Failed to register {name}: {e}")

    msg = f"✨ Hydrated WhiteMagic MCP Server with {count} tools"
    if skipped:
        msg += f" ({skipped} deferred in lite mode)"
    print(msg + ".", file=sys.stderr)
    logger.info(msg)

if __name__ == "__main__":
    lifecycle.startup()
    register_resources()
    register_tools()
    mcp.run()
