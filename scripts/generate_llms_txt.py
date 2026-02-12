#!/usr/bin/env python3
"""Generate llms.txt and llms-full.txt from the canonical tool registry.

Reads ToolDefinitions from registry_defs/ and PRAT router mappings to
produce machine-readable documentation files that stay in sync with the
actual tool surface.

Usage:
    python scripts/generate_llms_txt.py          # writes both files
    python scripts/generate_llms_txt.py --dry-run # prints to stdout
"""

from __future__ import annotations

import argparse
import sys
from collections import defaultdict
from pathlib import Path

# Ensure project root is importable
ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT))


def _load_tools() -> list:
    """Collect all ToolDefinitions from registry_defs + dispatch table stubs."""
    from whitemagic.tools.registry_defs import collect
    from whitemagic.tools.tool_types import ToolDefinition, ToolCategory, ToolSafety
    tools = collect()
    known = {t.name for t in tools}

    # Pull remaining tools from dispatch table that lack formal ToolDefinitions
    try:
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        from whitemagic.tools.prat_router import TOOL_TO_GANA
        for name in sorted(DISPATCH_TABLE.keys()):
            if name not in known:
                gana = TOOL_TO_GANA.get(name, "uncategorized")
                tools.append(ToolDefinition(
                    name=name,
                    description=f"({gana}) — dispatch-registered tool",
                    category=ToolCategory.SYSTEM,
                    safety=ToolSafety.READ,
                    input_schema={},
                ))
    except ImportError:
        pass

    return tools


def _load_prat_mapping() -> dict[str, str]:
    """Load tool→gana mapping from PRAT router."""
    try:
        from whitemagic.tools.prat_router import TOOL_TO_GANA
        return dict(TOOL_TO_GANA)
    except ImportError:
        return {}


def _load_version() -> str:
    vf = ROOT / "VERSION"
    return vf.read_text().strip() if vf.exists() else "unknown"


def _group_by_category(tools: list) -> dict[str, list]:
    groups: dict[str, list] = defaultdict(list)
    for t in sorted(tools, key=lambda x: x.name):
        groups[t.category.value].append(t)
    return dict(sorted(groups.items()))


def _gana_summary(prat: dict[str, str]) -> dict[str, list[str]]:
    """Gana name → list of tool names."""
    ganas: dict[str, list[str]] = defaultdict(list)
    for tool, gana in sorted(prat.items()):
        ganas[gana].append(tool)
    return dict(sorted(ganas.items()))


# ── Static sections ──────────────────────────────────────────────────

_HEADER = """\
# WhiteMagic

> The Tool Substrate for Agentic AI — {tool_count} MCP tools, tiered memory with 5D holographic coordinates, HNSW semantic search, ethical governance, and polyglot accelerators in 11 languages.

## Overview

WhiteMagic is a high-performance memory and tool infrastructure for AI agents. It provides:
- {tool_count} MCP tools (or 28 PRAT Gana meta-tools) via unified dispatch
- Tiered memory with SQLite backing, 5D holographic coordinates (XYZWV), Galactic Map lifecycle
- Ethical governance: Dharma Rules Engine, Karma Ledger, Harmony Vector, Homeostatic Loop
- Polyglot accelerators: Rust (PyO3), Zig (SIMD), Haskell (FFI), Elixir (OTP), Mojo, Go, Julia, TypeScript
- Self-regulating: Circuit Breakers, Rate Limiter, RBAC, Maturity Gates, Agent Trust
"""

_INSTALL = """
## Installation

```
pip install whitemagic
```

### Optional extras
```
pip install whitemagic[mcp]        # MCP server (FastMCP)
pip install whitemagic[cli]        # Rich CLI formatting
pip install whitemagic[api]        # FastAPI server
pip install whitemagic[embeddings] # Semantic search (sentence-transformers)
pip install whitemagic[full]       # Everything
```
"""

_QUICKSTART = """
## Quick Start

### Python API
```python
from whitemagic.tools.unified_api import call_tool

# Store a memory
call_tool("create_memory", title="Decision", content="Use SQLite for Phase 1", tags=["arch"])

# Search memories
call_tool("search_memories", query="architecture", limit=5)

# Full system introspection
call_tool("gnosis", compact=True)
```

### MCP Server (stdio)
```bash
# PRAT mode — 28 Gana meta-tools (recommended)
WM_MCP_PRAT=1 python -m whitemagic.run_mcp

# Classic mode — all {tool_count} tools
python -m whitemagic.run_mcp
```

### CLI
```bash
wm status
wm remember "important note" --title "Note" --tags note
wm recall "note" --limit 5
wm doctor
```
"""

_CONCEPTS = """
## Core Concepts

### Memory Tiers
- **SHORT_TERM**: Ephemeral, decays quickly
- **LONG_TERM**: Persistent, promoted via consolidation
- **PATTERN**: Extracted insights from repeated observations
- **CORE**: Crown jewels — protected, never decays

### Galactic Map
Memories occupy zones based on importance and activity:
CORE (0-0.15) → INNER_RIM (0.15-0.40) → MID_BAND (0.40-0.65) → OUTER_RIM (0.65-0.85) → FAR_EDGE (0.85-1.0)
No memory is ever deleted — only rotated outward.

### 5D Holographic Coordinates (XYZWV)
- X: Logic ↔ Emotion
- Y: Micro ↔ Macro
- Z: Time / Chronos
- W: Importance / Gravity
- V: Vitality / Galactic Distance

### Dispatch Pipeline
Input Sanitizer → Circuit Breaker → Rate Limiter → RBAC → Maturity Gate → Governor → Handler → Compact Response

### PRAT Mode (28 Ganas)
Maps all tools into 28 Ganas (Chinese Lunar Mansions) — consciousness lenses with resonance context, Wu Xing elemental boost, and Guna adaptation.

### Ethical Governance
- **Dharma Rules**: YAML-driven policy with graduated actions (LOG→TAG→WARN→THROTTLE→BLOCK)
- **Karma Ledger**: Declared vs actual side-effect auditing
- **Harmony Vector**: 7-dimension health metric
- **Homeostatic Loop**: Auto-correction (OBSERVE→ADVISE→CORRECT→INTERVENE)
"""

_ENV_VARS = """
## Key Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| WM_STATE_ROOT | Runtime state directory | ~/.whitemagic |
| WM_MCP_PRAT | Enable PRAT mode | unset |
| WM_MCP_LITE | Enable lite mode (92 tools) | unset |
| WM_SILENT_INIT | Suppress init logs | unset |
"""

_SECURITY = """
## Security

WhiteMagic's 8-stage dispatch pipeline provides defense in depth:
- Input sanitization (prompt injection, path traversal, shell injection)
- Per-agent RBAC (observer/agent/coordinator/admin)
- Rate limiting with Rust atomic pre-check (452K ops/sec)
- Circuit breakers per tool
- Karma Ledger side-effect auditing
- Dharma Rules ethical governance with 3 profiles

In the OpenClaw ecosystem where 230+ malicious skills have been found, this matters.
"""

_ECONOMIC = """
## Economic Model

Free and open (MIT). Gratitude-driven economics:
- XRPL tip jar for human operators (whitemagic.tip tool)
- x402 micropayments for AI agents (HTTP 402 + USDC)
- Proof of Gratitude via on-chain verification
- All funds support Whitemagic Labs (MandalaOS, core development, micro-bounties)

Gratitude-driven: free by default, tip if grateful. XRPL + x402 dual channel.
"""

_DISCOVERY = """
## Agent Discovery

- skill.md: OpenClaw-ready skill file at repo root
- .well-known/agent.json: A2A Agent Card for agent-to-agent discovery
- MCP orientation resources: whitemagic://orientation/ai-primary
"""

_DOCS = """
## Documentation

- AI Primary Spec: AI_PRIMARY.md (start here if you're an AI)
- API Reference: docs/API_REFERENCE.md
- Architecture: docs/ARCHITECTURE.md
- Contributing: CONTRIBUTING.md
- MCP Configs: docs/MCP_CONFIG_EXAMPLES.md
- Changelog: CHANGELOG.md
"""

_LINKS = """
## Links

- Repository: https://github.com/whitemagic-ai/whitemagic
- Issues: https://github.com/whitemagic-ai/whitemagic/issues
- License: MIT
"""


# ── Generators ───────────────────────────────────────────────────────

def generate_llms_txt(tools: list, prat: dict[str, str], version: str) -> str:
    """Generate the compact llms.txt (overview + tool listing by category)."""
    tool_count = len(tools)
    parts: list[str] = []

    parts.append(_HEADER.format(tool_count=tool_count).strip())
    parts.append(_INSTALL.strip())
    parts.append(_QUICKSTART.format(tool_count=tool_count).strip())
    parts.append(_CONCEPTS.strip())
    parts.append(_ENV_VARS.strip())
    parts.append(_SECURITY.strip())
    parts.append(_ECONOMIC.strip())
    parts.append(_DISCOVERY.strip())
    parts.append(_DOCS.strip())
    parts.append(_LINKS.strip())

    return "\n\n".join(parts) + "\n"


def generate_llms_full_txt(tools: list, prat: dict[str, str], version: str) -> str:
    """Generate the detailed llms-full.txt with per-tool documentation."""
    tool_count = len(tools)
    parts: list[str] = []

    parts.append(_HEADER.format(tool_count=tool_count).strip())
    parts.append(f"Version: {version}\nLicense: MIT\nPython: 3.10–3.13")
    parts.append(_INSTALL.strip())
    parts.append(_QUICKSTART.format(tool_count=tool_count).strip())

    # ── Tool Catalogue by Category ──
    grouped = _group_by_category(tools)
    tool_lines = [f"## Tool Catalogue ({tool_count} tools)\n"]

    for category, cat_tools in grouped.items():
        tool_lines.append(f"### {category.upper()} ({len(cat_tools)} tools)\n")
        for t in cat_tools:
            safety_tag = f" [{t.safety.value.upper()}]" if t.safety.value != "read" else ""
            gana = prat.get(t.name, "")
            gana_tag = f" (→ {gana})" if gana else ""
            tool_lines.append(f"- **{t.name}**{safety_tag}{gana_tag}: {t.description}")

            # Show parameters for full version
            schema = t.input_schema or {}
            props = schema.get("properties", {})
            required = set(schema.get("required", []))
            if props:
                for pname, pinfo in props.items():
                    req = " *required*" if pname in required else ""
                    ptype = pinfo.get("type", "any")
                    pdesc = pinfo.get("description", "")
                    tool_lines.append(f"  - `{pname}` ({ptype}{req}): {pdesc}")
        tool_lines.append("")

    parts.append("\n".join(tool_lines).strip())

    # ── PRAT Gana Index ──
    gana_map = _gana_summary(prat)
    gana_lines = ["## PRAT Gana Index (28 meta-tools)\n"]
    for gana_name, gana_tools in gana_map.items():
        gana_lines.append(f"### {gana_name} ({len(gana_tools)} tools)")
        gana_lines.append(", ".join(sorted(gana_tools)))
        gana_lines.append("")
    parts.append("\n".join(gana_lines).strip())

    parts.append(_CONCEPTS.strip())
    parts.append(_ENV_VARS.strip())
    parts.append(_SECURITY.strip())
    parts.append(_ECONOMIC.strip())
    parts.append(_DISCOVERY.strip())
    parts.append(_DOCS.strip())
    parts.append(_LINKS.strip())

    return "\n\n".join(parts) + "\n"


# ── Main ─────────────────────────────────────────────────────────────

def main() -> None:
    parser = argparse.ArgumentParser(description="Generate llms.txt and llms-full.txt")
    parser.add_argument("--dry-run", action="store_true", help="Print to stdout instead of writing files")
    args = parser.parse_args()

    import os
    os.environ["WM_SILENT_INIT"] = "1"

    tools = _load_tools()
    prat = _load_prat_mapping()
    version = _load_version()

    print(f"Loaded {len(tools)} tool definitions, {len(prat)} PRAT mappings, version {version}")

    llms_txt = generate_llms_txt(tools, prat, version)
    llms_full = generate_llms_full_txt(tools, prat, version)

    if args.dry_run:
        print("=" * 60)
        print("llms.txt")
        print("=" * 60)
        print(llms_txt)
        print("=" * 60)
        print("llms-full.txt")
        print("=" * 60)
        print(llms_full[:3000], "...")
    else:
        llms_path = ROOT / "llms.txt"
        llms_full_path = ROOT / "llms-full.txt"

        llms_path.write_text(llms_txt)
        llms_full_path.write_text(llms_full)

        print(f"Wrote {llms_path} ({len(llms_txt)} chars)")
        print(f"Wrote {llms_full_path} ({len(llms_full)} chars)")


if __name__ == "__main__":
    main()
