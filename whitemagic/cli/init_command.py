"""CLI command: `wm init` — scaffold a new WhiteMagic project directory.

Creates the minimal files an AI agent (or human) needs to understand,
configure, and launch WhiteMagic from a fresh install.

Generated files:
    .mcp.json        — MCP client config (works with Windsurf, Claude Desktop, etc.)
    README.md        — AI-readable orientation and quickstart
    run.sh           — One-line launcher for MCP server (auto-activates venv)
    playground.py    — Interactive demo: capabilities, memory round-trip, gnosis
    .env             — Default environment configuration
    .gitignore       — Sensible defaults for a WM project
    data/            — Runtime data directory
    logs/            — Log output directory
"""
from __future__ import annotations

import stat
from pathlib import Path

import click


# ---------------------------------------------------------------------------
# Template content — module-level strings, no indentation issues
# ---------------------------------------------------------------------------

_README = """\
# WhiteMagic Project

> **Version {version}** — Cognitive scaffolding for AI agents.

This directory was created by `wm init`. It contains everything you need
to start using WhiteMagic as an AI agent or human operator.

## What is WhiteMagic?

WhiteMagic is a **cognitive scaffolding layer** for AI agents. It provides:

- **313 MCP tools** organized into 28 Ganas (Polymorphic Resonant Adaptive Tools)
- **Persistent memory** with semantic search, embeddings, and galactic lifecycle
- **Ethical governance** via Dharma rules, Karma ledger, and Harmony Vector
- **Self-awareness** through Gnosis introspection, Self-Model forecasting, and Homeostasis
- **Multi-agent coordination** with task distribution, voting, and pipelines
- **9-language polyglot accelerators** (Rust, Zig, Haskell, Elixir, Go, Mojo, Julia, TypeScript)

## Quick Start

### For AI Agents (MCP)

The fastest path — launch the MCP server and connect your client:

```bash
# PRAT mode: 28 Gana meta-tools (recommended for advanced agents)
./run.sh

# Or manually:
WM_MCP_PRAT=1 python -m whitemagic.run_mcp
```

Your first three MCP calls should be:
1. `gnosis` (compact=true) — system health snapshot
2. `capabilities` — discover available tools
3. `session_bootstrap` — initialize a working session

### For AI Agents (Python API)

```python
from whitemagic.tools.unified_api import call_tool

# Check system health
result = call_tool("gnosis", compact=True)

# Store a memory
call_tool("remember", content="Important finding", title="Research note", tags=["research"])

# Recall it later
results = call_tool("recall", query="important finding")
```

### For Humans

```bash
# System health check
wm doctor

# Interactive playground
python playground.py
```

## MCP Configuration

A ready-to-use `.mcp.json` was generated in this directory. To use it with your
MCP client (Windsurf, Claude Desktop, Cursor, etc.), copy or symlink it:

```bash
# Windsurf / Cursor — project-level config (already in place)
cat .mcp.json

# Claude Desktop — copy to global config
cp .mcp.json ~/.claude/mcp.json
```

Or add manually to any MCP client config:

```json
{{
  "mcpServers": {{
    "whitemagic": {{
      "command": "python",
      "args": ["-m", "whitemagic.run_mcp"],
      "env": {{
        "WM_MCP_PRAT": "1",
        "WM_SILENT_INIT": "1"
      }}
    }}
  }}
}}
```

## Server Modes

| Mode | Env Var | Tools | Best For |
|------|---------|-------|----------|
| **PRAT** | `WM_MCP_PRAT=1` | 28 Gana meta-tools | Advanced agents (recommended) |
| **Lite** | `WM_MCP_LITE=1` | ~92 core tools | Simple integrations |
| **Full** | *(default)* | 313 tools | Maximum capability |

## Starter Packs

New to WhiteMagic? Use starter packs to discover tools by workflow:

| Pack | Focus | Key Tools |
|------|-------|-----------|
| `quickstart` | First steps | gnosis, capabilities, session_bootstrap |
| `memory` | Knowledge management | remember, recall, consolidate |
| `introspection` | Health & debugging | harmony_vector, karma_report |
| `coordination` | Multi-agent work | agent.register, task.distribute |
| `reasoning` | Deep analysis | reasoning.bicameral, pattern_search |
| `safety` | Ethics & governance | evaluate_ethics, dharma_rules |

```python
from whitemagic.tools.unified_api import call_tool
call_tool("starter_packs.get", name="quickstart")
```

## Key Concepts

- **Ganas**: 28 lunar mansion-inspired tool clusters. Each groups related tools by domain.
- **Dharma**: Ethical rule engine. Evaluates actions before execution.
- **Karma Ledger**: Tracks declared vs actual side-effects. Auditable.
- **Harmony Vector**: 7-dimensional health metric (balance, throughput, latency, error_rate, dharma, karma_debt, energy).
- **Gnosis**: Unified introspection portal. One call to see everything.
- **Galactic Map**: Memory lifecycle — memories orbit from CORE to FAR_EDGE, never deleted.

## Environment Variables

See `.env` for all configurable options. Key ones:

| Variable | Default | Purpose |
|----------|---------|---------|
| `WM_STATE_ROOT` | `~/.whitemagic` | Where runtime data lives |
| `WM_MCP_PRAT` | `0` | Enable PRAT mode (28 Gana tools) |
| `WM_SILENT_INIT` | `0` | Suppress startup logging |
| `WM_DB_PATH` | `$WM_STATE_ROOT/memory/whitemagic.db` | Database location |

## Documentation

- **AI Primary Spec**: `python -c "import whitemagic; help(whitemagic)"` or via MCP resource `whitemagic://orientation/ai-primary`
- **Full docs**: [github.com/whitemagic-ai/whitemagic](https://github.com/whitemagic-ai/whitemagic)

---
*Generated by `wm init` — WhiteMagic v{version}*
"""

_RUN_SH = """\
#!/usr/bin/env bash
# WhiteMagic MCP Server Launcher
# Usage: ./run.sh [--full|--lite|--prat]
#
# Modes:
#   --prat  (default) 28 Gana meta-tools — recommended for advanced agents
#   --lite  ~92 core tools — simpler integration
#   --full  All 313 tools — maximum capability

set -euo pipefail

# Auto-activate venv if not already active
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -z "${VIRTUAL_ENV:-}" ]; then
    if [ -f "$SCRIPT_DIR/venv/bin/activate" ]; then
        source "$SCRIPT_DIR/venv/bin/activate"
    elif [ -f "$SCRIPT_DIR/.venv/bin/activate" ]; then
        source "$SCRIPT_DIR/.venv/bin/activate"
    fi
fi

MODE="${1:---prat}"

case "$MODE" in
    --full)
        echo "Starting WhiteMagic MCP Server (full mode — 313 tools)..."
        exec python -m whitemagic.run_mcp
        ;;
    --lite)
        echo "Starting WhiteMagic MCP Server (lite mode — ~92 tools)..."
        WM_MCP_LITE=1 exec python -m whitemagic.run_mcp
        ;;
    --prat|*)
        echo "Starting WhiteMagic MCP Server (PRAT mode — 28 Gana tools)..."
        WM_MCP_PRAT=1 exec python -m whitemagic.run_mcp
        ;;
esac
"""

_PLAYGROUND_VERSION_PLACEHOLDER = "__WM_VERSION__"

_PLAYGROUND = r'''#!/usr/bin/env python3
"""
WhiteMagic Playground — Interactive demo for new agents and humans.

Run:  python playground.py

This script walks through WhiteMagic's core capabilities:
  1. System health check (Gnosis)
  2. Capability discovery
  3. Memory round-trip (store -> search -> read)
  4. Ethical evaluation (Dharma)
  5. Harmony Vector health pulse

WhiteMagic v__WM_VERSION__
"""
import os
import json
import sys

# Quiet startup
os.environ.setdefault("WM_SILENT_INIT", "1")

try:
    from whitemagic.tools.unified_api import call_tool
except ImportError:
    print("Error: whitemagic is not installed.")
    print("Install it:  pip install whitemagic")
    sys.exit(1)

try:
    from whitemagic import __version__
except Exception:
    __version__ = "unknown"


def section(title: str) -> None:
    print(f"\n{'=' * 60}")
    print(f"  {title}")
    print(f"{'=' * 60}")


def pretty(data: dict) -> str:
    return json.dumps(data, indent=2, default=str)[:2000]


def main() -> None:
    print(f"WhiteMagic Playground v{__version__}")
    print("This demo walks through core capabilities.\n")

    # 1. Gnosis — System Health
    section("1. Gnosis — System Health Snapshot")
    result = call_tool("gnosis", compact=True)
    if result.get("status") == "success":
        gnosis = result.get("details", {}).get("gnosis", result.get("details", {}))
        print(f"  Status: {gnosis.get('status', 'unknown')}")
        print(f"  Maturity: {gnosis.get('maturity_stage', 'unknown')}")
        alerts = gnosis.get("alerts", [])
        if alerts:
            print(f"  Alerts ({len(alerts)}):")
            for a in alerts[:5]:
                print(f"    - {a}")
        else:
            print("  No alerts — all clear.")
        actions = gnosis.get("next_actions", [])
        if actions:
            act = actions[0]
            if isinstance(act, dict):
                print(f"  Suggested: {act.get('tool', act)} — {act.get('reason', '')}")
            else:
                print(f"  Suggested: {act}")
    else:
        print(f"  Result: {pretty(result)}")

    # 2. Capabilities — Tool Discovery
    section("2. Capabilities — What Can I Do?")
    result = call_tool("capabilities")
    if result.get("status") == "success":
        details = result.get("details", {})
        print(f"  Version: {details.get('version', 'unknown')}")
        tools = details.get("tools", [])
        if isinstance(tools, dict):
            tools = list(tools.values())
        print(f"  Available tools: {len(tools)}")
        categories = {}
        for t_info in tools[:300]:
            if isinstance(t_info, dict):
                cat = t_info.get("category", "unknown")
            elif isinstance(t_info, str):
                cat = "unknown"
            else:
                continue
            categories[cat] = categories.get(cat, 0) + 1
        if categories:
            print("  By category:")
            for cat, count in sorted(categories.items(), key=lambda x: -x[1])[:8]:
                print(f"    {cat}: {count} tools")
    else:
        print(f"  Result: {pretty(result)}")

    # 3. Memory Round-Trip
    section("3. Memory — Store & Recall")
    print("  Storing a test memory...")
    store_result = call_tool(
        "remember",
        content="WhiteMagic playground test: the answer to everything is 42.",
        title="Playground Test Memory",
        tags=["test", "playground"],
    )
    if store_result.get("status") == "success":
        memory_id = store_result.get("details", {}).get("memory_id", "unknown")
        print(f"  Stored (ID: {memory_id})")
    else:
        print(f"  Store result: {store_result.get('status')}")

    print("  Searching for it...")
    search_result = call_tool("recall", query="answer to everything")
    if search_result.get("status") == "success":
        results = search_result.get("results", [])
        if results:
            print(f"  Found {len(results)} result(s):")
            top = results[0]
            print(f"    Top match: {top.get('content', '')[:100]}")
        else:
            print("  No results (embedding index may need a moment).")
    else:
        print(f"  Search result: {search_result.get('status')}")

    # 4. Dharma — Ethical Evaluation
    section("4. Dharma — Ethical Evaluation")
    result = call_tool("evaluate_ethics", action="Read a file from disk")
    if result.get("status") == "success":
        details = result.get("details", {})
        score = details.get("ethical_score", "unknown")
        concerns = details.get("concerns", [])
        print("  Action: 'Read a file from disk'")
        print(f"  Ethical score: {score}")
        if concerns:
            for c in concerns[:3]:
                print(f"  Concern: {c}")
        else:
            print("  No ethical concerns raised.")
    else:
        print(f"  Result: {pretty(result)}")

    # 5. Harmony Vector
    section("5. Harmony Vector — 7D Health Pulse")
    result = call_tool("harmony_vector")
    if result.get("status") == "success":
        hv = result.get("details", {}).get("harmony_vector", result.get("details", {}))
        core_dims = ["balance", "throughput", "latency", "error_rate", "dharma", "karma_debt", "energy"]
        for dim in core_dims:
            val = hv.get(dim)
            if val is not None:
                try:
                    fval = float(val)
                    bar = "#" * int(fval * 20)
                    print(f"  {dim:>15}: {fval:.2f} {bar}")
                except (TypeError, ValueError):
                    print(f"  {dim:>15}: {val}")
        score = hv.get("harmony_score")
        if score is not None:
            print(f"\n  Overall harmony: {float(score):.2f}")
    else:
        print(f"  Result: {pretty(result)}")

    # Done
    section("Done!")
    print("  WhiteMagic is ready. Next steps:")
    print("  - Launch MCP server:  ./run.sh")
    print("  - Check health:       wm doctor")
    print("  - Explore tools:      call_tool('starter_packs.list')")
    print()


if __name__ == "__main__":
    main()
'''

_ENV = """\
# WhiteMagic Environment Configuration
# =====================================
# Copy to .env and customize. All values shown are defaults.

# Where WhiteMagic stores runtime data (DB, sessions, cache, logs)
# Set to ./.whitemagic for project-local state (recommended for isolated installs)
WM_STATE_ROOT=./.whitemagic

# Database path (defaults to $WM_STATE_ROOT/memory/whitemagic.db)
# WM_DB_PATH=

# MCP Server Mode (set ONE of these)
# WM_MCP_PRAT=1       # 28 Gana meta-tools (recommended)
# WM_MCP_LITE=1       # ~92 core tools
# (neither)            # Full mode — all 313 tools

# MCP Client Adapter (adjusts schema for specific AI clients)
# WM_MCP_CLIENT=gemini   # Options: gemini, deepseek, qwen, kimi

# Suppress startup banners and logging (good for MCP stdio mode)
# WM_SILENT_INIT=1

# Dharma profile (ethical governance strictness)
# WM_DHARMA_PROFILE=default   # Options: default, creative, secure, violet

# Ollama endpoint for local LLM inference
# OLLAMA_HOST=http://localhost:11434

# XRP receive address for gratitude tips
# Default project address (tips go to WhiteMagic development):
# WM_XRP_ADDRESS=raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy
# Set your own address to receive tips for your deployment:
"""

_GITIGNORE = """\
# WhiteMagic project .gitignore
__pycache__/
*.pyc
.env
venv/
.venv/
*.db
*.db-journal
*.db-wal
logs/
tmp/
.whitemagic/
"""

_MCP_JSON = """\
{
  "mcpServers": {
    "whitemagic": {
      "command": "python",
      "args": ["-m", "whitemagic.run_mcp"],
      "env": {
        "WM_MCP_PRAT": "1",
        "WM_SILENT_INIT": "1",
        "WM_STATE_ROOT": "./.whitemagic"
      }
    }
  }
}
"""


# ---------------------------------------------------------------------------
# CLI command
# ---------------------------------------------------------------------------

@click.command(name="init")
@click.argument("directory", default=".", type=click.Path())
@click.option("--force", "-f", is_flag=True, help="Overwrite existing files")
@click.option("--minimal", "-m", is_flag=True, help="Only create README.md and run.sh")
def init_command(directory: str, force: bool, minimal: bool) -> None:
    """Initialize a new WhiteMagic project directory.

    Scaffolds the essential files an AI agent needs to understand, configure,
    and launch WhiteMagic. Run this after `pip install whitemagic`.

    \b
    Examples:
        wm init                  # Initialize current directory
        wm init my-project       # Create and initialize my-project/
        wm init . --force        # Overwrite existing files
        wm init . --minimal      # Only README.md + run.sh
    """
    try:
        from whitemagic import __version__
    except Exception:
        __version__ = "unknown"

    target = Path(directory).resolve()
    target.mkdir(parents=True, exist_ok=True)

    files: dict[str, str] = {
        ".mcp.json": _MCP_JSON,
        "README.md": _README.format(version=__version__),
        "run.sh": _RUN_SH,
    }

    if not minimal:
        files["playground.py"] = _PLAYGROUND.replace(_PLAYGROUND_VERSION_PLACEHOLDER, __version__)
        files[".env"] = _ENV
        files[".gitignore"] = _GITIGNORE

    created = []
    skipped = []

    for filename, content in files.items():
        filepath = target / filename
        if filepath.exists() and not force:
            skipped.append(filename)
            continue

        filepath.write_text(content, encoding="utf-8")
        created.append(filename)

        # Make .sh and .py files executable
        if filename.endswith(".sh") or filename.endswith(".py"):
            filepath.chmod(filepath.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)

    # Create runtime directories
    for dirname in ("data", "logs", "tmp"):
        (target / dirname).mkdir(exist_ok=True)

    # Print results
    click.echo(f"\n  WhiteMagic v{__version__} — Project initialized at {target}\n")

    if created:
        click.echo("  Created:")
        for f in created:
            click.echo(f"    + {f}")

    if skipped:
        click.echo("\n  Skipped (already exist, use --force to overwrite):")
        for f in skipped:
            click.echo(f"    ~ {f}")

    click.echo("\n  Next steps:")
    click.echo("    1. python playground.py      # Interactive demo")
    click.echo("    2. ./run.sh                  # Launch MCP server")
    click.echo("    3. wm doctor                 # System health check")
    click.echo()


def register(main: click.Group) -> None:
    """Register the init command with the main CLI group."""
    main.add_command(init_command)
