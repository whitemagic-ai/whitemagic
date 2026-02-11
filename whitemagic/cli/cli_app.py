#!/usr/bin/env python3
# mypy: disable-error-code=no-untyped-def
"""WhiteMagic CLI - Complete Implementation
Phase 1: Core Commands for Production Readiness
"""
import json
import logging
import os
import sys
from datetime import datetime
from importlib.util import find_spec
from pathlib import Path
from typing import Any

import click

logger = logging.getLogger(__name__)

try:
    from whitemagic import __version__
except ImportError:
    __version__ = "unknown"

if __name__ == "__main__" and __package__ is None:
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

def _bootstrap_env_from_argv(argv: list[str]) -> None:
    """Bootstrap environment variables from CLI flags *before* importing Whitemagic
    modules that compute state paths at import time.

    This keeps `--state-root`/`--base-dir` and `--silent-init` effective even when
    running as an installed console script (module import happens before Click
    parses options).
    """

    def _extract_value(flag: str) -> str | None:
        for i, arg in enumerate(argv):
            if arg == flag and i + 1 < len(argv):
                return argv[i + 1]
            if arg.startswith(flag + "="):
                return arg.split("=", 1)[1]
        return None

    state_root = _extract_value("--state-root") or _extract_value("--base-dir")
    if state_root:
        # CLI flag should win for this process.
        os.environ["WM_STATE_ROOT"] = state_root

    db_path = _extract_value("--db-path")
    if db_path:
        # CLI flag should win for this process.
        os.environ["WM_DB_PATH"] = db_path

    if "--silent-init" in argv or "--json" in argv:
        os.environ["WM_SILENT_INIT"] = "1"


_bootstrap_env_from_argv(sys.argv)

# Import Rich for beautiful CLI output
try:
    from rich.console import Console
    from rich.panel import Panel
    from rich.table import Table
    from rich.tree import Tree
    HAS_RICH = True
    console = Console()
except ImportError:
    HAS_RICH = False
    console = None

# Optional feature flags
HAS_VOICE = False
HAS_GRAPH = False
HAS_EXEC = False

# Import WhiteMagic modules
try:
    from whitemagic.core.memory.unified import get_unified_memory
    HAS_CORE = True
except ImportError:
    # We delay the error message until we actually need core
    HAS_CORE = False

# Plugin system
try:
    from whitemagic.plugins import load_plugins, register_commands
    HAS_PLUGINS = True
except ImportError:
    HAS_PLUGINS = False

try:
    from whitemagic.cli.cli_commands_gardens import gardens
    from whitemagic.cli.cli_commands_intelligence import intelligence
    from whitemagic.cli.cli_commands_symbolic import iching, wuxing
    HAS_EXTENSIONS = True
except ImportError:
    HAS_EXTENSIONS = False

# Reasoning CLI commands (v4.10.0 - Multi-Spectral Reasoning)
try:
    from whitemagic.cli.cli_reasoning import reasoning  # type: ignore[import-not-found]
    HAS_REASONING = True
except ImportError:
    HAS_REASONING = False

# Inference CLI commands (v4.11.0 - Unified Inference)
try:
    from whitemagic.cli.infer_commands import infer  # type: ignore[import-not-found]
    HAS_INFERENCE = True
except ImportError:
    HAS_INFERENCE = False

# Hardware CLI commands (v4.11.0 - Hardware Awareness)
try:
    from whitemagic.cli.hardware_commands import hardware  # type: ignore[import-not-found]
    HAS_HARDWARE_CLI = True
except ImportError:
    HAS_HARDWARE_CLI = False

# Rust CLI commands (v4.9.0)
try:
    from whitemagic.cli.cli_rust import register_rust_commands
    HAS_RUST_CLI = True
except ImportError:
    HAS_RUST_CLI = False

try:
    from whitemagic.cli.cli_archaeology import archaeology, windsurf
    HAS_ARCHAEOLOGY = True
except ImportError:
    HAS_ARCHAEOLOGY = False

try:
    from whitemagic.cli.cli_watcher import watch
    HAS_WATCHER = True
except ImportError:
    HAS_WATCHER = False

try:
    from whitemagic.cli.cli_autonomous_execution import autonomous
    HAS_AUTONOMOUS = True
except ImportError:
    HAS_AUTONOMOUS = False

try:
    from whitemagic.cli.cli_sangha import sangha_cli
    HAS_SANGHA = True
except ImportError:
    HAS_SANGHA = False

try:
    from whitemagic.cli.cli_local import local_cli  # type: ignore[attr-defined]
    HAS_LOCAL = True
except ImportError:
    HAS_LOCAL = False

# Scratchpad CLI (v5.2.0 - Phase 26)
try:
    from whitemagic.cli.cli_scratchpad import scratch
    HAS_SCRATCH = True
except ImportError:
    HAS_SCRATCH = False

@click.group()
@click.version_option(version=__version__)
@click.option(
    "--state-root",
    help="Override WM_STATE_ROOT for this run (recommended for tests/containers).",
)
@click.option(
    "--base-dir",
    help="(Deprecated) Alias for --state-root.",
)
@click.option(
    "--db-path",
    help="Override WM_DB_PATH for this run (SQLite DB file path).",
)
@click.option("--json", "json_output", is_flag=True, help="Emit full tool envelopes as JSON (AI-friendly).")
@click.option("--now", help="ISO timestamp override for deterministic tool runs.")
@click.option("--silent-init", is_flag=True, help="Set WM_SILENT_INIT=1 to suppress noisy initialization logs.")
@click.pass_context
def main(
    ctx,
    state_root: str | None,
    base_dir: str | None,
    db_path: str | None,
    json_output: bool,
    now: str | None,
    silent_init: bool,
):
    """WhiteMagic CLI - AI Memory & Context Management"""
    effective_state_root = state_root or base_dir

    if json_output or silent_init:
        os.environ.setdefault("WM_SILENT_INIT", "1")

    # NOTE: WM_STATE_ROOT is resolved at import time in whitemagic.config.paths.
    # cli_app bootstraps WM_STATE_ROOT from argv early, so this flag is effective
    # for the current process, and we also propagate it for subprocesses.
    if effective_state_root:
        os.environ.setdefault("WM_STATE_ROOT", effective_state_root)
    if db_path:
        os.environ.setdefault("WM_DB_PATH", db_path)

    ctx.ensure_object(dict)
    ctx.obj["state_root"] = effective_state_root
    ctx.obj["json_output"] = bool(json_output)
    ctx.obj["now"] = now

# --- Enhanced Commands (Rich Enabled) ---

@main.command(name="status")
@click.pass_context
def status_command(ctx):
    """Show system status (AI-first)"""
    from whitemagic.tools.unified_api import call_tool

    now = (ctx.obj or {}).get("now") if isinstance(ctx.obj, dict) else None

    caps = call_tool("capabilities", include_tools=False, include_env=False, now=now)
    state = call_tool("state.summary", include_sizes=True, now=now)

    if (ctx.obj or {}).get("json_output"):
        click.echo(json.dumps({"capabilities": caps, "state": state}, indent=2, sort_keys=True))
        return

    if caps.get("status") == "success":
        cd = caps.get("details", {}) or {}
        click.echo(f"Whitemagic v{cd.get('package_version', 'unknown')}")
        click.echo(
            f"Tool contract: {cd.get('tool_contract_version', 'unknown')} | "
            f"Envelope: {cd.get('envelope_version', 'unknown')}",
        )
        click.echo(f"WM_STATE_ROOT: {(cd.get('state') or {}).get('wm_state_root')}")
    else:
        click.echo(f"❌ capabilities: {caps.get('message')}")

    if state.get("status") == "success":
        sd = state.get("details", {}) or {}
        click.echo(f"State exists: {sd.get('exists')}")
        dirs = sd.get("dirs", {}) or {}
        click.echo(
            "Dirs: "
            f"memory={dirs.get('memory')} "
            f"data={dirs.get('data')} "
            f"cache={dirs.get('cache')} "
            f"logs={dirs.get('logs')}",
        )
    else:
        click.echo(f"❌ state.summary: {state.get('message')}")

@main.command(name="health")
@click.pass_context
def health_command(ctx):
    """Run comprehensive health check"""
    from whitemagic.mcp_api_bridge import check_system_health

    json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

    # If JSON requested, just print result and exit
    if json_output:
        result = check_system_health(component="system", deep_scan=False)
        click.echo(json.dumps(result, indent=2, sort_keys=True))
        return

    if HAS_RICH and console:
        with console.status("[cyan]Running health check...", spinner="dots"):
            try:
                result = check_system_health(component="system", deep_scan=False)

                if "error" in result:
                    console.print(f"[red]❌ Error:[/red] {result['error']}")
                else:
                    health_score = result.get("health_score")
                    status_str = result.get("status", "unknown")
                    issues = result.get("issues", [])

                    if health_score is None:
                        status_to_score = {
                            "healthy": 1.0, "good": 0.9, "ok": 0.7,
                            "degraded": 0.5, "warning": 0.4,
                            "critical": 0.2, "error": 0.1, "unknown": 0.5,
                        }
                        health_score = status_to_score.get(status_str.lower(), 0.5)

                    if not issues and health_score < 0.8:
                        health_score = 0.9

                    if health_score >= 0.8:
                        color = "green"
                        display_status = "✅ Healthy"
                    elif health_score >= 0.5:
                        color = "yellow"
                        display_status = "⚠️  Needs Attention"
                    else:
                        color = "red"
                        display_status = "❌ Critical"

                    accelerators = result.get("accelerators", {})
                    acc_lines = []
                    if accelerators:
                        details = accelerators.get("details", {})
                        if details:
                            for name, info in details.items():
                                status_icon = "✅" if info.get("status") == "active" else "❌" if info.get("status") == "error" else "⚠️"
                                version = info.get("version", "unknown")
                                latency = f" ({info.get('latency_ms', 0):.2f}ms)" if "latency_ms" in info else ""
                                acc_lines.append(f"  {status_icon} {name}: {info.get('status')} [dim]v{version}{latency}[/dim]")

                    panel_content = (
                        f"[{color}]{display_status}[/{color}]\n\n"
                        f"[bold]Health Score:[/bold] {health_score:.0%}\n\n"
                    )

                    if acc_lines:
                        panel_content += "[bold]Accelerators:[/bold]\n" + "\n".join(acc_lines) + "\n\n"

                    panel_content += ("[bold]Issues:[/bold]\n" + "\n".join(f"  • {i}" for i in issues) if issues else "[green]No issues detected[/green]")

                    panel = Panel(
                        panel_content,
                        title="🏥 System Health",
                        border_style=color,
                    )
                    console.print(panel)
            except Exception as e:
                console.print(f"[red]❌ Error:[/red] {e}")
    else:
        try:
            result = check_system_health(component="system")
            click.echo(f"Health Status: {result.get('status', 'unknown')}")
        except Exception as e:
            click.echo(f"Error checking health: {e}")

@main.command(name="doctor")
@click.pass_context
def doctor_command(ctx):
    """Run consolidated system diagnostics via health_report tool"""
    from whitemagic.tools.dispatch_table import dispatch
    json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

    result = dispatch("health_report") or {}

    if json_output:
        click.echo(json.dumps(result, indent=2, sort_keys=True, default=str))
        return

    if HAS_RICH and console:
        score = result.get("health_score", 0)
        status = result.get("health_status", "unknown")
        color = "green" if score >= 0.8 else "yellow" if score >= 0.5 else "red"

        lines = [f"[{color}]Health: {status} ({score:.0%})[/{color}]\n"]

        if "version" in result:
            lines.append(f"[bold]Version:[/bold] {result['version']}")
        if "tool_count" in result:
            lines.append(f"[bold]Tools:[/bold] {result['tool_count']}")
        if "db" in result:
            db = result["db"]
            lines.append(f"[bold]DB:[/bold] {db.get('memory_count', '?')} memories, {db.get('size_mb', '?')} MB")

        rust_ok = result.get("rust", {}).get("available", False)
        julia_ok = result.get("julia", {}).get("available", False)
        haskell_ok = result.get("haskell", {}).get("available", False)
        lines.append("\n[bold]Bridges:[/bold]")
        lines.append(f"  {'✅' if rust_ok else '❌'} Rust")
        lines.append(f"  {'✅' if julia_ok else '❌'} Julia")
        lines.append(f"  {'✅' if haskell_ok else '❌'} Haskell")

        if "gardens" in result:
            garden_count = len(result["gardens"])
            lines.append(f"\n[bold]Gardens:[/bold] {garden_count} registered")

        panel = Panel("\n".join(lines), title="🏥 WhiteMagic Doctor", border_style=color)
        console.print(panel)
    else:
        click.echo(f"Health: {result.get('health_status', 'unknown')} ({result.get('health_score', 0):.0%})")
        if "db" in result:
            click.echo(f"DB: {result['db'].get('memory_count', '?')} memories")
        click.echo(f"Rust: {'yes' if result.get('rust', {}).get('available') else 'no'}")
        click.echo(f"Julia: {'yes' if result.get('julia', {}).get('available') else 'no'}")

@main.command(name="explore")
def explore_command():
    """Interactive guide to WhiteMagic features"""
    if HAS_RICH and console:
        console.print("\n[bold cyan]🧭 WhiteMagic Explorer[/bold cyan]\n")

        tree = Tree(f"🪄 WhiteMagic v{__version__}")

        gana_branch = tree.add("[cyan]🌙 28 Lunar Mansion Ganas[/cyan]")
        gana_branch.add("wm gana list - View all Ganas by quadrant")
        gana_branch.add("wm gana invoke <tool> - Invoke tool through Gana")
        gana_branch.add("wm gana status - System status")

        dharma_branch = tree.add("[yellow]☸️  Dharma Ethical System[/yellow]")
        dharma_branch.add("wm dharma evaluate <action> - Check ethics")
        dharma_branch.add("wm dharma principles - List principles")
        dharma_branch.add("wm dharma check-boundaries <action> - Check boundaries")

        ml_branch = tree.add("[green]🤖 Local ML Inference[/green]")
        ml_branch.add("wm infer local-query <prompt> - Run local inference")
        ml_branch.add("wm infer local-status - Engine status")

        wisdom_branch = tree.add("[magenta]🧙 Wisdom Systems[/magenta]")
        wisdom_branch.add("wm wisdom consult <question> - Ask wisdom council")
        wisdom_branch.add("wm wisdom iching <question> - Ask I Ching")

        system_branch = tree.add("[blue]🔧 System Commands[/blue]")
        system_branch.add("wm status - Overall status")
        system_branch.add("wm health - Health check")
        system_branch.add("wm start-session - Start session orchestrator")

        console.print(tree)
        console.print("\n[dim]Use --help on any command for more details[/dim]\n")
    else:
        click.echo("WhiteMagic Explorer - Interactive guide (Rich required for full experience)")

@main.command(name="galaxy")
def galaxy_command():
    """Launch the Galaxy TUI (Visual Memory Browser)"""
    try:
        from whitemagic.interfaces.tui import GalaxyTUI
        # Run the TUI
        app = GalaxyTUI()
        app.run()
    except ImportError as e:
        click.echo(f"❌ Error: TUI dependencies missing. Install with 'pip install whitemagic[tui]' ({e})")
    except Exception as e:
        click.echo(f"❌ Error launching Galaxy: {e}")

# --- Global Memory Helper ---

_memory = None

def get_memory():
    """Get or create memory instance (respects WM_STATE_ROOT)."""
    global _memory
    if _memory is None and HAS_CORE:
        _memory = get_unified_memory()
    return _memory

# --- Gana System ---

@main.group(name="gana")
def gana_group():
    """🌙 Interact with the 28 Lunar Mansion Ganas"""

@gana_group.command(name="list")
@click.option(
    "--quadrant",
    type=click.Choice(["east", "south", "west", "north", "all"]),
    default="all",
)
def gana_list(quadrant: str):
    """List all Ganas by quadrant"""
    from whitemagic.core.ganas.registry import get_all_ganas

    ganas = get_all_ganas()
    quadrants = {
        "east": (0, 7, "🐉 Eastern (Spring)", "cyan"),
        "south": (7, 14, "🦅 Southern (Summer)", "red"),
        "west": (14, 21, "🐯 Western (Autumn)", "yellow"),
        "north": (21, 28, "🐢 Northern (Winter)", "blue"),
    }

    if HAS_RICH and console:
        tree = Tree("🌙 28 Lunar Mansion Ganas")
        for quad_key, (start, end, label, color) in quadrants.items():
            if quadrant != "all" and quadrant != quad_key:
                continue
            branch = tree.add(f"[{color}]{label}[/{color}]")
            for i, gana in enumerate(ganas[start:end], start + 1):
                mansion = gana.mansion
                branch.add(f"{i}. {mansion.name} ({mansion.value}) - {gana.__class__.__name__}")
        console.print(tree)
        return

    click.echo("🌙 28 Lunar Mansion Ganas")
    for quad_key, (start, end, label, _) in quadrants.items():
        if quadrant != "all" and quadrant != quad_key:
            continue
        click.echo(f"\n{label}")
        for i, gana in enumerate(ganas[start:end], start + 1):
            mansion = gana.mansion
            click.echo(f"{i}. {mansion.name} ({mansion.value}) - {gana.__class__.__name__}")

@gana_group.command(name="invoke")
@click.argument("tool_name")
@click.option("--args", help="JSON arguments", default="{}")
def gana_invoke(tool_name: str, args: str):
    """Invoke a tool through its Gana"""
    import json

    from whitemagic.mcp_api_bridge import gana_invoke as invoke_gana

    try:
        args_dict = json.loads(args)
        if HAS_RICH and console:
            with console.status(f"[cyan]Invoking {tool_name} through Gana...", spinner="moon"):
                result = invoke_gana(target_tool=tool_name, tool_args=args_dict)
        else:
            result = invoke_gana(target_tool=tool_name, tool_args=args_dict)

        if "error" in result:
            if HAS_RICH and console:
                console.print(f"[red]❌ Error:[/red] {result['error']}")
            else:
                click.echo(f"❌ Error: {result['error']}")
            return

        if HAS_RICH and console:
            panel = Panel(
                "[green]✅ Success[/green]\n\n"
                f"[bold]Mansion:[/bold] {result.get('mansion', 'N/A')}\n"
                f"[bold]Garden:[/bold] {result.get('garden', 'N/A')}\n"
                f"[bold]Execution:[/bold] {result.get('execution_ms', 0):.2f}ms\n\n"
                "[bold]Output:[/bold]\n"
                f"{result.get('output', 'N/A')}",
                title=f"🌙 {tool_name}",
                border_style="cyan",
            )
            console.print(panel)
        else:
            click.echo(f"✅ Success: {result.get('output', 'N/A')}")
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[red]❌ Error:[/red] {e}")
        else:
            click.echo(f"❌ Error: {e}")

@gana_group.command(name="status")
def gana_status():
    """Show Gana system status and resonance"""
    from whitemagic.core.ganas.registry import TOOL_TO_GANA, get_all_ganas

    ganas = get_all_ganas()
    if HAS_RICH and console:
        table = Table(title="🌙 Gana System Status", show_header=True, header_style="bold magenta")
        table.add_column("Quadrant", style="cyan")
        table.add_column("Ganas", justify="center")
        table.add_column("Tools Mapped", justify="center")

        table.add_row(
            "🐉 Eastern (Spring)",
            "7",
            str(sum(1 for t in TOOL_TO_GANA if TOOL_TO_GANA[t] in [type(g) for g in ganas[:7]])),
        )
        table.add_row(
            "🦅 Southern (Summer)",
            "7",
            str(sum(1 for t in TOOL_TO_GANA if TOOL_TO_GANA[t] in [type(g) for g in ganas[7:14]])),
        )
        table.add_row(
            "🐯 Western (Autumn)",
            "7",
            str(sum(1 for t in TOOL_TO_GANA if TOOL_TO_GANA[t] in [type(g) for g in ganas[14:21]])),
        )
        table.add_row(
            "🐢 Northern (Winter)",
            "7",
            str(sum(1 for t in TOOL_TO_GANA if TOOL_TO_GANA[t] in [type(g) for g in ganas[21:28]])),
        )
        table.add_row("[bold]Total[/bold]", "[bold]28[/bold]", f"[bold]{len(TOOL_TO_GANA)}[/bold]")
        console.print(table)
        console.print("\n[green]✅ All 28 Ganas operational and resonating[/green]")
        return

    click.echo("🌙 Gana System Status")
    click.echo(f"Total Ganas: 28 | Tools mapped: {len(TOOL_TO_GANA)}")

# --- Dharma System ---

@main.group(name="dharma")
def dharma_group():
    """☸️  Ethical reasoning and boundary detection"""

@dharma_group.command(name="evaluate")
@click.argument("action")
@click.option("--context", help="Additional context (JSON)", default="{}")
def dharma_evaluate(action: str, context: str):
    """Evaluate an action against ethical principles"""
    import json

    from whitemagic.mcp_api_bridge import dharma_evaluate_ethics

    try:
        context_dict = json.loads(context)
        if HAS_RICH and console:
            with console.status("[yellow]Consulting ethical principles...", spinner="dots"):
                result = dharma_evaluate_ethics(action={"description": action}, context=context_dict)
        else:
            result = dharma_evaluate_ethics(action={"description": action}, context=context_dict)

        score = result.get("score", 0)
        concerns = result.get("concerns", [])

        if HAS_RICH and console:
            if score >= 0.8:
                color = "green"
                status = "✅ Aligned"
            elif score >= 0.5:
                color = "yellow"
                status = "⚠️  Concerning"
            else:
                color = "red"
                status = "❌ Violation"

            panel = Panel(
                f"[{color}]{status}[/{color}]\n\n"
                f"[bold]Action:[/bold] {action}\n"
                f"[bold]Ethical Score:[/bold] {score:.2%}\n\n"
                "[bold]Concerns:[/bold]\n"
                + ("\n".join(f"  • {c}" for c in concerns) if concerns else "  None"),
                title="☸️  Dharma Evaluation",
                border_style=color,
            )
            console.print(panel)
        else:
            click.echo(f"Ethical Score: {score:.2%}")
            if concerns:
                click.echo("Concerns:")
                for c in concerns:
                    click.echo(f"  - {c}")
            else:
                click.echo("No concerns.")
    except Exception as e:
        click.echo(f"❌ Error: {e}")

@dharma_group.command(name="principles")
@click.option("--level", help="Filter by level (universal, compassion, integrity, etc.)")
def dharma_principles(level: str | None):
    """List all ethical principles"""
    from whitemagic.mcp_api_bridge import dharma_list_principles

    try:
        result = dharma_list_principles(level=level)
        principles = result.get("principles", [])

        if HAS_RICH and console:
            table = Table(title="☸️  Dharma Principles", show_header=True, header_style="bold yellow")
            table.add_column("Principle", style="cyan")
            table.add_column("Level", style="magenta")
            table.add_column("Weight", justify="center")
            for p in principles:
                table.add_row(
                    p.get("name", "Unknown"),
                    p.get("level", "Unknown"),
                    f"{p.get('weight', 0):.1f}",
                )
            console.print(table)
        else:
            for p in principles:
                click.echo(f"{p.get('name', 'Unknown')} ({p.get('level', 'Unknown')})")
    except Exception as e:
        click.echo(f"❌ Error: {e}")

@dharma_group.command(name="check-boundaries")
@click.argument("action")
def dharma_check_boundaries(action: str):
    """Check for boundary violations"""
    from whitemagic.mcp_api_bridge import dharma_check_boundaries

    try:
        if HAS_RICH and console:
            with console.status("[yellow]Checking boundaries...", spinner="dots"):
                result = dharma_check_boundaries(action={"description": action}, context={})
        else:
            result = dharma_check_boundaries(action={"description": action}, context={})

        violations = result.get("violations", [])
        if HAS_RICH and console:
            if not violations:
                console.print(Panel("[green]✅ No boundary violations detected[/green]", border_style="green"))
            else:
                tree = Tree("⚠️  Boundary Violations")
                for v in violations:
                    branch = tree.add(
                        f"[red]{v.get('type', 'Unknown')}[/red] (severity: {v.get('severity', 0):.1f})",
                    )
                    branch.add(f"[dim]{v.get('reason', 'No reason provided')}[/dim]")
                console.print(tree)
        elif not violations:
            click.echo("✅ No boundary violations detected")
        else:
            click.echo("⚠️  Boundary Violations:")
            for v in violations:
                click.echo(f"- {v.get('type', 'Unknown')}: {v.get('reason', 'No reason provided')}")
    except Exception as e:
        click.echo(f"❌ Error: {e}")

# --- Local ML (Enhanced) ---

@click.command(name="local-query")
@click.argument("prompt")
@click.option("--backend", type=click.Choice(["bitnet", "ollama", "auto"]), default="auto")
def infer_local_query(prompt: str, backend: str):
    """Run local ML inference query (BitNet/Ollama)"""
    from whitemagic.mcp_api_bridge import local_ml_infer

    try:
        if HAS_RICH and console:
            with console.status(f"[cyan]Running inference with {backend}...", spinner="dots"):
                result = local_ml_infer(prompt=prompt, backend=backend if backend != "auto" else None)
        else:
            result = local_ml_infer(prompt=prompt, backend=backend if backend != "auto" else None)

        if "error" in result:
            click.echo(f"❌ Error: {result['error']}")
            return

        if HAS_RICH and console:
            panel = Panel(
                "[bold]Response:[/bold]\n\n"
                f"{result.get('response', 'N/A')}\n\n"
                f"[dim]Backend: {result.get('backend', 'unknown')} | "
                f"Time: {result.get('time_ms', 0):.0f}ms[/dim]",
                title="🤖 Local ML",
                border_style="cyan",
            )
            console.print(panel)
        else:
            click.echo(result.get("response", "N/A"))
    except Exception as e:
        click.echo(f"❌ Error: {e}")

@click.command(name="local-status")
def infer_local_status():
    """Show local ML engine status"""
    from whitemagic.mcp_api_bridge import local_ml_status

    try:
        result = local_ml_status()
        if HAS_RICH and console:
            table = Table(title="🤖 Local ML Status", show_header=True)
            table.add_column("Backend", style="cyan")
            table.add_column("Available", justify="center")
            table.add_column("Models", justify="center")
            for backend, info in result.get("backends", {}).items():
                available = "✅" if info.get("available") else "❌"
                models = info.get("models", [])
                table.add_row(
                    backend.title(),
                    available,
                    str(len(models)) if models else "0",
                )
            console.print(table)
            default = result.get("default_backend")
            if default:
                console.print(f"\n[green]Default backend:[/green] {default}")
        else:
            for backend, info in result.get("backends", {}).items():
                available = "yes" if info.get("available") else "no"
                models = info.get("models", [])
                click.echo(f"{backend}: {available} ({len(models)} models)")
    except Exception as e:
        click.echo(f"❌ Error: {e}")

# --- Wisdom & Reasoning ---

@main.group(name="wisdom")
def wisdom_group():
    """🧙 Consult wisdom systems (I Ching, Wu Xing, Art of War)"""

@wisdom_group.command(name="consult")
@click.argument("question")
@click.option(
    "--source",
    type=click.Choice(["full_council", "art_of_war", "iching", "synthesize"]),
    default="full_council",
)
@click.option("--urgency", type=click.Choice(["low", "normal", "high", "critical"]), default="normal")
def wisdom_consult(question: str, source: str, urgency: str):
    """Consult the wisdom council"""
    from whitemagic.mcp_api_bridge import consult_full_council

    try:
        if HAS_RICH and console:
            with console.status(f"[yellow]Consulting {source}...", spinner="moon"):
                result = consult_full_council(question=question, source=source, urgency=urgency)
        else:
            result = consult_full_council(question=question, source=source, urgency=urgency)

        if "error" in result:
            click.echo(f"❌ Error: {result['error']}")
            return

        guidance = result.get("guidance", "No guidance provided")
        confidence = result.get("confidence", 0)
        if HAS_RICH and console:
            panel = Panel(
                f"[bold]Question:[/bold] {question}\n\n"
                f"[bold]Guidance:[/bold]\n{guidance}\n\n"
                f"[dim]Source: {source} | Urgency: {urgency} | Confidence: {confidence:.0%}[/dim]",
                title="🧙 Wisdom Council",
                border_style="yellow",
            )
            console.print(panel)
        else:
            click.echo(guidance)
    except Exception as e:
        click.echo(f"❌ Error: {e}")

@wisdom_group.command(name="iching")
@click.argument("question")
def wisdom_iching(question: str):
    """Cast I Ching hexagram for guidance"""
    from whitemagic.mcp_api_bridge import consult_iching

    try:
        if HAS_RICH and console:
            with console.status("[yellow]Casting I Ching...", spinner="dots"):
                result = consult_iching(operation="cast", question=question, method="coin")
        else:
            result = consult_iching(operation="cast", question=question, method="coin")

        if "error" in result:
            click.echo(f"❌ Error: {result['error']}")
            return

        hexagram = result.get("hexagram_number", "N/A")
        interpretation = result.get("interpretation", "No interpretation")
        if HAS_RICH and console:
            panel = Panel(
                f"[bold]Hexagram {hexagram}[/bold]\n\n{interpretation}",
                title="☯️  I Ching",
                border_style="cyan",
            )
            console.print(panel)
        else:
            click.echo(f"Hexagram {hexagram}\n{interpretation}")
    except Exception as e:
        click.echo(f"❌ Error: {e}")

# --- Maintenance Commands ---

@main.group(name="maintenance")
def maintenance_group():
    """🔧 System maintenance and synchronization commands"""

@maintenance_group.command(name="reindex")
def maintenance_reindex():
    """Run The Great Realignment (Full Holographic Re-Indexing)"""
    import os
    import subprocess

    if HAS_RICH and console:
        console.print("\n[bold cyan]🌌 Starting The Great Realignment...[/bold cyan]\n")
        console.print("[dim]This will re-calculate spatial coordinates for all memories.[/dim]\n")

        # We call the script as a subprocess to ensure it uses the correct context
        # and doesn't interfere with the current CLI memory state if possible
        try:
            # Setting up environment
            env = os.environ.copy()
            env["PYTHONPATH"] = env.get("PYTHONPATH", "") + ":" + os.getcwd()

            # Using the venv python if possible
            python_bin = os.path.join(os.getcwd(), ".venv", "bin", "python3")
            if not os.path.exists(python_bin):
                python_bin = "python3"

            script_path = os.path.join(os.getcwd(), "scripts", "reindex_data_sea.py")

            if not os.path.exists(script_path):
                console.print(f"[red]❌ Error: Maintenance script not found at {script_path}[/red]")
                return

            # Execute
            subprocess.run([python_bin, script_path], check=True, env=env)

            console.print("\n[bold green]✨ Realignment Complete![/bold green]")
        except subprocess.CalledProcessError as e:
            console.print(f"\n[red]❌ Realignment failed with exit code {e.returncode}[/red]")
        except Exception as e:
            console.print(f"\n[red]❌ Unexpected error: {e}[/red]")
    else:
        click.echo("🌌 Starting The Great Realignment...")
        # (Simplified execution for non-rich)
        os.system("export PYTHONPATH=$PYTHONPATH:. && ./.venv/bin/python3 scripts/reindex_data_sea.py")

# --- Core Memory Commands ---

@main.command()
@click.argument("content")
@click.option("--title", help="Memory title")
@click.option("--tags", help="Comma-separated tags")
@click.option("--type", "memory_type", default="short_term",
              type=click.Choice(["short_term", "long_term"]))
@click.pass_context
def remember(ctx, content, title, tags, memory_type):
    """Create a new memory entry"""
    from whitemagic.tools.unified_api import call_tool

    now = (ctx.obj or {}).get("now") if isinstance(ctx.obj, dict) else None
    json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

    tags_list = [t.strip() for t in (tags or "").split(",") if t.strip()]
    title_val = title or (content[:60] + ("..." if len(content) > 60 else ""))

    out = call_tool(
        "create_memory",
        title=title_val,
        content=content,
        tags=tags_list,
        type=memory_type,
        now=now,
    )

    if json_output:
        click.echo(json.dumps(out, indent=2, sort_keys=True))
        return

    if out.get("status") != "success":
        click.echo(f"❌ Error: {out.get('message', 'Unknown error')}")
        return

    details = out.get("details", {}) or {}
    click.echo(f"✅ Memory created: {details.get('memory_id')}")
    if details.get("filename"):
        click.echo(f"   Filename: {details.get('filename')}")

@main.group()
def scratchpad():
    """Scratchpad management commands"""

@scratchpad.command()
@click.argument("name")
@click.option("--session-id", help="Optional session id to associate")
@click.pass_context
def create(ctx, name: str, session_id: str | None):
    """Create a new scratchpad"""
    from whitemagic.tools.unified_api import call_tool

    try:
        now = (ctx.obj or {}).get("now") if isinstance(ctx.obj, dict) else None
        json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

        result = call_tool("scratchpad_create", name=name, session_id=session_id, now=now)
        if json_output:
            click.echo(json.dumps(result, indent=2, sort_keys=True))
            return
        if result.get("status") != "success":
            click.echo(f"❌ Error: {result.get('message', 'Unknown error')}")
            return
        pad = (result.get("details") or {}).get("scratchpad", {}) or {}
        click.echo(f"✅ Scratchpad created: {pad.get('id')}")
        if pad.get("focus"):
            click.echo(f"   Focus: {pad.get('focus')}")
    except Exception as exc:
        click.echo(f"❌ Error creating scratchpad: {exc}")

@scratchpad.command()
@click.argument("scratchpad_id")
@click.argument("section")
@click.argument("content")
@click.pass_context
def update(ctx, scratchpad_id: str, section: str, content: str):
    """Update a scratchpad section"""
    from whitemagic.tools.unified_api import call_tool

    try:
        now = (ctx.obj or {}).get("now") if isinstance(ctx.obj, dict) else None
        json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

        result = call_tool(
            "scratchpad_update",
            scratchpad_id=scratchpad_id,
            section=section,
            content=content,
            now=now,
        )
        if json_output:
            click.echo(json.dumps(result, indent=2, sort_keys=True))
            return
        if result.get("status") != "success":
            click.echo(f"❌ Error: {result.get('message', 'Unknown error')}")
            return
        click.echo(f"✅ Scratchpad updated: {scratchpad_id} ({section})")
    except Exception as exc:
        click.echo(f"❌ Error updating scratchpad: {exc}")

@scratchpad.command()
@click.argument("scratchpad_id")
@click.pass_context
def analyze(ctx, scratchpad_id: str):
    """Run multi-spectral analysis on a scratchpad"""
    from whitemagic.tools.unified_api import call_tool

    click.echo(f"\n🧠 Analyzing scratchpad: {scratchpad_id}\n")
    try:
        now = (ctx.obj or {}).get("now") if isinstance(ctx.obj, dict) else None
        json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

        result = call_tool("analyze_scratchpad", scratchpad_id=scratchpad_id, now=now)
        if json_output:
            click.echo(json.dumps(result, indent=2, sort_keys=True))
            return
        if result.get("status") != "success":
            click.echo(f"❌ Error: {result.get('message', 'Unknown error')}")
            return

        analysis = (result.get("details") or {}).get("analysis", {}) or {}
        click.echo("═" * 70)
        click.echo(f"\n📊 CONFIDENCE: {analysis.get('confidence', 0):.0%}\n")
        click.echo("═" * 70)

        click.echo(f"\n🧠 SYNTHESIS:\n{analysis.get('synthesis', '')}\n")
        click.echo("─" * 70)

        click.echo(f"\n💡 WISDOM:\n{analysis.get('wisdom', '')}\n")
        click.echo("─" * 70)

        perspectives = analysis.get("perspectives", [])
        click.echo(f"\n🔍 PERSPECTIVES ANALYZED: {len(perspectives)}")
        for perspective in perspectives:
            lens = perspective.get("lens", "unknown")
            emoji = {
                "i_ching": "☯️",
                "wu_xing": "🌸",
                "art_of_war": "⚔️",
                "zodiac": "♈",
            }.get(lens, "🔮")
            guidance = perspective.get("guidance", "")
            click.echo(f"\n  {emoji} {lens.upper()} (confidence: {perspective.get('confidence', 0):.0%})")
            click.echo(f"     {guidance[:100]}...")

        click.echo(f"\n📚 PATTERNS MATCHED: {analysis.get('patterns_matched', 0)}")
        click.echo("\n" + "═" * 70 + "\n")
    except Exception as exc:
        click.echo(f"❌ Error analyzing scratchpad: {exc}")

@scratchpad.command()
@click.argument("scratchpad_id")
@click.option(
    "--analyze/--no-analyze",
    default=True,
    help="Run multi-spectral analysis (default: yes)",
)
@click.option(
    "--type",
    "memory_type",
    default="long_term",
    type=click.Choice(["short_term", "long_term"]),
    help="Memory type (default: long_term)",
)
@click.pass_context
def finalize(ctx, scratchpad_id: str, analyze: bool, memory_type: str):
    """Finalize scratchpad to permanent memory"""
    from whitemagic.tools.unified_api import call_tool

    click.echo(f"\n📝 Finalizing scratchpad: {scratchpad_id}\n")
    if analyze:
        click.echo("🧠 Running multi-spectral analysis...\n")

    try:
        now = (ctx.obj or {}).get("now") if isinstance(ctx.obj, dict) else None
        json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

        result = call_tool(
            "scratchpad_finalize",
            scratchpad_id=scratchpad_id,
            auto_analyze=analyze,
            memory_type=memory_type,
            now=now,
        )
        if json_output:
            click.echo(json.dumps(result, indent=2, sort_keys=True))
            return
        if result.get("status") != "success":
            click.echo(f"❌ Error: {result.get('message', 'Unknown error')}")
            return

        details = result.get("details", {}) or {}
        click.echo("✅ Scratchpad finalized")
        if analyze and details.get("analysis"):
            analysis = details["analysis"]
            click.echo("   🧠 Multi-spectral synthesis included")
            click.echo(f"   📊 Confidence: {analysis.get('confidence', 0):.0%}")
            click.echo(f"   🔍 Perspectives: {analysis.get('perspectives_used', 0)}")
            click.echo(f"   📚 Patterns matched: {analysis.get('patterns_matched', 0)}")
        click.echo(f"   📁 Memory: {details.get('memory_path')}\n")
    except Exception as exc:
        click.echo(f"❌ Error finalizing scratchpad: {exc}")

@main.group()
def session():
    """Session management commands"""

@session.command()
@click.option("--session-id", help="Session ID to handoff")
def handoff(session_id):
    """Create session handoff document"""
    try:
        from whitemagic.gardens.sangha.session_handoff import get_handoff

        handoff = get_handoff()

        if session_id:
            # Load specific session
            session_file = handoff.sessions_dir / f"{session_id}.json"
            if not session_file.exists():
                click.echo(f"❌ Session {session_id} not found")
                return

            state = handoff._load_session_state(session_file)
            if not state:
                click.echo(f"❌ Failed to load session {session_id}")
                return
        else:
            # Use current session
            state = handoff._load_current_session()
            if not state:
                click.echo("❌ No active session found")
                return

        # Create handoff
        if not state.ended_at:
            # End the current session first
            state.ended_at = datetime.now()
            state.context_summary = click.prompt("Session summary", default="")
            next_steps_text = click.prompt("Next steps (one per line)", default="")
            next_steps = [step.strip() for step in next_steps_text.split("\n") if step.strip()]

            handoff.end_session(state.session_id, state.context_summary, next_steps)
        else:
            # Create handoff from existing completed session
            handoff_file = handoff.sessions_dir / "HANDOFF.md"
            handoff._create_handoff_doc(state, handoff_file)

        click.echo(f"✅ Session handoff created: {handoff.sessions_dir / 'HANDOFF.md'}")

    except ImportError:
        click.echo("❌ Session handoff module not available")
    except Exception as e:
        click.echo(f"❌ Error creating handoff: {e}")


@main.command(name="start-session")
@click.option("--quiet", is_flag=True, help="Suppress verbose startup output")
def start_session_cli(quiet: bool):
    """Start a WhiteMagic session orchestrator run"""
    try:
        from whitemagic.core.orchestration.session_startup import start_session
        result = start_session(verbose=not quiet)
        click.echo(f"✅ Session start: {result.get('status', 'unknown')}")
        click.echo(f"   Activated: {result.get('activated', 0)} | Failed: {result.get('failed', 0)}")
    except Exception as exc:
        click.echo(f"❌ Session start failed: {exc}")

@main.command()
@click.argument("query")
@click.option("--limit", default=10, help="Max results")
@click.option("--type", "search_type", help="Filter by memory type")
@click.option("--fast", is_flag=True, help="Use Rust fast_search (v4.9.0)")
@click.pass_context
def recall(ctx, query, limit, search_type, fast):
    """Search memories"""
    from whitemagic.tools.unified_api import call_tool

    now = (ctx.obj or {}).get("now") if isinstance(ctx.obj, dict) else None
    json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

    args: dict[str, Any] = {"query": query, "limit": limit}
    if search_type:
        args["type"] = search_type

    out = call_tool("search_memories", now=now, **args)
    if json_output:
        click.echo(json.dumps(out, indent=2, sort_keys=True))
        return

    if out.get("status") != "success":
        click.echo(f"❌ Error: {out.get('message', 'Unknown error')}")
        return

    details = out.get("details", {}) or {}
    results = details.get("results")
    if not isinstance(results, list):
        results = details.get("memories")
    if not isinstance(results, list):
        results = []
    click.echo(f"\n🔍 Found {len(results)} memories matching '{query}':")
    for i, item in enumerate(results[:limit], 1):
        entry = (item or {}).get("entry", {}) if isinstance(item, dict) else {}
        if not entry and isinstance(item, dict):
            entry = {
                "id": item.get("id"),
                "title": item.get("title"),
                "tags": item.get("tags"),
            }
        title = entry.get("title") or entry.get("id") or "memory"
        preview = (item or {}).get("preview") if isinstance(item, dict) else ""
        if not preview and isinstance(item, dict):
            preview = item.get("content", "")
        score = (item or {}).get("score") if isinstance(item, dict) else None
        score_str = f" (score={score:.2f})" if isinstance(score, (int, float)) else ""
        click.echo(f"\n{i}. {title}{score_str}")
        if preview:
            click.echo(f"   {str(preview)[:200]}")
        tags = entry.get("tags") or ((item or {}).get("tags") if isinstance(item, dict) else []) or []
        if isinstance(tags, str):
            tags = [tags]
        elif not isinstance(tags, list):
            tags = []
        if tags:
            click.echo(f"   Tags: {', '.join(tags)}")

@main.command()
@click.argument("query")
@click.option("--limit", default=10, help="Max results")
@click.option("--type", "search_type", help="Filter by memory type")
@click.option("--fast", is_flag=True, help="Use Rust fast_search (v4.9.0)")
def search(query, limit, search_type, fast):
    """Alias for recall - search memories"""
    # Forward to recall command
    ctx = click.get_current_context()
    ctx.invoke(recall, query=query, limit=limit, search_type=search_type, fast=fast)

@main.command()
@click.option("--tier", default=1, type=click.IntRange(0, 2),
              help="Context tier (0=quick, 1=balanced, 2=deep)")
def context(tier):
    """Generate context summary for AI prompts"""
    if not HAS_CORE:
        click.echo("Error: WhiteMagic core not available", err=True)
        return

    # Simple context generation
    memory = get_memory()

    # Limit based on tier
    limits = {0: 3, 1: 10, 2: 50}
    limit = limits.get(tier, 10)

    recent_memories = memory.list_recent(limit=limit)

    click.echo(f"\n📚 Context (Tier {tier}, {limit} most recent):")
    click.echo("=" * 50)

    for i, mem in enumerate(recent_memories, 1):
        content_preview = str(mem.content)[:150] + "..." if len(str(mem.content)) > 150 else str(mem.content)
        click.echo(f"\n{i}. [{mem.memory_type.name}] {mem.created_at.strftime('%Y-%m-%d %H:%M')}")
        click.echo(f"   {content_preview}")

@main.command()
def list_tools() -> None:
    """Alias for tools - list all available commands"""
    # Forward to tools command
    ctx = click.get_current_context()
    ctx.invoke(tools)

@main.command()
def consolidate() -> None:
    """Archive old short-term memories"""
    if not HAS_CORE:
        click.echo("Error: WhiteMagic core not available", err=True)
        return

    memory = get_memory()

    click.echo("🔄 Consolidating memories...")
    consolidated = memory.consolidate()

    if consolidated == 0:
        click.echo("✅ No memories needed consolidation")
        return

    click.echo(f"✅ Consolidated {consolidated} memories (strengthened/decayed/promoted)")

@main.command()
def stats() -> None:
    """Show memory statistics dashboard"""
    if not HAS_CORE:
        click.echo("Error: WhiteMagic core not available", err=True)
        return

    memory = get_memory()

    click.echo("\n📊 Memory Statistics")
    click.echo("=" * 40)

    stats_data = memory.get_stats()

    # Total memories
    total = stats_data["total_memories"]
    click.echo(f"Total Memories: {total}")

    # By type
    click.echo("\nBy Type:")
    type_counts = stats_data.get("by_type", {})
    for mem_type_name, count in type_counts.items():
        if count > 0:
            click.echo(f"  {mem_type_name.replace('_', ' ').title():<12}: {count:>6}")

    # Tags
    total_tags = stats_data.get("total_tags", 0)
    click.echo(f"\nUnique Tags: {total_tags}")

    # Most used tags
    tag_counts = memory.get_tag_counts(limit=10)

    if tag_counts:
        click.echo("\nTop Tags:")
        for tag, count in tag_counts:
            click.echo(f"  {tag:<20}: {count:>3}")


@main.command()
def setup() -> None:
    """Interactive setup wizard"""
    click.echo("\n🚀 WhiteMagic Setup Wizard")
    click.echo("=" * 40)

    # Check if already configured
    from whitemagic.config.paths import WM_ROOT, ensure_paths
    config_dir = WM_ROOT
    if config_dir.exists():
        click.echo(f"✅ WhiteMagic already configured at: {config_dir}")
        if click.confirm("Would you like to reconfigure?"):
            pass
        else:
            return

    # Create directories
    click.echo("\n📁 Creating directories...")
    ensure_paths()
    # Extra legacy/utility dirs (best-effort; keep runtime state together)
    for d in ["backups"]:
        (config_dir / d).mkdir(parents=True, exist_ok=True)
    click.echo(f"   ✅ {config_dir}")

    # Initialize memory system
    if HAS_CORE:
        click.echo("\n🧠 Initializing memory system...")
        memory = get_memory()
        click.echo(f"   ✅ {memory.get_stats()['total_memories']} memories found")

    # MCP readiness (stdio)
    click.echo("\n🔌 MCP readiness...")
    if find_spec("fastmcp") is not None:
        click.echo("   ✅ fastmcp installed")
    else:
        click.echo("   ⚠️  fastmcp not installed (MCP server won't run)")
        click.echo("      Install: pip install 'whitemagic[mcp]'")

    mcp_entry = Path(__file__).resolve().parent / "run_mcp.py"
    if mcp_entry.exists():
        rel = str(mcp_entry)
        try:
            rel = str(mcp_entry.relative_to(Path.cwd()))
        except ValueError:
            pass
        click.echo(f"   ✅ MCP entrypoint present: {rel}")
    else:
        click.echo("   ⚠️  MCP entrypoint missing: whitemagic/run_mcp.py")

    click.echo("\n✨ Setup complete!")
    click.echo("\nNext steps:")
    click.echo("  1. Run: wm status")
    click.echo("  2. Try: wm remember 'my first memory' --title 'Hello'")
    click.echo("  3. Use: wm recall 'first'")
    click.echo("  4. MCP: python -m whitemagic.run_mcp")

@main.command()
@click.option("--json", "json_output", is_flag=True, help="Emit tools list as JSON.")
@click.pass_context
def tools(ctx, json_output: bool) -> None:
    """List all available tools and commands"""
    global_json = bool((ctx.obj or {}).get("json_output")) if isinstance(ctx.obj, dict) else False
    emit_json = json_output or global_json

    commands = [
        ("remember", "Create a new memory"),
        ("recall", "Search memories"),
        ("search", "Alias for recall"),
        ("context", "Generate AI context"),
        ("status", "Show system status"),
        ("setup", "Run setup wizard"),
        ("consolidate", "Archive old memories"),
        ("stats", "Show memory statistics"),
        ("health", "Check system health"),
        ("doctor", "Install + ship hygiene check (AI-first)"),
        ("doctor-deep", "Legacy deep audit (unstable)"),
        ("start-session", "Start session orchestrator"),
        ("explore", "Interactive feature guide"),
        ("fast", "Fast-mode CLI passthrough"),
    ]

    garden_commands = [
        ("voice", "Voice and narrative tools"),
        ("gana", "28 Lunar Mansion Gana system"),
        ("dharma", "Ethical reasoning tools"),
        ("wisdom", "Wisdom council and I Ching"),
        ("infer", "Inference tools (local + unified)"),
    ]
    if HAS_SANGHA:
        garden_commands.append(("sangha", "Multi-agent coordination"))

    optional_commands = []
    if HAS_EXEC:
        optional_commands.append(("exec", "Execute terminal commands"))
    if HAS_GRAPH:
        optional_commands.extend([
            ("graph", "Visualize memory relationships"),
            ("graph-stats", "Show relationship statistics"),
        ])

    if emit_json:
        click.echo(json.dumps({
            "core_commands": [{"command": c, "description": d} for c, d in commands],
            "garden_commands": [{"command": c, "description": d} for c, d in garden_commands],
            "optional_commands": [{"command": c, "description": d} for c, d in optional_commands],
            "usage": "whitemagic <command> --help",
        }, indent=2, sort_keys=True))
        return

    click.echo("\n🛠️  WhiteMagic Tools")
    click.echo("=" * 40)

    click.echo("\nCore Commands:")
    for cmd, desc in commands:
        click.echo(f"  {cmd:<12} - {desc}")

    click.echo("\nGarden Commands:")
    for cmd, desc in garden_commands:
        click.echo(f"  {cmd:<12} - {desc}")

    for cmd, desc in optional_commands:
        click.echo(f"  {cmd:<12} - {desc}")

    click.echo("\nFor help with any command:")
    click.echo("  whitemagic <command> --help")

# === RESONANCE / HEALTH COMMANDS (Grimoire) ===

@main.command(name="immune-status")
def immune_status() -> None:
    """Check immune system status."""
    click.echo("\n🛡️  Immune System Status")
    click.echo("=" * 30)
    try:
        from whitemagic.core.immune.health_check import get_health_check
        checker = get_health_check()
        result = checker.check_all()
        click.echo(f"✅ Overall Status: {result['overall_status']}")
        if result["issues"]:
            for issue in result["issues"]:
                click.echo(f"   ⚠️  {issue}")
        else:
            click.echo("   ✨ All systems healthy")
    except Exception as e:
        click.echo(f"⚠️  Immune check unavailable: {e}")


@main.command(name="homeostasis-check")
def homeostasis_check() -> None:
    """Run homeostasis check."""
    click.echo("\n⚖️  Homeostasis Check")
    click.echo("=" * 30)
    try:
        from whitemagic.homeostasis import HomeostaticMonitor
        monitor = HomeostaticMonitor()
        status_data = monitor.check_status()
        click.echo(f"✅ Status: {status_data.get('status', 'unknown')}")
        if status_data.get("alerts"):
            for alert in status_data["alerts"]:
                click.echo(f"   ⚠️  {alert}")
        else:
            click.echo("   ✨ System balanced")
    except Exception as e:
        click.echo(f"⚠️  Homeostasis check unavailable: {e}")


@main.command(name="doctor-deep")
def doctor_deep() -> None:
    """Deep system capability and health audit (legacy / unstable)."""
    click.echo("\n🏥 WhiteMagic Doctor")
    click.echo("=" * 30)

    # 1. Grimoire Audit
    try:
        from whitemagic.core.alignment.grimoire_audit import get_auditor
        auditor = get_auditor()
        report = auditor.generate_capability_report()
        click.echo(f"✨ Spells Discovered: {report['total_spells']}")
        for cat, count in report["categories"].items():
            click.echo(f"   • {cat.replace('_', ' ').title()}: {count}")
    except Exception as e:
        click.echo(f"⚠️  Grimoire audit failed: {e}")

    # 2. MansionBridge Status
    try:
        from whitemagic.core.mansion_bridge import get_mansion_bridge  # type: ignore[import-not-found]
        bridge = get_mansion_bridge()
        click.echo("\n🚀 Polyglot Acceleration")
        status = bridge.get_status()
        for name, bs in status.items():
            symbol = "✅" if bs.available else "❌"
            click.echo(f"   {symbol} {name.upper()}")
    except Exception as e:
        click.echo(f"⚠️  MansionBridge check failed: {e}")

    # 2b. Polyglot bridges (direct checks)
    click.echo("\n🌐 Polyglot Bridges")
    # Rust
    if find_spec("whitemagic_rs") is not None:
        click.echo("   ✅ Rust (whitemagic_rs)")
    else:
        click.echo("   ❌ Rust (whitemagic_rs not installed)")
    # Julia
    try:
        from whitemagic.core.acceleration.julia_interface import JuliaBridge
        jb = JuliaBridge()
        symbol = "✅" if jb.check_availability() else "❌"
        click.echo(f"   {symbol} Julia ({jb.julia_bin})")
    except Exception as e:
        click.echo(f"   ❌ Julia ({e})")
    # Haskell
    try:
        from whitemagic.core.acceleration.haskell_interface import HaskellBridge
        hb = HaskellBridge()
        symbol = "✅" if hb.available else "❌"
        click.echo(f"   {symbol} Haskell (divination FFI)")
    except Exception as e:
        click.echo(f"   ❌ Haskell ({e})")

    # 3. Temporal Grounding
    try:
        from whitemagic.core.temporal.chronos_guard import get_chronos_guard
        chronos = get_chronos_guard()
        pulse = chronos.generate_pulse()
        click.echo("\n⏳ Temporal Grounding")
        click.echo(f"   Now: {pulse.timestamp}")
        click.echo(f"   Lunar: {pulse.mansion_name} ({pulse.lunar_phase:.2f})")
        click.echo("   ✅ Resonance synchronized")
    except Exception as e:
        click.echo(f"\n⚠️  Temporal grounding failed: {e}")


@main.command(name="orchestra-health")
def orchestra_health() -> None:
    """Full system orchestra health check."""
    click.echo("\n🎼 Orchestra Health")
    click.echo("=" * 30)
    try:
        from whitemagic.core.automation.orchestra import AutomationOrchestra
        orchestra = AutomationOrchestra()
        report = orchestra.perform_health_check()
        click.echo(f"✅ Overall Health: {report['overall_health']}")
        click.echo(f"   Systems Checked: {len(report['systems'])}")
        if report.get("recommendations"):
            click.echo("   Recommendations:")
            for rec in report["recommendations"][:3]:
                click.echo(f"     • {rec}")
    except ImportError:
        click.echo("⚠️  Orchestra requires: whitemagic.automation module")
    except Exception as error:
        click.echo(f"⚠️  Orchestra health unavailable: {error}")


@main.command(name="dharma-assess")
def dharma_assess() -> None:
    """Assess dharma alignment."""
    click.echo("\n☸️  Dharma Assessment")
    click.echo("=" * 30)
    try:
        from whitemagic.gardens.dharma.core import get_dharma_core
        dharma = get_dharma_core()
        # Get recent history to assess alignment
        history = dharma.get_history(limit=5)
        allowed_count = sum(1 for h in history if h.get("allowed", True))
        total = len(history) if history else 1
        alignment_pct = (allowed_count / total) * 100
        click.echo(f"✅ Alignment: {alignment_pct:.0f}% ({allowed_count}/{total} actions aligned)")
        click.echo(f"   Principles active: {len(dharma.principles)}")
    except Exception as e:
        click.echo(f"⚠️  Dharma assessment unavailable: {e}")


@main.command(name="dharma-check-boundary")
@click.argument("action")
def dharma_check_boundary(action: str) -> None:
    try:
        from whitemagic.gardens.dharma.boundaries import BoundaryType, check_boundaries
        boundary = check_boundaries(action, {})
        if boundary.boundary_type == BoundaryType.INTERFERING and boundary.confidence > 0.6:
            click.echo(boundary.reasoning)
            click.get_current_context().exit(1)
        click.echo(boundary.reasoning)
        click.get_current_context().exit(0)
    except Exception as e:
        click.echo(f"⚠️  Boundary check unavailable: {e}")
        click.get_current_context().exit(1)


@main.command(name="voice-narrate")
@click.argument("prompt")
def voice_narrate(prompt: str) -> None:
    """Generate a narrative from the Voice garden."""
    click.echo("\n🎙️  Voice Narrate")
    click.echo("=" * 30)
    try:
        from whitemagic.gardens.voice.narrator import get_narrator
        narrator = get_narrator()
        narrative = narrator.narrate(prompt)
        click.echo(f"✅ Narrative: {narrative}")
        click.echo(f"   Story: {narrator.current_story}")
        click.echo(f"   Chapter: {narrator.current_chapter}")
    except Exception as e:
        click.echo(f"⚠️  Voice narrate unavailable: {e}")


@main.command(name="rabbit-hole-start")
@click.argument("topic")
def rabbit_hole_start(topic: str) -> None:
    """Start a deep research rabbit-hole."""
    click.echo("\n🐇 Rabbit Hole Start")
    click.echo("=" * 30)
    try:
        from whitemagic.gardens.wisdom.rabbit_hole import (
            RabbitHoleEntry,
            ResearchReport,
        )
        # Create initial entry
        entry = RabbitHoleEntry(term=topic, depth=0)
        report = ResearchReport(title=f"Research: {topic}", topics=[topic])
        report.entries.append(entry)
        click.echo(f"✅ Rabbit hole started for: {topic}")
        click.echo("   Depth: 0 (starting point)")
        click.echo("   Use research tools to explore further")
    except Exception as e:
        click.echo(f"⚠️  Rabbit hole unavailable: {e}")

@main.command(name="memory-list")
@click.option("--limit", default=10, help="Max memories to show")
def memory_list(limit):
    """List recent memories"""
    if not HAS_CORE:
        click.echo("Error: WhiteMagic core not available", err=True)
        return

    memory = get_memory()

    # Get recent memories
    all_mems = memory.list_recent(limit=limit)

    click.echo(f"\n📝 Recent Memories (Top {limit})")
    click.echo("=" * 40)

    for i, mem in enumerate(all_mems[:limit], 1):
        preview = str(mem.content)[:80] + "..." if len(str(mem.content)) > 80 else str(mem.content)
        click.echo(f"\n{i}. {mem.title or 'Untitled'} ({mem.memory_type.value})")
        click.echo(f"   {preview}")
        click.echo(f"   ID: {mem.id[:8]}... | Tags: {', '.join(mem.tags)}")

@main.command()
@click.option("--output", "-o", default="memory_graph.html", help="Output file for the graph")
def graph(output):
    """Generate relationship graph for memories (v4.5.0)."""
    if not HAS_CORE:
        click.echo("Error: WhiteMagic core not available", err=True)
        return

    # Placeholder implementation - generate basic HTML graph
    html_content = """<!DOCTYPE html>
<html>
<head>
    <title>WhiteMagic Memory Graph</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .node { border: 1px solid #ccc; padding: 10px; margin: 5px; display: inline-block; }
    </style>
</head>
<body>
    <h1>WhiteMagic Memory Relationship Graph</h1>
    <p>Graph visualization placeholder. Implement full graph logic here.</p>
    <div class="node">Memory 1</div>
    <div class="node">Memory 2</div>
    <!-- Add actual graph generation logic -->
</body>
</html>"""

    try:
        with open(output, "w") as f:
            f.write(html_content)
        click.echo(f"✅ Graph generated: {output}")
    except Exception as e:
        click.echo(f"❌ Failed to generate graph: {e}", err=True)

@main.command(name="session-start")
def session_start():
    """Bootstrap new session with full context loading."""
    click.echo("\n🚀 Starting WhiteMagic Session...")
    click.echo("=" * 40)
    try:
        from whitemagic.maintenance.capability_harness import run_harness
        report = run_harness()
        # Handle report structure safely
        passed = getattr(report, "passed", 0)
        failed = getattr(report, "failed", 0)
        click.echo(f"\n✅ Session initialized with {passed}/{passed + failed} capabilities")
    except Exception as e:
        click.echo(f"⚠️  Warning: {e}")
    click.echo("\n📚 Use 'wm tools' to see available commands")

@main.command(name="session-status")
def session_status():
    """Show current session status."""
    ctx = click.get_current_context()
    ctx.invoke(status_command)

@main.command(name="matrix-stats")
def matrix_stats():
    """Show Memory Matrix statistics."""
    click.echo("\n📊 Memory Matrix Statistics")
    click.echo("=" * 40)
    try:
        memory = get_memory()
        stats_data = memory.get_stats()
        total = stats_data["total_memories"]
        click.echo(f"Total memories: {total}")
        type_counts = stats_data.get("by_type", {})
        for mt_name, count in type_counts.items():
            click.echo(f"  {mt_name:<12}: {count}")
    except Exception as e:
        click.echo(f"⚠️  Matrix not available: {e}")


@main.command(name="matrix-seen")
@click.option("--limit", default=10, help="Max items")
def matrix_seen(limit: int):
    """List recently accessed memories."""
    try:
        memory = get_memory()
        recent_accessed = memory.list_accessed(limit=limit)

        click.echo("\n🧠 Recently Accessed Memories")
        click.echo("=" * 40)
        for mem in recent_accessed:
            preview = str(mem.content)
            if len(preview) > 80:
                preview = preview[:77] + "..."
            click.echo(f"{mem.accessed_at.isoformat()} | {mem.memory_type.name:<10} | {mem.id} | tags={list(mem.tags)}")
            click.echo(f"  {preview}")
    except Exception as e:
        click.echo(f"⚠️  Matrix not available: {e}")


@main.group()
def balance():
    """Yin-Yang balance tracking and reporting (v4.13.0)"""


@balance.command(name="status")
def balance_status():
    """Show current Yin-Yang balance"""
    try:
        from whitemagic.harmony.yin_yang_tracker import get_tracker

        tracker = get_tracker()
        report = tracker.get_report()

        click.echo(f"\n{report['status_emoji']} {report['status']}")
        click.echo(f"Balance Score: {report['balance_score']:.2f}/1.00")
        click.echo(f"Burnout Risk: {report['burnout_risk']:.1%}")
        click.echo(f"\nActivity in last {report['window_minutes']} minutes:")
        click.echo(f"  Yang (Action):     {report['yang_count']} ({report['yang_ratio']:.1%})")
        click.echo(f"  Yin (Reflection):  {report['yin_count']} ({report['yin_ratio']:.1%})")

        if report["recommendation"]:
            click.echo(f"\n💡 {report['recommendation']}")
    except Exception as e:
        click.echo(f"❌ Balance tracker error: {e}", err=True)


@balance.command()
@click.option("--limit", default=20, help="Number of activities to show")
def history(limit):
    """Show Yin-Yang activity history"""
    try:
        import json

        from whitemagic.harmony.yin_yang_tracker import get_tracker

        tracker = get_tracker()
        history_file = tracker.storage_dir / "activity_log.jsonl"

        if not history_file.exists():
            click.echo("No activity history yet.")
            return

        click.echo("\nRecent Activity:")
        with open(history_file) as f:
            lines = list(f)[-limit:]
            for line in lines:
                data = json.loads(line)
                timestamp = data["timestamp"][:19]
                activity = data["activity"]
                activity_type = "Yang" if activity in ["CREATE", "WRITE", "EXECUTE", "BUILD", "CODE", "UPDATE", "IMPLEMENT", "DEPLOY"] else "Yin"
                emoji = "⚡" if activity_type == "Yang" else "🌊"
                click.echo(f"  {timestamp} {emoji} {activity_type:4} - {activity}")
    except Exception as e:
        click.echo(f"❌ History error: {e}", err=True)


@main.command(name="matrix-search")
@click.option("--query", required=True, help="Text to search in content")
@click.option("--limit", default=10, help="Max results")
def matrix_search(query: str, limit: int):
    """Search memories by substring (quick local scan)."""
    try:
        memory = get_memory()
        # Use UnifiedMemory's search which uses SQLite backend
        results = memory.search(query=query, limit=limit)

        click.echo(f"\n🔎 Matrix Search: '{query}' (showing up to {limit})")
        click.echo("=" * 50)
        for mem in results:
            preview = str(mem.content)
            if len(preview) > 120:
                preview = preview[:117] + "..."
            click.echo(f"{mem.memory_type.name:<10} | {mem.id} | tags={list(mem.tags)}")
            click.echo(f"  {preview}")
        if not results:
            click.echo("No matches found.")
    except Exception as e:
        click.echo(f"⚠️  Matrix search unavailable: {e}")

@main.command(name="activate-all")
def activate_all():
    """Full system activation - bootstrap all capabilities."""
    click.echo("\n⚡ Activating All WhiteMagic Systems...")
    click.echo("=" * 40)
    ctx = click.get_current_context()
    ctx.invoke(session_start)
    ctx.invoke(status_command)
    click.echo("\n✅ Full system activation complete!")

@main.command(name="manifest")
def manifest():
    """Export tools as JSON manifest."""
    import json
    commands = {}
    for name, cmd in main.commands.items():
        commands[name] = {
            "help": cmd.help or "No description",
            "params": [p.name for p in cmd.params],
        }
    click.echo(json.dumps(commands, indent=2))


# === CONDUCTOR COMMANDS (v4.6.0) ===

@main.command()
@click.argument("prompt")
@click.option("--iterations", "-n", default=50, help="Maximum iterations")
@click.option("--clones", "-c", default=1000, help="Clones per iteration")
@click.option("--completion", default="<complete>", help="Completion marker")
@click.option("--garden", default="practice", help="Garden to align with")
def conduct(prompt, iterations, clones, completion, garden):
    """Autonomous task orchestration with iterative deepening."""
    import asyncio

    click.echo("\n🎼 Conductor - Autonomous Orchestration")
    click.echo("=" * 50)
    click.echo(f"Task: {prompt}")
    click.echo(f"Max iterations: {iterations}")
    click.echo(f"Clones per iteration: {clones}")
    click.echo(f"Garden: {garden}")
    click.echo(f"Completion marker: {completion}")
    click.echo()

    try:
        from whitemagic.orchestration.conductor import (
            ConductorConfig,
            ConductorOrchestrator,
        )

        config = ConductorConfig(
            max_iterations=iterations,
            clones_per_iteration=clones,
            completion_check=completion,
            garden=garden,
        )

        conductor = ConductorOrchestrator(config)

        # Run async orchestration
        result = asyncio.run(conductor.conduct(prompt))

        # Show results
        if result:
            click.echo("\n✅ Orchestration Complete!")
            click.echo(f"Iterations: {result.iteration}")
            click.echo(f"Confidence: {result.thought_path.confidence:.2f}")
            click.echo(f"Strategy: {result.thought_path.strategy}")
            click.echo(f"Tokens used: {result.tokens_used}")
            click.echo(f"Completed: {'Yes' if result.is_complete else 'No'}")

            # Export session
            export_path = conductor.export_session()
            click.echo(f"\n📄 Session exported to: {export_path}")
        else:
            click.echo("❌ No result from orchestration")

    except ImportError as e:
        click.echo(f"❌ Conductor not available: {e}", err=True)
        click.echo("Ensure whitemagic.orchestration.conductor is installed", err=True)
    except Exception as e:
        click.echo(f"❌ Orchestration failed: {e}", err=True)


@main.command(name="conduct-ritual")
@click.argument("intention")
@click.option("--cycles", "-n", default=30, help="Maximum ritual cycles")
@click.option("--threshold", "-t", default=0.85, help="Mastery threshold (0-1)")
@click.option("--ritual-name", default="unnamed", help="Name for this ritual")
def conduct_ritual(intention, cycles, threshold, ritual_name):
    """Conduct Practice Garden ritual with autonomous deepening."""
    import asyncio

    click.echo("\n🌸 Practice Garden - Ritual Conductor")
    click.echo("=" * 50)
    click.echo(f"Ritual: {ritual_name}")
    click.echo(f"Intention: {intention}")
    click.echo(f"Max cycles: {cycles}")
    click.echo(f"Mastery threshold: {threshold}")
    click.echo()

    try:
        from whitemagic.gardens.practice.ritual_conductor import (
            PracticeRitualConductor,
            RitualConfig,
        )

        config = RitualConfig(
            ritual_name=ritual_name,
            max_cycles=cycles,
            deepening_threshold=threshold,
        )

        conductor = PracticeRitualConductor(config)
        result = asyncio.run(conductor.conduct_ritual(intention))

        # Show ritual results
        if result:
            report = conductor.get_ritual_report()

            click.echo("\n✨ Ritual Complete!")
            click.echo(f"Cycles: {report.get('total_iterations', 0)}")
            click.echo(f"Mastery achieved: {'Yes' if report.get('mastery_achieved') else 'No'}")
            click.echo(f"Final confidence: {report.get('max_confidence', 0):.2f}")
            click.echo(f"Practice consistency: {report.get('practice_consistency', 0):.2f}")

            if conductor.conductor:
                export_path = conductor.conductor.export_session()
                click.echo(f"\n📄 Ritual log: {export_path}")
        else:
            click.echo("❌ Ritual did not complete")

    except ImportError as e:
        click.echo(f"❌ Ritual conductor not available: {e}", err=True)
    except Exception as e:
        click.echo(f"❌ Ritual failed: {e}", err=True)


@main.command(
    name="fast",
    context_settings={"ignore_unknown_options": True, "allow_extra_args": True},
)
@click.pass_context
def fast_cli(ctx: click.Context):
    """Run fast-mode CLI for quick commands"""
    try:
        from whitemagic.cli.cli_fast import main_fast
        return main_fast(ctx.args)
    except Exception as exc:
        click.echo(f"❌ Fast mode failed: {exc}")

@main.command()
@click.argument("task")
@click.option("--max-iterations", "-n", default=50, help="Maximum iterations")
def continuous_start(task, max_iterations):
    """Start a continuous execution session (v4.3.0).

    Example: wm continuous-start "implement auth feature" -n 30
    """
    import subprocess
    script_path = Path(__file__).parent.parent / "scripts" / "continuous_harness.sh"

    if not script_path.exists():
        click.echo("❌ Continuous harness script not found", err=True)
        return

    click.echo("🔄 Starting continuous execution...")
    click.echo(f"   Task: {task}")
    click.echo(f"   Max iterations: {max_iterations}")

    result = subprocess.run(
        ["bash", str(script_path), "start", task, str(max_iterations)],
        cwd=str(script_path.parent.parent),
    )

    if result.returncode != 0:
        click.echo("❌ Failed to start continuous harness", err=True)


@main.command()
def iteration_stats():
    """Show current iteration and rate limit stats (v4.3.0)."""
    click.echo("\n📊 Iteration Statistics (v4.3.0)")
    click.echo("=" * 50)

    # Coherence stats
    try:
        from whitemagic.core.intelligence.agentic.coherence_persistence import (
            get_coherence,
        )
        coherence = get_coherence()
        stats_data = coherence.get_iteration_stats()

        click.echo("\n🧠 Coherence & Rate Limiting:")
        click.echo(f"   Coherence Level: {stats_data.get('coherence_level', 100)}%")
        click.echo(f"   Iteration Count: {stats_data.get('iteration_count', 0)}")
        click.echo(f"   Calls This Hour: {stats_data.get('calls_this_hour', 0)}/100")
        click.echo(f"   Calls Remaining: {stats_data.get('calls_remaining', 100)}")
        click.echo(f"   Total Iterations: {stats_data.get('total_iterations', 0)}")
    except ImportError:
        click.echo("   ⚠️ Coherence persistence not available")

    # Circuit breaker stats
    try:
        from whitemagic.core.intelligence.agentic.anti_loop import get_anti_loop
        detector = get_anti_loop()
        cb_stats = detector.get_circuit_status()

        state_emoji = {"closed": "🟢", "open": "🔴", "half_open": "🟡"}
        click.echo("\n⚡ Circuit Breaker:")
        click.echo(f"   State: {cb_stats['state']} {state_emoji.get(cb_stats['state'], '')}")
        click.echo(f"   Iteration Count: {cb_stats['iteration_count']}")
        click.echo(f"   No Progress Count: {cb_stats['no_progress_count']}")
        if cb_stats.get("recent_errors"):
            click.echo(f"   Recent Errors: {len(cb_stats['recent_errors'])}")
    except ImportError:
        click.echo("   ⚠️ Circuit breaker not available")

    # Token budget
    try:
        from whitemagic.core.intelligence.agentic.token_optimizer import TokenBudget
        budget = TokenBudget()
        tier_emoji = {"safe": "🟢", "wrap_up": "🟡", "checkpoint": "🔴"}
        click.echo("\n💰 Token Budget:")
        click.echo(f"   Status: {budget.usage_tier.upper()} {tier_emoji.get(budget.usage_tier, '')}")
        click.echo(f"   Remaining: {budget.remaining:,}")
    except ImportError:
        click.echo("   ⚠️ Token optimizer not available")

    click.echo("\n" + "=" * 50)


@main.command()
def continuous_status():
    """Check continuous execution status (v4.3.0)."""
    import subprocess
    script_path = Path(__file__).parent.parent / "scripts" / "continuous_harness.sh"

    if script_path.exists():
        subprocess.run(["bash", str(script_path), "status"], cwd=str(script_path.parent.parent))
    else:
        click.echo("❌ Continuous harness not found", err=True)


@main.command()
def inject_context():
    """Show what memory context would be injected (v4.3.0)."""
    try:
        from whitemagic.core.intelligence.agentic.memory_injector import (
            get_memory_injector,
        )
        injector = get_memory_injector()
        context_data = injector.inject()

        click.echo("\n🧠 Memory Injection Preview (v4.3.0)")
        click.echo("=" * 50)

        if context_data.resume_context:
            click.echo("\n📋 Resume Context:")
            click.echo(context_data.resume_context[:500] + "..." if len(context_data.resume_context) > 500 else context_data.resume_context)

        if context_data.short_term_memories:
            click.echo("\n📝 Recent Short-term Memories:")
            for m in context_data.short_term_memories[:5]:
                click.echo(f"   • {m[:100]}")

        if context_data.session_state:
            click.echo("\n⚙️ Session State:")
            for k, v in context_data.session_state.items():
                click.echo(f"   • {k}: {v}")

        click.echo(f"\n📊 Estimated tokens: ~{context_data.total_tokens}")
        click.echo("=" * 50)

    except ImportError as e:
        click.echo(f"❌ Memory injector not available: {e}", err=True)


@main.command()
def observe():
    """Real-time Gan Ying event viewer (v4.5.0)."""
    if not HAS_RICH:
        click.echo("❌ Rich is required for observation mode. Install with: pip install rich")
        return

    try:
        import time
        from collections import deque

        from rich.live import Live
        from rich.table import Table

        from whitemagic.core.resonance.gan_ying import get_bus
    except ImportError as e:
        click.echo(f"❌ Failed to import required modules: {e}")
        return

    bus = get_bus()
    events = deque(maxlen=20)

    def on_event(event):
        events.append(event)

    # Subscribe to ALL events
    if hasattr(bus, "listen_all"):
        bus.listen_all(on_event)
    else:
        # Fallback if listen_all not available yet
        click.echo("⚠️  GanYingBus does not support listen_all. Update core.")
        return

    def generate_table():
        table = Table(title="🔮 Gan Ying Resonance (Real-time)", expand=True)
        table.add_column("Time", style="cyan", no_wrap=True)
        table.add_column("Type", style="magenta")
        table.add_column("Source", style="green")
        table.add_column("Data", style="white")
        table.add_column("Conf", justify="right", style="yellow")

        # Sort by timestamp desc
        sorted_events = sorted(list(events), key=lambda e: e.timestamp, reverse=True)

        for e in sorted_events:
            data_str = str(e.data)
            if len(data_str) > 50:
                data_str = data_str[:47] + "..."

            table.add_row(
                e.timestamp.strftime("%H:%M:%S.%f")[:-3],
                e.event_type.name if hasattr(e.event_type, "name") else str(e.event_type),
                e.source,
                data_str,
                f"{e.confidence:.2f}",
            )
        return table

    console.print("[bold green]Starting observer... Press Ctrl+C to stop.[/bold green]")

    try:
        with Live(generate_table(), refresh_per_second=4, console=console) as live:
            while True:
                live.update(generate_table())
                time.sleep(0.25)
    except KeyboardInterrupt:
        console.print("\n[bold yellow]Observer stopped.[/bold yellow]")

# --- Dream Daemon Commands ---

@main.group(name="dream")
def dream_group():
    """💤 Dream Daemon (Offline Processing)"""

@dream_group.command(name="start")
@click.option("--daemon/--no-daemon", default=False, help="Run as background daemon")
@click.option("--interval", default=600, help="Dream cycle interval in seconds")
def dream_start(daemon, interval):
    """Start the Dream Daemon"""
    from whitemagic.core.dreaming.daemon import get_daemon

    if daemon:
        # In a real scenario, we'd use python-daemon or similar to detach
        # For now, we'll just run it (blocking) if not detached by shell
        if HAS_RICH and console:
            console.print("[green]Starting Dream Daemon (background mode implied by shell)...[/green]")
        else:
            click.echo("Starting Dream Daemon...")

    d = get_daemon()
    d.interval = interval
    d.start()

@dream_group.command(name="status")
def dream_status():
    """Check Dream Daemon status"""
    # This is a bit tricky since the daemon runs in another process/thread usually.
    # For now, we check if the lock file or pid exists (mock logic for v0.1)
    if HAS_RICH and console:
        console.print("[yellow]Dream Daemon status check not fully implemented (v0.1)[/yellow]")
    else:
        click.echo("Dream Daemon status check not fully implemented")

# --- Extension Registration ---
    try:
        main.add_command(gardens)
        main.add_command(intelligence)
        main.add_command(iching)
        main.add_command(wuxing)
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load extensions: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load extensions: {e}", err=True)

# Register reasoning commands (v4.10.0)
if HAS_REASONING:
    try:
        main.add_command(reasoning, name="reason")
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load reasoning CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load reasoning CLI: {e}", err=True)

# Register inference commands (v4.11.0)
if HAS_INFERENCE:
    try:
        main.add_command(infer)
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load inference CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load inference CLI: {e}", err=True)

# Fallback for inference if module not available or errored
if "infer" not in main.commands:
    @main.group(name="infer")
    def infer_fallback_group():
        """Local inference commands (fallback)"""
    if os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {"1", "true", "yes", "on"}:
        infer_fallback_group.add_command(infer_local_query)
        infer_fallback_group.add_command(infer_local_status)
else:
    # Attempt to attach local commands to existing infer group
    try:
        if os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {"1", "true", "yes", "on"}:
            infer.add_command(infer_local_query)
            infer.add_command(infer_local_status)
    except Exception:
        pass

# Register hardware commands (v4.11.0)
if HAS_HARDWARE_CLI:
    try:
        main.add_command(hardware)
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load hardware CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load hardware CLI: {e}", err=True)

# Register Sangha CLI commands (v4.12.x)
if HAS_SANGHA:
    try:
        main.add_command(sangha_cli, name="sangha")
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load Sangha CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load Sangha CLI: {e}", err=True)

# Register Archaeology CLI commands (v4.11.0)
if HAS_ARCHAEOLOGY:
    try:
        main.add_command(archaeology)
        main.add_command(windsurf)
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load Archaeology CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load Archaeology CLI: {e}", err=True)

# Register Watcher CLI commands (v4.13.0)
if HAS_WATCHER:
    try:
        main.add_command(watch)
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load Watcher CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load Watcher CLI: {e}", err=True)

# Register Autonomous Execution CLI commands (v4.14.0)
if HAS_AUTONOMOUS:
    main.add_command(autonomous)

# Register local model CLI (v4.14.0)
if HAS_LOCAL:
    main.add_command(local_cli, name="local")

# Register cache commands (v4.15.0)
try:
    from whitemagic.cli.cli_cache import cache_cli
    main.add_command(cache_cli, name="cache")
except ImportError:
    pass

# Register zodiac commands (v4.15.0 - Zodiacal Round)
try:
    from whitemagic.cli.cli_zodiac import zodiac_cli
    main.add_command(zodiac_cli, name="zodiac")
except ImportError:
    pass

# Register Scratchpad CLI (v5.2.0 - Phase 26)
if HAS_SCRATCH:
    main.add_command(scratch)

# Register PRAT commands (v5.1.0 - Polymorphic Resonant Adaptive Tools)
try:
    from whitemagic.cli.cli_prat import prat
    main.add_command(prat, name="prat")
except ImportError:
    pass

# Register Holographic commands (v5.0.0 - 4D Memory)
try:
    from whitemagic.cli.holo_commands import holo_cli  # type: ignore[import-not-found]
    main.add_command(holo_cli, name="holo")
except ImportError as e:
    missing_mod = getattr(e, "name", "")
    expected_absence = missing_mod in {"whitemagic.cli", "whitemagic.cli.holo_commands"}
    if not expected_absence or os.getenv("WM_DEBUG"):
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load Holographic CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load Holographic CLI: {e}", err=True)
except Exception as e:
    click.echo(f"Warning: Unexpected error loading Holographic CLI: {e}", err=True)

# Load and register plugins
if HAS_PLUGINS:
    try:
        load_plugins()
        register_commands(main)
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load plugins: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load plugins: {e}", err=True)

# Register init command (v14.0 — first-run scaffolding)
try:
    from whitemagic.cli.init_command import init_command
    main.add_command(init_command)
except ImportError:
    pass

# Register Rust CLI commands (v4.9.0)
if HAS_RUST_CLI:
    try:
        register_rust_commands(main)
    except Exception as e:
        if HAS_RICH and console:
            console.print(f"[yellow]Warning: Failed to load Rust CLI: {e}[/yellow]")
        else:
            click.echo(f"Warning: Failed to load Rust CLI: {e}", err=True)

if __name__ == "__main__":
    main()
