# mypy: disable-error-code=no-untyped-def
"""CLI commands for health diagnostics and system checks.

Extracted from cli_app.py in v13.0.0 for maintainability.
"""
from __future__ import annotations

import json
from importlib.util import find_spec

import click


def _console():
    try:
        from rich.console import Console
        return Console()
    except ImportError:
        return None


def _has_rich() -> bool:
    return find_spec("rich") is not None


@click.command(name="health")
@click.pass_context
def health_command(ctx: click.Context) -> None:
    """Run comprehensive health check"""
    from whitemagic.mcp_api_bridge import check_system_health

    json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False

    if json_output:
        result = check_system_health(component="system", deep_scan=False)
        click.echo(json.dumps(result, indent=2, sort_keys=True))
        return

    console = _console()
    if _has_rich() and console:
        from rich.panel import Panel

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
                    color = "green" if health_score >= 0.8 else "yellow" if health_score >= 0.5 else "red"
                    display_status = (
                        "✅ Healthy" if health_score >= 0.8
                        else "⚠️  Needs Attention" if health_score >= 0.5
                        else "❌ Critical"
                    )
                    acc_lines: list[str] = []
                    accelerators = result.get("accelerators", {})
                    if accelerators:
                        details = accelerators.get("details", {})
                        if details:
                            for name, info in details.items():
                                icon = "✅" if info.get("status") == "active" else "❌" if info.get("status") == "error" else "⚠️"
                                version = info.get("version", "unknown")
                                latency = f" ({info.get('latency_ms', 0):.2f}ms)" if "latency_ms" in info else ""
                                acc_lines.append(f"  {icon} {name}: {info.get('status')} [dim]v{version}{latency}[/dim]")
                    panel_content = (
                        f"[{color}]{display_status}[/{color}]\n\n"
                        f"[bold]Health Score:[/bold] {health_score:.0%}\n\n"
                    )
                    if acc_lines:
                        panel_content += "[bold]Accelerators:[/bold]\n" + "\n".join(acc_lines) + "\n\n"
                    panel_content += (
                        "[bold]Issues:[/bold]\n" + "\n".join(f"  • {i}" for i in issues)
                        if issues else "[green]No issues detected[/green]"
                    )
                    console.print(Panel(panel_content, title="🏥 System Health", border_style=color))
            except Exception as e:
                console.print(f"[red]❌ Error:[/red] {e}")
    else:
        try:
            result = check_system_health(component="system")
            click.echo(f"Health Status: {result.get('status', 'unknown')}")
        except Exception as e:
            click.echo(f"Error checking health: {e}")


@click.command(name="doctor")
@click.pass_context
def doctor_command(ctx: click.Context) -> None:
    """Run consolidated system diagnostics via health_report tool"""
    from whitemagic.tools.dispatch_table import dispatch

    json_output = (ctx.obj or {}).get("json_output") if isinstance(ctx.obj, dict) else False
    result = dispatch("health_report") or {}

    if json_output:
        click.echo(json.dumps(result, indent=2, sort_keys=True, default=str))
        return

    console = _console()
    if _has_rich() and console:
        from rich.panel import Panel

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
            lines.append(f"\n[bold]Gardens:[/bold] {len(result['gardens'])} registered")

        console.print(Panel("\n".join(lines), title="🏥 WhiteMagic Doctor", border_style=color))
    else:
        click.echo(f"Health: {result.get('health_status', 'unknown')} ({result.get('health_score', 0):.0%})")
        if "db" in result:
            click.echo(f"DB: {result['db'].get('memory_count', '?')} memories")
        click.echo(f"Rust: {'yes' if result.get('rust', {}).get('available') else 'no'}")
        click.echo(f"Julia: {'yes' if result.get('julia', {}).get('available') else 'no'}")


@click.command(name="immune-status")
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


@click.command(name="homeostasis-check")
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


def register(main: click.Group) -> None:
    """Register all diagnostic commands with the main CLI group."""
    main.add_command(health_command)
    main.add_command(doctor_command)
    main.add_command(immune_status)
    main.add_command(homeostasis_check)
