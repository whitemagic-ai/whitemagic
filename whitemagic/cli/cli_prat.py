# mypy: disable-error-code=no-untyped-def
"""CLI commands for Polymorphic Resonant Adaptive Tools (PRAT)

Commands:
    wm prat context  - Show current unified context
    wm prat invoke   - Invoke a tool through the adaptive portal
    wm prat morphologies - List available morphologies
"""

import asyncio

import click


@click.group()
def prat() -> None:
    """Polymorphic Resonant Adaptive Tools - context-aware tool system"""


@prat.command()
@click.option("--json", "as_json", is_flag=True, help="Output as JSON")
def context(as_json: bool) -> None:
    """Show current unified consciousness context"""
    import json

    from whitemagic.cascade.context_synthesizer import get_context_synthesizer

    synth = get_context_synthesizer()

    if as_json:
        ctx = synth.gather()
        output = {
            "primary_garden": ctx.primary_garden,
            "active_gardens": ctx.active_gardens,
            "wu_xing_phase": ctx.wu_xing_phase,
            "wu_xing_qualities": ctx.wu_xing_qualities,
            "zodiac_position": ctx.zodiac_position,
            "zodiac_element": ctx.zodiac_element,
            "zodiac_modality": ctx.zodiac_modality,
            "phase_intention": ctx.phase_intention,
            "yin_yang_balance": ctx.yin_yang_balance,
            "burnout_risk": ctx.burnout_risk,
            "coherence_level": ctx.coherence_level,
            "coherence_score": ctx.coherence_score,
            "dominant_influence": ctx.get_dominant_influence(),
            "recommended_morphology": ctx.get_recommended_morphology(),
            "time_of_day": ctx.time_of_day,
        }
        click.echo(json.dumps(output, indent=2))
    else:
        click.echo(synth.get_summary())


@prat.command()
@click.argument("tool_name")
@click.argument("query")
@click.option("--morphology", "-m", help="Force specific morphology")
@click.option("--limit", "-l", default=10, help="Result limit")
def invoke(tool_name: str, query: str, morphology: str | None, limit: int) -> None:
    """Invoke a tool through the adaptive portal

    Example:
        wm prat invoke recall "authentication patterns"
        wm prat invoke recall "design patterns" -m wisdom

    """
    from whitemagic.cascade.adaptive_portal import get_adaptive_portal

    async def run():
        portal = get_adaptive_portal()
        result = await portal.invoke(
            tool_name,
            {"query": query, "limit": limit},
            force_morphology=morphology,
        )
        return result

    result = asyncio.run(run())

    # Display result
    meta = result.pop("_meta", {})

    click.echo("\n🔮 PRAT Invocation Result")
    click.echo("=" * 50)
    click.echo(f"Tool: {meta.get('tool', tool_name)}")
    click.echo(f"Morphology: {meta.get('morphology', 'unknown')}")
    click.echo(f"Duration: {meta.get('duration_ms', 0):.1f}ms")

    ctx = meta.get("context_snapshot", {})
    click.echo("\nContext:")
    click.echo(f"  Wu Xing: {ctx.get('wu_xing_phase', '?')}")
    click.echo(f"  Zodiac: {ctx.get('zodiac_position', '?')}")
    click.echo(f"  Yin-Yang: {ctx.get('yin_yang_balance', 0):+.1f}")

    click.echo("\nResult:")
    click.echo(f"  Mode: {result.get('mode', 'unknown')}")
    click.echo(f"  Count: {result.get('count', 0)}")

    # Show mode-specific enhancements
    if "patterns" in result:
        click.echo(f"  Patterns: {result['patterns']}")
    if "questions" in result:
        click.echo(f"  Questions: {result['questions']}")
    if "lessons" in result:
        click.echo(f"  Lessons: {result['lessons']}")


@prat.command()
@click.option("--tool", "-t", help="Show morphologies for specific tool")
def morphologies(tool: str | None) -> None:
    """List available tool morphologies"""
    from whitemagic.cascade.adaptive_portal import get_adaptive_portal

    portal = get_adaptive_portal()

    click.echo("\n🎭 Available Morphologies")
    click.echo("=" * 50)

    if tool:
        variants = portal.morphologies.get(tool, {})
        if variants:
            click.echo(f"\n{tool}:")
            for name in sorted(variants.keys()):
                click.echo(f"  - {name}")
        else:
            click.echo(f"No morphologies found for tool: {tool}")
    else:
        for tool_name, variants in sorted(portal.morphologies.items()):
            click.echo(f"\n{tool_name}:")
            for name in sorted(variants.keys()):
                click.echo(f"  - {name}")


@prat.command()
def status() -> None:
    """Show PRAT system status"""
    from whitemagic.cascade.adaptive_portal import get_adaptive_portal
    from whitemagic.cascade.context_synthesizer import get_context_synthesizer

    synth = get_context_synthesizer()
    portal = get_adaptive_portal()

    click.echo("\n🔮 PRAT System Status")
    click.echo("=" * 50)

    # Count morphologies
    total_morphologies = sum(len(v) for v in portal.morphologies.values())
    click.echo(f"Tools registered: {len(portal.morphologies)}")
    click.echo(f"Total morphologies: {total_morphologies}")

    # Context status
    ctx = synth.gather()
    click.echo("\nCurrent Context:")
    click.echo(f"  Dominant influence: {ctx.get_dominant_influence()}")
    click.echo(f"  Recommended morphology: {ctx.get_recommended_morphology()}")
    click.echo(f"  Coherence: {ctx.coherence_level}")

    # System readiness
    click.echo("\nSystem Readiness:")
    click.echo("  ✅ ContextSynthesizer: Active")
    click.echo("  ✅ AdaptiveToolPortal: Active")
    click.echo(f"  ✅ Morphologies: {total_morphologies} loaded")


if __name__ == "__main__":
    prat()
