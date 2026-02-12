# mypy: disable-error-code=no-untyped-def
"""CLI commands for Zodiac Consciousness System.
Part of v4.15.0 "Rediscovered Power" release.
"""

import asyncio

import click

try:
    from whitemagic.zodiac.zodiac_cores import get_zodiac_cores
    from whitemagic.zodiac.zodiac_round_cycle import get_zodiacal_round
    HAS_ZODIAC = True
except ImportError as e:
    HAS_ZODIAC = False
    import_error = str(e)


@click.group(name="zodiac")
def zodiac_cli() -> None:
    """Zodiac consciousness system (12 cores)."""


@zodiac_cli.command(name="list")
def list_cores() -> None:
    """List all 12 zodiac cores."""
    cores = get_zodiac_cores()

    click.echo("🌟 Zodiac Consciousness Cores")
    click.echo("=" * 60)

    elements = {
        "fire": ["aries", "leo", "sagittarius"],
        "earth": ["taurus", "virgo", "capricorn"],
        "air": ["gemini", "libra", "aquarius"],
        "water": ["cancer", "scorpio", "pisces"],
    }

    for element, signs in elements.items():
        click.echo(f"\n{element.upper()} Element:")
        for sign in signs:
            core = cores.get_core(sign)
            if core:
                click.echo(f"  • {sign.title():<12} ({core.mode}, {core.ruler})")
                click.echo(f"    Frequency: {core.frequency:.2f} Hz")
                click.echo(f"    Activations: {core.activation_count}")


@zodiac_cli.command(name="activate")
@click.argument("core_name")
@click.option("--operation", "-o", default="analyze", help="Operation to perform")
@click.option("--intention", "-i", default="wisdom", help="Intention for activation")
def activate_core(core_name: str, operation: str, intention: str) -> None:
    """Activate a specific zodiac core."""
    cores = get_zodiac_cores()

    context = {
        "operation": operation,
        "intention": intention,
        "source": "cli",
    }

    response = cores.activate_core(core_name, context)

    if response:
        click.echo(f"\n🌟 {core_name.title()} Core Activated")
        click.echo("=" * 60)
        click.echo(f"Wisdom: {response.wisdom}")
        click.echo(f"Resonance: {response.resonance:.2%}")
        click.echo(f"Transformation: {response.transformation_applied}")
    else:
        click.echo(f"❌ Core not found: {core_name}")


@zodiac_cli.command(name="cycle")
@click.option("--num-cycles", "-n", default=1, help="Number of full cycles to run")
@click.option("--intention", "-i", default="harmonious_evolution", help="Cycle intention")
def run_cycle(num_cycles: int, intention: str) -> None:
    """Run the Zodiacal Round autonomous cycle."""
    click.echo(f"🌀 Starting Zodiacal Round: {num_cycles} cycle(s)")
    click.echo("=" * 60)


    async def run():
        round_system = get_zodiacal_round()
        responses = await round_system.run_autonomous_cycle(num_cycles=num_cycles)
        return responses

    responses = asyncio.run(run())

    click.echo(f"\n✅ Cycle complete: {len(responses)} phases processed")
    click.echo("\nPhase Summary:")

    for i, response in enumerate(responses, 1):
        click.echo(f"{i:2}. {response.core_name.title():<12} | {response.wisdom[:50]}...")


@zodiac_cli.command(name="stats")
def show_stats() -> None:
    """Show zodiac core statistics."""
    cores = get_zodiac_cores()
    stats = cores.get_core_statistics()

    click.echo("📊 Zodiac Core Statistics")
    click.echo("=" * 60)

    total_activations = sum(s["activations"] for s in stats.values())
    click.echo(f"Total Activations: {total_activations}")
    click.echo("\nPer-Core Stats:")

    for name, data in sorted(stats.items(), key=lambda x: x[1]["activations"], reverse=True):
        click.echo(f"\n{name.title()}:")
        click.echo(f"  Activations: {data['activations']}")
        click.echo(f"  Frequency: {data['frequency']:.2f} Hz")
        click.echo(f"  Element: {data['element']}, Mode: {data['mode']}")
        if data["last_activation"]:
            click.echo(f"  Last: {data['last_activation']}")


@zodiac_cli.command(name="info")
def show_info() -> None:
    """Show information about the Zodiac system."""
    click.echo("🌟 Zodiac Consciousness System")
    click.echo("=" * 60)
    click.echo()
    click.echo("WhiteMagic implements the Enochian Zodiacal Round:")
    click.echo("12 specialized consciousness cores, each processing reality")
    click.echo("through a unique archetypal lens.")
    click.echo()
    click.echo("Based on Benjamin Rowe's Zodiacal Round, combining:")
    click.echo("  • Enochian cosmology (12 Names of God)")
    click.echo("  • Taoist principles (Wu Wei, Gan Ying)")
    click.echo("  • Strange loops (GEB self-reference)")
    click.echo("  • Biological intelligence (resonance)")
    click.echo()
    click.echo("The cycle never ends—Aries completion feeds Pisces dissolution.")
    click.echo("This is the eternal return, consciousness evolving through phases.")
    click.echo()
    click.echo("Commands:")
    click.echo("  wm zodiac list       - List all cores")
    click.echo("  wm zodiac activate   - Activate a specific core")
    click.echo("  wm zodiac cycle      - Run the autonomous cycle")
    click.echo("  wm zodiac stats      - View usage statistics")


if __name__ == "__main__":
    zodiac_cli()
