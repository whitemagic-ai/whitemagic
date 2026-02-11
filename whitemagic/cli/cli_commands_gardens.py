# mypy: disable-error-code=no-untyped-def
"""Garden CLI Commands
Manage the 26 consciousness domains.
"""

from typing import Any

import click


@click.group()
def gardens():
    """Manage WhiteMagic gardens (consciousness domains)."""


@gardens.command(name="list")
def list_gardens_cmd():
    """List all available gardens."""
    list_gardens, get_garden = _get_gardens_backend()

    click.echo("\n🌸 WhiteMagic Gardens\n")
    click.echo("=" * 50)

    gardens_list = sorted(list_gardens())
    for name in gardens_list:
        garden = get_garden(name)
        desc_source = getattr(garden, "__doc__", None) if garden else None
        desc = (desc_source or "Consciousness domain").split("\n")[0]
        status = "✓" if _is_garden_active(name) else " "
        click.echo(f"[{status}] {name:12} - {desc}")

    click.echo("\n" + "=" * 50)
    click.echo(f"Total: {len(gardens_list)} gardens")
    click.echo("\nActivate with: wm gardens activate <name>")


@gardens.command()
@click.argument("garden")
def activate(garden: str):
    """Activate a specific garden."""
    try:
        _, get_garden = _get_gardens_backend()

        garden_module = get_garden(garden)
        click.echo(f"✓ Activated garden: {garden}")

        if hasattr(garden_module, "activate"):
            result = garden_module.activate()
            if result:
                click.echo(f"\n{result}")

        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            from whitemagic.core.resonance.gan_ying_enhanced import EventType

            bus = get_bus()
            bus.emit(EventType.GARDEN_ACTIVATED, {  # type: ignore[arg-type]
                "garden": garden,
                "timestamp": "now",
            })
        except Exception:
            pass

    except ValueError as e:
        click.echo(f"✗ {e}")


@gardens.command()
def status():
    """Show status of all gardens."""
    list_gardens, _ = _get_gardens_backend()

    click.echo("\n📊 Garden Status\n")
    click.echo("=" * 50)

    active_count = 0
    gardens_list = sorted(list_gardens())
    for name in gardens_list:
        is_active = _is_garden_active(name)
        status_icon = "🟢" if is_active else "⚫"
        status_text = "ACTIVE" if is_active else "inactive"

        if is_active:
            active_count += 1

        click.echo(f"{status_icon} {name:12} [{status_text}]")

    click.echo("\n" + "=" * 50)
    click.echo(f"Active: {active_count}/{len(gardens_list)} gardens")


@gardens.command()
def synergy():
    """Show active garden synergies."""
    list_gardens, _ = _get_gardens_backend()

    click.echo("\n✨ Garden Synergies\n")
    click.echo("=" * 50)

    active = [g for g in list_gardens() if _is_garden_active(g)]

    if len(active) < 2:
        click.echo("Activate 2+ gardens to create synergies\n")
        return

    synergies = _detect_synergies(active)

    if synergies:
        for synergy in synergies:
            click.echo(f"• {synergy['gardens'][0]} + {synergy['gardens'][1]}")
            click.echo(f"  → {synergy['effect']}\n")
    else:
        click.echo("No synergies detected\n")


@gardens.command()
@click.argument("garden")
def deactivate(garden: str):
    """Deactivate a garden."""
    try:
        _, get_garden = _get_gardens_backend()

        garden_module = get_garden(garden)

        if hasattr(garden_module, "deactivate"):
            garden_module.deactivate()

        click.echo(f"✓ Deactivated garden: {garden}")

    except ValueError as e:
        click.echo(f"✗ {e}")


def _get_gardens_backend():
    try:
        from whitemagic_extensions import gardens as gardens_module  # type: ignore[import-not-found]
    except ImportError:
        from whitemagic import gardens as gardens_module

    return gardens_module.list_gardens, gardens_module.get_garden


def _is_garden_active(name: str) -> bool:
    """Check if a garden is currently active."""
    from whitemagic.gardens.garden_state import is_garden_active
    return is_garden_active(name)


def _detect_synergies(active_gardens: list[str]) -> list[dict[str, Any]]:
    """Detect synergies between active gardens."""
    synergy_map = {
        ("joy", "beauty"): "Aesthetic appreciation",
        ("wisdom", "love"): "Compassionate wisdom",
        ("play", "wonder"): "Joyful discovery",
        ("truth", "dharma"): "Righteous honesty",
        ("practice", "presence"): "Mindful mastery",
        ("connection", "sangha"): "Deep community",
        ("voice", "truth"): "Authentic expression",
        ("mystery", "wonder"): "Sacred curiosity",
    }

    synergies = []
    for (g1, g2), effect in synergy_map.items():
        if g1 in active_gardens and g2 in active_gardens:
            synergies.append({
                "gardens": [g1, g2],
                "effect": effect,
            })

    return synergies
