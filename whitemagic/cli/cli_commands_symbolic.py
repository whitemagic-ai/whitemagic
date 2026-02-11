# mypy: disable-error-code=no-untyped-def
"""Symbolic CLI Commands
I Ching, Wu Xing, and symbolic reasoning.
"""

import random

import click


@click.group()
def iching():
    """Consult the I Ching oracle."""


@iching.command()
@click.argument("question")
def consult(question: str):
    """Ask the I Ching a question."""
    try:
        from whitemagic_extensions.symbolic import consult_iching  # type: ignore[import-not-found]

        click.echo("\n📿 Consulting the I Ching...\n")
        click.echo(f"Question: {question}\n")

        result = consult_iching(question)
        click.echo(result)

    except ImportError:
        from whitemagic.oracle.quantum_iching import QuantumIChing

        click.echo("\n📿 Consulting the I Ching...\n")
        click.echo(f"Question: {question}\n")

        iching = QuantumIChing()
        consult_fn = getattr(iching.consult, "__wrapped__", iching.consult)
        result = consult_fn(question=question, context=None)

        click.echo(f"Hexagram {result.primary_hexagram}: {result.primary_name}")
        click.echo(f"Judgment: {result.primary_judgment}")
        click.echo(f"Image: {result.primary_image}\n")
        click.echo(result.guidance)


@iching.command()
@click.argument("number", type=int)
def meaning(number: int):
    """Get meaning of a hexagram."""
    if number < 1 or number > 64:
        click.echo("✗ Hexagram number must be 1-64")
        return

    from whitemagic.oracle.quantum_iching import HexagramDatabase
    db = HexagramDatabase()
    data = db.get_hexagram(number)
    if data is not None:
        click.echo(f"\n📖 Hexagram {number}: {data.get('name')}\n")
        click.echo(f"Judgment: {data.get('judgment')}")
        click.echo(f"Image: {data.get('image')}\n")
    else:
        click.echo(f"\n✗ Hexagram {number} not found")


@click.group()
def wuxing():
    """Wu Xing (Five Elements) balance."""


@wuxing.command()
def balance():
    """Check elemental balance."""
    elements = {
        "Wood": random.uniform(0.5, 1.0),
        "Fire": random.uniform(0.5, 1.0),
        "Earth": random.uniform(0.5, 1.0),
        "Metal": random.uniform(0.5, 1.0),
        "Water": random.uniform(0.5, 1.0),
    }

    click.echo("\n🌟 Wu Xing Balance\n")
    click.echo("=" * 50)

    for element, value in elements.items():
        bar = "█" * int(value * 20)
        click.echo(f"{element:6} [{value:.2f}] {bar}")

    avg = sum(elements.values()) / len(elements)
    status = "Balanced" if avg > 0.7 else "Needs attention"

    click.echo("=" * 50)
    click.echo(f"Overall: {status} (avg: {avg:.2f})\n")


@wuxing.command()
def recommend():
    """Get recommendations for balance."""
    click.echo("\n💡 Wu Xing Recommendations\n")
    click.echo("• Strengthen Water element through meditation")
    click.echo("• Balance Fire with calming practices")
    click.echo("• Maintain Earth stability through routine\n")


def _get_hexagram_name(number: int) -> str:
    """Get hexagram name (fallback)."""
    try:
        from whitemagic.oracle.quantum_iching import HexagramDatabase
        result = HexagramDatabase().get_hexagram(number)
        return str((result or {}).get("name", "Unknown"))
    except Exception:
        return "Unknown"
