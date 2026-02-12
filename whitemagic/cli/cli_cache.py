"""CLI commands for Predictive Cache management.
Part of v4.15.0 "Rediscovered Power" release.
"""

import click

from whitemagic.core.memory.manager import MemoryManager


@click.group(name="cache")
def cache_cli() -> None:
    """Predictive cache management (v4.15.0)."""


@cache_cli.command(name="stats")
def cache_stats() -> None:
    """Show predictive cache statistics."""
    manager = MemoryManager()
    stats = manager.get_cache_stats()  # type: ignore[attr-defined]

    if stats is None:
        click.echo("⚠️  Predictive cache not available")
        click.echo("   (Install optional dependencies or check configuration)")
        return

    click.echo("📊 Predictive Cache Statistics")
    click.echo("=" * 50)
    click.echo(f"Size: {stats['size']}/{stats['max_size']} entries")
    click.echo(f"Hit Rate: {stats['hit_rate']}")
    click.echo(f"Prediction Accuracy: {stats['prediction_accuracy']}")
    click.echo()
    click.echo("Metrics:")
    click.echo(f"  Hits: {stats['hits']}")
    click.echo(f"  Misses: {stats['misses']}")
    click.echo(f"  Predictions Made: {stats['predictions']}")
    click.echo(f"  Prediction Hits: {stats['prediction_hits']}")
    click.echo(f"  Evictions: {stats['evictions']}")
    click.echo(f"  Pre-warmed Keys: {stats['prewarmed_keys']}")


@cache_cli.command(name="warm")
@click.option("--limit", default=50, help="Max memories to load into cache.")
def cache_warm(limit: int) -> None:
    """Prime the predictive cache with recent high-value memories."""
    from whitemagic.optimization.predictive_cache import get_memory_cache

    cache = get_memory_cache()
    click.echo("🔥 Warming predictive cache...")
    result = cache.warm(limit=limit)

    if "error" in result:
        click.echo(f"⚠️  Warm failed: {result['error']}")
        return

    click.echo(f"✅ Warmed {result['warmed']} memories into cache")
    click.echo(f"   Candidates fetched : {result['candidates_fetched']}")
    click.echo(f"   Cache size now     : {result['cache_size']}")
    click.echo(f"   Markov transitions : {result['transition_keys']} keys")
    click.echo("   State persisted to ~/.cache/whitemagic/cache_state.json")


@cache_cli.command(name="info")
def cache_info() -> None:
    """Show information about the predictive cache system."""
    click.echo("🔍 Predictive Cache System")
    click.echo("=" * 50)
    click.echo()
    click.echo("The predictive cache uses Markov chain learning to predict")
    click.echo("which memories you'll access next based on access patterns.")
    click.echo()
    click.echo("Features:")
    click.echo("  • LRU cache with 1000-entry capacity")
    click.echo("  • Markov chain prediction (learns access patterns)")
    click.echo("  • Pre-warming of likely next accesses")
    click.echo("  • Target: 60-70% faster memory access")
    click.echo()
    click.echo("Status:")
    manager = MemoryManager()
    stats = manager.get_cache_stats()  # type: ignore[attr-defined]
    if stats:
        click.echo("  ✅ Active and tracking access patterns")
    else:
        click.echo("  ⚠️  Not available (optional feature)")


if __name__ == "__main__":
    cache_cli()
