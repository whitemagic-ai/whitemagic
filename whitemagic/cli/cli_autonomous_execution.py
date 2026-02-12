# mypy: disable-error-code=no-untyped-def
"""CLI commands for autonomous execution."""

import asyncio
import json
from datetime import datetime
from pathlib import Path

import click

from whitemagic.config.paths import WM_ROOT


@click.group()
def autonomous():
    """Autonomous execution commands"""


@autonomous.command()
@click.argument("initial_objective")
@click.option("--max-iterations", default=100, help="Max iterations")
@click.option("--max-time", default=7200, help="Max time in seconds")
@click.option("--max-tokens", default=150000, help="Max tokens")
def start(initial_objective, max_iterations, max_time, max_tokens):
    """Start autonomous execution loop."""
    from whitemagic.autonomous.executor.continuous_executor import (
        ContinuousExecutor,
        ExecutorConfig,
        Objective,
    )

    config = ExecutorConfig(
        max_iterations=max_iterations,
        max_time_seconds=max_time,
        max_tokens=max_tokens,
    )

    executor = ContinuousExecutor(config)
    executor.add_objective(Objective(description=initial_objective))

    click.echo("🤖 Starting autonomous execution...")
    click.echo(f"   Initial objective: {initial_objective}")
    click.echo(f"   Limits: {max_iterations} iter, {max_time}s, {max_tokens} tokens")

    asyncio.run(executor.run_continuous())


@autonomous.command()
def status():
    """Show autonomous execution status."""
    checkpoint_path = WM_ROOT / "autonomous" / "checkpoint.json"

    if not checkpoint_path.exists():
        click.echo("No autonomous execution running.")
        return

    checkpoint = json.loads(checkpoint_path.read_text())

    click.echo("\n🤖 Autonomous Execution Status")
    click.echo("=" * 50)
    click.echo(f"Objectives: {len(checkpoint['objectives'])} pending")
    click.echo(f"Completed: {len(checkpoint['completed'])}")
    click.echo(f"Failed: {len(checkpoint['failed'])}")

    if checkpoint.get("strategy"):
        click.echo(f"\nCurrent Strategy: {checkpoint['strategy'].get('goal', 'N/A')}")

    click.echo("\nResources:")
    for key, value in checkpoint["limits"].items():
        click.echo(f"  {key}: {value}")


@autonomous.command()
@click.option("--count", default=1000, help="Number of objectives to test")
@click.option("--batch-size", default=100, help="Batch size")
@click.option("--max-concurrent", default=1000, help="Max concurrent tasks")
def benchmark(count, batch_size, max_concurrent):
    """Benchmark asyncio batch execution."""
    import asyncio

    from whitemagic.autonomous.executor import (
        ContinuousExecutor,
        ExecutorConfig,
        Objective,
    )

    async def run_benchmark():
        config = ExecutorConfig(
            max_iterations=count * 2,
            max_time_seconds=600,
        )

        executor = ContinuousExecutor(config)

        # Create test objectives
        objectives = [
            Objective(description=f"Benchmark objective {i}")
            for i in range(count)
        ]

        click.echo("\n🚀 Benchmarking objectives")
        click.echo(f"   Count: {count}")
        click.echo(f"   Batch size: {batch_size}")
        click.echo(f"   Max concurrent: {max_concurrent}")

        start = datetime.now()
        results = await executor.execute_batch_async(
            objectives,
            batch_size=batch_size,
            max_concurrent=max_concurrent,
        )
        duration = (datetime.now() - start).total_seconds()

        successes = sum(1 for r in results if r.success)

        click.echo("\n✅ Benchmark Complete")
        click.echo(f"   Duration: {duration:.2f} s")
        click.echo(f"   Throughput: {count / duration:.0f} objectives/sec")
        click.echo(f"   Success rate: {successes} / {count} ({successes / count * 100:.1f}%)")

    asyncio.run(run_benchmark())
@autonomous.command()
@click.argument("protocol_name")
@click.option("--background", is_flag=True, help="Run protocol in a background process")
def run(protocol_name, background):
    """Run a specific WhiteMagic protocol (e.g., satkona_v5, reindex)."""
    import subprocess
    import sys

    click.echo(f"🚀 Executing protocol: {protocol_name}...")

    # Map protocol names to script paths
    protocol_map = {
        "satkona_v5": "scripts/satkona_v5.py",
        "reindex": "scripts/reindex_data_sea.py",
        "probe": "scripts/systemic_activation_probe.py",
        "ignite": "audit/ignite_emergence.py",
    }

    script_path = protocol_map.get(protocol_name)
    if not script_path:
        # Try to find it in scripts/ directly
        potential_path = Path(f"scripts/{protocol_name}.py")
        if potential_path.exists():
            script_path = str(potential_path)
        else:
            click.echo(f"❌ Error: Protocol '{protocol_name}' not found.")
            return

    python_bin = sys.executable
    cmd = [python_bin, script_path]

    try:
        if background:
            subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            click.echo(f"✅ Protocol {protocol_name} started in background.")
        else:
            subprocess.run(cmd, check=True)
            click.echo(f"✅ Protocol {protocol_name} completed successfully.")
    except Exception as e:
        click.echo(f"❌ Error running protocol: {e}")
