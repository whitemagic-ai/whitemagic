"""CLI Commands for Semantic File Watcher

Watch directories for changes, with automatic archaeology tracking
and Gan Ying event emission.
"""

from pathlib import Path

import click


@click.group()
def watch() -> None:
    """👁️ Semantic file watcher - monitor file changes."""


@watch.command("add")
@click.argument("path", default=".")
@click.option("--recursive/--no-recursive", "-r/-R", default=True, help="Watch subdirectories")
@click.option("--pattern", "-p", multiple=True, help="File patterns (e.g., *.py)")
@click.option("--ignore", "-i", multiple=True, help="Patterns to ignore")
def add_watch(
    path: str,
    recursive: bool,
    pattern: tuple[str, ...],
    ignore: tuple[str, ...],
) -> None:
    """Add a directory to watch."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()

    patterns = list(pattern) if pattern else None
    ignore_patterns = list(ignore) if ignore else None

    result = watcher.add_watch(
        path,
        recursive=recursive,
        patterns=patterns,
        ignore_patterns=ignore_patterns,
    )

    if result:
        abs_path = str(Path(path).resolve())
        click.echo(f"✅ Added watch: {abs_path}")
        click.echo(f"   Recursive: {recursive}")
        if patterns:
            click.echo(f"   Patterns: {', '.join(patterns)}")
    else:
        click.echo(f"❌ Failed to add watch: {path}")
        click.echo("   (Directory may not exist)")


@watch.command("remove")
@click.argument("path")
def remove_watch(path: str) -> None:
    """Remove a directory from watching."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()
    result = watcher.remove_watch(path)

    if result:
        click.echo(f"✅ Removed watch: {path}")
    else:
        click.echo(f"❌ Watch not found: {path}")


@watch.command("list")
def list_watches() -> None:
    """List all configured watches."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()
    watches = watcher.list_watches()

    if not watches:
        click.echo("📂 No watches configured")
        click.echo("   Use 'wm watch add <path>' to add a directory")
        return

    click.echo(f"📂 Configured watches ({len(watches)}):")
    for w in watches:
        click.echo(f"\n   📁 {w['path']}")
        click.echo(f"      Recursive: {w.get('recursive', True)}")
        click.echo(f"      Patterns: {', '.join(w.get('patterns', []))}")


@watch.command("start")
def start_watcher() -> None:
    """Start the file watcher."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()

    if not watcher._watchdog_available:
        click.echo("❌ watchdog not installed")
        click.echo("   Install with: pip install whitemagic[watcher]")
        return

    watches = watcher.list_watches()
    if not watches:
        click.echo("❌ No watches configured")
        click.echo("   Use 'wm watch add <path>' to add a directory first")
        return

    if watcher.is_running():
        click.echo("⚠️ Watcher already running")
        return

    result = watcher.start()

    if result:
        click.echo("✅ Watcher started")
        click.echo(f"   Watching {len(watches)} directories")
        click.echo("   Press Ctrl+C to stop")

        # Keep running until interrupted
        try:
            import time
            while watcher.is_running():
                time.sleep(1)
        except KeyboardInterrupt:
            watcher.stop()
            click.echo("\n🛑 Watcher stopped")
    else:
        click.echo("❌ Failed to start watcher")


@watch.command("stop")
def stop_watcher() -> None:
    """Stop the file watcher."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()

    if not watcher.is_running():
        click.echo("⚠️ Watcher not running")
        return

    result = watcher.stop()

    if result:
        click.echo("🛑 Watcher stopped")
    else:
        click.echo("❌ Failed to stop watcher")


@watch.command("status")
def watcher_status() -> None:
    """Show watcher status."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()
    status = watcher.status()

    # Running status
    if status["running"]:
        click.echo("🟢 Watcher is RUNNING")
    else:
        click.echo("🔴 Watcher is STOPPED")

    # Watchdog availability
    if status["watchdog_available"]:
        click.echo("   ✅ watchdog: installed")
    else:
        click.echo("   ❌ watchdog: not installed")
        click.echo("      Install with: pip install whitemagic[watcher]")

    # Watches
    click.echo(f"\n📂 Watches configured: {status['watches_configured']}")
    for path in status.get("watches", []):
        click.echo(f"   - {path}")

    # Events
    click.echo(f"\n📊 Events recorded: {status['events_recorded']}")
    click.echo(f"   Callbacks registered: {status['callbacks_registered']}")


@watch.command("events")
@click.option("--limit", "-l", default=20, help="Number of events to show")
def recent_events(limit: int) -> None:
    """Show recent file events."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()
    events = watcher.recent_events(limit)

    if not events:
        click.echo("📭 No events recorded")
        return

    click.echo(f"📋 Recent events ({len(events)}):")
    for event in events:
        event_type = event.get("event_type", "UNKNOWN")
        path = event.get("path", "unknown")
        timestamp = event.get("timestamp", "")[:19]

        # Icon for event type
        icons = {
            "CREATED": "✨",
            "MODIFIED": "📝",
            "DELETED": "🗑️",
            "MOVED": "📦",
        }
        icon = icons.get(event_type, "📄")

        click.echo(f"\n   {icon} {event_type}")
        click.echo(f"      Path: {Path(path).name}")
        click.echo(f"      Time: {timestamp}")

        # Show themes if detected
        themes = event.get("metadata", {}).get("themes", [])
        if themes:
            click.echo(f"      Themes: {', '.join(themes)}")


@watch.command("stats")
def watcher_stats() -> None:
    """Show watcher statistics."""
    from whitemagic.core.semantic_fs import get_watcher

    watcher = get_watcher()
    stats = watcher.stats()

    click.echo("📊 Watcher Statistics")
    click.echo(f"   Total events: {stats.get('total_events', 0)}")
    click.echo(f"   Watches: {stats.get('watches', 0)}")
    click.echo(f"   Running: {stats.get('running', False)}")

    # Event breakdown
    event_counts = stats.get("event_counts", {})
    if event_counts:
        click.echo("\n   Events by type:")
        for event_type, count in event_counts.items():
            click.echo(f"      {event_type}: {count}")
