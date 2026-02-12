"""CLI Commands for File Archaeology

Track what files have been read, find unread files,
and explore Windsurf conversation history.
"""

from pathlib import Path

import click


@click.group()
def archaeology() -> None:
    """📚 File archaeology - track reads & explore conversations."""


@archaeology.command("mark")
@click.argument("path")
@click.option("--context", "-c", help="Context for reading (e.g., 'Phase 1 scan')")
@click.option("--note", "-n", help="Notes about what was found")
@click.option("--insight", "-i", help="Key insight extracted")
def mark_read(path: str, context: str | None, note: str | None, insight: str | None) -> None:
    """Mark a file as read."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()
    entry = arch.mark_read(path, context=context, note=note, insight=insight)

    click.echo(f"✅ Marked as read: {Path(entry.path).name}")
    click.echo(f"   Times read: {entry.times_read}")
    if entry.contexts:
        click.echo(f"   Contexts: {', '.join(entry.contexts)}")


@archaeology.command("check")
@click.argument("path")
def check_read(path: str) -> None:
    """Check if a file has been read."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()
    entry = arch.get_entry(path)

    if entry:
        click.echo(f"✅ Read {entry.times_read} time(s)")
        click.echo(f"   First: {entry.first_read[:10]}")
        click.echo(f"   Last: {entry.last_read[:10]}")
        if entry.contexts:
            click.echo(f"   Contexts: {', '.join(entry.contexts)}")
        if entry.key_insights:
            click.echo(f"   Insights: {len(entry.key_insights)}")
    else:
        click.echo(f"❌ Not read yet: {path}")


@archaeology.command("unread")
@click.argument("directory", default=".")
@click.option("--pattern", "-p", multiple=True, help="Glob patterns (e.g., *.md)")
@click.option("--limit", "-l", default=50, help="Max results to show")
def find_unread(directory: str, pattern: tuple[str, ...], limit: int) -> None:
    """Find unread files in a directory."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()
    patterns = list(pattern) if pattern else ["*.md", "*.py"]
    unread = arch.find_unread(directory, patterns=patterns)

    click.echo(f"📂 Unread files in {directory} ({len(unread)} total):")
    for path in unread[:limit]:
        click.echo(f"   - {Path(path).name}")

    if len(unread) > limit:
        click.echo(f"   ... and {len(unread) - limit} more")


@archaeology.command("recent")
@click.option("--limit", "-l", default=20, help="Number of results")
def recent_reads(limit: int) -> None:
    """Show recently read files."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()
    recent = arch.get_recent_reads(limit)

    click.echo(f"📚 Recent reads ({len(recent)} shown):")
    for entry in recent:
        name = Path(entry.path).name
        age = f"{entry.age_hours():.1f}h ago"
        click.echo(f"   [{entry.times_read}x] {name} - {age}")


@archaeology.command("stats")
def show_stats() -> None:
    """Show reading statistics."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()
    click.echo(arch.reading_report())


@archaeology.command("changed")
@click.option("--directory", "-d", help="Directory to check")
def find_changed(directory: str | None) -> None:
    """Find files that changed since last read."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()
    changed = arch.find_changed(directory)

    click.echo(f"📝 Changed files ({len(changed)} total):")
    for entry in changed[:20]:
        click.echo(f"   - {Path(entry.path).name}")


@archaeology.command("search")
@click.argument("query")
def search_reads(query: str) -> None:
    """Search read files by path, notes, or insights."""
    from whitemagic.archaeology import get_archaeologist

    arch = get_archaeologist()
    results = arch.search(query)

    click.echo(f"🔍 Search results for '{query}' ({len(results)} found):")
    for entry in results[:20]:
        click.echo(f"   - {Path(entry.path).name}")


# Windsurf conversation commands
@click.group()
def windsurf() -> None:
    """💬 Windsurf conversation tools."""


@windsurf.command("list")
@click.option("--limit", "-l", default=20, help="Number of results")
def list_conversations(limit: int) -> None:
    """List Windsurf conversation files."""
    from whitemagic.archaeology import WindsurfConversationReader

    reader = WindsurfConversationReader()
    convs = reader.list_conversations()

    click.echo(f"💬 Windsurf conversations ({len(convs)} total):")
    for conv in convs[:limit]:
        size = f"{conv['size_kb']:.1f}KB"
        modified = conv["modified"][:10]
        click.echo(f"   [{size}] {conv['id'][:30]}... ({modified})")


@windsurf.command("read")
@click.argument("path")
@click.option("--format", "-f", type=click.Choice(["markdown", "json", "text"]), default="markdown")
def read_conversation(path: str, format: str) -> None:
    """Read and export a conversation."""
    from whitemagic.archaeology import WindsurfConversationReader

    reader = WindsurfConversationReader()
    content = reader.export_conversation(path, format)
    click.echo(content)


@windsurf.command("search")
@click.argument("query")
def search_conversations(query: str) -> None:
    """Search across all conversations."""
    from whitemagic.archaeology import WindsurfConversationReader

    reader = WindsurfConversationReader()
    results = reader.search_conversations(query)

    click.echo(f"🔍 Search results for '{query}' ({len(results)} found):")
    for r in results[:20]:
        click.echo(f"   [{r['role']}] {r['conversation_id'][:20]}...")
        click.echo(f"      {r['snippet'][:100]}...")


@windsurf.command("stats")
def conversation_stats() -> None:
    """Show Windsurf conversation statistics."""
    from whitemagic.archaeology import WindsurfConversationReader

    reader = WindsurfConversationReader()
    stats = reader.stats()

    click.echo("📊 Windsurf Conversation Stats")
    click.echo(f"   Total: {stats['total_conversations']} conversations")
    click.echo(f"   Size: {stats['total_size_mb']} MB")
    if stats["newest"]:
        click.echo(f"   Newest: {stats['newest']['modified'][:10]}")
    if stats["oldest"]:
        click.echo(f"   Oldest: {stats['oldest']['modified'][:10]}")
