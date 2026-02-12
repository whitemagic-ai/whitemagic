# mypy: disable-error-code=no-untyped-def
"""CLI commands for the Ram's Horn Scratchpad."""

import click

from whitemagic.core.automation.scratchpad import get_active_scratchpad


@click.group()
def scratch():
    """Ram's Horn Scratchpad commands"""

@scratch.command(name="add")
@click.argument("text")
@click.option("--tags", help="Comma-separated tags")
def scratch_add(text, tags):
    """Add an entry to the current session's scratchpad."""
    pad = get_active_scratchpad()
    tag_list = tags.split(",") if tags else []
    pad.add_entry(text, tag_list)

@scratch.command(name="list")
def scratch_list():
    """List all entries in the current scratchpad."""
    pad = get_active_scratchpad()
    click.echo(f"\n📝 Scratchpad: {pad.session_id}")
    click.echo("-" * 40)
    pad.list_entries()
    click.echo("")

@scratch.command(name="finalize")
def scratch_finalize():
    """Finalize the current scratchpad into long-term memory."""
    pad = get_active_scratchpad()
    pad.finalize()
