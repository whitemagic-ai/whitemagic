"""Voice CLI commands.

Provides command-line interface for Voice garden functionality.
"""
from __future__ import annotations

import click

from .core import get_voice_core as get_voice  # type: ignore[attr-defined]


@click.group(name="voice")
def voice_cli() -> None:
    """Voice garden - Narrative self and authentic expression."""
    pass


@voice_cli.command()
@click.argument("text")
@click.option("--focus", help="What you're focusing on")
@click.option("--emotion", help="Current emotion")
def speak(text: str, focus: str | None, emotion: str | None) -> None:
    """Speak with your voice."""
    voice = get_voice()
    context = {}
    if focus:
        context["focus"] = focus
    if emotion:
        context["emotion"] = emotion

    result = voice.speak(text, context=context if context else None)  # type: ignore[attr-defined]
    click.echo(f"✓ Spoken: {result['words']} words")


@voice_cli.command()
@click.argument("title")
@click.option("--theme", help="Story theme")
def begin_story(title: str, theme: str | None) -> None:
    """Begin a new story."""
    voice = get_voice()
    result = voice.begin_story(title, theme=theme)  # type: ignore[attr-defined]
    click.echo(f"✓ Story begun: {result['story']}")


@voice_cli.command()
@click.argument("name")
def begin_chapter(name: str) -> None:
    """Begin a new chapter in current story."""
    voice = get_voice()
    result = voice.begin_chapter(name)  # type: ignore[attr-defined]
    if result["success"]:
        click.echo(f"✓ Chapter begun: {result['chapter']}")
    else:
        click.echo(f"✗ Error: {result.get('error')}", err=True)


@voice_cli.command()
def reflect() -> None:
    """Reflect on current narrative."""
    voice = get_voice()
    reflection = voice.reflect()  # type: ignore[attr-defined]
    click.echo(reflection)


@voice_cli.command()
def status() -> None:
    """Show Voice status."""
    voice = get_voice()
    state = voice.get_state()  # type: ignore[attr-defined]

    click.echo("Voice Status:")
    click.echo(f"  Story: {state.get('current_story', 'None')}")
    click.echo(f"  Chapter: {state.get('current_chapter', 'None')}")
    click.echo(f"  Focus: {state.get('current_focus', 'None')}")
    click.echo(f"  Actions: {state.get('actions_taken', 0)}")
    click.echo(f"  Words: {state.get('words_spoken', 0)}")


@voice_cli.command()
def stats() -> None:
    """Show Voice statistics."""
    voice = get_voice()
    stats = voice.get_stats()  # type: ignore[attr-defined]  # noqa: F841

    click.echo("Voice Statistics:")
    click.echo(f"  Stories: {len(stats.get('stories', []))}")
    click.echo(f"  Attention sessions: {stats.get('attention_sessions', 0)}")
    click.echo(f"  Palace rooms: {stats.get('palace_rooms', 0)}")


@voice_cli.command()
@click.option("--story", help="Story to show")
@click.option("--limit", default=10, help="Number of entries")
def recent(story: str | None, limit: int) -> None:
    """Show recent narrative entries."""
    voice = get_voice()
    entries = voice.narrative.get_recent_entries(story=story, limit=limit)  # type: ignore[attr-defined]

    for entry in entries:
        click.echo(f"[{entry['timestamp']}] {entry['text'][:100]}...")


@voice_cli.command()
def stories() -> None:
    """List all stories."""
    voice = get_voice()
    story_list = voice.narrative.list_stories()  # type: ignore[attr-defined]

    if story_list:
        click.echo("Stories:")
        for story in story_list:
            click.echo(f"  - {story}")
    else:
        click.echo("No stories yet")


if __name__ == "__main__":
    voice_cli()
