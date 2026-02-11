"""Sangha CLI - Command line interface for multi-agent coordination"""

import click

from whitemagic.gardens.sangha.chat import get_chat
from whitemagic.gardens.sangha.resources import get_resources


@click.group()
def sangha_cli() -> None:
    """Sangha - Multi-agent coordination and communication"""

# --- Chat Commands ---

@sangha_cli.group()
def chat() -> None:
    """Sangha Chat commands"""

@chat.command()
@click.argument("message")
@click.option("--channel", "-c", default="general", help="Channel name")
@click.option("--tags", "-t", multiple=True, help="Tags")
@click.option("--sender", "-s", default="CLI_User", help="Sender ID")
@click.option("--priority", "-p", default="normal", type=click.Choice(["low", "normal", "high", "urgent"]), help="Message priority")
def send(message: str, channel: str, tags: tuple[str, ...], sender: str, priority: str) -> None:
    """Send a message"""
    chat_system = get_chat()
    msg = chat_system.send_message(
        sender_id=sender,
        content=message,
        channel=channel,
        tags=list(tags),
        priority=priority,
    )
    click.echo(f"Message sent to #{channel}: {msg.id}")

@chat.command()
@click.option("--channel", "-c", default="general", help="Channel name")
@click.option("--limit", "-n", default=10, help="Number of messages")
@click.option("--priority", "-p", type=click.Choice(["low", "normal", "high", "urgent"]), help="Filter by priority")
def read(channel: str, limit: int, priority: str | None) -> None:
    """Read messages"""
    chat_system = get_chat()
    msgs = chat_system.read_messages(channel=channel, limit=limit, priority_filter=priority)
    click.echo(f"\n=== #{channel} ({len(msgs)} messages) ===\n")
    for m in msgs:
        tags_str = f" [{' '.join(m.tags)}]" if m.tags else ""
        priority_str = f"[Priority: {m.priority.upper()}]" if m.priority != "normal" else ""
        click.echo(f"[{m.timestamp.strftime('%H:%M')}] {m.sender_id}{tags_str} {priority_str}: {m.content}")
    click.echo("")

@chat.command()
def channels() -> None:
    """List channels"""
    chat_system = get_chat()
    for c in chat_system.get_channels():
        click.echo(f"- {c}")

# --- Lock Commands ---

@sangha_cli.group()
def lock() -> None:
    """Resource locking commands"""

@lock.command("list")
def list_locks() -> None:
    """List active locks"""
    res = get_resources()
    locks = res.list_locks()
    if not locks:
        click.echo("No active locks.")
    else:
        click.echo(f"{len(locks)} active locks:")
        for lock in locks:
            click.echo(
                f"- {lock.resource_id} (by {lock.locked_by}): "
                f"{lock.reason} [expires {lock.expires_at.strftime('%H:%M:%S')}]",
            )

@lock.command()
@click.argument("resource")
@click.option("--reason", "-r", required=True, help="Reason for locking")
@click.option("--ttl", default=300, help="Time to live in seconds")
@click.option("--sender", "-s", default="CLI_User", help="Agent ID acquiring lock")
def acquire(resource: str, reason: str, ttl: int, sender: str) -> None:
    """Acquire a lock"""
    res = get_resources()
    success = res.acquire_lock(
        resource_id=resource,
        agent_id=sender,
        reason=reason,
        ttl_seconds=ttl,
    )
    if success:
        click.echo(f"Locked {resource} for {ttl} seconds")
    else:
        status = res.get_lock_status(resource)
        if status:
            click.echo(f"Failed to lock {resource}. Currently locked by {status.locked_by}: {status.reason}")
        else:
            click.echo(f"Failed to lock {resource} (unknown reason)")

@lock.command()
@click.argument("resource")
@click.option("--ttl", default=300, help="Additional time to live in seconds")
@click.option("--sender", "-s", default="CLI_User", help="Agent ID renewing lock")
def renew(resource: str, ttl: int, sender: str) -> None:
    """Renew a lock to extend its duration"""
    res = get_resources()
    status = res.get_lock_status(resource)
    if status and status.locked_by == sender:
        success = res.acquire_lock(
            resource_id=resource,
            agent_id=sender,
            reason=status.reason,
            ttl_seconds=ttl,
        )
        if success:
            click.echo(f"Renewed lock on {resource} for {ttl} seconds")
        else:
            click.echo(f"Failed to renew lock on {resource}")
    else:
        click.echo(f"Cannot renew lock on {resource}. Not owned by {sender} or not locked.")

@lock.command()
@click.argument("resource")
@click.option("--sender", "-s", default="CLI_User", help="Agent ID releasing lock")
def release(resource: str, sender: str) -> None:
    """Release a lock"""
    res = get_resources()
    success = res.release_lock(resource, sender)
    if success:
        click.echo(f"Released {resource}")
    else:
        click.echo(f"Failed to release {resource} (locked by someone else?)")

# --- Task Commands ---

@sangha_cli.group()
def task() -> None:
    """Task management commands for multi-agent coordination"""

@task.command()
@click.argument("title")
@click.argument("description")
@click.option("--assigned-to", "-a", help="Assign to an agent")
@click.option("--status", "-s", default="open", type=click.Choice(["open", "in_progress", "completed", "blocked"]), help="Task status")
@click.option("--priority", "-p", default="normal", type=click.Choice(["low", "normal", "high", "urgent"]), help="Task priority")
@click.option("--due-date", "-d", help="Due date (YYYY-MM-DD)")
@click.option("--channel", "-c", help="Related channel")
@click.option("--sender", "-u", default="CLI_User", help="Creator ID")
def create(
    title: str,
    description: str,
    assigned_to: str | None,
    status: str,
    priority: str,
    due_date: str | None,
    channel: str | None,
    sender: str,
) -> None:
    """Create a new task"""
    from datetime import datetime
    chat_system = get_chat()
    due = datetime.strptime(due_date, "%Y-%m-%d") if due_date else None
    task = chat_system.create_task(
        title=title,
        description=description,
        created_by=sender,
        assigned_to=assigned_to,
        status=status,
        priority=priority,
        due_date=due,
        channel=channel,
    )
    click.echo(f"Task created: {task.id} - {task.title}")

@task.command()
@click.argument("task_id")
@click.option("--content", "-c", help="Update comment or note")
@click.option("--status", "-s", type=click.Choice(["open", "in_progress", "completed", "blocked"]), help="Update task status")
@click.option("--assigned-to", "-a", help="Reassign task to an agent")
@click.option("--priority", "-p", type=click.Choice(["low", "normal", "high", "urgent"]), help="Update task priority")
@click.option("--due-date", "-d", help="Update due date (YYYY-MM-DD)")
@click.option("--sender", "-u", default="CLI_User", help="Updater ID")
def update(
    task_id: str,
    content: str | None,
    status: str | None,
    assigned_to: str | None,
    priority: str | None,
    due_date: str | None,
    sender: str,
) -> None:
    """Update a task's status or add a comment"""
    from datetime import datetime
    chat_system = get_chat()
    due = datetime.strptime(due_date, "%Y-%m-%d") if due_date else None
    updated_task = chat_system.update_task(
        task_id=task_id,
        sender=sender,
        content=content,
        status=status,
        assigned_to=assigned_to,
        priority=priority,
        due_date=due,
    )
    if updated_task:
        click.echo(f"Task updated: {task_id} - {updated_task.title}")
    else:
        click.echo(f"Task not found or update failed: {task_id}")

@task.command()
@click.option("--status", "-s", type=click.Choice(["open", "in_progress", "completed", "blocked"]), help="Filter by status")
@click.option("--channel", "-c", help="Filter by channel")
@click.option("--assigned-to", "-a", help="Filter by assignee")
def list(status: str | None, channel: str | None, assigned_to: str | None) -> None:
    """List tasks, optionally filtered"""
    chat_system = get_chat()
    tasks = chat_system.list_tasks(status=status, channel=channel, assigned_to=assigned_to)
    if not tasks:
        click.echo("No tasks found matching the criteria.")
    else:
        click.echo(f"{len(tasks)} tasks found:")
        for t in tasks:
            assigned_str = t.assigned_to or "Unassigned"
            f"[Priority: {t.priority.upper()}]" if t.priority != "normal" else ""
            click.echo(f"- {t.id}: {t.title} ({t.status}, {assigned_str}, Priority: {t.priority})\n")

# --- Summary Commands ---

@sangha_cli.group()
def summary() -> None:
    """Summary generation commands for quick onboarding"""

@summary.command()
@click.option("--channel", "-c", default="general", help="Channel to summarize")
@click.option("--days", "-d", default=1, help="Number of days to look back")
@click.option("--priority", "-p", type=click.Choice(["low", "normal", "high", "urgent"]), help="Filter by priority")
@click.option("--save", "-s", is_flag=True, help="Save summary to file")
def generate(channel: str, days: int, priority: str | None, save: bool) -> None:
    """Generate a summary of chat history and tasks"""
    chat_system = get_chat()
    if save:
        summary_file = chat_system.save_summary(channel=channel, days=days, priority_filter=priority)
        click.echo(f"Summary saved to: {summary_file}")
    else:
        summary_content = chat_system.generate_summary(channel=channel, days=days, priority_filter=priority)
        click.echo(summary_content)
