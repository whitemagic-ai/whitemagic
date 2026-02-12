from __future__ import annotations

import json
import importlib
import logging
from collections.abc import Generator
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.utils.core import parse_datetime

from .workspace import get_sangha_base_dir

logger = logging.getLogger(__name__)

"""Sangha Chat - File-based inter-agent communication protocol

Enables multiple AI instances (Agents) to communicate, coordinate, and plan
via shared markdown chat logs. Designed for parallel execution in separate
terminal tabs/sessions.
"""


try:
    from whitemagic.utils.fileio import atomic_write as _atomic_write
    from whitemagic.utils.fileio import file_lock as _file_lock
    FILEIO_AVAILABLE = True
except ImportError:
    FILEIO_AVAILABLE = False

    @contextmanager
    def _file_lock(filepath: str | Path, timeout: float = 5.0) -> Generator[None, None, None]:
        _ = timeout
        yield

    def _atomic_write(filepath: str | Path, content: str) -> None:
        Path(filepath).write_text(content, encoding="utf-8")

file_lock = _file_lock
atomic_write = _atomic_write
_gan_ying_enhanced_mod: Any | None
try:
    _gan_ying_enhanced_mod = importlib.import_module("whitemagic.core.resonance.gan_ying_enhanced")
except ImportError:
    _gan_ying_enhanced_mod = None
EventType = getattr(_gan_ying_enhanced_mod, "EventType", None)

_gan_ying_mod: Any | None
try:
    _gan_ying_mod = importlib.import_module("whitemagic.core.resonance")
except ImportError:
    _gan_ying_mod = None
GanYingBus = getattr(_gan_ying_mod, "GanYingBus", None)
GAN_YING_AVAILABLE = GanYingBus is not None

@dataclass
class ChatMessage:
    """A single message in the Sangha Chat."""

    id: str
    timestamp: datetime
    sender_id: str
    content: str
    channel: str
    tags: list[str]
    priority: str = "normal"  # Added priority field: 'low', 'normal', 'high', 'urgent'
    reply_to: str | None = None

    def to_markdown(self) -> str:
        """Convert to readable markdown format."""
        time_str = self.timestamp.strftime("%H:%M:%S")
        tags_str = " ".join(f"#{t}" for t in self.tags)
        priority_str = f"[Priority: {self.priority.upper()}]" if self.priority != "normal" else ""

        md = f"### [{time_str}] **{self.sender_id}** {priority_str}"
        if self.tags:
            md += f" `{tags_str}`"
        md += "\n\n"

        if self.reply_to:
            md += f"> Replying to {self.reply_to}\n\n"

        md += f"{self.content}\n\n---\n"
        return md

    @classmethod
    def from_dict(cls, data: dict) -> "ChatMessage":
        return cls(
            id=data["id"],
            timestamp=parse_datetime(data["timestamp"]),
            sender_id=data["sender_id"],
            content=data["content"],
            channel=data["channel"],
            tags=data.get("tags", []),
            priority=data.get("priority", "normal"),
            reply_to=data.get("reply_to"),
        )

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "sender_id": self.sender_id,
            "content": self.content,
            "channel": self.channel,
            "tags": self.tags,
            "priority": self.priority,
            "reply_to": self.reply_to,
        }


@dataclass
class Task:
    """A task to be managed within Sangha Chat for multi-agent coordination."""

    id: str
    title: str
    description: str
    created_at: datetime
    created_by: str
    assigned_to: str | None = None
    status: str = "open"  # open, in_progress, completed, blocked
    priority: str = "normal"  # low, normal, high, urgent
    due_date: datetime | None = None
    channel: str | None = None
    updates: list[dict[str, Any]] | None = None

    def to_markdown(self) -> str:
        """Convert to readable markdown format."""
        time_str = self.created_at.strftime("%Y-%m-%d %H:%M:%S")
        status_str = f"Status: {self.status.upper()}"
        priority_str = f"Priority: {self.priority.upper()}" if self.priority != "normal" else ""
        assigned_str = f"Assigned to: {self.assigned_to}" if self.assigned_to else "Unassigned"
        due_str = f"Due: {self.due_date.strftime('%Y-%m-%d')}" if self.due_date else "No due date"

        md = f"### Task [{self.id}] **{self.title}** ({time_str})\n"
        md += f"- {status_str}\n"
        md += f"- {priority_str}\n"
        md += f"- {assigned_str}\n"
        md += f"- {due_str}\n"
        if self.channel:
            md += f"- Channel: #{self.channel}\n"
        md += f"\n**Description:**\n{self.description}\n"
        if self.updates:
            md += "\n**Updates:**\n"
            for update in self.updates:
                md += f"- [{update['timestamp'].strftime('%Y-%m-%d %H:%M:%S')}] {update['sender']}: {update['content']}\n"
        md += "\n---\n"
        return md

    @classmethod
    def from_dict(cls, data: dict) -> "Task":
        due_date = parse_datetime(data["due_date"]) if data.get("due_date") else None
        updates = data.get("updates", [])
        for update in updates:
            update["timestamp"] = parse_datetime(update["timestamp"])
        return cls(
            id=data["id"],
            title=data["title"],
            description=data["description"],
            created_at=parse_datetime(data["created_at"]),
            created_by=data["created_by"],
            assigned_to=data.get("assigned_to"),
            status=data.get("status", "open"),
            priority=data.get("priority", "normal"),
            due_date=due_date,
            channel=data.get("channel"),
            updates=updates,
        )

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "title": self.title,
            "description": self.description,
            "created_at": self.created_at.isoformat(),
            "created_by": self.created_by,
            "assigned_to": self.assigned_to,
            "status": self.status,
            "priority": self.priority,
            "due_date": self.due_date.isoformat() if self.due_date else None,
            "channel": self.channel,
            "updates": [
                {
                    "timestamp": update["timestamp"].isoformat(),
                    "sender": update["sender"],
                    "content": update["content"],
                } for update in self.updates or []
            ],
        }


class SanghaChat:
    """Manages file-based chat rooms and tasks for AI coordination."""

    def __init__(self, base_dir: Path | None = None) -> None:
        self.base_dir = base_dir if base_dir is not None else get_sangha_base_dir()
        self.chat_dir = self.base_dir / "memory" / "collective" / "chat"
        self.tasks_dir = self.base_dir / "memory" / "collective" / "tasks"
        self.chat_dir.mkdir(parents=True, exist_ok=True)
        self.tasks_dir.mkdir(parents=True, exist_ok=True)

        # Ensure default channels exist
        self._ensure_channel("general")
        self._ensure_channel("council")
        # Ensure tasks file exists
        self._ensure_tasks_file()
        # Initialize Gan Ying Bus integration if available
        self.bus = GanYingBus() if GAN_YING_AVAILABLE and callable(GanYingBus) else None
        if GAN_YING_AVAILABLE:
            self._subscribe_to_events()

    def _subscribe_to_events(self) -> None:
        """Subscribe to relevant Gan Ying Bus events for automatic notifications."""
        if GAN_YING_AVAILABLE and self.bus and EventType is not None:
            metric_event = getattr(EventType, "METRIC_UPDATE", None)
            garden_event = getattr(EventType, "GARDEN_ACTIVITY", None)
            alert_event = getattr(EventType, "SYSTEM_ALERT", None)

            if metric_event is not None:
                self.bus.listen(metric_event, self._handle_metric_update)
            if garden_event is not None:
                self.bus.listen(garden_event, self._handle_garden_activity)
            if alert_event is not None:
                self.bus.listen(alert_event, self._handle_system_alert)

    def _handle_metric_update(self, event_data: dict[str, Any]) -> None:
        """Handle metric update events by posting to relevant channels."""
        garden = event_data.get("garden", "unknown")
        metrics = event_data.get("metrics", {})
        content = f"Metric Update for {garden}: "
        for metric, value in metrics.items():
            content += f"{metric} = {value}; "
        content = content.rstrip("; ")
        self.send_message(
            sender_id="SystemBot",
            content=content,
            channel="technical",
            tags=["metric", garden],
            priority="normal",
        )

    def _handle_garden_activity(self, event_data: dict[str, Any]) -> None:
        """Handle garden activity events by posting to relevant channels."""
        garden = event_data.get("garden", "unknown")
        activity = event_data.get("activity", "Activity detected")
        impact = event_data.get("impact", 0.0)
        content = f"Garden Activity in {garden}: {activity} (Impact: {impact})"
        self.send_message(
            sender_id="SystemBot",
            content=content,
            channel="general",
            tags=["activity", garden],
            priority="normal",
        )

    def _handle_system_alert(self, event_data: dict[str, Any]) -> None:
        """Handle system alert events by posting urgent messages."""
        alert_type = event_data.get("type", "ALERT")
        message = event_data.get("message", "System alert occurred")
        content = f"SYSTEM ALERT [{alert_type}]: {message}"
        self.send_message(
            sender_id="SystemBot",
            content=content,
            channel="general",
            tags=["alert", alert_type.lower()],
            priority="urgent",
        )

    def _ensure_channel(self, channel: str) -> None:
        """Make sure channel file exists."""
        channel_file = self.chat_dir / f"{channel}.md"
        if not channel_file.exists():
            with file_lock(channel_file):
                if not channel_file.exists():
                    header = f"# Sangha Chat - #{channel}\n\nCreated: {datetime.now().isoformat()}\n\n---\n\n"
                    atomic_write(channel_file, header)

                    # Also create JSON index for machine reading
                    index_file = self.chat_dir / f"{channel}.jsonl"
                    atomic_write(index_file, "")

    def _ensure_tasks_file(self) -> None:
        """Make sure tasks file exists."""
        tasks_file = self.tasks_dir / "tasks.jsonl"
        tasks_md = self.tasks_dir / "tasks.md"
        if not tasks_file.exists():
            with file_lock(tasks_file):
                if not tasks_file.exists():
                    atomic_write(tasks_file, "")
                    atomic_write(tasks_md, f"# Sangha Tasks\n\nCreated: {datetime.now().isoformat()}\n\n---\n\n")

    def send_message(self,
                     sender_id: str,
                     content: str,
                     channel: str = "general",
                     tags: list[str] | None = None,
                     priority: str = "normal",
                     reply_to: str | None = None) -> ChatMessage:
        """Send a message to a channel."""
        self._ensure_channel(channel)

        msg = ChatMessage(
            id=f"msg_{datetime.now().strftime('%Y%m%d%H%M%S')}_{sender_id[:4]}",
            timestamp=datetime.now(),
            sender_id=sender_id,
            content=content,
            channel=channel,
            tags=tags or [],
            priority=priority,
            reply_to=reply_to,
        )

        # 1. Append to Markdown (Human readable)
        channel_md = self.chat_dir / f"{channel}.md"
        with file_lock(channel_md):
            with open(channel_md, "a") as f:
                f.write(msg.to_markdown())

        # 2. Append to JSONL (Machine readable)
        channel_jsonl = self.chat_dir / f"{channel}.jsonl"
        with file_lock(channel_jsonl):
            with open(channel_jsonl, "a") as f:
                f.write(json.dumps(msg.to_dict()) + "\n")

        return msg

    def read_messages(self,
                      channel: str = "general",
                      limit: int = 50,
                      since: datetime | None = None,
                      priority_filter: str | None = None) -> list[ChatMessage]:
        """Read recent messages from a channel, optionally filter by priority."""
        channel_jsonl = self.chat_dir / f"{channel}.jsonl"
        if not channel_jsonl.exists():
            return []

        messages = []
        try:
            # Read last N lines efficiently would be better, but for now read all and slice
            # Optimization for later: seek to end and read backwards
            with open(channel_jsonl, "r") as f:
                lines = f.readlines()

            for line in lines[-limit:]:
                try:
                    data = json.loads(line)
                    msg = ChatMessage.from_dict(data)
                    if since and msg.timestamp <= since:
                        continue
                    if priority_filter and msg.priority != priority_filter:
                        continue
                    messages.append(msg)
                except json.JSONDecodeError:
                    continue

        except Exception as e:
            logger.info(f"Error reading chat {channel}: {e}")

        return messages

    def get_channels(self) -> list[str]:
        """List available channels."""
        return [f.stem for f in self.chat_dir.glob("*.md")]

    def clear_channel(self, channel: str) -> None:
        """Archive and clear a channel."""
        # Implementation needed: Move channel to archive directory and create new empty channel
        # Archive path: self.archive_dir / f"{channel}_{timestamp}.md"
        pass

    def create_task(self, title: str, description: str, created_by: str, assigned_to: str | None = None, status: str = "open", priority: str = "normal", due_date: datetime | None = None, channel: str | None = None) -> Task:
        """Create a new task for coordination."""
        task_id = f"task_{datetime.now().strftime('%Y%m%d%H%M%S')}_{created_by[:4]}"
        task = Task(
            id=task_id,
            title=title,
            description=description,
            created_at=datetime.now(),
            created_by=created_by,
            assigned_to=assigned_to,
            status=status,
            priority=priority,
            due_date=due_date,
            channel=channel,
            updates=[],
        )

        # 1. Append to Markdown (Human readable)
        tasks_md = self.tasks_dir / "tasks.md"
        with file_lock(tasks_md):
            with open(tasks_md, "a") as f:
                f.write(task.to_markdown())

        # 2. Append to JSONL (Machine readable)
        return task

    def propose_to_council(self, sender_id: str, title: str, description: str) -> str:
        """Create a proposal in the #council channel."""
        from whitemagic.core.governance.zodiac_council import get_council
        council = get_council()
        prop_id = council.create_proposal(title, description, sender_id)

        content = f"### [PROPOSAL {prop_id}] {title}\n\n{description}\n\n"
        content += "**Cast your vote on the spectrum:**\n"
        content += f"`/vote {prop_id} logic=0.x micro=0.x time=0.x importance=0.x`"

        self.send_message(
            sender_id=sender_id,
            content=content,
            channel="council",
            tags=["proposal", prop_id],
            priority="high",
        )
        return prop_id

    def update_task(self, task_id: str, sender: str, content: str | None = None, status: str | None = None, assigned_to: str | None = None, priority: str | None = None, due_date: datetime | None = None) -> Task | None:
        """Update a task's status or add a comment."""
        tasks = self.list_tasks()
        target_task = None
        for task in tasks:
            if task.id == task_id:
                target_task = task
                break

        if not target_task:
            return None

        if content:
            if not target_task.updates:
                target_task.updates = []
            target_task.updates.append({
                "timestamp": datetime.now(),
                "sender": sender,
                "content": content,
            })
        if status:
            target_task.status = status
        if assigned_to is not None:
            target_task.assigned_to = assigned_to
        if priority:
            target_task.priority = priority
        if due_date is not None:
            target_task.due_date = due_date

        # Rewrite the entire tasks file (inefficient but ensures consistency)
        tasks_jsonl = self.tasks_dir / "tasks.jsonl"
        tasks_md = self.tasks_dir / "tasks.md"
        with file_lock(tasks_jsonl):
            with open(tasks_jsonl, "w") as f:
                for t in tasks:
                    f.write(json.dumps(t.to_dict()) + "\n")
        with file_lock(tasks_md):
            with open(tasks_md, "w") as f:
                f.write(f"# Sangha Tasks\n\nCreated: {datetime.now().isoformat()}\n\n---\n\n")
                for t in tasks:
                    f.write(t.to_markdown())
        return target_task

    def list_tasks(self, status: str | None = None, channel: str | None = None, assigned_to: str | None = None) -> list[Task]:
        """List all tasks, optionally filtered by status, channel, or assignee."""
        tasks_jsonl = self.tasks_dir / "tasks.jsonl"
        if not tasks_jsonl.exists():
            return []

        tasks = []
        try:
            with open(tasks_jsonl, "r") as f:
                lines = f.readlines()
                for line in lines:
                    try:
                        data = json.loads(line)
                        task = Task.from_dict(data)
                        if status and task.status != status:
                            continue
                        if channel and task.channel != channel:
                            continue
                        if assigned_to is not None and task.assigned_to != assigned_to:
                            continue
                        tasks.append(task)
                    except json.JSONDecodeError:
                        continue
        except Exception as e:
            logger.info(f"Error reading tasks: {e}")
        return tasks

    def generate_summary(self, channel: str = "general", days: int = 1, priority_filter: str | None = None) -> str:
        """Generate a summary of recent chat history and tasks for onboarding new agents."""
        from datetime import timedelta
        since = datetime.now() - timedelta(days=days)
        messages = self.read_messages(channel=channel, limit=1000, since=since, priority_filter=priority_filter)
        tasks = self.list_tasks()

        summary = f"# Sangha Chat Summary for #{channel} (Last {days} Day{'s' if days > 1 else ''})\n\n"
        summary += f"Generated: {datetime.now().isoformat()}\n\n"
        summary += "## Key Messages\n"
        if messages:
            summary += f"Total messages: {len(messages)}\n"
            high_priority = [m for m in messages if m.priority in ["high", "urgent"]]
            if high_priority:
                summary += f"- High Priority Messages: {len(high_priority)}\n"
                for msg in high_priority[:5]:
                    summary += f"  - [{msg.timestamp.strftime('%H:%M')}] {msg.sender_id} ({msg.priority.upper()}): {msg.content[:100]}...\n"
            normal_msgs = [m for m in messages if m.priority not in ["high", "urgent"]]
            if normal_msgs:
                summary += f"- Other Notable Messages: {len(normal_msgs)}\n"
                for msg in normal_msgs[:3]:
                    summary += f"  - [{msg.timestamp.strftime('%H:%M')}] {msg.sender_id}: {msg.content[:100]}...\n"
        else:
            summary += "No messages found in this period.\n"

        summary += "\n## Active Tasks\n"
        open_tasks = [t for t in tasks if t.status in ["open", "in_progress"] and (t.channel == channel or t.channel is None)]
        if open_tasks:
            summary += f"Total active tasks: {len(open_tasks)}\n"
            for task in open_tasks[:5]:
                assigned = task.assigned_to if task.assigned_to else "Unassigned"
                summary += f"- {task.id}: {task.title} ({task.status}, {assigned}, Priority: {task.priority})\n"
        else:
            summary += "No active tasks related to this channel.\n"

        summary += "\n## Quick Links\n"
        summary += f"- Full Chat History: {self.chat_dir / f'{channel}.md'}\n"
        summary += f"- Full Task List: {self.tasks_dir / 'tasks.md'}\n"

        return summary

    def save_summary(self, channel: str = "general", days: int = 1, priority_filter: str | None = None) -> Path:
        """Generate and save a summary report to file."""
        summary_content = self.generate_summary(channel, days, priority_filter)
        summary_file = self.chat_dir / f"summary_{channel}_{datetime.now().strftime('%Y%m%d')}.md"
        with file_lock(summary_file):
            atomic_write(summary_file, summary_content)
        return summary_file


# Global instance
_chat: SanghaChat | None = None

def get_chat() -> SanghaChat:
    global _chat
    if _chat is None:
        _chat = SanghaChat()
    return _chat
