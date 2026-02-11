
import shutil
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from whitemagic.core.bridge.utils import logger


def sangha_lock_acquire(resource: str, reason: str = "", timeout: int = 3600, **kwargs: Any) -> dict[str, Any]:
    """Acquire a resource lock."""
    try:
        from whitemagic.gardens.sangha.resources import get_resources
        rm = get_resources()
        agent_id = kwargs.get("agent_id", "system")
        acquired = rm.acquire_lock(resource, agent_id, reason, ttl_seconds=timeout)
        return {"lock_id": f"lock_{resource}", "resource": resource, "acquired": acquired, "timeout": timeout}
    except Exception as e:
        logger.error(f"Failed to acquire sangha lock: {e}")
        return {"error": str(e), "acquired": False}

def sangha_lock_release(lock_id: str | None = None, resource: str | None = None, **kwargs: Any) -> dict[str, Any]:
    """Release a resource lock."""
    try:
        from whitemagic.gardens.sangha.resources import get_resources
        rm = get_resources()
        agent_id = kwargs.get("agent_id", "system")
        # NOTE: Avoid precedence traps with ternaries + boolean ops.
        res_id = None
        if resource:
            res_id = resource
        elif lock_id:
            res_id = lock_id.replace("lock_", "")
        if not res_id:
            return {"error": "No resource or lock_id provided", "released": False}
        released = rm.release_lock(res_id, agent_id)
        return {"released": released, "resource": res_id}
    except Exception as e:
        logger.error(f"Failed to release sangha lock: {e}")
        return {"error": str(e), "released": False}

def sangha_lock_list(**kwargs: Any) -> dict[str, Any]:
    """List active locks."""
    try:
        from whitemagic.gardens.sangha.resources import get_resources
        rm = get_resources()
        locks = rm.list_locks()
        return {
            "locks": [
                {
                    "resource": lock.resource_id,
                    "owner": lock.locked_by,
                    "expires": lock.expires_at.isoformat(),
                }
                for lock in locks
            ],
            "count": len(locks),
        }
    except Exception as e:
        logger.error(f"Failed to list sangha locks: {e}")
        return {"error": str(e), "locks": [], "count": 0}

def sangha_chat_send(message: str, channel: str = "general", sender: str = "system", **kwargs: Any) -> dict[str, Any]:
    """Send message to Sangha chat."""
    try:
        from whitemagic.gardens.sangha.chat import SanghaChat
        chat = SanghaChat()
        msg = chat.send_message(sender_id=sender, content=message, channel=channel)

        # Emit mesh signal if bridge is active
        try:
            from whitemagic.core.polyglot.mesh_bridge import get_mesh_bridge
            bridge = get_mesh_bridge()
            bridge.broadcast(f"CHAT[{channel}]: {message[:50]}...", confidence=0.8)
        except Exception:
            pass # Non-critical if mesh is down

        return {"sent": True, "message_id": msg.id, "channel": channel, "sender": sender}
    except Exception as e:
        logger.error(f"Failed to send sangha chat: {e}")
        return {"error": str(e), "sent": False}

def sangha_chat_read(channel: str = "general", limit: int = 10, **kwargs: Any) -> dict[str, Any]:
    """Read messages from Sangha chat."""
    try:
        from whitemagic.gardens.sangha.chat import SanghaChat
        chat = SanghaChat()
        messages = chat.read_messages(channel=channel, limit=limit)
        return {
            "messages": [
                {
                    "sender": getattr(m, "sender", None) or getattr(m, "sender_id", None),
                    "content": getattr(m, "content", ""),
                    "time": (m.timestamp.isoformat() if getattr(m, "timestamp", None) else None),
                }
                for m in messages
            ],
            "channel": channel,
            "count": len(messages),
        }
    except Exception as e:
        logger.error(f"Failed to read sangha chat: {e}")
        return {"error": str(e), "messages": [], "count": 0}

def garden_sangha_workspace_info(**kwargs: Any) -> dict[str, Any]:
    """Get Sangha workspace information."""
    try:
        from whitemagic.gardens.sangha.collective_memory import get_collective
        from whitemagic.gardens.sangha.workspace import get_sangha_base_dir
        coll = get_collective()
        stats = coll.get_stats()
        return {
            "workspace": str(get_sangha_base_dir()),
            "gardens": 28,
            "active_sessions": stats["participants"],
            "insights_count": stats["insights"],
            "goals_active": stats["active_goals"],
        }
    except Exception as e:
        logger.error(f"Failed to get sangha info: {e}")
        return {"error": str(e)}

def profile_get_profile(**kwargs: Any) -> dict[str, Any]:
    """Get user profile."""
    return {"profile": {"name": "User"}, "preferences": {}}

def profile_update_preferences(preferences: dict[str, Any], **kwargs: Any) -> dict[str, Any]:
    """Update user preferences."""
    return {"updated": True, "preferences": preferences}

# ---------------------------------------------------------------------------
# Windsurf Archival Utilities
# ---------------------------------------------------------------------------

def windsurf_backup(
    source_dir: str | None = None,
    destination_dir: str | None = None,
    label: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Create a *copy* of a Windsurf (or any) directory into a timestamped archive folder.

    Notes:
    - This is intentionally non-destructive (copy-only).
    - Defaults are workspace-friendly and WM_STATE_ROOT-aware.

    """
    try:
        from whitemagic.config.paths import WM_ROOT

        src = Path(source_dir) if source_dir else (Path.cwd() / "windsurf_conversations")
        if not src.exists():
            return {
                "status": "error",
                "message": f"source_dir does not exist: {src}",
            }

        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        safe_label = (label or "windsurf").replace("/", "_").replace("\\", "_")
        dest_root = Path(destination_dir) if destination_dir else (WM_ROOT / "archives" / "windsurf")
        dest_root.mkdir(parents=True, exist_ok=True)
        dest = dest_root / f"{safe_label}_{stamp}"

        shutil.copytree(src, dest)

        return {
            "status": "success",
            "source": str(src),
            "archive": str(dest),
        }
    except Exception as exc:
        logger.error(f"windsurf_backup failed: {exc}")
        return {"status": "error", "message": str(exc)}


def windsurf_merge_backups(
    backup_dirs: list[str] | None = None,
    destination_dir: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Merge multiple backup directories into a destination folder.

    This is best-effort and non-destructive (existing files are preserved; conflicts are skipped).
    """
    try:
        from whitemagic.config.paths import WM_ROOT

        dirs = [Path(p) for p in (backup_dirs or [])]
        if not dirs:
            return {"status": "error", "message": "backup_dirs is required"}

        dest = Path(destination_dir) if destination_dir else (WM_ROOT / "archives" / "windsurf_merged")
        dest.mkdir(parents=True, exist_ok=True)

        copied = 0
        skipped = 0
        for src in dirs:
            if not src.exists():
                skipped += 1
                continue
            for path in src.rglob("*"):
                if path.is_dir():
                    continue
                rel = path.relative_to(src)
                out = dest / rel
                if out.exists():
                    skipped += 1
                    continue
                out.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(path, out)
                copied += 1

        return {
            "status": "success",
            "destination": str(dest),
            "copied_files": copied,
            "skipped": skipped,
        }
    except Exception as exc:
        logger.error(f"windsurf_merge_backups failed: {exc}")
        return {"status": "error", "message": str(exc)}

# Re-export agent management here as it's part of collaboration
# manage_agent_collaboration is imported
