"""Sangha Workspace Configuration
Phase B: Explicit workspace_id for cross-worktree sharing.
"""

import os
from pathlib import Path

from whitemagic.config.paths import WM_ROOT


def get_workspace_id() -> str | None:
    """Get the workspace ID from environment variable.

    Returns None if not set (falls back to repo-local mode).

    Option A (explicit): Requires WHITEMAGIC_WORKSPACE_ID env var
    for cross-worktree sharing.
    """
    return os.environ.get("WHITEMAGIC_WORKSPACE_ID")


def get_sangha_base_dir() -> Path:
    """Get the base directory for Sangha storage.

    Behavior:
    - If WHITEMAGIC_WORKSPACE_ID is set: WM_STATE_ROOT/workspaces/<workspace_id>/
    - Otherwise: WM_STATE_ROOT/sangha/

    This allows multiple git worktrees to share the same Sangha state
    when using the same workspace_id.
    """
    workspace_id = get_workspace_id()

    if workspace_id:
        # Shared workspace mode
        base = WM_ROOT / "workspaces" / workspace_id
        base.mkdir(parents=True, exist_ok=True)
        return base

    # Default local mode: keep Sangha state under WM_STATE_ROOT.
    base = WM_ROOT / "sangha"
    base.mkdir(parents=True, exist_ok=True)
    return base


def get_sangha_storage_info() -> dict:
    """Get information about current Sangha storage configuration.

    Useful for debugging and status commands.
    """
    workspace_id = get_workspace_id()
    base_dir = get_sangha_base_dir()

    subdirs = {
        "chat": str(base_dir / "memory" / "collective" / "chat"),
        "tasks": str(base_dir / "memory" / "collective" / "tasks"),
        "locks": str(base_dir / "memory" / "collective" / "locks"),
        "patterns": str(base_dir / "memory" / "collective" / "patterns"),
        "sessions": str(base_dir / "memory" / "collective" / "sessions"),
        "dharma": str(base_dir / "memory" / "collective" / "dharma"),
    }

    return {
        "workspace_id": workspace_id,
        "mode": "shared" if workspace_id else "repo-local",
        "base_dir": str(base_dir),
        "exists": base_dir.exists(),
        "subdirs": subdirs,
    }
