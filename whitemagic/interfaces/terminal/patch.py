"""Patch preview system for write operations."""

import subprocess
from pathlib import Path


class PatchPreview:
    """Generate patch previews for write operations."""

    @staticmethod
    def git_diff(cwd: str | None = None) -> str:
        """Get git diff for current changes."""
        try:
            result = subprocess.run(
                ["git", "diff", "--cached"], cwd=cwd, capture_output=True, text=True, timeout=5,
            )

            if result.returncode == 0:
                return result.stdout
            return ""
        except Exception:
            return ""

    @staticmethod
    def file_change_preview(filepath: str, operation: str) -> str:
        """Preview file change operation."""
        path = Path(filepath)

        if operation == "create":
            return f"[CREATE] {filepath}"
        elif operation == "delete":
            if path.exists():
                size = path.stat().st_size
                return f"[DELETE] {filepath} ({size} bytes)"
            return f"[DELETE] {filepath}"
        elif operation == "modify":
            return f"[MODIFY] {filepath}"

        return f"[{operation.upper()}] {filepath}"

    @staticmethod
    def command_preview(cmd: str, args: list) -> str:
        """Preview command execution."""
        full_cmd = cmd + (" " + " ".join(args) if args else "")

        # Detect operation type
        if cmd in ["git", "npm", "pip", "cargo"]:
            return f"[PACKAGE/VCS] {full_cmd}"
        elif cmd in ["cp", "mv", "mkdir", "touch"]:
            return f"[FILE_OP] {full_cmd}"
        else:
            return f"[EXECUTE] {full_cmd}"
