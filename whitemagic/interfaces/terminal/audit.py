"""Audit logging for command execution."""

import json
import uuid
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path

from whitemagic.config.paths import WM_ROOT


@dataclass
class AuditLog:
    """Audit log entry."""

    run_id: str
    correlation_id: str | None
    command: str
    exit_code: int
    duration_ms: float
    timestamp: str
    user: str | None = None


class AuditLogger:
    """Log command executions."""

    def __init__(self, log_dir: Path | None = None) -> None:
        preferred = log_dir or (WM_ROOT / "audit")
        resolved = self._ensure_writable(preferred)
        if resolved is None:
            raise PermissionError("Unable to create writable audit log directory")
        self.log_dir = resolved

    def _ensure_writable(self, directory: Path) -> Path | None:
        """Attempt to create directory and verify it is writable."""
        try:
            directory.mkdir(parents=True, exist_ok=True)
            test_file = directory / ".write_test"
            with open(test_file, "w") as tmp:
                tmp.write("ok")
            test_file.unlink(missing_ok=True)
            return directory
        except PermissionError:
            return None

    def log(
        self,
        command: str,
        exit_code: int,
        duration_ms: float,
        correlation_id: str | None = None,
        user: str | None = None,
    ) -> str:
        """Log execution."""
        run_id = str(uuid.uuid4())[:8]

        entry = AuditLog(
            run_id=run_id,
            correlation_id=correlation_id,
            command=command,
            exit_code=exit_code,
            duration_ms=duration_ms,
            timestamp=datetime.now(timezone.utc).isoformat(),
            user=user,
        )

        # Write to log file
        log_file = self.log_dir / f"{datetime.now(timezone.utc).strftime('%Y%m%d')}.jsonl"
        with open(log_file, "a") as f:
            f.write(json.dumps(asdict(entry)) + "\n")

        return run_id
