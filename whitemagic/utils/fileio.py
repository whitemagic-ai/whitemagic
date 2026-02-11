"""File locking utilities with cross-platform fallbacks."""

from __future__ import annotations

import os
import tempfile
import time
from collections.abc import Generator
from contextlib import contextmanager
from pathlib import Path
from typing import Any, cast

try:  # POSIX platforms
    import fcntl as _fcntl  # type: ignore[attr-defined]

    fcntl: Any = _fcntl
    HAS_FCNTL = True
except ImportError:
    # pragma: no cover - Windows specific branch
    fcntl = None
    HAS_FCNTL = False

try:  # Windows fallback
    import msvcrt as _msvcrt  # type: ignore[attr-defined]

    msvcrt: Any = _msvcrt
    HAS_MSVCRT = True
except ImportError:
    # pragma: no cover - non-Windows platforms
    msvcrt = None
    HAS_MSVCRT = False


@contextmanager
def file_lock(filepath: str | Path, timeout: float = 5.0) -> Generator[None, None, None]:
    """Context manager providing an advisory lock on ``filepath``.

    Uses ``fcntl`` on POSIX and ``msvcrt`` on Windows. If neither backend
    exists, the context still yields so callers proceed rather than crash.
    """
    filepath = Path(filepath)
    lock_file = filepath.parent / f".{filepath.name}.lock"

    if HAS_FCNTL:
        fcntl_mod = cast(Any, fcntl)
        lock_file.touch(exist_ok=True)
        fd = None
        try:
            fd = os.open(lock_file, os.O_RDWR | os.O_CREAT)
            fcntl_mod.flock(fd, fcntl_mod.LOCK_EX)
            yield
        finally:
            if fd is not None:
                fcntl_mod.flock(fd, fcntl_mod.LOCK_UN)
                os.close(fd)
        return

    if HAS_MSVCRT:  # pragma: no cover - Windows only
        msvcrt_mod = cast(Any, msvcrt)
        lock_file.parent.mkdir(parents=True, exist_ok=True)
        lock_file.touch(exist_ok=True)
        file_handle = open(lock_file, "a+")
        start = time.time()
        acquired = False
        try:
            while True:
                try:
                    msvcrt_mod.locking(file_handle.fileno(), msvcrt_mod.LK_LOCK, 1)
                    acquired = True
                    break
                except OSError:
                    if timeout is not None and (time.time() - start) >= timeout:
                        raise TimeoutError(f"Timed out acquiring lock for {filepath}") from None
                    time.sleep(0.05)
            yield
        finally:
            if acquired:
                try:
                    msvcrt_mod.locking(file_handle.fileno(), msvcrt_mod.LK_UNLCK, 1)
                finally:
                    file_handle.close()
            else:
                file_handle.close()
        return

    # No locking backend available (very rare platform) - degrade gracefully.
    yield


def atomic_write(filepath: str | Path, content: str) -> None:
    """Write ``content`` to ``filepath`` atomically via write-then-replace.
    """
    filepath = Path(filepath)
    temp_fd, temp_path = tempfile.mkstemp(
        dir=filepath.parent,
        prefix=f".{filepath.name}.tmp",
        text=True,
    )
    try:
        os.write(temp_fd, content.encode("utf-8"))
        os.close(temp_fd)
        os.replace(temp_path, filepath)  # overwrite-safe on all major OSes
    except OSError:
        try:
            os.unlink(temp_path)
        except OSError:
            pass
        raise
