"""Watch for test changes and auto-run."""

import time
from collections.abc import Callable
from pathlib import Path


class ProductionTestWatcher:  # Renamed to avoid pytest collection
    """Watch test files for changes."""

    def __init__(self, test_dir: str = "tests"):
        self.test_dir = Path(test_dir)
        self.last_modified: dict[Path, float] = {}

    def watch(self, callback: Callable[[list[str]], None]) -> None:
        """Watch for file changes."""
        while True:
            changed = self._check_changes()
            if changed:
                callback(changed)
            time.sleep(1)

    def _check_changes(self) -> list[str]:
        """Check for changed files."""
        changed = []
        for test_file in self.test_dir.glob("**/*.py"):
            mtime = test_file.stat().st_mtime
            if test_file not in self.last_modified or self.last_modified[test_file] != mtime:
                self.last_modified[test_file] = mtime
                changed.append(str(test_file))
        return changed

def watch(test_dir: str = "tests", callback: Callable[[list[str]], None] | None = None) -> ProductionTestWatcher:
    """Start watching tests."""
    watcher = ProductionTestWatcher(test_dir)
    if callback:
        watcher.watch(callback)
    return watcher
