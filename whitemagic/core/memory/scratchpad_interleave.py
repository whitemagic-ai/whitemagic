"""Scratchpad Interleaving - Controlled Cross-Pollination of Ideas.

Manages multiple parallel scratchpads with intelligent interleaving
at phase boundaries for creative synthesis.
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.fileio import atomic_write, file_lock


class Scratchpad:
    """A single working memory scratchpad."""

    def __init__(self, name: str, focus: str | None = None):
        self.name = name
        self.focus = focus
        self.entries: list[dict[str, Any]] = []
        self.created = datetime.now()
        self.last_active = datetime.now()

    def write(self, content: str, tag: str | None = None) -> None:
        """Write to scratchpad."""
        self.entries.append({
            "content": content,
            "tag": tag,
            "timestamp": datetime.now().isoformat(),
        })
        self.last_active = datetime.now()

    def read_recent(self, count: int = 5) -> list[str]:
        """Read recent entries."""
        return [e["content"] for e in self.entries[-count:]]

    def clear(self) -> None:
        """Clear scratchpad."""
        self.entries = []

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "focus": self.focus,
            "entries": self.entries,
            "created": self.created.isoformat(),
            "last_active": self.last_active.isoformat(),
        }


class ScratchpadManager:
    """Manages multiple scratchpads with interleaving."""

    MAX_SCRATCHPADS = 7  # Working memory limit (Miller's Law)

    def __init__(self, scratch_dir: Path | None = None):
        self.scratch_dir = scratch_dir or (WM_ROOT / "scratchpads")
        self.scratch_dir.mkdir(parents=True, exist_ok=True)
        self.scratchpads: dict[str, Scratchpad] = {}
        self.interleave_history: list[dict[str, Any]] = []
        self._load_scratchpads()

    def _load_scratchpads(self) -> None:
        """Load existing scratchpads."""
        for pad_file in self.scratch_dir.glob("*.json"):
            try:
                with file_lock(pad_file):
                    data: dict[str, Any] = json.loads(pad_file.read_text())
                pad = Scratchpad(data["name"], data.get("focus"))
                pad.entries = data.get("entries", [])
                self.scratchpads[data["name"]] = pad
            except json.JSONDecodeError:
                pass

    def _save_scratchpad(self, name: str) -> None:
        """Save a scratchpad."""
        if name in self.scratchpads:
            pad_file = self.scratch_dir / f"{name}.json"
            with file_lock(pad_file):
                atomic_write(pad_file, json.dumps(self.scratchpads[name].to_dict(), indent=2))

    def create(self, name: str, focus: str | None = None) -> Scratchpad:
        """Create a new scratchpad."""
        if len(self.scratchpads) >= self.MAX_SCRATCHPADS:
            # Remove oldest inactive
            oldest = min(self.scratchpads.values(), key=lambda p: p.last_active)
            del self.scratchpads[oldest.name]

        pad = Scratchpad(name, focus)
        self.scratchpads[name] = pad
        self._save_scratchpad(name)
        return pad

    def write_to(self, name: str, content: str, tag: str | None = None) -> None:
        """Write to a specific scratchpad."""
        if name not in self.scratchpads:
            self.create(name)
        self.scratchpads[name].write(content, tag)
        self._save_scratchpad(name)

    def interleave(self, pad_names: list[str] | None = None) -> dict[str, Any]:
        """Interleave scratchpads - merge insights at phase boundary.

        This is the "dream synthesis" moment where separate
        working memories cross-pollinate.
        """
        pads = pad_names or list(self.scratchpads.keys())

        # Gather all recent entries
        all_entries = []
        for name in pads:
            if name in self.scratchpads:
                for entry in self.scratchpads[name].entries[-3:]:
                    all_entries.append({
                        "source": name,
                        "content": entry["content"],
                        "tag": entry.get("tag"),
                    })

        # Create synthesis
        synthesis = {
            "timestamp": datetime.now().isoformat(),
            "sources": pads,
            "entries_merged": len(all_entries),
            "entries": all_entries,
        }

        self.interleave_history.append(synthesis)
        return synthesis

    def get_active_pads(self) -> list[str]:
        """Get names of active scratchpads."""
        return list(self.scratchpads.keys())

    def summarize_all(self) -> dict[str, list[str]]:
        """Get summary of all scratchpads."""
        return {
            name: pad.read_recent(3)
            for name, pad in self.scratchpads.items()
        }


# Singleton
_manager: ScratchpadManager | None = None

def get_scratchpad_manager() -> ScratchpadManager:
    global _manager
    if _manager is None:
        _manager = ScratchpadManager()
    return _manager
