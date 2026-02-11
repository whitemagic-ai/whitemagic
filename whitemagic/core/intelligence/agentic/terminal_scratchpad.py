"""Terminal-based scratchpad with auto-finalization to memory."""

import os
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path
from types import TracebackType
from typing import Any


class TerminalScratchpad:
    """Scratchpad that lives in terminal, auto-finalizes to permanent memory.

    This is a "hack" that gives AI free reasoning space outside token economy.
    Write thoughts, decisions, questions to terminal - no token cost!
    When done, auto-converts to structured memory.
    """

    def __init__(self, task_name: str) -> None:
        self.task_name = task_name
        self.sections: dict[str, list[str]] = {
            "thoughts": [],
            "decisions": [],
            "questions": [],
            "next_steps": [],
            "ideas": [],
        }
        self.terminal_file = self._create_terminal_file()
        self.terminal_process: subprocess.Popen[Any] | None = None

    def _create_terminal_file(self) -> Path:
        """Create temp file for terminal display."""
        fd, path = tempfile.mkstemp(suffix=".md", prefix=f"scratch_{self.task_name}_")
        os.close(fd)
        filepath = Path(path)
        # Initialize with header
        filepath.write_text(f"# 🧠 Scratchpad: {self.task_name}\n\n*Live updates...*\n")
        return filepath

    def start_terminal(self) -> None:
        """Launch terminal with live-updating display.

        SECURITY: Passes user input as arguments to prevent command injection.
        """
        # SECURITY FIX: Use printf with %s and pass task_name as argument
        script = """#!/bin/bash
printf '🧠 SCRATCHPAD: %s\n' "$1"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Live updates below..."
echo ""
tail -f "$2"
"""
        fd, script_path = tempfile.mkstemp(suffix=".sh")
        os.close(fd)
        Path(script_path).write_text(script)
        os.chmod(script_path, 0o700)

        # SECURITY: Pass user input as arguments
        self.terminal_process = subprocess.Popen(
            ["/bin/bash", script_path, self.task_name, str(self.terminal_file)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

    def think(self, thought: str) -> None:
        """Add thought to scratchpad (free tokens!)."""
        self.sections["thoughts"].append(thought)
        self._update_display()

    def decide(self, decision: str) -> None:
        """Record decision."""
        self.sections["decisions"].append(decision)
        self._update_display()

    def question(self, question: str) -> None:
        """Add open question."""
        self.sections["questions"].append(question)
        self._update_display()

    def next_step(self, step: str) -> None:
        """Add next action step."""
        self.sections["next_steps"].append(step)
        self._update_display()

    def idea(self, idea: str) -> None:
        """Capture random idea."""
        self.sections["ideas"].append(idea)
        self._update_display()

    def _update_display(self) -> None:
        """Update terminal file (appears live in terminal)."""
        content = f"# 🧠 Scratchpad: {self.task_name}\n\n"
        content += f"*Updated: {datetime.now().strftime('%H:%M:%S')}*\n\n"

        for section_name, items in self.sections.items():
            if items:
                title = section_name.replace("_", " ").title()
                content += f"## {title}\n\n"
                for i, item in enumerate(items, 1):
                    content += f"{i}. {item}\n"
                content += "\n"

        self.terminal_file.write_text(content)

    def finalize(self, memory_type: str = "short_term") -> str:
        """Convert scratchpad to permanent memory."""
        try:
            from whitemagic import MemoryManager

            manager = MemoryManager()
            memory_content = self._generate_memory_content()

            filename = manager.create_memory(
                title=f"Scratchpad: {self.task_name}",
                content=memory_content,
                tags=["scratchpad", "auto-finalized", self.task_name.lower()],
                memory_type=memory_type,
            )

            return str(filename)
        finally:
            self.cleanup()

    def _generate_memory_content(self) -> str:
        """Generate structured memory from scratchpad."""
        content = f"# {self.task_name} - Working Notes\n\n"
        content += f"**Created**: {datetime.now().isoformat()}\n"
        content += "**Type**: Auto-finalized scratchpad\n"
        content += "**Source**: Terminal reasoning (zero token cost)\n\n"

        for section_name, items in self.sections.items():
            if items:
                title = section_name.replace("_", " ").title()
                content += f"## {title}\n\n"
                for i, item in enumerate(items, 1):
                    content += f"{i}. {item}\n"
                content += "\n"

        return content

    def cleanup(self) -> None:
        """Clean up terminal and temp files."""
        if self.terminal_process:
            self.terminal_process.terminate()

        if self.terminal_file.exists():
            self.terminal_file.unlink()

    def __enter__(self) -> "TerminalScratchpad":
        """Context manager entry."""
        self.start_terminal()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        """Context manager exit - auto-finalize."""
        if exc_type is None:
            self.finalize()
        else:
            self.cleanup()
