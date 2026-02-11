from __future__ import annotations

import json
import sqlite3
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from whitemagic.config.paths import DB_PATH


# Colors for terminal output
class Colors:
    HEADER = "\033[95m"
    BLUE = "\033[94m"
    CYAN = "\033[96m"
    GREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"

@dataclass
class Thought:
    """A single unit of reasoning."""

    type: str  # think, decide, question, observation
    content: str
    timestamp: datetime = field(default_factory=datetime.now)

class TerminalScratchpad:
    """Context manager for "token-free" reasoning.
    Captures thoughts, displays them live, and saves to memory on exit.
    """

    def __init__(self, task_name: str, db_path: str | None = None):
        self.task_name = task_name
        self.db_path = db_path or str(DB_PATH)
        self.thoughts: list[Thought] = []
        self.start_time: datetime | None = None
        self.session_id = str(uuid.uuid4())

    def __enter__(self) -> TerminalScratchpad:
        self.start_time = datetime.now()
        print(f"\n{Colors.HEADER}🧠 OPENING SCRATCHPAD: {self.task_name}{Colors.ENDC}")
        print(f"{Colors.BLUE}Session ID: {self.session_id}{Colors.ENDC}\n")
        return self

    def __exit__(self, exc_type: type[BaseException] | None, exc_val: BaseException | None, exc_tb: Any) -> None:
        assert self.start_time is not None
        duration = datetime.now() - self.start_time

        print(f"\n{Colors.HEADER}🔒 CLOSING SCRATCHPAD{Colors.ENDC}")
        print(f"{Colors.BLUE}Duration: {duration}{Colors.ENDC}")

        if exc_type:
            print(f"{Colors.FAIL}Exited with error: {exc_val}{Colors.ENDC}")
            self.think(f"CRITICAL ERROR: {exc_val}")

        self._save_to_memory()

    def think(self, content: str) -> None:
        """Record a general thought."""
        self._record("think", content, Colors.CYAN)

    def decide(self, content: str) -> None:
        """Record a decision."""
        self._record("decide", content, Colors.GREEN)

    def question(self, content: str) -> None:
        """Record a question."""
        self._record("question", content, Colors.WARNING)

    def observe(self, content: str) -> None:
        """Record an observation."""
        self._record("observe", content, Colors.BLUE)

    def _record(self, type: str, content: str, color: str) -> None:
        t = Thought(type=type, content=content)
        self.thoughts.append(t)

        prefix = f"[{type.upper()}]"
        timestamp = t.timestamp.strftime("%H:%M:%S")
        print(f"{color}{timestamp} {prefix.ljust(10)} {content}{Colors.ENDC}")
        time.sleep(0.1) # UX pause

    def _save_to_memory(self) -> None:
        """Serialize thoughts and save to SQLite."""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Format content as markdown
            md_content = f"# Scratchpad Session: {self.task_name}\n\n"
            md_content += f"**ID**: {self.session_id}\n"
            assert self.start_time is not None
            md_content += f"**Date**: {self.start_time.isoformat()}\n\n"

            for t in self.thoughts:
                icon = "🤔"
                if t.type == "decide":
                    icon = "✅"
                elif t.type == "question":
                    icon = "❓"
                elif t.type == "observe":
                    icon = "👀"

                md_content += f"### {icon} {t.type.title()}\n"
                md_content += f"*{t.timestamp.strftime('%H:%M:%S')}*\n\n"
                md_content += f"{t.content}\n\n"

            # Metadata
            metadata = {
                "source": "terminal_scratchpad",
                "thought_count": len(self.thoughts),
                "duration_seconds": (datetime.now() - (self.start_time or datetime.now())).total_seconds(),
            }

            cursor.execute("""
                INSERT OR IGNORE INTO memories (
                    id, content, memory_type, created_at,
                    metadata, title,
                    neuro_score, novelty_score, importance
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                self.session_id,
                md_content,
                "scratchpad_session",
                (self.start_time or datetime.now()).isoformat(),
                json.dumps(metadata),
                f"Scratchpad: {self.task_name}",
                1.0, 1.0, 1.0,
            ))

            conn.commit()
            conn.close()
            print(f"{Colors.GREEN}💾 Saved to permanent memory.{Colors.ENDC}")

        except Exception as e:
            print(f"{Colors.FAIL}❌ Failed to save memory: {e}{Colors.ENDC}")

if __name__ == "__main__":
    # Demo
    with TerminalScratchpad("Demo Session") as pad:
        pad.think("Initializing demo sequence...")
        pad.question("Does the color output work correctly?")
        pad.observe("Console is showing cyan text.")
        pad.decide("Yes, colors are functional.")
        pad.think("Saving to database now...")
