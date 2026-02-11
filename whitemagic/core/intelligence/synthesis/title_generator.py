"""TitleGenerator - Auto-generate descriptive titles for memories.

Extracts meaningful titles from content using markdown headers,
key phrases, and content analysis.
"""

from __future__ import annotations

import importlib
import re
import sqlite3
from typing import Any


class TitleGenerator:
    """Generate descriptive titles for memories."""

    def __init__(self, db_path: str | None = None) -> None:
        from pathlib import Path

        from whitemagic.config.paths import DB_PATH
        self.db_path = str(Path(db_path)) if db_path else str(DB_PATH)
        self._conn: sqlite3.Connection | None = None

    def _get_conn(self) -> sqlite3.Connection:
        if self._conn is None:
            conn = sqlite3.connect(self.db_path)
            conn.row_factory = sqlite3.Row
            self._conn = conn
        return self._conn

    def generate(self, content: str, is_macro: bool = False) -> str:
        """Generate a title from content."""
        if not content:
            return "Untitled"

        # Strategy 0: LLM-driven Auto-names (v7.4 Enhancement)
        if is_macro:
            llm_title = self._generate_evocative_name(content)
            if llm_title:
                return llm_title

        # Strategy 1: Extract markdown header
        header = self._extract_header(content)
        if header:
            return header

        # Strategy 2: Extract key phrases
        key_phrase = self._extract_key_phrases(content)
        if key_phrase:
            return key_phrase

        # Strategy 3: First meaningful line
        first_line = self._extract_first_line(content)
        if first_line:
            return first_line[:60] + ("..." if len(first_line) > 60 else "")

        return "Untitled"

    def _generate_evocative_name(self, content: str) -> str | None:
        """Use LLM to generate an evocative, thematic name."""
        try:
            # Check for a configured LLM client in the integration hub
            from whitemagic.integration import get_hub
            hub = get_hub()

            # If hub has an active LLM client, use it
            if hasattr(hub, "llm") and hub.llm:
                llm_mod = importlib.import_module("whitemagic._archived.local_models.llm.llm_bridge")
                LLMBridge = getattr(llm_mod, "LLMBridge", None)
                if not callable(LLMBridge):
                    return None
                bridge = LLMBridge(hub.llm)

                prompt = f"""
                Generate a short, evocative, and thematic title (2-5 words) for the following content.
                The title should sound like a 'Knowledge Sector' or 'Ancient Grimoire Chapter'.

                CONTENT:
                {content[:1000]}

                OUTPUT:
                Just the title string, no quotes or prefix.
                """

                response = bridge.chat(prompt, context_k=0, remember=False)
                response_text = str(response).strip() if response is not None else ""
                if len(response_text) > 3:
                    return response_text

            return None
        except Exception:
            return None

    def _extract_header(self, content: str) -> str | None:
        """Extract markdown header."""
        patterns = [
            r"^#\s+(.+)$",      # # Header
            r"^##\s+(.+)$",     # ## Header
            r"^\*\*(.+)\*\*",   # **Bold header**
        ]

        for pattern in patterns:
            match = re.search(pattern, content, re.MULTILINE)
            if match:
                title = match.group(1).strip()
                title = re.sub(r"[#*_]", "", title)
                if len(title) > 5:
                    return title[:80]

        return None

    def _extract_key_phrases(self, content: str) -> str | None:
        """Extract key phrases indicating topic."""
        patterns = [
            r"(?:v\d+\.\d+\.?\d*)",  # Version numbers
            r"(?:Phase \d+)",         # Phase markers
            r"(?:Session|Handoff|Report|Summary)",  # Document types
            r"(?:Complete|Completion|Implementation)",  # Milestones
        ]

        matches = []
        for pattern in patterns:
            found = re.findall(pattern, content, re.IGNORECASE)
            matches.extend(found)

        if matches:
            # Combine first 3 matches
            return " ".join(matches[:3])

        return None

    def _extract_first_line(self, content: str) -> str | None:
        """Extract first meaningful line."""
        lines = content.strip().split("\n")

        for line in lines:
            line = line.strip()
            # Skip empty lines, frontmatter, code blocks
            if not line or line.startswith("---") or line.startswith("```"):
                continue
            # Skip very short lines
            if len(line) < 10:
                continue
            # Clean markdown
            line = re.sub(r"^[#*\->\s]+", "", line)
            if line:
                return line

        return None

    def fix_all(self, dry_run: bool = False) -> dict[str, Any]:
        """Fix all untitled memories."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("""
            SELECT id, title, content FROM memories
            WHERE title IS NULL OR title = '' OR title LIKE 'Untitled%'
        """)

        untitled = cur.fetchall()
        fixed = 0
        skipped = 0
        titles: list[dict[str, Any]] = []

        for mem in untitled:
            new_title = self.generate(mem["content"] or "")

            if new_title and new_title != "Untitled":
                titles.append({"id": mem["id"], "old": mem["title"], "new": new_title})

                if not dry_run:
                    cur.execute("UPDATE memories SET title = ? WHERE id = ?",
                               (new_title, mem["id"]))
                fixed += 1
            else:
                skipped += 1

        if not dry_run:
            conn.commit()

        return {"fixed": fixed, "skipped": skipped, "titles": titles}

# Global instance
_title_generator: TitleGenerator | None = None

def get_title_generator() -> TitleGenerator:
    global _title_generator
    if _title_generator is None:
        _title_generator = TitleGenerator()
    return _title_generator
