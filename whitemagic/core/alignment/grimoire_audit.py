"""Grimoire Audit — Brain Upgrade #3.
================================
Self-auditing system for tools and capabilities.
Scans grimoire/ and core/ for executable "spells".
"""

from __future__ import annotations

import logging
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

@dataclass
class Spell:
    """An executable tool or script found in the grimoire or core."""

    id: str
    name: str
    path: Path
    category: str # infrastructure, intelligence, archaeology, etc.
    is_executable: bool = True

class GrimoireAuditor:
    """Audits the system for available tools and scripts."""

    def __init__(self, project_root: Path | None = None) -> None:
        if project_root is None:
            self.project_root = Path(__file__).resolve().parent.parent.parent.parent
        else:
            self.project_root = project_root

        self.grimoire_path = self.project_root / "grimoire"
        self.core_path = self.project_root / "whitemagic" / "core"

    def audit(self) -> list[Spell]:
        """Perform a full audit of available spells."""
        spells: list[Spell] = []

        # 1. Scan Grimoire for references
        if self.grimoire_path.exists():
            for root, _, files in os.walk(self.grimoire_path):
                for file in files:
                    if file.endswith(".md"):
                        # In a real system, we'd parse the MD for @tool markers
                        pass

        # 2. Scan scripts/ for executable Python files
        scripts_path = self.project_root / "scripts"
        if scripts_path.exists():
            for script_path in scripts_path.iterdir():
                if script_path.is_file() and script_path.name.endswith(".py"):
                    spells.append(Spell(
                        id=f"script_{script_path.stem}",
                        name=script_path.stem.replace("_", " ").title(),
                        path=script_path,
                        category="automation",
                    ))

        # 3. Scan core/ for key engines
        if self.core_path.exists():
            for engine_dir in self.core_path.iterdir():
                if engine_dir.is_dir() and not engine_dir.name.startswith("__"):
                    spells.append(Spell(
                        id=f"engine_{engine_dir.name}",
                        name=f"{engine_dir.name.title()} Engine",
                        path=engine_dir,
                        category="core_intelligence",
                    ))

        logger.info(f"Grimoire Audit complete: Found {len(spells)} potential spells.")
        return spells

    def generate_capability_report(self) -> dict[str, Any]:
        """Generate a structured report for the AI Contract or CLI."""
        spells = self.audit()
        return {
            "total_spells": len(spells),
            "categories": {
                cat: len([s for s in spells if s.category == cat])
                for cat in set(s.category for s in spells)
            },
            "spells": [asdict(s) for s in spells],
        }

def asdict(obj: Any) -> dict[str, Any]:
    """Helper to convert dataclass to dict."""
    return {k: str(v) if isinstance(v, Path) else v for k, v in obj.__dict__.items()}

# Singleton
_auditor = None

def get_auditor() -> GrimoireAuditor:
    global _auditor
    if _auditor is None:
        _auditor = GrimoireAuditor()
    return _auditor
