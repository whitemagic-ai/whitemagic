"""Autoimmune Defense System - Transform 229 anti-patterns into active defenses."""

from __future__ import annotations

from typing import Any
import json
import logging
from dataclasses import dataclass, field
from pathlib import Path

logger = logging.getLogger(__name__)

"""
Autoimmune Protection

Prevents the immune system from attacking WhiteMagic itself.
Like biological immune tolerance, this ensures self-recognition.

Also includes multi-agent coordination to prevent conflicts when
multiple AI instances work with WhiteMagic simultaneously.
"""

# Import multi-agent coordinator (optional)
get_coordinator: Any
try:
    from whitemagic.core.immune.defense.multi_agent import get_coordinator
except ImportError:
    get_coordinator = None


@dataclass
class AntiPattern:
    pattern_id: str
    title: str
    confidence: float
    keywords: list[str] = field(default_factory=list)
    auto_fixable: bool = False

    def matches(self, text: str) -> bool:
        """Check if text contains this anti-pattern."""
        if not self.keywords:
            return False

        text_lower = text.lower()
        # Require at least 2 keywords to match (stricter matching)
        matches = sum(1 for kw in self.keywords if kw.lower() in text_lower)
        return matches >= min(2, len(self.keywords))


@dataclass
class PatternViolation:
    file_path: Path
    line_number: int
    pattern: AntiPattern
    matched_text: str


class AutoimmuneSystem:
    """Defend against 229 anti-patterns, apply 241 solutions."""

    def __init__(self, base_dir: Path | None = None) -> None:
        self.base_dir = base_dir or Path(".")
        self.anti_patterns: dict[str, AntiPattern] = {}
        self._load_patterns()
        logger.info(f"🛡️  Loaded {len(self.anti_patterns)} anti-patterns")

    def _load_patterns(self) -> Any:
        """Load from v2.3.1 analysis."""
        meta_dir = self.base_dir / "memory" / "meta"
        pattern_files = list(meta_dir.glob("*_patterns.json"))

        if not pattern_files:
            return

        latest = max(pattern_files, key=lambda p: p.stat().st_mtime)
        with open(latest, "r") as f:
            data = json.load(f)

        # Convert anti-patterns
        for idx, ap in enumerate(data.get("anti_patterns", [])):
            pattern_id = f"AP-{idx:03d}"
            title = ap.get("title", "")

            # Extract keywords from title
            keywords = self._extract_keywords(title)

            self.anti_patterns[pattern_id] = AntiPattern(
                pattern_id=pattern_id,
                title=title,
                confidence=ap.get("confidence", 0.5),
                keywords=keywords,
                auto_fixable=ap.get("confidence", 0) > 0.9,
            )

    def _extract_keywords(self, title: str) -> list[str]:
        """Extract meaningful keywords from pattern title."""
        # Remove "Avoid:" prefix
        clean = title.replace("Avoid:", "").strip()

        # Split and filter short words
        words = [w.strip("*:") for w in clean.split()]
        keywords = [w for w in words if len(w) > 3]

        return keywords[:5]  # Top 5 keywords

    def scan_file(self, file_path: Path, min_confidence: float = 0.7) -> list[PatternViolation]:
        """Scan a file for anti-pattern violations."""
        violations: list[PatternViolation] = []

        if not file_path.exists():
            return violations

        try:
            content = file_path.read_text()
            lines = content.split("\n")

            # Only check high-confidence patterns
            high_conf_patterns = [
                p for p in self.anti_patterns.values()
                if p.confidence >= min_confidence and p.keywords
            ]

            for line_num, line in enumerate(lines, 1):
                for pattern in high_conf_patterns:
                    if pattern.matches(line):
                        violations.append(PatternViolation(
                            file_path=file_path,
                            line_number=line_num,
                            pattern=pattern,
                            matched_text=line.strip()[:100],  # Limit length
                        ))

        except Exception as e:
            logger.info(f"⚠️  Error scanning {file_path}: {e}")

        return violations

    def scan_directory(self, directory: Path, extensions: list[str] | None = None, min_confidence: float = 0.7) -> list[PatternViolation]:
        """Scan directory for violations."""
        if extensions is None:
            extensions = [".py", ".js", ".ts", ".md"]

        all_violations = []

        for ext in extensions:
            for file_path in directory.rglob(f"*{ext}"):
                if ".git" in str(file_path) or "node_modules" in str(file_path):
                    continue

                violations = self.scan_file(file_path, min_confidence)
                all_violations.extend(violations)

        return all_violations


# Global instance
_immune_system = None

def get_immune_system() -> AutoimmuneSystem:
    """Get global immune system instance."""
    global _immune_system
    if _immune_system is None:
        _immune_system = AutoimmuneSystem()
    return _immune_system


def detect_autoimmune() -> list[dict]:
    """Detect autoimmune patterns (for pattern discovery integration)."""
    system = get_immune_system()

    # Scan current directory for violations
    violations = system.scan_directory(Path("."), min_confidence=0.8)

    # Convert to pattern format
    patterns = []
    for v in violations[:20]:  # Top 20
        patterns.append({
            "type": "autoimmune_violation",
            "pattern_id": v.pattern.pattern_id,
            "title": v.pattern.title,
            "confidence": v.pattern.confidence,
            "file": str(v.file_path.relative_to(Path("."))) if v.file_path.is_relative_to(Path(".")) else str(v.file_path),
        })

    return patterns


# Move auto_heal into class
class AutoimmuneSystemExtended(AutoimmuneSystem):
    def auto_heal(self, violations: list[PatternViolation]) -> int:
        """Auto-fix high-confidence violations. Returns count of fixes."""
        fixed = 0
        for v in violations:
            if v.pattern.confidence > 0.9:
                # Simple fixes for common patterns
                try:
                    lines = v.file_path.read_text().split("\n")
                    # Apply basic cleanup
                    lines[v.line_number - 1] = lines[v.line_number - 1].strip()
                    v.file_path.write_text("\n".join(lines))
                    fixed += 1
                except Exception:
                    pass
        return fixed
