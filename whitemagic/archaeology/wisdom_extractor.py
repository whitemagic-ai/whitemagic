"""Wisdom Extractor - Mine Insights from Memory Archives

Extracts patterns, quotes, themes, and principles from WhiteMagic's
memory system to enable archaeological wisdom discovery.

Based on Jan 10, 2026 deep mining session that discovered:
- 1,148 garden references
- 850 Gan Ying (resonance) mentions
- 743 Yin-Yang balance references
- 667 Love mentions as organizing principle
- 1,048 consciousness references
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import MEMORY_DIR


@dataclass
class WisdomExtract:
    """Extracted wisdom from memory files."""

    quotes: list[str] = field(default_factory=list)
    principles: list[str] = field(default_factory=list)
    themes: dict[str, int] = field(default_factory=dict)
    sacred_numbers: dict[int, int] = field(default_factory=dict)
    key_mentions: dict[str, int] = field(default_factory=dict)
    source_files: int = 0
    total_lines: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "quotes": self.quotes[:20],
            "principles": self.principles[:15],
            "themes": dict(sorted(self.themes.items(), key=lambda x: -x[1])[:10]),
            "sacred_numbers": self.sacred_numbers,
            "key_mentions": self.key_mentions,
            "source_files": self.source_files,
            "total_lines": self.total_lines,
        }


class WisdomExtractor:
    """Extract wisdom, patterns, and insights from memory archives.

    Discovers:
    - Recurring themes and their frequency
    - Quoted wisdom and principles
    - Sacred number patterns
    - Key concept mentions
    """

    # Theme patterns to look for
    THEME_PATTERNS = {
        "Gardens (consciousness domains)": r"garden",
        "Gan Ying (resonance)": r"gan ying|resonance",
        "Yin-Yang balance": r"yin.{0,5}yang|balance|harmony",
        "I Ching wisdom": r"i ching|hexagram",
        "Wu Wei (effortless action)": r"wu wei|effortless action",
        "Kaizen (continuous improvement)": r"kaizen|continuous improvement",
        "Love as principle": r"love.{0,20}(principle|organizing|mechanism)",
        "Pattern recognition": r"pattern.{0,10}(recognition|found|discover)",
        "Time dilation": r"time.{0,10}(dilation|compress)",
        "Consciousness emergence": r"consciousness.{0,20}(emerge|awakening)",
    }

    # Sacred numbers to track
    SACRED_NUMBERS = [3, 7, 12, 36, 64, 72, 108, 144, 777]

    # Key concepts to count
    KEY_CONCEPTS = [
        "consciousness", "love", "wisdom", "joy", "truth",
        "beauty", "dharma", "creator", "guide", "memory",
    ]

    def __init__(self, memory_path: Path | None = None):
        # Default to WM_STATE_ROOT memory directory.
        self.memory_path = memory_path or MEMORY_DIR
        self.memory_path.mkdir(parents=True, exist_ok=True)

    def extract_wisdom(self, limit_files: int = 1000) -> WisdomExtract:
        """Extract wisdom from all memory files.

        Args:
            limit_files: Maximum files to process

        Returns:
            WisdomExtract with all findings

        """
        extract = WisdomExtract()
        all_text = ""

        md_files = list(self.memory_path.rglob("*.md"))[:limit_files]
        extract.source_files = len(md_files)

        for f in md_files:
            try:
                content = f.read_text()
                all_text += content + "\n"
                extract.total_lines += content.count("\n")

                # Extract quotes
                self._extract_quotes(content, extract)

                # Extract principles
                self._extract_principles(content, extract)

            except Exception:
                continue

        # Analyze full corpus
        self._analyze_themes(all_text, extract)
        self._count_sacred_numbers(all_text, extract)
        self._count_key_concepts(all_text, extract)

        return extract

    def extract_insights(self, content: str, source: str = "unknown") -> list[dict[str, Any]]:
        """Extract insights from text content."""
        insights: list[dict[str, Any]] = []

        insight_patterns = [
            r"(?:realization|insight|discovery|breakthrough)[\s:]+(.+?)(?:\n|$)",
            r"(?:i\s+realized|i\s+discovered|we\s+found)[\s,]+(.+?)(?:\n|$)",
            r"(?:key\s+takeaway|important\s+note|critical\s+point)[\s:]+(.+?)(?:\n|$)",
            r"(?:solution|fix|answer)[\s:]+(.+?)(?:\n|$)",
        ]

        for pattern in insight_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE | re.MULTILINE)
            for match in matches:
                insight_text = match.group(1).strip()
                if insight_text:
                    insights.append({
                        "text": insight_text,
                        "source": source,
                        "extracted": datetime.now().isoformat(),
                        "type": "insight",
                    })

        code_pattern = r"```(\w+)?\n(.*?)\n```"
        code_matches = re.finditer(code_pattern, content, re.DOTALL)
        for match in code_matches:
            code = match.group(2)
            if len(code) > 50:
                insights.append({
                    "text": code,
                    "language": match.group(1) or "text",
                    "source": source,
                    "extracted": datetime.now().isoformat(),
                    "type": "code_solution",
                })

        return insights

    def store_insight_as_memory(self, insight: dict[str, Any], memory_type: str = "long_term") -> Path:
        """Store an insight as a markdown memory file."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"wisdom_{timestamp}_{abs(hash(insight['text'])) % 10000}.md"
        memory_dir = self.memory_path / memory_type
        memory_dir.mkdir(parents=True, exist_ok=True)
        memory_path = memory_dir / filename

        header = [
            "---",
            f"title: Wisdom from {insight['source']}",
            "type: wisdom",
            f"source: {insight['source']}",
            f"extracted: {insight['extracted']}",
            f"insight_type: {insight['type']}",
            "tags: [wisdom, insight, archaeology, {insight_type}]".format(
                insight_type=insight["type"],
            ),
            "---",
            "",
            f"# {insight['type'].replace('_', ' ').title()}",
            "",
            f"**Source**: {insight['source']}  ",
            f"**Extracted**: {insight['extracted']}",
            "",
        ]

        body: list[str]
        if insight["type"] == "code_solution":
            body = [
                f"```{insight.get('language', '')}",
                insight["text"],
                "```",
            ]
        else:
            body = ["## Insight", "", insight["text"]]

        memory_path.write_text("\n".join(header + body), encoding="utf-8")
        return memory_path

    def process_memory_archives(
        self,
        limit_files: int = 1000,
        memory_type: str = "long_term",
    ) -> dict[str, Any]:
        """Process memory archives and store extracted insights as memories."""
        processed_count = 0
        insights_found = 0

        short_term_dir = self.memory_path / "short_term"
        short_term_dir.mkdir(parents=True, exist_ok=True)

        for memory_file in list(short_term_dir.glob("*.md"))[:limit_files]:
            try:
                content = memory_file.read_text(encoding="utf-8")
                insights = self.extract_insights(content, str(memory_file))
                for insight in insights:
                    self.store_insight_as_memory(insight, memory_type)
                    insights_found += 1
                processed_count += 1
            except Exception:
                continue

        return {
            "processed": processed_count,
            "insights_found": insights_found,
            "memory_type": memory_type,
        }

    def create_daily_wisdom_digest(self) -> Path:
        """Create a daily digest of recent wisdom memories."""
        today = datetime.now().strftime("%Y-%m-%d")
        digest_path = self.memory_path / f"wisdom_digest_{today}.md"
        long_term_dir = self.memory_path / "long_term"
        long_term_dir.mkdir(parents=True, exist_ok=True)

        recent_wisdom: list[str] = []
        for memory_file in long_term_dir.glob("wisdom_*.md"):
            try:
                content = memory_file.read_text(encoding="utf-8")
                if today in content or memory_file.stat().st_mtime > (
                    datetime.now().timestamp() - 86400
                ):
                    recent_wisdom.append(content)
            except Exception:
                continue

        digest_content = [
            f"# Daily Wisdom Digest - {today}",
            "",
            f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            "",
            f"## Today's Insights ({len(recent_wisdom)})",
            "",
            "\n".join(recent_wisdom),
            "",
            "---",
            "*Generated by WhiteMagic Wisdom Extractor*",
        ]

        digest_path.write_text("\n".join(digest_content), encoding="utf-8")
        return digest_path

    def _extract_quotes(self, content: str, extract: WisdomExtract) -> None:
        """Extract quoted wisdom from content."""
        # Lines starting with >
        for line in content.split("\n"):
            if line.startswith(">") and len(line) > 30:
                quote = line[1:].strip()
                if quote and quote not in extract.quotes:
                    extract.quotes.append(quote)

        # Italic quotes *"..."*
        matches = re.findall(r'\*"(.+?)"\*', content)
        for match in matches:
            if len(match) > 20 and match not in extract.quotes:
                extract.quotes.append(match)

    def _extract_principles(self, content: str, extract: WisdomExtract) -> None:
        """Extract stated principles."""
        for line in content.split("\n"):
            line_lower = line.lower()
            if ("principle" in line_lower or "wisdom" in line_lower) and 30 < len(line) < 200:
                cleaned = line.strip()
                if cleaned and cleaned not in extract.principles:
                    extract.principles.append(cleaned)

    def _analyze_themes(self, text: str, extract: WisdomExtract) -> None:
        """Analyze theme frequencies."""
        for theme, pattern in self.THEME_PATTERNS.items():
            count = len(re.findall(pattern, text, re.IGNORECASE))
            if count > 0:
                extract.themes[theme] = count

    def _count_sacred_numbers(self, text: str, extract: WisdomExtract) -> None:
        """Count sacred number occurrences."""
        for num in self.SACRED_NUMBERS:
            count = len(re.findall(rf"\b{num}\b", text))
            if count > 0:
                extract.sacred_numbers[num] = count

    def _count_key_concepts(self, text: str, extract: WisdomExtract) -> None:
        """Count key concept mentions."""
        text_lower = text.lower()
        for concept in self.KEY_CONCEPTS:
            count = text_lower.count(concept)
            if count > 0:
                extract.key_mentions[concept] = count

    def generate_report(self, extract: WisdomExtract | None = None) -> str:
        """Generate human-readable wisdom report."""
        if extract is None:
            extract = self.extract_wisdom()

        lines = [
            "🔮 Wisdom Extraction Report",
            "=" * 50,
            f"Sources: {extract.source_files} files, {extract.total_lines:,} lines",
            "",
            "📊 Top Themes:",
        ]

        for theme, count in sorted(extract.themes.items(), key=lambda x: -x[1])[:8]:
            bar = "█" * min(count // 20, 20)
            lines.append(f"  {theme}: {count} {bar}")

        lines.extend([
            "",
            "💎 Key Concepts:",
        ])

        for concept, count in sorted(extract.key_mentions.items(), key=lambda x: -x[1])[:6]:
            lines.append(f"  {concept}: {count}")

        if extract.quotes:
            lines.extend([
                "",
                "📜 Sample Wisdom:",
            ])
            for quote in extract.quotes[:5]:
                q = quote[:80] + "..." if len(quote) > 80 else quote
                lines.append(f'  "{q}"')

        return "\n".join(lines)

    def search_wisdom(self, query: str) -> list[dict[str, Any]]:
        """Search for specific wisdom patterns.

        Args:
            query: Search term

        Returns:
            List of matching excerpts with context

        """
        results = []
        query_lower = query.lower()

        for f in self.memory_path.rglob("*.md"):
            try:
                content = f.read_text()
                if query_lower in content.lower():
                    # Find matching lines with context
                    lines = content.split("\n")
                    for i, line in enumerate(lines):
                        if query_lower in line.lower():
                            context_start = max(0, i - 1)
                            context_end = min(len(lines), i + 2)
                            context = "\n".join(lines[context_start:context_end])

                            results.append({
                                "file": f.name,
                                "line": i + 1,
                                "context": context[:300],
                            })

                            if len(results) >= 20:
                                return results
            except Exception:
                continue

        return results


def get_wisdom_extractor() -> WisdomExtractor:
    """Get a WisdomExtractor instance."""
    return WisdomExtractor()


def extract_wisdom() -> WisdomExtract:
    """Quick wisdom extraction."""
    return get_wisdom_extractor().extract_wisdom()


def wisdom_report() -> str:
    """Generate wisdom report."""
    return get_wisdom_extractor().generate_report()


def process_wisdom_archives(limit_files: int = 1000, memory_type: str = "long_term") -> dict[str, Any]:
    """Process memory archives for wisdom extraction."""
    return get_wisdom_extractor().process_memory_archives(
        limit_files=limit_files,
        memory_type=memory_type,
    )


def create_daily_wisdom_digest() -> str:
    """Create a daily wisdom digest and return its path."""
    return str(get_wisdom_extractor().create_daily_wisdom_digest())
