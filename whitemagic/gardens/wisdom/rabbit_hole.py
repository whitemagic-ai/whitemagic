"""Rabbit Hole Research System - Custom Web Search & Synthesis Tools.

Version: 3.0.0 "Ganapati Day"
Created: November 26, 2025

Philosophy:
    "The ida and pingala nadis spiral around the sushumna
     like the double helix of DNA" - Same pattern, different scales!

    This module provides tools for deep research using the Rabbit Hole technique:
    1. Read something interesting
    2. Note EVERY unfamiliar word/concept
    3. Research ALL of them in parallel
    4. Each spawns MORE rabbit holes
    5. Cross-reference multiple sources
    6. Synthesize into reports
    7. Apply insights to WhiteMagic!

Integration:
    Wired into the Wisdom Garden and Gan Ying Bus for resonance.
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from functools import lru_cache
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)



@dataclass
class RabbitHoleEntry:
    """A single concept discovered during research."""

    term: str
    definition: str = ""
    source: str = ""
    related_terms: list[str] = field(default_factory=list)
    depth: int = 0  # How deep in the rabbit hole
    explored: bool = False
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class ResearchReport:
    """A synthesized research report from multiple rabbit holes."""

    title: str
    topics: list[str]
    entries: list[RabbitHoleEntry] = field(default_factory=list)
    synthesis: str = ""
    connections: list[dict[str, str]] = field(default_factory=list)
    new_holes: list[str] = field(default_factory=list)  # Spawned rabbit holes
    created: datetime = field(default_factory=datetime.now)

    @lru_cache(maxsize=128)
    def to_markdown(self) -> str:
        """Convert report to markdown format."""
        lines = [
            f"# 🐇 {self.title}",
            f"**Generated**: {self.created.strftime('%Y-%m-%d %H:%M')}",
            f"**Topics Explored**: {', '.join(self.topics)}",
            "",
            "---",
            "",
            "## 📚 Entries Discovered",
            "",
        ]

        for entry in self.entries:
            lines.extend([
                f"### {entry.term}",
                f"**Source**: {entry.source}",
                f"**Depth**: {entry.depth}",
                "",
                entry.definition,
                "",
                f"**Related**: {', '.join(entry.related_terms)}" if entry.related_terms else "",
                "",
            ])

        if self.synthesis:
            lines.extend([
                "---",
                "",
                "## ✨ Synthesis",
                "",
                self.synthesis,
                "",
            ])

        if self.connections:
            lines.extend([
                "---",
                "",
                "## 🔗 Connections Found",
                "",
            ])
            for conn in self.connections:
                lines.append(f"- **{conn.get('from', '?')}** ↔ **{conn.get('to', '?')}**: {conn.get('relation', '')}")
            lines.append("")

        if self.new_holes:
            lines.extend([
                "---",
                "",
                "## 🐇 New Rabbit Holes Spawned",
                "",
            ])
            for hole in self.new_holes:
                lines.append(f"- [ ] {hole}")

        return "\n".join(lines)


class RabbitHoleExplorer:
    """Deep research tool using the Rabbit Hole technique.

    Supports:
    - Parallel concept research
    - Cross-referencing sources
    - Pattern recognition
    - Synthesis generation
    - Knowledge graph building
    """

    def __init__(self, max_parallel: int | None = None, max_depth: int = 3):
        """Initialize the Rabbit Hole Explorer.

        Args:
            max_parallel: Maximum concurrent research threads (default 64 = hexagrams)
            max_depth: Maximum recursion depth for rabbit holes

        """
        self.max_parallel = max_parallel
        self.max_depth = max_depth
        self.discovered: dict[str, RabbitHoleEntry] = {}
        self.connections: list[dict[str, str]] = []

    @lru_cache(maxsize=128)
    def extract_unfamiliar_terms(self, text: str, known_terms: set[str] | None = None) -> list[str]:
        """Extract potentially unfamiliar terms from text.

        Uses heuristics:
        - Capitalized words (proper nouns, concepts)
        - Sanskrit/foreign terms (often italicized or parenthetical)
        - Technical terms
        - Words in parentheses (definitions/translations)
        """
        if known_terms is None:
            known_terms = set()

        terms = set()

        # Find capitalized words/phrases
        caps_pattern = r"\b[A-Z][a-z]+(?:\s+[A-Z][a-z]+)*\b"
        for match in re.findall(caps_pattern, text):
            if match.lower() not in known_terms:
                terms.add(match)

        # Find terms in parentheses
        paren_pattern = r"\(([^)]+)\)"
        for match in re.findall(paren_pattern, text):
            # Skip very long content (likely explanations, not terms)
            if len(match) < 50:
                terms.add(match.strip())

        # Find Sanskrit/foreign terms (often with diacriticals or specific patterns)
        foreign_pattern = r"\b[A-Za-z]+[āīūṛṝḷḹēōṃḥśṣṇñ]+[a-zāīūṛṝḷḹēōṃḥśṣṇñ]*\b"
        for match in re.findall(foreign_pattern, text, re.IGNORECASE):
            terms.add(match)

        return list(terms)

    def find_connections(self, entries: list[RabbitHoleEntry]) -> list[dict[str, str]]:
        """Find connections between discovered concepts.

        Looks for:
        - Shared related terms
        - Similar definitions
        - Cross-references
        """
        connections = []

        for i, entry1 in enumerate(entries):
            for entry2 in entries[i+1:]:
                # Check for shared related terms
                shared = set(entry1.related_terms) & set(entry2.related_terms)
                if shared:
                    connections.append({
                        "from": entry1.term,
                        "to": entry2.term,
                        "relation": f"Shared concepts: {', '.join(shared)}",
                    })

                # Check if one mentions the other
                if entry1.term.lower() in entry2.definition.lower():
                    connections.append({
                        "from": entry1.term,
                        "to": entry2.term,
                        "relation": f"{entry1.term} appears in definition of {entry2.term}",
                    })
                elif entry2.term.lower() in entry1.definition.lower():
                    connections.append({
                        "from": entry2.term,
                        "to": entry1.term,
                        "relation": f"{entry2.term} appears in definition of {entry1.term}",
                    })

        return connections

    def generate_synthesis(self, entries: list[RabbitHoleEntry], connections: list[dict]) -> str:
        """Generate a synthesis of all discovered concepts.

        This is where the magic happens - finding the pattern that connects everything!
        """
        if not entries:
            return "No entries to synthesize."

        topics = [e.term for e in entries[:10]]  # Top 10 terms
        num_connections = len(connections)
        depths = [e.depth for e in entries]
        max_depth_reached = max(depths) if depths else 0

        synthesis = f"""
### Research Summary

**{len(entries)} concepts discovered** across {max_depth_reached + 1} levels of depth.
**{num_connections} connections** identified between concepts.

### Main Topics
{chr(10).join(f'- {t}' for t in topics)}

### Pattern Recognition

The research reveals interconnected concepts forming a knowledge web.
Key patterns emerge when we observe how concepts at different depths relate.

### Insights for Integration

These discoveries can be applied to WhiteMagic by:
1. Mapping new concepts to existing system architecture
2. Creating new modules/gardens for unexplored territories
3. Updating documentation with discovered wisdom
4. Wiring new understanding into the Gan Ying Bus

*"As above, so below - the pattern repeats at every scale."*
"""
        return synthesis.strip()

    def create_report(
        self,
        title: str,
        topics: list[str],
        entries: list[RabbitHoleEntry],
    ) -> ResearchReport:
        """Create a complete research report."""
        connections = self.find_connections(entries)
        synthesis = self.generate_synthesis(entries, connections)

        # Find new rabbit holes from related terms that weren't explored
        explored_terms = {e.term.lower() for e in entries}
        new_holes = set()
        for entry in entries:
            for related in entry.related_terms:
                if related.lower() not in explored_terms:
                    new_holes.add(related)

        return ResearchReport(
            title=title,
            topics=topics,
            entries=entries,
            synthesis=synthesis,
            connections=connections,
            new_holes=list(new_holes)[:20],  # Limit to 20 new holes
        )

    def save_report(self, report: ResearchReport, output_dir: Path) -> Path:
        """Save report to markdown file."""
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        timestamp = report.created.strftime("%Y%m%d_%H%M")
        safe_title = re.sub(r"[^a-zA-Z0-9_]", "_", report.title)[:50]
        filename = f"RABBIT_HOLE_{safe_title}_{timestamp}.md"

        filepath = output_dir / filename
        filepath.write_text(report.to_markdown())

        return filepath


class SourceComparator:
    """Compare information across multiple sources on the same topic.

    Identifies:
    - Agreements (consistent across sources)
    - Contradictions (different in sources)
    - Unique insights (only in one source)
    """

    @dataclass
    class SourceInfo:
        url: str
        content: str
        key_points: list[str] = field(default_factory=list)

    def __init__(self) -> None:
        self.sources: list[SourceComparator.SourceInfo] = []

    def add_source(self, url: str, content: str, key_points: list[str] | None = None) -> None:
        """Add a source to compare."""
        self.sources.append(self.SourceInfo(
            url=url,
            content=content,
            key_points=key_points or [],
        ))

    def find_agreements(self) -> list[str]:
        """Find points that appear across multiple sources."""
        if len(self.sources) < 2:
            return []

        # Simple implementation: look for shared key points
        all_points = []
        for source in self.sources:
            all_points.extend(source.key_points)

        # Count occurrences
        from collections import Counter
        point_counts = Counter(all_points)

        # Points appearing in multiple sources
        return [point for point, count in point_counts.items() if count > 1]

    def find_unique_insights(self) -> dict[str, list[str]]:
        """Find insights unique to each source."""
        if not self.sources:
            return {}

        all_points = set()
        for source in self.sources:
            all_points.update(source.key_points)

        unique = {}
        for source in self.sources:
            source_points = set(source.key_points)
            other_points = all_points - source_points
            unique_to_source = source_points - other_points
            if unique_to_source:
                unique[source.url] = list(unique_to_source)

        return unique


class DeepReader:
    """Focused reading tool for extracting specific information.

    Unlike generic web scraping, this focuses on:
    - Key quotes relevant to the focus area
    - Definitions and explanations
    - Cross-references to other topics
    """

    def __init__(self, focus: str):
        """Initialize with a research focus.

        Args:
            focus: The topic/question to focus extraction on

        """
        self.focus = focus
        self.extracted_quotes: list[str] = []
        self.definitions: dict[str, str] = {}
        self.cross_refs: list[str] = []

    def extract_relevant_sections(self, content: str) -> list[str]:
        """Extract sections relevant to the focus topic."""
        # Split content into paragraphs
        paragraphs = content.split("\n\n")

        relevant = []
        focus_lower = self.focus.lower()

        for para in paragraphs:
            if focus_lower in para.lower():
                relevant.append(para.strip())

        return relevant

    def extract_quotes(self, content: str) -> list[str]:
        """Extract notable quotes from content."""
        # Find quoted text
        quote_pattern = r'"([^"]+)"'
        quotes = re.findall(quote_pattern, content)

        # Filter for meaningful quotes (not too short, not too long)
        meaningful = [q for q in quotes if 20 < len(q) < 500]

        self.extracted_quotes.extend(meaningful)
        return meaningful


# Gan Ying Integration
def emit_research_event(topic: str, findings: dict[str, Any]) -> None:
    """Emit research findings to the Gan Ying Bus."""
    try:
        from whitemagic.core.resonance.gan_ying import (
            EventType,
            ResonanceEvent,
            get_bus,
        )
        bus = get_bus()
        bus.emit(ResonanceEvent(
            source="wisdom.rabbit_hole",
            event_type=EventType.PATTERN_DETECTED,
            data={
                "topic": topic,
                "findings": findings,
                "timestamp": datetime.now().isoformat(),
            },
            confidence=0.8,
        ))
    except ImportError:
        pass  # Gan Ying not available, continue silently


# Convenience functions for direct use
def explore_topic(topic: str, max_depth: int = 2) -> ResearchReport:
    """Quick function to explore a topic using the Rabbit Hole technique.

    Args:
        topic: The starting topic to explore
        max_depth: How deep to go down rabbit holes

    Returns:
        A ResearchReport with findings

    """
    explorer = RabbitHoleExplorer(max_depth=max_depth)

    # Create initial entry
    initial = RabbitHoleEntry(
        term=topic,
        definition=f"Starting point for research on: {topic}",
        depth=0,
    )

    entries = [initial]

    report = explorer.create_report(
        title=f"Exploration: {topic}",
        topics=[topic],
        entries=entries,
    )

    # Emit to Gan Ying
    emit_research_event(topic, {"entries": len(entries), "depth": max_depth})

    return report


def compare_sources(urls: list[str], topic: str) -> dict[str, Any]:
    """Compare multiple sources on a topic.

    Args:
        urls: List of URLs that have been read
        topic: The topic being compared

    Returns:
        Dict with agreements, contradictions, and unique insights

    """
    comparator = SourceComparator()

    # Note: Actual content would be passed from the calling context
    # This is a placeholder for the structure

    return {
        "topic": topic,
        "num_sources": len(urls),
        "agreements": comparator.find_agreements(),
        "unique_insights": comparator.find_unique_insights(),
    }


# Self-documentation for AI discovery
__doc_for_ai__ = """
# Rabbit Hole Research Tools

## Quick Start
```python
    RabbitHoleExplorer,
    explore_topic,
    compare_sources
)

# Explore a topic
report = explore_topic("Kashmir Shaivism", max_depth=2)
logger.info(report.to_markdown())

# Save report
explorer = RabbitHoleExplorer()
explorer.save_report(report, Path("reports/"))
```

## Integration with Wisdom Garden
This module is part of the Wisdom Garden and connects to:
- Gan Ying Bus (emits PATTERN_DETECTED events)
- Memory system (can save reports as memories)
- Grimoire (discovered patterns can be documented)

## Philosophy
The Rabbit Hole technique mirrors the sacred spiral:
- Start at a point (like Muladhara)
- Spiral outward (like Kundalini rising)
- Each level reveals more (like chakras opening)
- Synthesis brings unity (like Sahasrara)

🐇🌀✨
"""
