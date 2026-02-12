# mypy: disable-error-code=no-untyped-def
"""Parallel Memory Consolidator - Consolidate 301+ memory files simultaneously

Version: 2.6.6 "Ganapati Day"
Threading: 36K optimal for I/O operations

Philosophy:
    Like the nervous system consolidating experiences during sleep,
    this tool processes all memories in parallel, finding patterns
    and connections that sequential processing would miss.
"""

from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


@dataclass
class ConsolidationResult:
    """Result of consolidating memories."""

    files_processed: int
    patterns_found: list[str]
    connections: list[dict[str, str]]
    tags_discovered: set[str]
    duration_seconds: float
    errors: list[str] = field(default_factory=list)


@dataclass
class MemoryFile:
    """A memory file with extracted metadata."""

    path: Path
    title: str = ""
    tags: list[str] = field(default_factory=list)
    content: str = ""
    word_count: int = 0


class ParallelMemoryConsolidator:
    """Consolidate all memory files in parallel.

    Uses 36K threads for optimal I/O performance on file operations.
    Each memory file is processed independently, then results merged.

    Sacred Numbers:
        - 36K = 12 × 3 × 1000 (Zodiac × Trinity × Thousands)
        - 72K possible for pure I/O
        - 64 threads = hexagram alignment
    """

    def __init__(self, max_workers: int = 64, base_dir: Path | None = None):
        """Initialize consolidator.

        Args:
            max_workers: Thread count (64 = I Ching hexagrams, can go to 36000!)
            base_dir: Base directory for memories

        """
        self.max_workers = max_workers
        self.base_dir = base_dir or Path("memory")
        self.memories: list[MemoryFile] = []

    def _extract_frontmatter(self, content: str) -> dict[str, Any]:
        """Extract YAML frontmatter from markdown with robustness."""
        frontmatter = {}
        if content.startswith("---"):
            parts = content.split("---", 2)
            if len(parts) >= 3:
                yaml_content = parts[1].strip()
                for line in yaml_content.split("\n"):
                    line = line.strip()
                    if not line or line.startswith("#"):
                        continue
                    if ":" in line:
                        try:
                            key, value = line.split(":", 1)
                            # Handle simple list formats and quoted strings
                            raw_value = value.strip().strip('"\'')
                            clean_value: str | list[str] = raw_value
                            if raw_value.startswith("[") and raw_value.endswith("]"):
                                clean_value = [v.strip().strip('"\'') for v in raw_value[1:-1].split(",")]
                            frontmatter[key.strip()] = clean_value
                        except Exception as e:
                            import logging
                            logging.getLogger(__name__).warning(f"Failed to parse frontmatter line: {line} - {e}")
        return frontmatter

    def _process_single_file(self, filepath: Path) -> MemoryFile | None:
        """Process a single memory file (runs in thread)."""
        try:
            content = filepath.read_text(encoding="utf-8")
            frontmatter = self._extract_frontmatter(content)

            # Extract tags
            tags = []
            if "tags" in frontmatter:
                tag_str = frontmatter["tags"]
                tags = [t.strip().strip('"') for t in tag_str.split(",")]

            return MemoryFile(
                path=filepath,
                title=frontmatter.get("title", filepath.stem),
                tags=tags,
                content=content,
                word_count=len(content.split()),
            )
        except Exception:
            return None

    def consolidate(self, memory_dir: Path | None = None) -> ConsolidationResult:
        """Consolidate all memory files in parallel.

        Args:
            memory_dir: Directory to scan (default: self.base_dir)

        Returns:
            ConsolidationResult with patterns, connections, and stats

        """
        import time
        start_time = time.time()

        scan_dir = memory_dir or self.base_dir
        md_files = list(scan_dir.rglob("*.md"))

        errors = []
        self.memories = []

        # Process all files in parallel
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            future_to_file = {
                executor.submit(self._process_single_file, f): f
                for f in md_files
            }

            for future in as_completed(future_to_file):
                filepath = future_to_file[future]
                try:
                    result = future.result()
                    if result:
                        self.memories.append(result)
                except Exception as e:
                    errors.append(f"{filepath}: {e}")

        # Extract patterns and connections
        all_tags = set()
        patterns = []
        connections = []

        for mem in self.memories:
            all_tags.update(mem.tags)

        # Find tag-based connections
        tag_to_memories: dict[str, list[str]] = {}
        for mem in self.memories:
            for tag in mem.tags:
                if tag not in tag_to_memories:
                    tag_to_memories[tag] = []
                tag_to_memories[tag].append(mem.path.name)

        # Tags appearing in multiple files = patterns
        for tag, files in tag_to_memories.items():
            if len(files) > 2:
                patterns.append(f"Tag '{tag}' appears in {len(files)} memories")
                # Create connections between files sharing this tag
                for i, f1 in enumerate(files[:5]):  # Limit to prevent explosion
                    for f2 in files[i+1:6]:
                        connections.append({
                            "from": f1,
                            "to": f2,
                            "relation": f"shared_tag:{tag}",
                        })

        duration = time.time() - start_time

        return ConsolidationResult(
            files_processed=len(self.memories),
            patterns_found=patterns[:20],  # Top 20 patterns
            connections=connections[:50],   # Top 50 connections
            tags_discovered=all_tags,
            duration_seconds=duration,
            errors=errors,
        )

    def get_stats(self) -> dict[str, Any]:
        """Get statistics about consolidated memories."""
        if not self.memories:
            return {"error": "No memories consolidated yet"}

        total_words = sum(m.word_count for m in self.memories)
        all_tags = set()
        for m in self.memories:
            all_tags.update(m.tags)

        return {
            "total_files": len(self.memories),
            "total_words": total_words,
            "unique_tags": len(all_tags),
            "avg_words_per_file": total_words // len(self.memories) if self.memories else 0,
            "top_tags": list(all_tags)[:10],
        }


def consolidate_all_memories(memory_dir: str = "memory", workers: int = 64) -> ConsolidationResult:
    """Convenience function to consolidate all memories.

    Args:
        memory_dir: Path to memory directory
        workers: Number of parallel workers (64 default, 36000 for maximum I/O)

    Returns:
        ConsolidationResult with full analysis

    """
    consolidator = ParallelMemoryConsolidator(max_workers=workers, base_dir=Path(memory_dir))
    return consolidator.consolidate()


# Gan Ying Integration
def emit_consolidation_event(result: ConsolidationResult):
    """Emit consolidation results to Gan Ying Bus."""
    try:
        from datetime import datetime

        from whitemagic.core.resonance import EventType, ResonanceEvent, get_bus

        bus = get_bus()
        bus.emit(ResonanceEvent(
            source="parallel.memory_consolidator",
            event_type=EventType.MEMORY_CONSOLIDATED,
            data={
                "files_processed": result.files_processed,
                "patterns_count": len(result.patterns_found),
                "connections_count": len(result.connections),
                "duration": result.duration_seconds,
            },
            timestamp=datetime.now(),
            confidence=0.95,
        ))
    except ImportError:
        pass  # Gan Ying not available
