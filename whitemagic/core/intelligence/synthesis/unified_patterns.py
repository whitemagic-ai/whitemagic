"""UnifiedPatternAPI - Single interface to all pattern engines.

Provides unified access to search and correlate patterns across:
- Core Pattern Engine (solutions, anti-patterns)
- Holographic Pattern Engine (density, flows)
- Edge Pattern Learning (query rules)
- Cascade Patterns (tool chains)
"""

from dataclasses import dataclass, field
from enum import Enum
from importlib.util import find_spec
from typing import Any


class PatternType(Enum):
    SOLUTION = "solution"
    ANTI_PATTERN = "anti_pattern"
    HEURISTIC = "heuristic"
    OPTIMIZATION = "optimization"
    DENSITY = "density"
    FLOW = "flow"
    QUERY_RULE = "query_rule"
    TOOL_CHAIN = "tool_chain"

@dataclass
class UnifiedPattern:
    """A pattern from any engine."""

    id: str
    pattern_type: PatternType
    title: str
    description: str
    source_engine: str
    confidence: float
    metadata: dict[str, Any] = field(default_factory=dict)

class UnifiedPatternAPI:
    """Unified interface to all pattern engines."""

    def __init__(self, db_path: str | None = None) -> None:
        from pathlib import Path

        from whitemagic.config.paths import DB_PATH
        self.db_path = str(Path(db_path)) if db_path else str(DB_PATH)
        self._engines: dict[str, Any] = {}

    def _load_engines(self) -> Any:
        """Lazy load available engines."""
        if self._engines:
            return

        # Try to load each engine with correct paths
        try:
            from whitemagic.core.memory.pattern_engine import PatternEngine
            self._engines["core"] = PatternEngine()
        except ImportError:
            pass

        try:
            from whitemagic.core.intelligence.hologram.patterns import (
                HolographicPatternEngine,
            )
            self._engines["holographic"] = HolographicPatternEngine()
        except ImportError:
            pass

        # Load Rust similarity for cross-correlation
        self._rust_available = find_spec("whitemagic_rs") is not None

    def search(self, query: str | None = None,
               engines: list[str] | None = None,
               pattern_types: list[PatternType] | None = None,
               min_confidence: float = 0.5) -> list[UnifiedPattern]:
        """Search patterns across all engines."""
        self._load_engines()

        results = []
        target_engines = engines or list(self._engines.keys())

        for engine_name in target_engines:
            if engine_name not in self._engines:
                continue

            engine = self._engines[engine_name]

            try:
                if engine_name == "core":
                    patterns = self._search_core(engine, query, min_confidence)
                elif engine_name == "holographic":
                    patterns = self._search_holographic(engine, query, min_confidence)
                elif engine_name == "edge":
                    patterns = self._search_edge(engine, query, min_confidence)
                else:
                    patterns = []

                results.extend(patterns)
            except Exception:
                continue

        # Filter by pattern type if specified
        if pattern_types:
            results = [p for p in results if p.pattern_type in pattern_types]

        # Sort by confidence
        results.sort(key=lambda p: -p.confidence)

        return results

    def _search_core(self, engine: Any, query: str | None,
                     min_confidence: float) -> list[UnifiedPattern]:
        """Search core pattern engine."""
        patterns = []

        try:
            report = engine.extract_patterns(min_confidence=min_confidence)

            for p in report.solutions:
                patterns.append(UnifiedPattern(
                    id=f"core_sol_{hash(p.title)}",
                    pattern_type=PatternType.SOLUTION,
                    title=p.title[:100] if p.title else "Untitled",
                    description=p.description[:200] if p.description else "",
                    source_engine="core",
                    confidence=p.confidence,
                    metadata={"frequency": p.frequency, "examples": p.examples[:3] if p.examples else []},
                ))

            for p in report.anti_patterns:
                patterns.append(UnifiedPattern(
                    id=f"core_anti_{hash(p.title)}",
                    pattern_type=PatternType.ANTI_PATTERN,
                    title=p.title[:100] if p.title else "Untitled",
                    description=p.description[:200] if p.description else "",
                    source_engine="core",
                    confidence=p.confidence,
                    metadata={"frequency": p.frequency},
                ))

            for p in report.heuristics:
                patterns.append(UnifiedPattern(
                    id=f"core_heur_{hash(p.title)}",
                    pattern_type=PatternType.HEURISTIC,
                    title=p.title[:100] if p.title else "Untitled",
                    description=p.description[:200] if p.description else "",
                    source_engine="core",
                    confidence=p.confidence,
                    metadata={"frequency": p.frequency},
                ))

            for p in report.optimizations:
                patterns.append(UnifiedPattern(
                    id=f"core_opt_{hash(p.title)}",
                    pattern_type=PatternType.OPTIMIZATION,
                    title=p.title[:100] if p.title else "Untitled",
                    description=p.description[:200] if p.description else "",
                    source_engine="core",
                    confidence=p.confidence,
                    metadata={"frequency": p.frequency},
                ))
        except Exception:
            pass

        return patterns

    def _search_holographic(self, engine: Any, query: str | None,
                            min_confidence: float) -> list[UnifiedPattern]:
        """Search holographic pattern engine."""
        patterns = []

        try:
            # Get patterns from analyze() which returns all pattern types
            analysis = engine.analyze()
            holo_patterns = analysis.get("patterns", [])

            for p in holo_patterns:
                conf = getattr(p, "confidence", 0.5)
                if min_confidence <= conf:
                    ptype = getattr(p, "pattern_type", "unknown")
                    if ptype == "density":
                        pattern_type = PatternType.DENSITY
                    elif ptype in ("temporal", "flow"):
                        pattern_type = PatternType.FLOW
                    else:
                        pattern_type = PatternType.DENSITY

                    patterns.append(UnifiedPattern(
                        id=f"holo_{ptype}_{hash(getattr(p, 'description', ''))}",
                        pattern_type=pattern_type,
                        title=getattr(p, "description", "Holographic Pattern")[:100],
                        description=f"Location: {getattr(p, 'location', 'unknown')}",
                        source_engine="holographic",
                        confidence=conf,
                        metadata={
                            "location": getattr(p, "location", None),
                            "evidence": getattr(p, "evidence", [])[:3],
                        },
                    ))
        except Exception:
            pass

        return patterns

    def _search_edge(self, engine: Any, query: str | None,
                     min_confidence: float) -> list[UnifiedPattern]:
        """Search edge pattern learning."""
        patterns = []

        try:
            rules = engine.get_active_rules()

            for rule in rules:
                if min_confidence <= rule.get("confidence", 0.5):
                    patterns.append(UnifiedPattern(
                        id=f"edge_{hash(rule.get('pattern', ''))}",
                        pattern_type=PatternType.QUERY_RULE,
                        title=rule.get("pattern", "Edge Rule"),
                        description=rule.get("action", ""),
                        source_engine="edge",
                        confidence=rule.get("confidence", 0.5),
                        metadata=rule,
                    ))
        except Exception:
            pass

        return patterns

    def find_correlations(self) -> list[dict[str, Any]]:
        """Find correlations between patterns from different engines."""
        all_patterns = self.search(min_confidence=0.3)
        correlations = []

        # Group by similar titles/descriptions
        for i, p1 in enumerate(all_patterns):
            for p2 in all_patterns[i+1:]:
                if p1.source_engine != p2.source_engine:
                    similarity = self._text_similarity(
                        f"{p1.title} {p1.description}",
                        f"{p2.title} {p2.description}",
                    )

                    if similarity > 0.3:
                        correlations.append({
                            "pattern1_id": p1.id,
                            "pattern2_id": p2.id,
                            "pattern1_title": p1.title[:50],
                            "pattern2_title": p2.title[:50],
                            "engines": [p1.source_engine, p2.source_engine],
                            "similarity": similarity,
                            "combined_confidence": (p1.confidence + p2.confidence) / 2,
                        })

        return sorted(correlations, key=lambda c: -c["similarity"])

    def find_thematic_overlaps(self) -> list[dict[str, Any]]:
        """Find thematic overlaps by analyzing shared evidence/memory references."""
        self._load_engines()
        overlaps = []

        # Get holographic patterns with their evidence
        holo_evidence = {}
        if "holographic" in self._engines:
            try:
                analysis = self._engines["holographic"].analyze()
                for p in analysis.get("patterns", []):
                    evidence = getattr(p, "evidence", [])
                    holo_evidence[getattr(p, "description", "")[:50]] = {
                        "pattern": p,
                        "evidence_titles": evidence[:10],
                    }
            except Exception:
                pass

        # Get core patterns
        core_titles = {}
        if "core" in self._engines:
            try:
                report = self._engines["core"].extract_patterns(min_confidence=0.3)
                for p in report.solutions[:100]:
                    core_titles[p.title[:50]] = p
            except Exception:
                pass

        # Find overlaps where holographic evidence contains core pattern titles
        for holo_desc, holo_data in holo_evidence.items():
            for core_title, core_pattern in core_titles.items():
                # Check if any evidence title contains core pattern keywords
                core_words = set(core_title.lower().split()[:5])
                for evidence_title in holo_data["evidence_titles"]:
                    evidence_words = set(evidence_title.lower().split()[:10])
                    overlap = core_words & evidence_words
                    if len(overlap) >= 2:
                        overlaps.append({
                            "holographic_pattern": holo_desc,
                            "core_pattern": core_title,
                            "shared_terms": list(overlap),
                            "connection_strength": len(overlap) / len(core_words) if core_words else 0,
                        })
                        break

        return sorted(overlaps, key=lambda x: -x["connection_strength"])

    def _text_similarity(self, text1: str, text2: str) -> float:
        """Calculate similarity using Rust if available, else word overlap."""
        if not text1 or not text2:
            return 0.0

        # Use Rust-accelerated similarity if available
        if getattr(self, "_rust_available", False):
            try:
                import whitemagic_rs
                return float(whitemagic_rs.fast_similarity(text1, text2))
            except Exception:
                pass

        # Fallback to word overlap
        words1 = set(text1.lower().split())
        words2 = set(text2.lower().split())

        if not words1 or not words2:
            return 0.0

        intersection = words1 & words2
        union = words1 | words2

        return len(intersection) / len(union)

    def get_stats(self) -> dict[str, Any]:
        """Get statistics about all patterns."""
        self._load_engines()

        all_patterns = self.search(min_confidence=0.0)

        by_engine: dict[str, int] = {}
        by_type: dict[str, int] = {}

        for p in all_patterns:
            by_engine[p.source_engine] = by_engine.get(p.source_engine, 0) + 1
            by_type[p.pattern_type.value] = by_type.get(p.pattern_type.value, 0) + 1

        return {
            "total_patterns": len(all_patterns),
            "by_engine": by_engine,
            "by_type": by_type,
            "engines_loaded": list(self._engines.keys()),
            "avg_confidence": sum(p.confidence for p in all_patterns) / len(all_patterns) if all_patterns else 0,
        }

    def search_tag_cooccurrence(self, min_count: int = 5, limit: int = 20) -> list[dict[str, Any]]:
        """Search for frequently co-occurring tags in the database."""
        import sqlite3
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row
        cur = conn.cursor()

        try:
            cur.execute("""
                SELECT t1.tag as tag1, t2.tag as tag2, COUNT(*) as co_count
                FROM tags t1
                JOIN tags t2 ON t1.memory_id = t2.memory_id AND t1.tag < t2.tag
                GROUP BY t1.tag, t2.tag
                HAVING co_count >= ?
                ORDER BY co_count DESC
                LIMIT ?
            """, (min_count, limit))
            return [dict(row) for row in cur.fetchall()]
        except Exception:
            return []
        finally:
            conn.close()

# Global instance
_pattern_api = None

def get_pattern_api() -> UnifiedPatternAPI:
    global _pattern_api
    if _pattern_api is None:
        _pattern_api = UnifiedPatternAPI()
    return _pattern_api
