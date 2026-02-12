"""Pattern Extraction Engine - Week 3 of v2.3.1.

Analyzes long-term memories to extract:
- Solutions (when X happens, do Y)
- Anti-patterns (avoid Z because...)
- Heuristics (if condition, then action)
- Optimizations (proven approaches)
"""

import json
import logging
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path

from whitemagic.core.memory.unified import get_unified_memory

logger = logging.getLogger(__name__)

try:
    from whitemagic.core.resonance.gan_ying import EventType
    # Ensure EventType is used to satisfy linter
    _ = EventType
    RESONANCE_AVAILABLE = True
except ImportError:
    RESONANCE_AVAILABLE = False

try:
    import whitemagic_rs
    # Ensure whitemagic_rs is used to satisfy linter
    _rs_ref = whitemagic_rs  # noqa: F841
    RUST_AVAILABLE = True
except ImportError:
    RUST_AVAILABLE = False


@dataclass
class Pattern:
    """Represents an extracted pattern."""

    pattern_type: str  # solution, anti_pattern, heuristic, optimization
    title: str
    description: str
    confidence: float
    frequency: int = 1
    examples: list[str] | None = None

    def __post_init__(self) -> None:
        if self.examples is None:
            self.examples = []


@dataclass
class PatternReport:
    """Report of pattern extraction results."""

    total_memories: int
    patterns_found: int
    solutions: list[Pattern]
    anti_patterns: list[Pattern]
    heuristics: list[Pattern]
    optimizations: list[Pattern]
    duration_seconds: float

    def to_dict(self) -> dict:
        """Convert to dictionary."""
        return {
            "total_memories": self.total_memories,
            "patterns_found": self.patterns_found,
            "solutions": [asdict(p) for p in self.solutions],
            "anti_patterns": [asdict(p) for p in self.anti_patterns],
            "heuristics": [asdict(p) for p in self.heuristics],
            "optimizations": [asdict(p) for p in self.optimizations],
            "duration_seconds": self.duration_seconds,
        }


class PatternEngine:
    """Extracts patterns from long-term memories."""

    def __init__(self, base_dir: Path | None = None):
        self.base_dir = base_dir or Path(".")
        self.memory_dir = self.base_dir / "memory"
        self.long_term_dir = self.memory_dir / "long_term"
        self.short_term_dir = self.memory_dir / "short_term"
        self.meta_dir = self.memory_dir / "meta"
        self.meta_dir.mkdir(parents=True, exist_ok=True)

    def extract_patterns(self, min_confidence: float = 0.6, use_rust: bool = True) -> PatternReport:
        """Extract patterns from ALL memories (via Unified Memory)."""
        if use_rust:
            try:
                return self._extract_rust(min_confidence)
            except Exception as e:
                logger.warning(f"Rust pattern extraction failed, falling back to Python: {e}")

        return self._extract_python(min_confidence)

    def _extract_rust(self, min_confidence: float) -> PatternReport:
        """Use Rust extension for high-performance extraction on DB content."""
        import whitemagic_rs

        # 1. Fetch content from Unified Memory (DB)
        memories = get_unified_memory().fetch_all_contents(limit=5000)

        if not memories:
            logger.warning("No memories found in Unified Memory for pattern extraction")
            return PatternReport(0, 0, [], [], [], [], 0.0)

        # 2. Call Rust extension with list of strings
        # Signature: (total, found, solutions, anti, heuristics, opts, duration)
        total, found, solutions, anti, heuristics, opts, duration = \
            whitemagic_rs.extract_patterns_from_content(memories, min_confidence)

        # Helper to convert string lists to Pattern objects
        def to_patterns(type_name: str, descriptions: list[str]) -> list[Pattern]:
            return [
                Pattern(
                    pattern_type=type_name,
                    title=desc[:80] + "..." if len(desc) > 80 else desc,
                    description=desc,
                    confidence=min_confidence + 0.1,  # Synthetic confidence
                    examples=[desc],
                )
                for desc in descriptions
            ]

        return PatternReport(
            total_memories=total,
            patterns_found=found,
            solutions=to_patterns("solution", solutions),
            anti_patterns=to_patterns("anti_pattern", anti),
            heuristics=to_patterns("heuristic", heuristics),
            optimizations=to_patterns("optimization", opts),
            duration_seconds=duration,
        )

    def _extract_python(self, min_confidence: float) -> PatternReport:
        """Fallback Python implementation using DB content."""
        import time
        start_time = time.time()

        # 1. Fetch content from Unified Memory
        memories = get_unified_memory().fetch_all_contents(limit=5000)

        solutions = []
        anti_patterns = []
        heuristics: list[Pattern] = []
        optimizations: list[Pattern] = []

        for content in memories:
            # Simple keyword matching fallback
            if "solution" in content.lower() or "fix" in content.lower():
                solutions.append(Pattern("solution", "Found solution", content[:100], 0.6))
            if "avoid" in content.lower() or "never" in content.lower():
                anti_patterns.append(Pattern("anti_pattern", "Found anti-pattern", content[:100], 0.6))

        duration = time.time() - start_time
        total_patterns = len(solutions) + len(anti_patterns) + len(heuristics) + len(optimizations)

        return PatternReport(
            total_memories=len(memories),
            patterns_found=total_patterns,
            solutions=solutions,
            anti_patterns=anti_patterns,
            heuristics=heuristics,
            optimizations=optimizations,
            duration_seconds=duration,
        )

    def save_patterns(self, report: PatternReport) -> Path:
        """Save patterns to meta-memory file."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{timestamp}_patterns.json"
        filepath = self.meta_dir / filename

        # Save as JSON
        with open(filepath, "w") as f:
            json.dump(report.to_dict(), f, indent=2)

        # Also create markdown summary
        md_file = self.meta_dir / f"{timestamp}_patterns.md"
        self._create_markdown_summary(report, md_file)

        return filepath

    def _create_markdown_summary(self, report: PatternReport, filepath: Path) -> None:
        """Create human-readable markdown summary."""
        with open(filepath, "w") as f:
            f.write("# Pattern Extraction Report\n\n")
            f.write(f"**Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            f.write(f"**Memories Analyzed**: {report.total_memories}\n")
            f.write(f"**Patterns Found**: {report.patterns_found}\n")
            f.write(f"**Duration**: {report.duration_seconds:.3f}s\n\n")

            f.write(f"## 💡 Solutions ({len(report.solutions)})\n\n")
            for p in report.solutions[:10]:  # Top 10
                f.write(f"- **{p.title}** (confidence: {p.confidence:.2f})\n")
            f.write("\n")

            f.write(f"## ⚠️ Anti-Patterns ({len(report.anti_patterns)})\n\n")
            for p in report.anti_patterns[:10]:
                f.write(f"- **{p.title}** (confidence: {p.confidence:.2f})\n")
            f.write("\n")

            f.write(f"## 🎯 Heuristics ({len(report.heuristics)})\n\n")
            for p in report.heuristics[:10]:
                f.write(f"- **{p.title}** (confidence: {p.confidence:.2f})\n")
            f.write("\n")

            f.write(f"## ⚡ Optimizations ({len(report.optimizations)})\n\n")
            for p in report.optimizations[:10]:
                f.write(f"- **{p.title}** (confidence: {p.confidence:.2f})\n")
            f.write("\n")


# Global instance
_engine = None

def get_engine() -> PatternEngine:
    """Get global pattern engine instance."""
    global _engine
    if _engine is None:
        _engine = PatternEngine()
    return _engine
