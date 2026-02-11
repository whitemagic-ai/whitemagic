"""Evolution System - Week 4 of v2.3.1.

Autonomous improvement proposals based on pattern analysis.
The system examines its own performance and suggests enhancements.
"""

import json
from dataclasses import asdict, dataclass
from datetime import datetime
from functools import lru_cache
from pathlib import Path
from typing import TextIO

from whitemagic.core.memory.pattern_engine import get_engine


@dataclass
class EvolutionProposal:
    """Represents a self-improvement proposal."""

    proposal_id: str
    category: str  # performance, reliability, usability, architecture
    title: str
    description: str
    rationale: str  # Why this improvement matters
    confidence: float  # 0.0-1.0
    priority: str  # critical, high, medium, low
    estimated_impact: str  # How much improvement expected
    implementation_notes: str
    related_patterns: list[str] | None = None

    def __post_init__(self) -> None:
        if self.related_patterns is None:
            self.related_patterns = []


@dataclass
class EvolutionReport:
    """Report of evolution analysis."""

    timestamp: str
    patterns_analyzed: int
    proposals_generated: int
    proposals: list[EvolutionProposal]
    meta_insights: list[str]  # High-level observations

    @lru_cache(maxsize=128)
    def to_dict(self) -> dict:
        return {
            "timestamp": self.timestamp,
            "patterns_analyzed": self.patterns_analyzed,
            "proposals_generated": self.proposals_generated,
            "proposals": [asdict(p) for p in self.proposals],
            "meta_insights": self.meta_insights,
        }


class EvolutionEngine:
    """Generates autonomous improvement proposals."""

    def __init__(self, base_dir: Path | None = None):
        self.base_dir = base_dir or Path(".")
        self.evolution_dir = self.base_dir / "memory" / "evolution"
        self.evolution_dir.mkdir(parents=True, exist_ok=True)
        self.meta_dir = self.base_dir / "memory" / "meta"

    @lru_cache(maxsize=128)
    def get_evolution_patterns(self) -> list[dict]:
        """Get recent evolution patterns (for pattern discovery)."""
        # Return recent evolution proposals as patterns
        evolution_files = sorted(self.evolution_dir.glob("*.json"), reverse=True)
        patterns = []

        for evo_file in evolution_files[:10]:  # Last 10
            try:
                with open(evo_file) as f:
                    data = json.load(f)
                    if "proposals" in data:
                        for proposal in data["proposals"]:
                            patterns.append({
                                "type": "evolution_proposal",
                                "title": proposal.get("title", ""),
                                "category": proposal.get("category", ""),
                                "confidence": proposal.get("confidence", 0.5),
                            })
            except Exception:
                continue

        return patterns

    @lru_cache(maxsize=128)
    def analyze_and_propose(self, min_confidence: float = 0.7) -> EvolutionReport:
        """Analyze patterns and generate improvement proposals.

        Args:
            min_confidence: Minimum confidence for proposals

        Returns:
            EvolutionReport with proposals

        """
        # Get pattern engine and extract patterns
        engine = get_engine()
        pattern_report = engine.extract_patterns(min_confidence=0.6)

        proposals = []
        meta_insights = []

        # Analyze anti-patterns for reliability improvements
        if pattern_report.anti_patterns:
            proposals.extend(self._analyze_anti_patterns(pattern_report.anti_patterns))
            meta_insights.append(
                f"Found {len(pattern_report.anti_patterns)} anti-patterns - "
                "opportunity for automated guards",
            )

        # Analyze optimizations for performance improvements
        if pattern_report.optimizations:
            proposals.extend(self._analyze_optimizations(pattern_report.optimizations))
            meta_insights.append(
                f"Found {len(pattern_report.optimizations)} optimization patterns - "
                "can codify best practices",
            )

        # Analyze solutions for reusability
        if pattern_report.solutions:
            proposals.extend(self._analyze_solutions(pattern_report.solutions))
            meta_insights.append(
                f"Found {len(pattern_report.solutions)} solution patterns - "
                "candidates for library functions",
            )

        # Analyze heuristics for automation
        if pattern_report.heuristics:
            proposals.extend(self._analyze_heuristics(pattern_report.heuristics))
            meta_insights.append(
                f"Found {len(pattern_report.heuristics)} heuristic patterns - "
                "can create decision trees",
            )

        # Filter by confidence
        proposals = [p for p in proposals if p.confidence >= min_confidence]

        # Sort by priority
        priority_order = {"critical": 0, "high": 1, "medium": 2, "low": 3}
        proposals.sort(key=lambda p: priority_order.get(p.priority, 4))

        return EvolutionReport(
            timestamp=datetime.now().isoformat(),
            patterns_analyzed=pattern_report.patterns_found,
            proposals_generated=len(proposals),
            proposals=proposals,
            meta_insights=meta_insights,
        )

    def _analyze_anti_patterns(self, anti_patterns: list) -> list[EvolutionProposal]:
        """Generate proposals from anti-patterns."""
        proposals = []

        # Group similar anti-patterns
        error_patterns = [p for p in anti_patterns if "error" in p.title.lower()]
        bug_patterns = [p for p in anti_patterns if "bug" in p.title.lower()]

        if len(error_patterns) > 3:
            proposals.append(EvolutionProposal(
                proposal_id=f"AP-{datetime.now().strftime('%Y%m%d-%H%M%S')}-001",
                category="reliability",
                title="Implement automated error detection",
                description="Create pre-commit hooks to detect common error patterns",
                rationale=f"Found {len(error_patterns)} error-related anti-patterns",
                confidence=0.85,
                priority="high",
                estimated_impact="30-50% reduction in runtime errors",
                implementation_notes="Use pattern matching in CI/CD pipeline",
                related_patterns=[p.title for p in error_patterns[:3]],
            ))

        if len(bug_patterns) > 2:
            proposals.append(EvolutionProposal(
                proposal_id=f"AP-{datetime.now().strftime('%Y%m%d-%H%M%S')}-002",
                category="reliability",
                title="Expand test coverage for bug-prone areas",
                description="Add integration tests for areas with repeated bugs",
                rationale=f"Found {len(bug_patterns)} bug-related patterns",
                confidence=0.80,
                priority="high",
                estimated_impact="20-30% reduction in bug reports",
                implementation_notes="Focus on edge cases and error handling",
                related_patterns=[p.title for p in bug_patterns[:3]],
            ))

        return proposals

    def _analyze_optimizations(self, optimizations: list) -> list[EvolutionProposal]:
        """Generate proposals from optimization patterns."""
        proposals = []

        performance_patterns = [p for p in optimizations if "faster" in p.title.lower() or "performance" in p.title.lower()]

        if len(performance_patterns) > 2:
            proposals.append(EvolutionProposal(
                proposal_id=f"OPT-{datetime.now().strftime('%Y%m%d-%H%M%S')}-001",
                category="performance",
                title="Create performance optimization guide",
                description="Document proven optimization strategies",
                rationale=f"Found {len(performance_patterns)} optimization patterns",
                confidence=0.90,
                priority="medium",
                estimated_impact="Knowledge sharing, faster development",
                implementation_notes="Add to docs/ directory as best practices",
                related_patterns=[p.title for p in performance_patterns[:5]],
            ))

        return proposals

    def _analyze_solutions(self, solutions: list) -> list[EvolutionProposal]:
        """Generate proposals from solution patterns."""
        proposals = []

        if len(solutions) > 10:
            proposals.append(EvolutionProposal(
                proposal_id=f"SOL-{datetime.now().strftime('%Y%m%d-%H%M%S')}-001",
                category="usability",
                title="Create solution pattern library",
                description="Extract common solutions into reusable utilities",
                rationale=f"Found {len(solutions)} solution patterns",
                confidence=0.75,
                priority="medium",
                estimated_impact="Faster problem resolution",
                implementation_notes="Create whitemagic.utils.patterns module",
                related_patterns=[p.title for p in solutions[:5]],
            ))

        return proposals

    def _analyze_heuristics(self, heuristics: list) -> list[EvolutionProposal]:
        """Generate proposals from heuristic patterns."""
        proposals = []

        if len(heuristics) > 5:
            proposals.append(EvolutionProposal(
                proposal_id=f"HEU-{datetime.now().strftime('%Y%m%d-%H%M%S')}-001",
                category="architecture",
                title="Implement decision automation system",
                description="Create rules engine based on heuristic patterns",
                rationale=f"Found {len(heuristics)} heuristic patterns",
                confidence=0.70,
                priority="low",
                estimated_impact="Automated decision making",
                implementation_notes="Use pattern matching and confidence scoring",
                related_patterns=[p.title for p in heuristics[:5]],
            ))

        return proposals

    def save_report(self, report: EvolutionReport) -> Path:
        """Save evolution report."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Save JSON
        json_file = self.evolution_dir / f"{timestamp}_evolution.json"
        with open(json_file, "w") as f:
            json.dump(report.to_dict(), f, indent=2)

        # Save Markdown
        md_file = self.evolution_dir / f"{timestamp}_evolution.md"
        self._create_markdown_report(report, md_file)

        return json_file

    def _create_markdown_report(self, report: EvolutionReport, filepath: Path) -> None:
        """Create human-readable markdown report."""
        with open(filepath, "w") as f:
            f.write("# 🧬 Evolution Report\n\n")
            f.write(f"**Generated**: {report.timestamp}\n\n")
            f.write(f"**Patterns Analyzed**: {report.patterns_analyzed}\n")
            f.write(f"**Proposals Generated**: {report.proposals_generated}\n\n")

            f.write("## 💡 Meta-Insights\n\n")
            for insight in report.meta_insights:
                f.write(f"- {insight}\n")
            f.write("\n")

            # Group by priority
            critical = [p for p in report.proposals if p.priority == "critical"]
            high = [p for p in report.proposals if p.priority == "high"]
            medium = [p for p in report.proposals if p.priority == "medium"]
            low = [p for p in report.proposals if p.priority == "low"]

            if critical:
                f.write(f"## 🔴 Critical Priority ({len(critical)})\n\n")
                for p in critical:
                    self._write_proposal(f, p)

            if high:
                f.write(f"## 🟠 High Priority ({len(high)})\n\n")
                for p in high:
                    self._write_proposal(f, p)

            if medium:
                f.write(f"## 🟡 Medium Priority ({len(medium)})\n\n")
                for p in medium:
                    self._write_proposal(f, p)

            if low:
                f.write(f"## 🟢 Low Priority ({len(low)})\n\n")
                for p in low:
                    self._write_proposal(f, p)

    def _write_proposal(self, f: TextIO, proposal: EvolutionProposal) -> None:
        """Write a single proposal to markdown."""
        f.write(f"### {proposal.title}\n\n")
        f.write(f"**ID**: `{proposal.proposal_id}`  \n")
        f.write(f"**Category**: {proposal.category}  \n")
        f.write(f"**Confidence**: {proposal.confidence:.0%}  \n")
        f.write(f"**Estimated Impact**: {proposal.estimated_impact}\n\n")
        f.write(f"**Description**: {proposal.description}\n\n")
        f.write(f"**Rationale**: {proposal.rationale}\n\n")
        f.write(f"**Implementation**: {proposal.implementation_notes}\n\n")
        if proposal.related_patterns:
            f.write("**Related Patterns**:\n")
            for pattern in proposal.related_patterns[:3]:
                f.write(f"- {pattern}\n")
            f.write("\n")
        f.write("---\n\n")


# Global instance
_evolution_engine = None

def get_evolution_engine() -> EvolutionEngine:
    """Get global evolution engine instance."""
    global _evolution_engine
    if _evolution_engine is None:
        _evolution_engine = EvolutionEngine()
    assert _evolution_engine is not None
    return _evolution_engine
