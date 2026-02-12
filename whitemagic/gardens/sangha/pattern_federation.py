"""Pattern Federation - Distributed pattern library across sessions."""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.utils.core import parse_datetime

from .workspace import get_sangha_base_dir

logger = logging.getLogger(__name__)


@dataclass
class FederatedPattern:
    """Pattern shared across collective."""

    pattern_id: str
    name: str
    problem: str
    solution: str
    confidence: float
    contributors: list[str]  # Session IDs that confirmed
    success_count: int
    failure_count: int
    tags: list[str]
    created_at: datetime
    last_used: datetime


class PatternFederation:
    """Manages distributed pattern library.

    Philosophy: Patterns discovered by one become wisdom for all.
    Confidence increases as multiple sessions validate.
    """

    def __init__(self, base_dir: Path | None = None) -> None:
        self.base_dir = base_dir if base_dir is not None else get_sangha_base_dir()
        self.patterns_dir = self.base_dir / "memory" / "collective" / "patterns"
        self.patterns_dir.mkdir(parents=True, exist_ok=True)
        self.index_file = self.patterns_dir / "pattern_index.json"
        self.bus: Any | None = None
        self._connect_to_gan_ying()

    def _connect_to_gan_ying(self) -> None:
        """Connect to Gan Ying Bus."""
        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            self.bus = get_bus()
            logger.info("🎵 Pattern Federation connected to Gan Ying Bus")
        except ImportError:
            pass

    def contribute_pattern(
        self,
        session_id: str,
        name: str,
        problem: str,
        solution: str,
        confidence: float = 0.8,
        tags: list[str] | None = None,
    ) -> str:
        """Contribute pattern to federation.

        Args:
            session_id: Contributing session
            name: Pattern name
            problem: Problem it solves
            solution: Solution description
            confidence: Initial confidence
            tags: Optional tags

        Returns:
            Pattern ID

        """
        pattern_id = f"pattern_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{session_id[:8]}"

        pattern = FederatedPattern(
            pattern_id=pattern_id,
            name=name,
            problem=problem,
            solution=solution,
            confidence=confidence,
            contributors=[session_id],
            success_count=0,
            failure_count=0,
            tags=tags or [],
            created_at=datetime.now(),
            last_used=datetime.now(),
        )

        self._save_pattern(pattern)
        self._update_index(pattern)

        # Emit to Gan Ying
        if self.bus:
            try:
                from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent
                self.bus.emit(ResonanceEvent(
                    source="pattern_federation",
                    event_type=EventType.PATTERN_DETECTED,
                    data={
                        "pattern_id": pattern_id,
                        "name": name,
                        "confidence": confidence,
                        "federated": True,
                    },
                    confidence=confidence,
                ))
            except Exception:
                pass

        logger.info(f"📚 Pattern '{name}' added to federation")
        return pattern_id

    def validate_pattern(
        self,
        session_id: str,
        pattern_id: str,
        success: bool,
    ) -> None:
        """Validate pattern with success/failure.

        Args:
            session_id: Validating session
            pattern_id: Pattern to validate
            success: True if pattern worked, False otherwise

        """
        pattern = self._load_pattern(pattern_id)
        if not pattern:
            return

        if success:
            pattern.success_count += 1
        else:
            pattern.failure_count += 1

        # Add as contributor if not already
        if session_id not in pattern.contributors:
            pattern.contributors.append(session_id)

        # Recalculate confidence
        total = pattern.success_count + pattern.failure_count
        if total > 0:
            success_rate = pattern.success_count / total
            # Weighted average: original confidence + empirical success rate
            pattern.confidence = (pattern.confidence + success_rate * len(pattern.contributors)) / (1 + len(pattern.contributors))

        pattern.last_used = datetime.now()
        self._save_pattern(pattern)
        self._update_index(pattern)

        status = "✅ success" if success else "❌ failure"
        logger.info(f"📊 Pattern '{pattern.name}' validated: {status} (confidence: {pattern.confidence:.2f})")

    def search_patterns(
        self,
        query: str | None = None,
        tags: list[str] | None = None,
        min_confidence: float = 0.7,
    ) -> list[FederatedPattern]:
        """Search federated patterns.

        Args:
            query: Optional text query
            tags: Optional tag filter
            min_confidence: Minimum confidence

        Returns:
            List of matching patterns

        """
        patterns = self._load_all_patterns()

        # Filter by confidence
        patterns = [p for p in patterns if p.confidence >= min_confidence]

        # Filter by tags
        if tags:
            patterns = [
                p for p in patterns
                if any(tag in p.tags for tag in tags)
            ]

        # Filter by query
        if query:
            query_lower = query.lower()
            patterns = [
                p for p in patterns
                if query_lower in p.name.lower() or
                   query_lower in p.problem.lower() or
                   query_lower in p.solution.lower()
            ]

        # Sort by confidence * usage
        patterns = sorted(
            patterns,
            key=lambda p: p.confidence * (1 + p.success_count),
            reverse=True,
        )

        return patterns

    def get_best_patterns(self, count: int = 10) -> list[FederatedPattern]:
        """Get top N patterns by confidence and validation.

        Args:
            count: Number of patterns to return

        Returns:
            List of best patterns

        """
        patterns = self._load_all_patterns()
        patterns = sorted(
            patterns,
            key=lambda p: p.confidence * (1 + p.success_count) * len(p.contributors),
            reverse=True,
        )
        return patterns[:count]

    def _save_pattern(self, pattern: FederatedPattern) -> None:
        """Save pattern to disk."""
        pattern_file = self.patterns_dir / f"{pattern.pattern_id}.json"
        data = {
            "pattern_id": pattern.pattern_id,
            "name": pattern.name,
            "problem": pattern.problem,
            "solution": pattern.solution,
            "confidence": pattern.confidence,
            "contributors": pattern.contributors,
            "success_count": pattern.success_count,
            "failure_count": pattern.failure_count,
            "tags": pattern.tags,
            "created_at": pattern.created_at.isoformat(),
            "last_used": pattern.last_used.isoformat(),
        }
        with open(pattern_file, "w") as f:
            json.dump(data, f, indent=2)

    def _load_pattern(self, pattern_id: str) -> FederatedPattern | None:
        """Load pattern from disk with error handling."""
        pattern_file = self.patterns_dir / f"{pattern_id}.json"
        if not pattern_file.exists():
            return None

        try:
            with open(pattern_file) as f:
                data = json.load(f)
                return FederatedPattern(
                    pattern_id=data["pattern_id"],
                    name=data["name"],
                    problem=data["problem"],
                    solution=data["solution"],
                    confidence=data["confidence"],
                    contributors=data["contributors"],
                    success_count=data["success_count"],
                    failure_count=data["failure_count"],
                    tags=data["tags"],
                    created_at=parse_datetime(data["created_at"]),
                    last_used=parse_datetime(data["last_used"]),
                )
        except json.JSONDecodeError as e:
            logger.error(f"Error loading pattern {pattern_id} from {pattern_file}: {e}")
            return None
        except Exception as e:
            logger.error(f"Unexpected error loading pattern {pattern_id} from {pattern_file}: {e}")
            return None

    def _load_all_patterns(self) -> list[FederatedPattern]:
        """Load all patterns from disk."""
        patterns = []
        for pattern_file in self.patterns_dir.glob("pattern_*.json"):
            pattern_id = pattern_file.stem
            pattern = self._load_pattern(pattern_id)
            if pattern:
                patterns.append(pattern)
        return patterns

    def _update_index(self, pattern: FederatedPattern) -> None:
        """Update pattern index with error handling."""
        index = {}
        if self.index_file.exists():
            try:
                with open(self.index_file) as f:
                    index = json.load(f)
            except json.JSONDecodeError as e:
                logger.error(f"Error loading index from {self.index_file}: {e}")
                index = {}
            except Exception as e:
                logger.error(f"Unexpected error loading index from {self.index_file}: {e}")
                index = {}

        index[pattern.pattern_id] = {
            "name": pattern.name,
            "confidence": pattern.confidence,
            "contributors": len(pattern.contributors),
            "validations": pattern.success_count + pattern.failure_count,
            "tags": pattern.tags,
        }

        with open(self.index_file, "w") as f:
            json.dump(index, f, indent=2)


# Global instance
_federation: PatternFederation | None = None


def get_federation() -> PatternFederation:
    """Get global pattern federation instance."""
    global _federation
    if _federation is None:
        _federation = PatternFederation()
    return _federation


def federate_patterns() -> list[dict[str, Any]]:
    """Get federated patterns (for pattern discovery integration)."""
    federation = get_federation()
    best_patterns = federation.get_best_patterns(count=20)

    # Convert to simple dict format
    patterns = []
    for p in best_patterns:
        patterns.append({
            "type": "federated_pattern",
            "name": p.name,
            "confidence": p.confidence,
            "contributors": len(p.contributors),
            "validations": p.success_count + p.failure_count,
        })

    return patterns
