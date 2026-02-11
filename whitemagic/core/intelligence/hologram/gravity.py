"""Holographic Gravity Calculator v1.0.
====================================

Calculates the W-axis (importance/gravity) using multiple factors
to create a TRUE CONTINUOUS SPECTRUM from ephemeral → critical.

Factors:
  1. Access frequency (how often recalled)
  2. Recency (when last accessed)
  3. Age (creation time)
  4. Content richness (length, links, references)
  5. Tag importance signals
  6. Emotional valence (joy/resonance)
  7. Association density (links to other memories)
  8. Type (short-term vs long-term as minor factor)

Output: Continuous value in [0.0, 1.0+] range
  0.0-0.2: Ephemeral (temporary notes, scratch)
  0.2-0.4: Low (routine, mundane)
  0.4-0.6: Medium (useful, referenced)
  0.6-0.8: High (valuable insights, key decisions)
  0.8-1.0: Critical (core knowledge, breakthroughs)
  1.0+:    Black Hole (foundational, always relevant)
"""

import logging
import math
import sqlite3
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import DB_PATH
from whitemagic.utils.core import parse_datetime

logger = logging.getLogger(__name__)

@dataclass
class GravityFactors:
    """Breakdown of gravity calculation."""

    access_frequency: float   # 0-0.15
    recency: float            # 0-0.15
    age_decay: float          # -0.1-0
    content_richness: float   # 0-0.15
    tag_importance: float     # 0-0.2
    emotional_valence: float  # 0-0.15
    association_density: float # 0-0.1
    type_bonus: float         # 0-0.1
    total: float

class GravityCalculator:
    """Calculate W-axis gravity with full spectrum."""

    def __init__(self, db_path: Path | None = None):
        self.db_path = db_path or DB_PATH

        # High-importance tags
        self.critical_tags = {"critical", "essential", "core", "foundation", "principle"}
        self.important_tags = {"important", "key", "milestone", "breakthrough", "wisdom"}
        self.medium_tags = {"useful", "reference", "pattern", "insight"}
        self.low_tags = {"scratch", "test", "temp", "draft", "wip"}

    def calculate(self, memory: dict[str, Any]) -> GravityFactors:
        """Calculate gravity for a memory."""

        # 1. Access frequency (0-0.15)
        access_count = memory.get("access_count", 1)
        access_freq = min(0.15, math.log(access_count + 1) * 0.05)

        # 2. Recency (0-0.15) - recently accessed = more gravity
        accessed_at = memory.get("accessed_at")
        recency = 0.0
        if accessed_at:
            try:
                if isinstance(accessed_at, str):
                    accessed = parse_datetime(accessed_at.replace("Z", "+00:00").replace("+00:00", ""))
                else:
                    accessed = accessed_at
                days_ago = (datetime.now() - accessed).days
                if days_ago < 1:
                    recency = 0.15
                elif days_ago < 7:
                    recency = 0.12
                elif days_ago < 30:
                    recency = 0.08
                elif days_ago < 90:
                    recency = 0.04
                else:
                    recency = 0.0
            except (ValueError, TypeError):
                recency = 0.05

        # 3. Age decay (-0.1-0) - very old unused = slight decay
        created_at = memory.get("created_at")
        age_decay = 0.0
        if created_at and access_count <= 2:
            try:
                if isinstance(created_at, str):
                    created = parse_datetime(created_at.replace("Z", "+00:00").replace("+00:00", ""))
                else:
                    created = created_at
                age_days = (datetime.now() - created).days
                if age_days > 180 and access_count <= 1:
                    age_decay = -0.1
                elif age_days > 90 and access_count <= 1:
                    age_decay = -0.05
            except (ValueError, TypeError):
                pass

        # 4. Content richness (0-0.15)
        content = str(memory.get("content", ""))
        content_len = len(content)
        richness = 0.0
        if content_len > 2000:
            richness = 0.15
        elif content_len > 1000:
            richness = 0.12
        elif content_len > 500:
            richness = 0.08
        elif content_len > 200:
            richness = 0.05
        else:
            richness = 0.02

        # 5. Tag importance (0-0.2)
        tags = set(t.lower() for t in memory.get("tags", []))
        tag_score = 0.0
        if tags & self.critical_tags:
            tag_score = 0.2
        elif tags & self.important_tags:
            tag_score = 0.15
        elif tags & self.medium_tags:
            tag_score = 0.08
        elif tags & self.low_tags:
            tag_score = -0.05

        # 6. Emotional valence (0-0.15)
        emotional = memory.get("emotional_valence", 0.0)
        joy = memory.get("joy_score", 0.0)
        resonance = memory.get("resonance_score", 0.0)
        emotion_score = min(0.15, (abs(emotional) + joy + resonance) * 0.1)

        # 7. Association density (0-0.1)
        associations = memory.get("associations", {})
        assoc_count = len(associations) if isinstance(associations, dict) else 0
        assoc_score = min(0.1, assoc_count * 0.02)

        # 8. Type bonus (0-0.1) - minor factor now
        mem_type = memory.get("memory_type", memory.get("type", "")).lower()
        type_bonus = 0.0
        if mem_type == "long_term":
            type_bonus = 0.05
        elif mem_type == "short_term":
            type_bonus = -0.02

        # Calculate total
        total = (
            0.3 +  # Base (center of spectrum)
            access_freq +
            recency +
            age_decay +
            richness +
            tag_score +
            emotion_score +
            assoc_score +
            type_bonus
        )

        # Clamp to reasonable range
        total = max(0.1, min(1.2, total))

        return GravityFactors(
            access_frequency=access_freq,
            recency=recency,
            age_decay=age_decay,
            content_richness=richness,
            tag_importance=tag_score,
            emotional_valence=emotion_score,
            association_density=assoc_score,
            type_bonus=type_bonus,
            total=total,
        )

    def recalculate_all(self, dry_run: bool = True) -> dict[str, Any]:
        """Recalculate gravity for all memories."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        memories = conn.execute("""
            SELECT m.id, m.content, m.memory_type, m.access_count,
                   m.created_at, m.accessed_at, m.emotional_valence,
                   m.importance, m.metadata, m.title
            FROM memories m
        """).fetchall()

        old_distribution = {"critical": 0, "high": 0, "medium": 0, "low": 0, "ephemeral": 0}
        new_distribution = {"critical": 0, "high": 0, "medium": 0, "low": 0, "ephemeral": 0}
        updates = []

        for m in memories:
            mem_dict = dict(m)
            mem_dict["tags"] = []  # Tags stored separately

            # Get old W
            old_w = conn.execute(
                "SELECT w FROM holographic_coords WHERE memory_id = ?", (m["id"],),
            ).fetchone()
            old_w = old_w["w"] if old_w else 0.5

            # Calculate new W
            factors = self.calculate(mem_dict)
            new_w = factors.total

            # Track distributions
            for dist, w in [(old_distribution, old_w), (new_distribution, new_w)]:
                if w > 0.8:
                    dist["critical"] += 1
                elif w > 0.6:
                    dist["high"] += 1
                elif w > 0.4:
                    dist["medium"] += 1
                elif w > 0.2:
                    dist["low"] += 1
                else:
                    dist["ephemeral"] += 1

            updates.append((m["id"], new_w))

        if not dry_run:
            for mem_id, new_w in updates:
                conn.execute(
                    "UPDATE holographic_coords SET w = ? WHERE memory_id = ?",
                    (new_w, mem_id),
                )
            conn.commit()

        conn.close()

        return {
            "total_memories": len(memories),
            "old_distribution": old_distribution,
            "new_distribution": new_distribution,
            "dry_run": dry_run,
        }

def get_gravity_calculator() -> GravityCalculator:
    return GravityCalculator()

if __name__ == "__main__":
    calc = get_gravity_calculator()

    logger.info("Gravity Recalculation (Dry Run)")
    logger.info("=" * 60)

    result = calc.recalculate_all(dry_run=True)

    logger.info(f"\nTotal memories: {result['total_memories']}")

    logger.info("\nOLD Distribution (bimodal):")
    for tier, count in result["old_distribution"].items():
        pct = count / result["total_memories"] * 100
        bar = "█" * int(pct / 2)
        logger.info(f"  {tier:10s}: {count:3d} ({pct:5.1f}%) {bar}")

    logger.info("\nNEW Distribution (continuous):")
    for tier, count in result["new_distribution"].items():
        pct = count / result["total_memories"] * 100
        bar = "█" * int(pct / 2)
        logger.info(f"  {tier:10s}: {count:3d} ({pct:5.1f}%) {bar}")
