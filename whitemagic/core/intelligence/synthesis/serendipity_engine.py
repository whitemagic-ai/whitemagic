"""SerendipityEngine - Surface Dormant Knowledge.

Surfaces rarely-accessed but valuable memories through intelligent random selection.
Uses weighted sampling favoring high-gravity, low-access memories.
"""

from __future__ import annotations

import random
import sqlite3
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Any

from whitemagic.utils.core import parse_datetime


@dataclass
class SurfacedMemory:
    """A memory surfaced by serendipity."""

    id: str
    title: str
    content_preview: str
    gravity: float
    last_accessed: datetime | None
    access_count: int
    reason: str  # Why this was surfaced
    relevance_score: float

class SerendipityEngine:
    """Surfaces dormant knowledge through weighted random selection.

    Modes:
    - balanced: Mix of all strategies
    - dormant: Focus on rarely accessed, high-gravity
    - ancient: Focus on old memories (>30 days)
    - bridge: Cross-cluster connections
    - random: Pure random selection
    """

    def __init__(self, db_path: str | None = None) -> None:
        from whitemagic.config.paths import DB_PATH
        self.db_path = db_path or str(DB_PATH)
        self._core_access: Any | None = None
        # Ensure parent directory exists for restricted/sandbox environments.
        try:
            from pathlib import Path
            Path(self.db_path).expanduser().resolve().parent.mkdir(parents=True, exist_ok=True)
        except Exception:
            pass
        self._conn: sqlite3.Connection | None = None

    def _get_conn(self) -> sqlite3.Connection:
        if self._conn is None:
            # Be resilient to concurrent readers/writers from other subsystems.
            conn = sqlite3.connect(self.db_path, timeout=30, check_same_thread=False)
            conn.row_factory = sqlite3.Row
            try:
                conn.execute("PRAGMA journal_mode = WAL")
                conn.execute("PRAGMA synchronous = NORMAL")
                conn.execute("PRAGMA foreign_keys = ON")
                conn.execute("PRAGMA busy_timeout = 30000")
            except Exception:
                pass
            self._conn = conn
        return self._conn

    def surface(self, context: str | None = None, count: int = 5,
                mode: str = "balanced") -> list[SurfacedMemory]:
        """Surface memories based on mode."""

        if mode == "dormant":
            return self._surface_dormant(count)
        elif mode == "ancient":
            return self._surface_ancient(count)
        elif mode == "bridge":
            return self._surface_bridges(count)
        elif mode == "random":
            return self._surface_random(count)
        elif mode == "constellation_bridges":
            return self._surface_constellation_bridges(count)
        elif mode == "orphans":
            return self._surface_association_orphans(count)
        else:  # balanced
            results = []
            results.extend(self._surface_dormant(max(1, count // 3)))
            results.extend(self._surface_constellation_bridges(max(1, count // 4)))
            results.extend(self._surface_association_orphans(max(1, count // 4)))
            results.extend(self._surface_ancient(count - len(results)))
            return results[:count]

    def _surface_dormant(self, count: int) -> list[SurfacedMemory]:
        """Surface high-gravity, rarely-accessed memories."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("""
            SELECT m.id, m.title, SUBSTR(m.content, 1, 200) as preview,
                   h.w as gravity, m.accessed_at, m.access_count
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
            WHERE h.w > 0.5
            ORDER BY (1.0 / MAX(1, m.access_count)) * h.w DESC
            LIMIT ?
        """, (count * 2,))

        candidates = cur.fetchall()

        # Weighted random selection
        if not candidates:
            return []

        weights = [(1.0 / max(1, c["access_count"])) * c["gravity"] for c in candidates]
        total = sum(weights)
        weights = [w / total for w in weights]

        selected = random.choices(candidates, weights=weights, k=min(count, len(candidates)))

        return [SurfacedMemory(
            id=m["id"],
            title=m["title"] or "Untitled",
            content_preview=m["preview"] or "",
            gravity=m["gravity"],
            last_accessed=parse_datetime(m["accessed_at"]) if m["accessed_at"] else None,
            access_count=m["access_count"] or 0,
            reason="High gravity, rarely accessed",
            relevance_score=m["gravity"],
        ) for m in selected]

    def _surface_ancient(self, count: int) -> list[SurfacedMemory]:
        """Surface old memories that may be forgotten."""
        conn = self._get_conn()
        cur = conn.cursor()

        threshold = (datetime.now() - timedelta(days=30)).isoformat()

        cur.execute("""
            SELECT m.id, m.title, SUBSTR(m.content, 1, 200) as preview,
                   h.w as gravity, m.accessed_at, m.access_count, m.created_at
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
            WHERE m.created_at < ? AND h.w > 0.3
            ORDER BY RANDOM()
            LIMIT ?
        """, (threshold, count))

        memories = cur.fetchall()

        return [SurfacedMemory(
            id=m["id"],
            title=m["title"] or "Untitled",
            content_preview=m["preview"] or "",
            gravity=m["gravity"],
            last_accessed=parse_datetime(m["accessed_at"]) if m["accessed_at"] else None,
            access_count=m["access_count"] or 0,
            reason="Ancient memory (>30 days old)",
            relevance_score=m["gravity"] * 0.8,
        ) for m in memories]

    def _surface_bridges(self, count: int) -> list[SurfacedMemory]:
        """Surface memories that could bridge clusters."""
        conn = self._get_conn()
        cur = conn.cursor()

        # Find memories near cluster boundaries (x or y near 0)
        cur.execute("""
            SELECT m.id, m.title, SUBSTR(m.content, 1, 200) as preview,
                   h.w as gravity, m.accessed_at, m.access_count,
                   ABS(h.x) + ABS(h.y) as boundary_distance
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
            WHERE ABS(h.x) < 0.3 OR ABS(h.y) < 0.3
            ORDER BY RANDOM()
            LIMIT ?
        """, (count,))

        memories = cur.fetchall()

        return [SurfacedMemory(
            id=m["id"],
            title=m["title"] or "Untitled",
            content_preview=m["preview"] or "",
            gravity=m["gravity"],
            last_accessed=parse_datetime(m["accessed_at"]) if m["accessed_at"] else None,
            access_count=m["access_count"] or 0,
            reason="Bridge memory (near cluster boundary)",
            relevance_score=m["gravity"] * 0.9,
        ) for m in memories]

    def _surface_random(self, count: int) -> list[SurfacedMemory]:
        """Pure random selection."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("""
            SELECT m.id, m.title, SUBSTR(m.content, 1, 200) as preview,
                   h.w as gravity, m.accessed_at, m.access_count
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
            ORDER BY RANDOM()
            LIMIT ?
        """, (count,))

        memories = cur.fetchall()

        return [SurfacedMemory(
            id=m["id"],
            title=m["title"] or "Untitled",
            content_preview=m["preview"] or "",
            gravity=m["gravity"],
            last_accessed=parse_datetime(m["accessed_at"]) if m["accessed_at"] else None,
            access_count=m["access_count"] or 0,
            reason="Random discovery",
            relevance_score=m["gravity"] * 0.7,
        ) for m in memories]

    def mark_accessed(self, memory_id: str) -> None:
        """Mark a memory as accessed and emit Gan Ying event."""
        import sqlite3
        import time

        conn = self._get_conn()
        cur = conn.cursor()
        for attempt in range(6):
            try:
                cur.execute(
                    """
                    UPDATE memories
                    SET access_count = COALESCE(access_count, 0) + 1,
                        accessed_at = ?
                    WHERE id = ?
                    """,
                    (datetime.now().isoformat(), memory_id),
                )
                conn.commit()
                break
            except sqlite3.OperationalError as exc:
                # SQLite can transiently lock under concurrent writers. Retry a few times.
                if "locked" not in str(exc).lower() or attempt >= 5:
                    raise
                time.sleep(0.05 * (2**attempt))

        # Emit Gan Ying event
        self._emit_access_event(memory_id)

    def _emit_access_event(self, memory_id: str) -> None:
        """Emit memory access event to Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            bus = get_bus()
            event = ResonanceEvent(
                source="serendipity_engine",
                event_type=EventType.MEMORY_ACCESSED,
                data={"memory_id": memory_id, "source": "serendipity"},
                confidence=1.0,
                timestamp=datetime.now(),
            )
            bus.emit(event)
        except Exception:
            pass  # Gan Ying not available

    def _get_core_access(self) -> Any:
        """Lazy-load the CoreAccessLayer."""
        if self._core_access is None:
            try:
                from whitemagic.core.intelligence.core_access import get_core_access
                self._core_access = get_core_access()
            except Exception:
                pass
        return self._core_access

    def _surface_constellation_bridges(self, count: int) -> list[SurfacedMemory]:
        """Surface memories that bridge two constellations.

        These are high-value discovery targets: they link disparate
        knowledge domains that might not otherwise be connected.
        """
        cal = self._get_core_access()
        if not cal:
            return self._surface_random(count)

        bridges = cal.find_constellation_bridges(limit=count * 2)
        if not bridges:
            return self._surface_random(count)

        conn = self._get_conn()
        results = []
        seen = set()

        for bridge in bridges:
            for key in ("source_id", "target_id"):
                mid = bridge.get(key, "")
                if mid in seen:
                    continue
                seen.add(mid)

                try:
                    row = conn.execute(
                        "SELECT m.id, m.title, SUBSTR(m.content, 1, 200) as preview, "
                        "h.w as gravity, m.accessed_at, m.access_count "
                        "FROM memories m "
                        "JOIN holographic_coords h ON m.id = h.memory_id "
                        "WHERE m.id = ?",
                        (mid,),
                    ).fetchone()
                except Exception:
                    continue

                if not row:
                    continue

                results.append(SurfacedMemory(
                    id=row["id"],
                    title=row["title"] or "Untitled",
                    content_preview=row["preview"] or "",
                    gravity=row["gravity"] or 0.5,
                    last_accessed=parse_datetime(row["accessed_at"]) if row["accessed_at"] else None,
                    access_count=row["access_count"] or 0,
                    reason=(
                        f"Bridges constellations '{bridge.get('constellation_1', '?')}' "
                        f"and '{bridge.get('constellation_2', '?')}'"
                    ),
                    relevance_score=bridge.get("strength", 0.5) * 1.2,
                ))

                if len(results) >= count:
                    break
            if len(results) >= count:
                break

        return results[:count]

    def _surface_association_orphans(self, count: int) -> list[SurfacedMemory]:
        """Surface high-gravity memories with few or no associations.

        These are isolated knowledge nodes that deserve attention — they
        have high importance but aren't connected to the rest of the graph.
        """
        cal = self._get_core_access()
        if not cal:
            return self._surface_random(count)

        orphans = cal.find_association_orphans(min_gravity=0.5, limit=count * 2)
        if not orphans:
            return self._surface_random(count)

        conn = self._get_conn()
        results = []

        for orphan in orphans:
            mid = orphan.get("id", "")
            try:
                row = conn.execute(
                    "SELECT SUBSTR(content, 1, 200) as preview, accessed_at, access_count "
                    "FROM memories WHERE id = ?",
                    (mid,),
                ).fetchone()
            except Exception:
                continue

            results.append(SurfacedMemory(
                id=mid,
                title=orphan.get("title") or "Untitled",
                content_preview=(row["preview"] if row else "") or "",
                gravity=orphan.get("gravity", 0.5),
                last_accessed=parse_datetime(row["accessed_at"]) if row and row["accessed_at"] else None,
                access_count=(row["access_count"] if row else 0) or 0,
                reason=f"High-gravity orphan (gravity={orphan.get('gravity', 0):.2f}, {orphan.get('assoc_count', 0)} associations)",
                relevance_score=orphan.get("gravity", 0.5) * 1.1,
            ))

            if len(results) >= count:
                break

        return results[:count]

    def start(self) -> None:
        """Start listening to Gan Ying for pattern events."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import EventType, get_bus
            bus = get_bus()
            bus.listen(EventType.PATTERN_DETECTED, self._on_pattern_detected)
        except Exception:
            pass

    def _on_pattern_detected(self, event: Any) -> None:
        """Handle pattern detection - surface related dormant memories."""
        # Could trigger targeted surfacing based on pattern context
        pass

# Global instance
_serendipity_engine = None

def get_serendipity_engine() -> SerendipityEngine:
    global _serendipity_engine
    if _serendipity_engine is None:
        _serendipity_engine = SerendipityEngine()
    return _serendipity_engine
