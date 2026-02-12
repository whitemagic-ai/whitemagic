"""SQLite Backend for Unified Memory
Implements a scalable, ACID-compliant persistence layer for WhiteMagic.
"""

import json
import logging
import sqlite3
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.core.memory.db_manager import get_db_pool
from whitemagic.core.memory.unified_types import Memory, MemoryType
from whitemagic.utils.core import parse_datetime

logger = logging.getLogger(__name__)

class SQLiteBackend:
    """SQLite backend for UnifiedMemory.
    Replaces JSON file storage with a single SQLite database.
    Supports cold storage fallback (v13.3.2) for archived memories.
    """

    def __init__(self, db_path: Path) -> None:
        self.db_path = db_path
        self.pool = get_db_pool(str(db_path))
        self._cold_pool: Any = None
        self._init_db()

    def _get_cold_pool(self) -> Any:
        """Lazy-load cold storage DB pool if it exists."""
        if self._cold_pool is not None:
            return self._cold_pool
        try:
            from whitemagic.config.paths import COLD_DB_PATH
            if COLD_DB_PATH.exists():
                self._cold_pool = get_db_pool(str(COLD_DB_PATH))
                return self._cold_pool
        except Exception:
            pass
        self._cold_pool = False  # Sentinel: checked but unavailable
        return None

    def _init_db(self) -> None:
        """Initialize database schema and handle migrations.

        Note: PRAGMAs (WAL, mmap_size, cache_size, temp_store, etc.) are
        set centrally in db_manager.ConnectionPool._create_connection().
        """
        with self.pool.connection() as conn:

            # 1. Memories table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS memories (
                    id TEXT PRIMARY KEY,
                    content TEXT,
                    memory_type TEXT,
                    created_at TEXT,
                    updated_at TEXT,
                    accessed_at TEXT,
                    access_count INTEGER,
                    emotional_valence REAL,
                    importance REAL,
                    neuro_score REAL DEFAULT 1.0,
                    novelty_score REAL DEFAULT 1.0,
                    recall_count INTEGER DEFAULT 0,
                    half_life_days REAL DEFAULT 30.0,
                    is_protected INTEGER DEFAULT 0,
                    metadata TEXT,
                    title TEXT
                )
            """)

            # Migration: Add missing columns if they don't exist
            cursor = conn.execute("PRAGMA table_info(memories)")
            existing_columns = {row[1] for row in cursor.fetchall()}

            new_columns = {
                "neuro_score": "REAL DEFAULT 1.0",
                "novelty_score": "REAL DEFAULT 1.0",
                "recall_count": "INTEGER DEFAULT 0",
                "half_life_days": "REAL DEFAULT 30.0",
                "is_protected": "INTEGER DEFAULT 0",
                "memory_type": "TEXT DEFAULT 'SHORT_TERM'",
                "updated_at": "TEXT",
                "accessed_at": "TEXT",
                "access_count": "INTEGER DEFAULT 0",
                "emotional_valence": "REAL DEFAULT 0.0",
                "metadata": "TEXT DEFAULT '{}'",
                "galactic_distance": "REAL DEFAULT 0.0",
                "retention_score": "REAL DEFAULT 0.5",
                "last_retention_sweep": "TEXT",
                "content_hash": "TEXT",  # v14.1.1: SHA-256 for dedup
                "event_time": "TEXT",       # v14.2: bitemporal — when the fact became true
                "ingestion_time": "TEXT",   # v14.2: bitemporal — when WM learned it
                "is_private": "INTEGER DEFAULT 0",    # v15: exclude from MCP responses
                "model_exclude": "INTEGER DEFAULT 0", # v15: exclude from AI context
            }

            for col_name, col_type in new_columns.items():
                if col_name not in existing_columns:
                    logger.debug(f"Adding column {col_name} to memories table")
                    try:
                        conn.execute(f"ALTER TABLE memories ADD COLUMN {col_name} {col_type}")
                    except sqlite3.OperationalError as e:
                        logger.warning(f"Could not add column {col_name}: {e}")

            # 2. Tags table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS tags (
                    memory_id TEXT,
                    tag TEXT,
                    FOREIGN KEY (memory_id) REFERENCES memories(id) ON DELETE CASCADE,
                    PRIMARY KEY (memory_id, tag)
                )
            """)
            conn.execute("CREATE INDEX IF NOT EXISTS idx_tags_tag ON tags(tag)")

            # 3. Associations table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS associations (
                    source_id TEXT,
                    target_id TEXT,
                    strength REAL,
                    FOREIGN KEY (source_id) REFERENCES memories(id) ON DELETE CASCADE,
                    FOREIGN KEY (target_id) REFERENCES memories(id) ON DELETE CASCADE,
                    PRIMARY KEY (source_id, target_id)
                )
            """)

            # 4. Full Text Search (FTS5) - Use internal content for string ID support
            conn.execute("""
                CREATE VIRTUAL TABLE IF NOT EXISTS memories_fts USING fts5(
                    id UNINDEXED,
                    title,
                    content,
                    tags_text
                )
            """)

            # 5. Holographic Coordinates table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS holographic_coords (
                    memory_id TEXT PRIMARY KEY,
                    x REAL,
                    y REAL,
                    z REAL,
                    w REAL,
                    v REAL DEFAULT 0.5,
                    FOREIGN KEY (memory_id) REFERENCES memories(id) ON DELETE CASCADE
                )
            """)

            # Migration: add temporal columns to associations if missing
            assoc_cursor = conn.execute("PRAGMA table_info(associations)")
            assoc_columns = {row[1] for row in assoc_cursor.fetchall()}
            for col_name, col_def in [
                ("last_traversed_at", "TEXT"),
                ("traversal_count", "INTEGER DEFAULT 0"),
                ("created_at", "TEXT"),
                # v14.0 Living Graph columns (Overlap B — 4 research teams)
                ("direction", "TEXT DEFAULT 'undirected'"),       # 5D Causal: causal directionality
                ("relation_type", "TEXT DEFAULT 'associated_with'"),  # Holographic Gap: typed traversal
                ("edge_type", "TEXT DEFAULT 'semantic'"),         # Living Memory: decay curve selection
                ("valid_from", "TEXT"),                           # Living Memory: bitemporal modeling
                ("valid_until", "TEXT"),                          # Living Memory: bitemporal modeling
                ("ingestion_time", "TEXT"),                       # Living Memory: bitemporal modeling
            ]:
                if col_name not in assoc_columns:
                    try:
                        conn.execute(f"ALTER TABLE associations ADD COLUMN {col_name} {col_def}")
                        logger.info(f"Added {col_name} column to associations table")
                    except sqlite3.OperationalError:
                        pass

            # Migration: add v column to holographic_coords if missing
            hc_cursor = conn.execute("PRAGMA table_info(holographic_coords)")
            hc_columns = {row[1] for row in hc_cursor.fetchall()}
            if "v" not in hc_columns:
                try:
                    conn.execute("ALTER TABLE holographic_coords ADD COLUMN v REAL DEFAULT 0.5")
                    logger.info("Added v column to holographic_coords table")
                except sqlite3.OperationalError:
                    pass

            # 6. Constellation Membership table (v14.3 — Recall Boost)
            conn.execute("""
                CREATE TABLE IF NOT EXISTS constellation_membership (
                    memory_id TEXT PRIMARY KEY,
                    constellation_name TEXT NOT NULL,
                    membership_confidence REAL DEFAULT 1.0,
                    updated_at TEXT,
                    FOREIGN KEY (memory_id) REFERENCES memories(id) ON DELETE CASCADE
                )
            """)
            conn.execute("CREATE INDEX IF NOT EXISTS idx_constellation_name ON constellation_membership(constellation_name)")

            # 7. Akashic Seeds table (v5.0 Integration)
            conn.execute("""
                CREATE TABLE IF NOT EXISTS akashic_seeds (
                    id TEXT PRIMARY KEY,
                    content TEXT,
                    bloom_conditions TEXT,
                    planted_at TEXT,
                    times_bloomed INTEGER DEFAULT 0,
                    last_bloomed TEXT,
                    potency REAL DEFAULT 1.0,
                    keywords TEXT
                )
            """)

            # 7. Dharma Audit Log table (Phase 4)
            conn.execute("""
                CREATE TABLE IF NOT EXISTS dharma_audit (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp TEXT NOT NULL,
                    action TEXT NOT NULL,
                    ethical_score REAL,
                    harmony_score REAL,
                    consent_level TEXT,
                    boundary_type TEXT,
                    concerns TEXT,
                    context TEXT,
                    decision TEXT
                )
            """)
            # Add performance indexes
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_importance ON memories(importance)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_created_at ON memories(created_at)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_updated_at ON memories(updated_at)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_tags_composite ON tags(tag, memory_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_dharma_timestamp ON dharma_audit(timestamp)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_content_hash ON memories(content_hash)")
            # P6: Additional indexes for hot query patterns (v13.3.3)
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_galactic ON memories(galactic_distance)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_type ON memories(memory_type)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_neuro ON memories(neuro_score)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_accessed ON memories(accessed_at)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_assoc_source ON associations(source_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_memories_protected ON memories(is_protected)")
            # v14.0: Living Graph indexes for graph traversal
            conn.execute("CREATE INDEX IF NOT EXISTS idx_assoc_target ON associations(target_id)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_assoc_edge_type ON associations(edge_type)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_assoc_direction ON associations(direction)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_assoc_strength ON associations(strength)")

    def find_by_content_hash(self, content_hash: str) -> str | None:
        """Find an existing memory ID by content SHA-256 hash.

        Returns the memory ID if a match is found, None otherwise.
        Used for dedup at ingest (v14.1.1).
        """
        with self.pool.connection() as conn:
            row = conn.execute(
                "SELECT id FROM memories WHERE content_hash = ? LIMIT 1",
                (content_hash,),
            ).fetchone()
            return row[0] if row else None

    def store(self, memory: Memory, content_hash: str | None = None) -> str:
        """Store or update a memory."""
        with self.pool.connection() as conn:
            with conn: # Standard transaction context manager
                # Upsert Memory
                conn.execute("""
                    INSERT OR REPLACE INTO memories (
                        id, content, memory_type, created_at, updated_at, accessed_at,
                        access_count, emotional_valence, importance,
                        neuro_score, novelty_score, recall_count, half_life_days, is_protected,
                        metadata, title,
                        galactic_distance, retention_score, last_retention_sweep,
                        content_hash, event_time, ingestion_time,
                        is_private, model_exclude
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    memory.id,
                    json.dumps(memory.content) if not isinstance(memory.content, str) else memory.content,
                    memory.memory_type.name,
                    memory.created_at.isoformat(),
                    (memory.last_modified or memory.created_at).isoformat(),
                    memory.accessed_at.isoformat(),
                    memory.access_count,
                    memory.emotional_valence,
                    memory.importance,
                    memory.neuro_score,
                    memory.novelty_score,
                    memory.recall_count,
                    memory.half_life_days,
                    1 if memory.is_protected else 0,
                    json.dumps(memory.metadata),
                    memory.title,
                    memory.galactic_distance,
                    memory.retention_score,
                    memory.last_retention_sweep.isoformat() if memory.last_retention_sweep else None,
                    content_hash,
                    memory.metadata.get("event_time"),  # caller-provided or None
                    datetime.now().isoformat(),          # always set at ingestion
                    1 if memory.is_private else 0,
                    1 if memory.model_exclude else 0,
                ))

                # Update Tags
                conn.execute("DELETE FROM tags WHERE memory_id = ?", (memory.id,))
                if memory.tags:
                    conn.executemany(
                        "INSERT INTO tags (memory_id, tag) VALUES (?, ?)",
                        [(memory.id, tag) for tag in memory.tags],
                    )

                # Update Associations
                conn.execute("DELETE FROM associations WHERE source_id = ?", (memory.id,))
                if memory.associations:
                    conn.executemany(
                        "INSERT INTO associations (source_id, target_id, strength) VALUES (?, ?, ?)",
                        [(memory.id, target, strength) for target, strength in memory.associations.items()],
                    )

                # Update FTS (Internal content)
                conn.execute("DELETE FROM memories_fts WHERE id = ?", (memory.id,))
                tags_text = " ".join(memory.tags) if memory.tags else ""
                conn.execute("""
                    INSERT INTO memories_fts (id, title, content, tags_text) VALUES (?, ?, ?, ?)
                """, (memory.id, memory.title or "", str(memory.content), tags_text))

        return memory.id

    def recall(self, memory_id: str) -> Memory | None:
        """Retrieve a memory by ID. Falls back to cold storage if not in hot DB."""
        result = self._recall_from_pool(self.pool, memory_id)
        if result is not None:
            return result

        # Cold storage fallback (v13.3.2)
        cold = self._get_cold_pool()
        if cold and cold is not False:
            return self._recall_from_pool(cold, memory_id)
        return None

    def _recall_from_pool(self, pool: Any, memory_id: str) -> Memory | None:
        """Retrieve a memory by ID from a specific DB pool."""
        with pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            row = conn.execute("SELECT * FROM memories WHERE id = ?", (memory_id,)).fetchone()

            if not row:
                return None

            # Fetch Tags
            tags_rows = conn.execute("SELECT tag FROM tags WHERE memory_id = ?", (memory_id,)).fetchall()
            tags = {r["tag"] for r in tags_rows}

            # Fetch Associations
            assoc_rows = conn.execute("SELECT target_id, strength FROM associations WHERE source_id = ?", (memory_id,)).fetchall()
            associations = {r["target_id"]: r["strength"] for r in assoc_rows}

            # Content hydration
            content = row["content"]
            if isinstance(content, str) and (content.startswith("{") or content.startswith("[")):
                try:
                    content = json.loads(content)
                except Exception:
                    pass # Fallback to raw string if not valid JSON

            return Memory(
                id=row["id"],
                content=content,
                memory_type=getattr(MemoryType, row["memory_type"], MemoryType.SHORT_TERM),
                title=row["title"],
                created_at=parse_datetime(row["created_at"]),
                accessed_at=parse_datetime(row["accessed_at"]),
                access_count=row["access_count"],
                tags=tags,
                associations=associations,
                emotional_valence=row["emotional_valence"],
                importance=row["importance"],
                neuro_score=row["neuro_score"] if "neuro_score" in row.keys() else 1.0,
                novelty_score=row["novelty_score"] if "novelty_score" in row.keys() else 1.0,
                recall_count=row["recall_count"] if "recall_count" in row.keys() else 0,
                half_life_days=row["half_life_days"] if "half_life_days" in row.keys() else 30.0,
                is_protected=bool(row["is_protected"]) if "is_protected" in row.keys() else False,
                is_private=bool(row["is_private"]) if "is_private" in row.keys() else False,
                model_exclude=bool(row["model_exclude"]) if "model_exclude" in row.keys() else False,
                galactic_distance=row["galactic_distance"] if "galactic_distance" in row.keys() else 0.0,
                retention_score=row["retention_score"] if "retention_score" in row.keys() else 0.5,
                last_retention_sweep=parse_datetime(row["last_retention_sweep"]) if "last_retention_sweep" in row.keys() and row["last_retention_sweep"] else None,
                metadata=json.loads(row["metadata"]) if row["metadata"] else {},
            )

    def fetch_memory_contents(self, memory_type: str | None = None, limit: int = 10000) -> list[str]:
        """Fetch contents of memories efficiently for pattern analysis."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            sql = "SELECT content FROM memories"
            params: list[Any] = []
            if memory_type:
                sql += " WHERE memory_type = ?"
                params.append(memory_type)
            sql += " ORDER BY importance DESC LIMIT ?"
            params.append(limit)

            rows = conn.execute(sql, params).fetchall()
            return [str(row["content"]) for row in rows if row["content"]]

    def search(self, query: str | None = None, tags: set[str] | None = None,
               memory_type: MemoryType | None = None, min_importance: float = 0.0,
               limit: int = 10) -> list[Memory]:
        """Search memories with FTS5 BM25 ranking."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row

            if query:
                # FTS search with BM25 ranking (lower rank = better match)
                fts_query = query.strip()
                # Sanitize FTS5-unsafe characters (brackets, quotes, etc.)
                for ch in '[]{}()^~*':
                    fts_query = fts_query.replace(ch, '')
                fts_query = fts_query.strip()
                if not fts_query:
                    fts_query = query.strip().replace('[', '').replace(']', '')
                if " " in fts_query and not (fts_query.startswith('"') and fts_query.endswith('"')):
                    # Multi-word query: try as phrase OR individual keywords
                    keywords = [k for k in fts_query.split() if k]
                    if keywords:
                        fts_query = f'"{fts_query}" OR {" OR ".join(keywords)}'

                sql = """
                    SELECT m.*, fts.rank
                    FROM memories m
                    JOIN (
                        SELECT id, rank
                        FROM memories_fts
                        WHERE memories_fts MATCH ?
                        ORDER BY rank
                        LIMIT ?
                    ) fts ON m.id = fts.id
                    WHERE m.importance >= ?
                """
                params = [fts_query, limit * 3, min_importance]  # Get more candidates for filtering

                if memory_type:
                    sql += " AND m.memory_type = ?"
                    params.append(memory_type.name)

                if tags:
                    placeholders = ",".join("?" * len(tags))
                    sql += f" AND m.id IN (SELECT memory_id FROM tags WHERE tag IN ({placeholders}) GROUP BY memory_id HAVING COUNT(DISTINCT tag) = ?)"
                    params.extend(tags)
                    params.append(len(tags))

                # Order by FTS rank (relevance) weighted by galactic proximity
                # FTS5 ranks are negative (more negative = better match), so use ABS
                # ABS(rank) * (0.5 + galactic_distance): lower = better
                # Core memories (distance≈0) get multiplier 0.5, edge (distance≈1) gets 1.5
                sql += " ORDER BY (ABS(fts.rank) * (0.5 + COALESCE(m.galactic_distance, 0.5))) ASC, m.importance DESC LIMIT ?"
                params.append(limit)

            else:
                # No query: return recent/important memories
                sql = "SELECT * FROM memories WHERE importance >= ?"
                params = [min_importance]

                if memory_type:
                    sql += " AND memory_type = ?"
                    params.append(memory_type.name)

                if tags:
                    placeholders = ",".join("?" * len(tags))
                    sql += f" AND id IN (SELECT memory_id FROM tags WHERE tag IN ({placeholders}) GROUP BY memory_id HAVING COUNT(DISTINCT tag) = ?)"
                    params.extend(tags)
                    params.append(len(tags))

                sql += " ORDER BY COALESCE(galactic_distance, 0.5) ASC, importance DESC, accessed_at DESC LIMIT ?"
                params.append(limit)

            rows = conn.execute(sql, params).fetchall()
            return self._batch_hydrate(rows, conn)

    def get_weakest_memories(self, limit: int = 100) -> list[Memory]:
        """Retrieve memories with the lowest neuro_score first."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            rows = conn.execute(
                "SELECT * FROM memories WHERE is_protected = 0 ORDER BY neuro_score ASC LIMIT ?",
                (limit,),
            ).fetchall()
            return self._batch_hydrate(rows, conn)

    def get_all_akashic_seeds(self) -> dict[str, dict[str, Any]]:
        """Retrieve all Akashic seeds from database."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute("SELECT * FROM akashic_seeds")
            return {
                row["id"]: {
                    "content": row["content"],
                    "bloom_conditions": json.loads(row["bloom_conditions"]) if row["bloom_conditions"] else [],
                    "planted_at": row["planted_at"],
                    "times_bloomed": row["times_bloomed"],
                    "last_bloomed": row["last_bloomed"],
                    "potency": row["potency"],
                    "keywords": json.loads(row["keywords"]) if row["keywords"] else [],
                }
                for row in cursor
            }

    def store_akashic_seed(
        self,
        seed_id: str,
        content: str,
        bloom_conditions: list[str],
        planted_at: str,
        times_bloomed: int,
        last_bloomed: str | None,
        potency: float,
        keywords: list[str],
    ) -> None:
        """Store or update an Akashic seed."""
        with self.pool.connection() as conn:
            conn.execute("""
                INSERT OR REPLACE INTO akashic_seeds (
                    id, content, bloom_conditions, planted_at,
                    times_bloomed, last_bloomed, potency, keywords
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                seed_id,
                content,
                json.dumps(bloom_conditions),
                planted_at,
                times_bloomed,
                last_bloomed,
                potency,
                json.dumps(keywords),
            ))

    def get_stats(self) -> dict[str, Any]:
        """Get database statistics."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            total_memories = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]

            type_counts = {}
            for row in conn.execute("SELECT memory_type, COUNT(*) FROM memories GROUP BY memory_type"):
                type_counts[row[0]] = row[1]

            total_tags = conn.execute("SELECT COUNT(DISTINCT tag) FROM tags").fetchone()[0]

            # Akashic stats
            akashic_count = 0
            try:
                akashic_count = conn.execute("SELECT COUNT(*) FROM akashic_seeds").fetchone()[0]
            except sqlite3.OperationalError:
                pass

            return {
                "total_memories": total_memories,
                "by_type": type_counts,
                "total_tags": total_tags,
                "akashic_seeds": akashic_count,
                "db_size_kb": self.db_path.stat().st_size // 1024 if self.db_path.exists() else 0,
            }

    def archive_to_edge(self, memory_id: str, galactic_distance: float = 0.95) -> bool:
        """Archive a memory to the galactic edge — never delete.

        No memory is ever truly forgotten. Instead it is rotated outward
        from the galactic core (active spotlight) toward the far edge
        (deep archive). It remains searchable but deprioritized.

        Args:
            memory_id: Memory to archive
            galactic_distance: How far from core (0.0=core, 1.0=outermost edge)

        """
        with self.pool.connection() as conn:
            with conn:
                conn.execute("""
                    UPDATE memories
                    SET galactic_distance = ?,
                        neuro_score = MAX(0.05, neuro_score * 0.3),
                        metadata = json_set(COALESCE(metadata, '{}'),
                            '$.archived_at', ?,
                            '$.archive_reason', 'galactic_rotation')
                    WHERE id = ?
                """, (galactic_distance, datetime.now().isoformat(), memory_id))
            return True

    def delete(self, memory_id: str) -> bool:
        """DEPRECATED: Use archive_to_edge() instead.
        Kept for backward compatibility but now redirects to archive.
        No memory is ever truly forgotten.
        """
        logger.warning(
            f"delete() called for {memory_id} — redirecting to archive_to_edge(). "
            f"No memory is ever truly forgotten.",
        )
        return self.archive_to_edge(memory_id, galactic_distance=0.95)

    def update_galactic_distance(self, memory_id: str, distance: float) -> bool:
        """Update a memory's galactic distance (0.0=core, 1.0=edge)."""
        distance = max(0.0, min(1.0, distance))
        with self.pool.connection() as conn:
            with conn:
                conn.execute(
                    "UPDATE memories SET galactic_distance = ? WHERE id = ?",
                    (distance, memory_id),
                )
            return True

    def update_retention_score(self, memory_id: str, score: float) -> bool:
        """Update a memory's retention score from the mindful forgetting engine."""
        with self.pool.connection() as conn:
            with conn:
                conn.execute(
                    "UPDATE memories SET retention_score = ?, last_retention_sweep = ? WHERE id = ?",
                    (score, datetime.now().isoformat(), memory_id),
                )
            return True

    def batch_update_galactic(self, updates: list) -> int:
        """Batch update galactic_distance and retention_score for many memories.

        Args:
            updates: list of (memory_id, galactic_distance, retention_score) tuples
        Returns:
            Number of rows updated

        """
        if not updates:
            return 0
        with self.pool.connection() as conn:
            with conn:
                conn.executemany(
                    """UPDATE memories
                       SET galactic_distance = ?, retention_score = ?, last_retention_sweep = ?
                       WHERE id = ?""",
                    [(dist, ret, datetime.now().isoformat(), mid) for mid, dist, ret in updates],
                )
            return len(updates)

    def store_coords(self, memory_id: str, x: float, y: float, z: float, w: float, v: float = 0.5) -> None:
        """Store holographic coordinates (5D: x, y, z, w, v)."""
        with self.pool.connection() as conn:
            with conn:
                conn.execute(
                    """
                    INSERT OR REPLACE INTO holographic_coords (memory_id, x, y, z, w, v)
                    VALUES (?, ?, ?, ?, ?, ?)
                    """,
                    (memory_id, x, y, z, w, v),
                )

    def get_coords(self, memory_id: str) -> tuple | None:
        """Get holographic coordinates for a memory (5D: x, y, z, w, v)."""
        with self.pool.connection() as conn:
            row = conn.execute(
                "SELECT x, y, z, w, COALESCE(v, 0.5) FROM holographic_coords WHERE memory_id = ?",
                (memory_id,),
            ).fetchone()
            if row:
                return (row[0], row[1], row[2], row[3], row[4])
            return None

    def get_all_coords(self) -> dict[str, tuple]:
        """Get all holographic coordinates (5D: x, y, z, w, v)."""
        with self.pool.connection() as conn:
            cursor = conn.execute("SELECT memory_id, x, y, z, w, COALESCE(v, 0.5) FROM holographic_coords")
            return {row[0]: (row[1], row[2], row[3], row[4], row[5]) for row in cursor}

    def _batch_hydrate(self, rows: list[Any], conn: sqlite3.Connection) -> list[Memory]:
        """Efficiently hydrate Memory objects from raw rows in batch.
        Prevents N+1 query issues by fetching tags and associations in bulk.
        """
        if not rows:
            return []

        memories: list[Memory] = []
        ids = [row["id"] for row in rows]
        id_placeholders = ",".join("?" * len(ids))

        # 1. Bulk Fetch Tags
        tags_map: dict[str, set[str]] = {id: set() for id in ids}
        try:
            flag_rows = conn.execute(
                f"SELECT memory_id, tag FROM tags WHERE memory_id IN ({id_placeholders})",
                ids,
            ).fetchall()
            for r in flag_rows:
                tags_map[r["memory_id"]].add(r["tag"])
        except sqlite3.OperationalError:
            pass # Tables might not be fully ready in some edge cases

        # 2. Bulk Fetch Associations
        assoc_map: dict[str, dict[str, float]] = {id: {} for id in ids}
        try:
            assoc_rows = conn.execute(
                f"SELECT source_id, target_id, strength FROM associations WHERE source_id IN ({id_placeholders})",
                ids,
            ).fetchall()
            for r in assoc_rows:
                assoc_map[r["source_id"]][r["target_id"]] = r["strength"]
        except sqlite3.OperationalError:
            pass

        # 3. Assemble Memory Objects
        for row in rows:
            mem_id = row["id"]
            # Content hydration
            content = row["content"]
            if isinstance(content, str) and (content.startswith("{") or content.startswith("[")):
                try:
                    content = json.loads(content)
                except Exception:
                    pass

            try:
                mem = Memory(
                    id=mem_id,
                    content=content,
                    memory_type=getattr(MemoryType, row["memory_type"], MemoryType.SHORT_TERM),
                    title=row["title"],
                    created_at=parse_datetime(row["created_at"]),
                    accessed_at=parse_datetime(row["accessed_at"]),
                    access_count=row["access_count"],
                    tags=tags_map.get(mem_id, set()),
                    associations=assoc_map.get(mem_id, {}),
                    emotional_valence=row["emotional_valence"],
                    importance=row["importance"],
                    neuro_score=row["neuro_score"] if "neuro_score" in row.keys() else 1.0,
                    novelty_score=row["novelty_score"] if "novelty_score" in row.keys() else 1.0,
                    recall_count=row["recall_count"] if "recall_count" in row.keys() else 0,
                    half_life_days=row["half_life_days"] if "half_life_days" in row.keys() else 30.0,
                    is_protected=bool(row["is_protected"]) if "is_protected" in row.keys() else False,
                    galactic_distance=row["galactic_distance"] if "galactic_distance" in row.keys() else 0.0,
                    retention_score=row["retention_score"] if "retention_score" in row.keys() else 0.5,
                    last_retention_sweep=parse_datetime(row["last_retention_sweep"]) if "last_retention_sweep" in row.keys() and row["last_retention_sweep"] else None,
                    metadata=json.loads(row["metadata"]) if row["metadata"] else {},
                )

                # Attach FTS rank if present (from search)
                if "rank" in row.keys():
                    mem.metadata["fts_rank"] = float(row["rank"])

                memories.append(mem)
            except Exception as e:
                logger.error(f"Error hydrating memory {mem_id}: {e}")

        return memories

    def list_recent(self, limit: int = 10, memory_type: MemoryType | None = None) -> list[Memory]:
        """List recent memories."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            sql = "SELECT * FROM memories"
            params: list[Any] = []

            if memory_type:
                sql += " WHERE memory_type = ?"
                params.append(memory_type.name)

            sql += " ORDER BY created_at DESC LIMIT ?"
            params.append(limit)

            rows = conn.execute(sql, params).fetchall()
            return self._batch_hydrate(rows, conn)

    def list_accessed(self, limit: int = 10) -> list[Memory]:
        """List recently accessed memories."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row
            sql = "SELECT * FROM memories ORDER BY accessed_at DESC LIMIT ?"
            rows = conn.execute(sql, (limit,)).fetchall()
            return self._batch_hydrate(rows, conn)

    def get_tag_counts(self, limit: int = 10) -> list[tuple[str, int]]:
        """Get most common tags."""
        with self.pool.connection() as conn:
            return conn.execute("""
                SELECT tag, COUNT(*) as count
                FROM tags
                GROUP BY tag
                ORDER BY count DESC
                LIMIT ?
            """, (limit,)).fetchall()

    def consolidate(self) -> int:
        """Consolidate memories efficiently using SQL and Neural/Hebbian logic.
        - Strengthen frequently accessed (>5)
        - Decay rarely accessed short-term (<2) based on half-life
        - Promote important short-term to long-term (>0.8).
        """
        consolidated_count = 0
        with self.pool.connection() as conn:
            try:
                conn.execute("BEGIN IMMEDIATE")
                # 1. Neural Decay: Update neuro_score based on half-life
                # Equation: score = score * 0.5^(days_since_recall / half_life)
                # In SQL, we use a simpler linear approximation or direct update if time elapsed
                conn.execute("""
                    UPDATE memories
                    SET neuro_score = MAX(0.1, neuro_score * 0.95)
                    WHERE is_protected = 0 AND (julianday('now') - julianday(accessed_at)) > 1
                """)

                # 2. Strengthen frequently accessed
                cursor = conn.execute("""
                    UPDATE memories
                    SET importance = MIN(1.0, importance + 0.05),
                        neuro_score = MIN(1.0, neuro_score + 0.1)
                    WHERE access_count > 5
                """)
                consolidated_count += cursor.rowcount

                # 3. Decay rarely accessed short-term
                cursor = conn.execute("""
                    UPDATE memories
                    SET importance = MAX(0.0, importance - 0.1),
                        neuro_score = MAX(0.1, neuro_score - 0.05)
                    WHERE memory_type = 'SHORT_TERM' AND access_count < 2 AND is_protected = 0
                """)
                consolidated_count += cursor.rowcount

                # 4. Promote important short-term to long-term
                cursor = conn.execute("""
                    UPDATE memories
                    SET memory_type = 'LONG_TERM'
                    WHERE memory_type = 'SHORT_TERM' AND (importance > 0.8 OR neuro_score > 0.8)
                """)
                consolidated_count += cursor.rowcount
                conn.commit()
            except Exception as e:
                conn.rollback()
                logger.error(f"Consolidation failed: {e}")
                raise

        return consolidated_count

    # ==================== v14.0 Living Graph: Association Decay ==============

    def decay_associations(self, batch_size: int = 5000) -> dict[str, Any]:
        """Apply time-based decay to association strengths (v14.0 Living Graph).

        Episodic edges decay fast (30-day half-life, exponential):
            w(t) = w_prev × e^(-0.0231 × Δt_days)

        Semantic edges decay slow (power-law, long tail):
            w(t) = w_0 × (1 + 0.1 × Δt_days)^(-0.5)

        Associations with w(t) < 0.05 are pruned.

        Returns:
            Dict with decay stats.
        """
        import math

        now_iso = datetime.now().isoformat()
        decayed = 0
        pruned = 0
        total = 0

        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row

            # Process in batches for large association tables
            rows = conn.execute(
                """SELECT source_id, target_id, strength,
                          COALESCE(edge_type, 'semantic') as edge_type,
                          created_at
                   FROM associations
                   WHERE strength >= 0.05
                   ORDER BY ROWID
                   LIMIT ?""",
                (batch_size,),
            ).fetchall()

            total = len(rows)
            updates: list[tuple[float, str, str, str]] = []
            prune_pairs: list[tuple[str, str]] = []

            for row in rows:
                old_strength = row["strength"]
                edge_type = row["edge_type"]
                created_at = row["created_at"]

                if not created_at:
                    continue

                try:
                    created = datetime.fromisoformat(created_at)
                    days_old = max(0.0, (datetime.now() - created).total_seconds() / 86400.0)
                except Exception:
                    continue

                if days_old < 0.5:
                    continue  # Skip very recent edges

                # Compute decayed strength
                if edge_type == "episodic":
                    # Exponential decay: 30-day half-life
                    new_strength = old_strength * math.exp(-0.0231 * days_old)
                else:
                    # Semantic: power-law (long tail)
                    new_strength = old_strength * ((1.0 + 0.1 * days_old) ** -0.5)

                new_strength = max(0.0, new_strength)

                if new_strength < 0.05:
                    prune_pairs.append((row["source_id"], row["target_id"]))
                    pruned += 1
                elif abs(new_strength - old_strength) > 0.001:
                    updates.append((new_strength, now_iso, row["source_id"], row["target_id"]))
                    decayed += 1

            # Apply updates
            if updates:
                with conn:
                    conn.executemany(
                        """UPDATE associations
                           SET strength = ?, last_traversed_at = COALESCE(last_traversed_at, ?)
                           WHERE source_id = ? AND target_id = ?""",
                        updates,
                    )

            # Prune dead edges
            if prune_pairs:
                with conn:
                    conn.executemany(
                        "DELETE FROM associations WHERE source_id = ? AND target_id = ?",
                        prune_pairs,
                    )

        result = {
            "status": "success",
            "associations_evaluated": total,
            "associations_decayed": decayed,
            "associations_pruned": pruned,
        }
        if total > 0:
            logger.info(
                f"🔗 Association decay: {decayed} decayed, {pruned} pruned out of {total} evaluated",
            )
        return result

    # ------------------------------------------------------------------
    # Constellation Membership (v14.3 — Recall Boost)
    # ------------------------------------------------------------------

    def update_constellation_membership(
        self,
        memberships: list[tuple[str, str, float]],
    ) -> int:
        """Bulk-update constellation membership for memories.

        Args:
            memberships: List of (memory_id, constellation_name, confidence) tuples.

        Returns:
            Number of rows upserted.
        """
        if not memberships:
            return 0
        ts = datetime.now().isoformat()
        with self.pool.connection() as conn:
            with conn:
                conn.executemany(
                    """INSERT OR REPLACE INTO constellation_membership
                       (memory_id, constellation_name, membership_confidence, updated_at)
                       VALUES (?, ?, ?, ?)""",
                    [(mid, name, conf, ts) for mid, name, conf in memberships],
                )
        return len(memberships)

    def get_constellation_membership(self, memory_id: str) -> dict[str, Any] | None:
        """Get the constellation membership for a single memory.

        Returns dict with constellation_name, membership_confidence, or None.
        """
        with self.pool.connection() as conn:
            row = conn.execute(
                "SELECT constellation_name, membership_confidence FROM constellation_membership WHERE memory_id = ?",
                (memory_id,),
            ).fetchone()
            if row:
                return {"constellation_name": row[0], "membership_confidence": row[1]}
        return None

    def get_constellation_members(self, constellation_name: str) -> list[str]:
        """Get all memory IDs belonging to a constellation."""
        with self.pool.connection() as conn:
            rows = conn.execute(
                "SELECT memory_id FROM constellation_membership WHERE constellation_name = ?",
                (constellation_name,),
            ).fetchall()
            return [r[0] for r in rows]

    def prune_associations(self, min_strength: float = 0.3) -> dict[str, Any]:
        """Delete weak association edges below min_strength threshold.

        Returns:
            Dict with pruning stats (total, pruned, remaining, db_size_delta).
        """
        with self.pool.connection() as conn:
            total = conn.execute("SELECT COUNT(*) FROM associations").fetchone()[0]

            # Get DB size before
            db_path = self.pool._db_path if hasattr(self.pool, "_db_path") else None
            size_before = 0
            if db_path:
                from pathlib import Path
                p = Path(db_path)
                size_before = p.stat().st_size if p.exists() else 0

            # Count orphaned associations (referencing non-existent memories)
            orphaned = conn.execute("""
                SELECT COUNT(*) FROM associations
                WHERE source_id NOT IN (SELECT id FROM memories)
                   OR target_id NOT IN (SELECT id FROM memories)
            """).fetchone()[0]

            with conn:
                # Delete orphaned associations
                if orphaned > 0:
                    conn.execute("""
                        DELETE FROM associations
                        WHERE source_id NOT IN (SELECT id FROM memories)
                           OR target_id NOT IN (SELECT id FROM memories)
                    """)

                # Delete weak associations
                cursor = conn.execute(
                    "DELETE FROM associations WHERE strength < ?",
                    (min_strength,),
                )
                weak_pruned = cursor.rowcount

            remaining = conn.execute("SELECT COUNT(*) FROM associations").fetchone()[0]

            # VACUUM to reclaim space
            try:
                conn.execute("VACUUM")
            except Exception:
                pass

            size_after = 0
            if db_path:
                from pathlib import Path
                p = Path(db_path)
                size_after = p.stat().st_size if p.exists() else 0

        result = {
            "status": "success",
            "total_before": total,
            "orphaned_pruned": orphaned,
            "weak_pruned": weak_pruned,
            "remaining": remaining,
            "size_before_mb": round(size_before / 1048576, 2),
            "size_after_mb": round(size_after / 1048576, 2),
            "size_delta_mb": round((size_before - size_after) / 1048576, 2),
        }
        logger.info(
            f"✂️ Association pruning: {orphaned} orphaned + {weak_pruned} weak "
            f"(< {min_strength}) pruned. {total} → {remaining}",
        )
        return result

    def get_tag_stats(self) -> list[tuple[str, int]]:
        """Get tag frequency distribution (tag, count) sorted descending."""
        with self.pool.connection() as conn:
            rows = conn.execute(
                "SELECT tag, COUNT(*) as cnt FROM tags GROUP BY tag ORDER BY cnt DESC"
            ).fetchall()
            return [(row[0], row[1]) for row in rows]

    def rename_tag(self, old_tag: str, new_tag: str) -> int:
        """Rename a tag across all memories. Returns count of updated rows."""
        with self.pool.connection() as conn:
            with conn:
                # Delete conflicts where memory already has new_tag
                conn.execute("""
                    DELETE FROM tags WHERE tag = ? AND memory_id IN (
                        SELECT memory_id FROM tags WHERE tag = ?
                    )
                """, (old_tag, new_tag))
                # Rename remaining
                cursor = conn.execute(
                    "UPDATE tags SET tag = ? WHERE tag = ?",
                    (new_tag, old_tag),
                )
                count = cursor.rowcount
                # Update FTS
                conn.execute("""
                    UPDATE memories_fts SET tags_text = (
                        SELECT GROUP_CONCAT(tag, ' ') FROM tags WHERE tags.memory_id = memories_fts.id
                    ) WHERE id IN (SELECT memory_id FROM tags WHERE tag = ?)
                """, (new_tag,))
                return count

    def delete_tag(self, tag: str) -> int:
        """Delete a tag from all memories. Returns count of deleted rows."""
        with self.pool.connection() as conn:
            with conn:
                cursor = conn.execute("DELETE FROM tags WHERE tag = ?", (tag,))
                return cursor.rowcount

    def hebbian_strengthen(self, source_id: str, target_id: str) -> None:
        """Hebbian strengthening: co-accessed memories get stronger edges.

        w_new = min(1.0, w_old + 0.05 × (1.0 - w_old))

        Called when two memories are recalled in close temporal proximity.
        """
        try:
            with self.pool.connection() as conn:
                with conn:
                    conn.execute(
                        """UPDATE associations
                           SET strength = MIN(1.0, strength + 0.05 * (1.0 - strength)),
                               traversal_count = COALESCE(traversal_count, 0) + 1,
                               last_traversed_at = ?
                           WHERE source_id = ? AND target_id = ?""",
                        (datetime.now().isoformat(), source_id, target_id),
                    )
        except Exception:
            pass  # Non-critical

    # ==================== Dharma Audit Methods (Phase 4) ====================

    def log_dharma_audit(
        self,
        action: str,
        ethical_score: float | None = None,
        harmony_score: float | None = None,
        consent_level: str | None = None,
        boundary_type: str | None = None,
        concerns: list[str] | None = None,
        context: dict[str, Any] | None = None,
        decision: str | None = None,
    ) -> int:
        """Log a Dharma audit entry for ethical tracking."""
        with self.pool.connection() as conn:
            cursor = conn.execute("""
                INSERT INTO dharma_audit (
                    timestamp, action, ethical_score, harmony_score,
                    consent_level, boundary_type, concerns, context, decision
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                datetime.now().isoformat(),
                action,
                ethical_score,
                harmony_score,
                consent_level,
                boundary_type,
                json.dumps(concerns) if concerns else None,
                json.dumps(context) if context else None,
                decision,
            ))
            return cursor.lastrowid or 0

    def get_dharma_audit_log(self, limit: int = 50, since: datetime | None = None) -> list[dict[str, Any]]:
        """Retrieve Dharma audit log entries."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row

            if since:
                rows = conn.execute("""
                    SELECT * FROM dharma_audit
                    WHERE timestamp >= ?
                    ORDER BY timestamp DESC LIMIT ?
                """, (since.isoformat(), limit)).fetchall()
            else:
                rows = conn.execute("""
                    SELECT * FROM dharma_audit
                    ORDER BY timestamp DESC LIMIT ?
                """, (limit,)).fetchall()

            return [
                {
                    "id": row["id"],
                    "timestamp": row["timestamp"],
                    "action": row["action"],
                    "ethical_score": row["ethical_score"],
                    "harmony_score": row["harmony_score"],
                    "consent_level": row["consent_level"],
                    "boundary_type": row["boundary_type"],
                    "concerns": json.loads(row["concerns"]) if row["concerns"] else [],
                    "context": json.loads(row["context"]) if row["context"] else {},
                    "decision": row["decision"],
                }
                for row in rows
            ]

    def get_dharma_stats(self) -> dict[str, Any]:
        """Get Dharma audit statistics."""
        with self.pool.connection() as conn:
            conn.row_factory = sqlite3.Row

            # Total entries
            total = conn.execute("SELECT COUNT(*) as c FROM dharma_audit").fetchone()["c"]

            # Average scores
            scores = conn.execute("""
                SELECT
                    AVG(ethical_score) as avg_ethical,
                    AVG(harmony_score) as avg_harmony,
                    MIN(ethical_score) as min_ethical,
                    MAX(ethical_score) as max_ethical
                FROM dharma_audit
                WHERE ethical_score IS NOT NULL
            """).fetchone()

            # Consent breakdown
            consent_counts = conn.execute("""
                SELECT consent_level, COUNT(*) as c
                FROM dharma_audit
                WHERE consent_level IS NOT NULL
                GROUP BY consent_level
            """).fetchall()

            return {
                "total_audits": total,
                "avg_ethical_score": scores["avg_ethical"],
                "avg_harmony_score": scores["avg_harmony"],
                "min_ethical_score": scores["min_ethical"],
                "max_ethical_score": scores["max_ethical"],
                "consent_breakdown": {row["consent_level"]: row["c"] for row in consent_counts},
            }
