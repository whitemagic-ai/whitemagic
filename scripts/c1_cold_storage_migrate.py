#!/usr/bin/env python3
"""
C1: Migrate deep_archive + scavenged memories to cold storage DB.

Hot DB: LONG_TERM, SHORT_TERM, PATTERN, protected memories
Cold DB: deep_archive, scavenged (105K memories, ~5GB)

The cold DB has identical schema. Memories are MOVED (deleted from hot
after confirmed insertion in cold). Tags, associations, holographic
coords, and FTS entries are all migrated.
"""
import sqlite3
import os
import sys
import time

DB_PATH = os.path.expanduser("~/.whitemagic/memory/whitemagic.db")
COLD_DB_PATH = os.path.expanduser("~/.whitemagic/memory/whitemagic_cold.db")

COLD_TYPES = ("deep_archive", "scavenged")
BATCH_SIZE = 2000


def init_cold_db(cold_conn):
    """Create schema in cold DB (mirrors hot DB)."""
    cold_conn.execute("PRAGMA journal_mode=WAL")
    cold_conn.execute("PRAGMA synchronous=NORMAL")

    cold_conn.execute("""
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
            title TEXT,
            galactic_distance REAL DEFAULT 0.0,
            retention_score REAL DEFAULT 0.5,
            last_retention_sweep TEXT,
            gana_processed INTEGER DEFAULT 0,
            gana_context TEXT,
            quadrant TEXT
        )
    """)
    cold_conn.execute("""
        CREATE TABLE IF NOT EXISTS tags (
            memory_id TEXT,
            tag TEXT,
            PRIMARY KEY (memory_id, tag)
        )
    """)
    cold_conn.execute("""
        CREATE TABLE IF NOT EXISTS associations (
            source_id TEXT,
            target_id TEXT,
            strength REAL,
            PRIMARY KEY (source_id, target_id)
        )
    """)
    cold_conn.execute("""
        CREATE TABLE IF NOT EXISTS holographic_coords (
            memory_id TEXT PRIMARY KEY,
            x REAL, y REAL, z REAL, w REAL, v REAL DEFAULT 0.5
        )
    """)
    cold_conn.execute("""
        CREATE VIRTUAL TABLE IF NOT EXISTS memories_fts USING fts5(
            id, title, content, tags_text
        )
    """)
    cold_conn.execute("CREATE INDEX IF NOT EXISTS idx_tags_memory ON tags(memory_id)")
    cold_conn.execute("CREATE INDEX IF NOT EXISTS idx_tags_tag ON tags(tag)")
    cold_conn.execute("CREATE INDEX IF NOT EXISTS idx_assoc_source ON associations(source_id)")
    cold_conn.execute("CREATE INDEX IF NOT EXISTS idx_mem_type ON memories(memory_type)")
    cold_conn.execute("CREATE INDEX IF NOT EXISTS idx_mem_importance ON memories(importance)")
    cold_conn.commit()


def main():
    if not os.path.exists(DB_PATH):
        print(f"ERROR: Hot DB not found at {DB_PATH}")
        sys.exit(1)

    hot_size_before = os.path.getsize(DB_PATH)
    print(f"Hot DB: {DB_PATH} ({hot_size_before / 1e9:.2f} GB)")
    print(f"Cold DB: {COLD_DB_PATH}")

    hot = sqlite3.connect(DB_PATH)
    hot.execute("PRAGMA journal_mode=WAL")
    hot.execute("PRAGMA synchronous=NORMAL")

    cold = sqlite3.connect(COLD_DB_PATH)
    init_cold_db(cold)

    # Count candidates (exclude protected)
    cur = hot.execute("""
        SELECT COUNT(*) FROM memories
        WHERE memory_type IN (?, ?) AND is_protected = 0
    """, COLD_TYPES)
    total = cur.fetchone()[0]
    print(f"\nMemories to migrate: {total:,} ({', '.join(COLD_TYPES)})")

    if total == 0:
        print("Nothing to migrate.")
        hot.close()
        cold.close()
        return

    t0 = time.perf_counter()
    migrated = 0

    # Get column names
    cur = hot.execute("PRAGMA table_info(memories)")
    mem_columns = [row[1] for row in cur.fetchall()]
    col_list = ", ".join(mem_columns)
    placeholders = ", ".join("?" * len(mem_columns))

    while True:
        # Fetch a batch of IDs
        cur = hot.execute(f"""
            SELECT id FROM memories
            WHERE memory_type IN (?, ?) AND is_protected = 0
            LIMIT {BATCH_SIZE}
        """, COLD_TYPES)
        batch_ids = [row[0] for row in cur.fetchall()]

        if not batch_ids:
            break

        id_placeholders = ", ".join("?" * len(batch_ids))

        # 1. Copy memories
        rows = hot.execute(
            f"SELECT {col_list} FROM memories WHERE id IN ({id_placeholders})",
            batch_ids,
        ).fetchall()
        cold.executemany(
            f"INSERT OR REPLACE INTO memories ({col_list}) VALUES ({placeholders})",
            rows,
        )

        # 2. Copy tags
        tag_rows = hot.execute(
            f"SELECT memory_id, tag FROM tags WHERE memory_id IN ({id_placeholders})",
            batch_ids,
        ).fetchall()
        if tag_rows:
            cold.executemany(
                "INSERT OR REPLACE INTO tags (memory_id, tag) VALUES (?, ?)",
                tag_rows,
            )

        # 3. Copy associations (where source is in batch)
        assoc_rows = hot.execute(
            f"SELECT source_id, target_id, strength FROM associations WHERE source_id IN ({id_placeholders})",
            batch_ids,
        ).fetchall()
        if assoc_rows:
            cold.executemany(
                "INSERT OR REPLACE INTO associations (source_id, target_id, strength) VALUES (?, ?, ?)",
                assoc_rows,
            )

        # 4. Copy holographic coords
        coord_rows = hot.execute(
            f"SELECT memory_id, x, y, z, w, COALESCE(v, 0.5) FROM holographic_coords WHERE memory_id IN ({id_placeholders})",
            batch_ids,
        ).fetchall()
        if coord_rows:
            cold.executemany(
                "INSERT OR REPLACE INTO holographic_coords (memory_id, x, y, z, w, v) VALUES (?, ?, ?, ?, ?, ?)",
                coord_rows,
            )

        # 5. Copy FTS entries
        fts_rows = hot.execute(
            f"SELECT id, title, content, tags_text FROM memories_fts WHERE id IN ({id_placeholders})",
            batch_ids,
        ).fetchall()
        if fts_rows:
            cold.executemany(
                "INSERT OR REPLACE INTO memories_fts (id, title, content, tags_text) VALUES (?, ?, ?, ?)",
                fts_rows,
            )

        cold.commit()

        # 6. Delete from hot DB
        hot.execute(f"DELETE FROM memories_fts WHERE id IN ({id_placeholders})", batch_ids)
        hot.execute(f"DELETE FROM holographic_coords WHERE memory_id IN ({id_placeholders})", batch_ids)
        hot.execute(f"DELETE FROM tags WHERE memory_id IN ({id_placeholders})", batch_ids)
        hot.execute(f"DELETE FROM associations WHERE source_id IN ({id_placeholders})", batch_ids)
        hot.execute(f"DELETE FROM memories WHERE id IN ({id_placeholders})", batch_ids)
        hot.commit()

        migrated += len(batch_ids)
        elapsed = time.perf_counter() - t0
        rate = migrated / elapsed if elapsed > 0 else 0
        print(f"  {migrated:,}/{total:,} migrated ({rate:.0f}/s)")

    elapsed = time.perf_counter() - t0
    print(f"\nMigration complete: {migrated:,} memories in {elapsed:.1f}s")

    # Verify
    print("\n=== Verification ===")
    for label, conn, path in [("Hot", hot, DB_PATH), ("Cold", cold, COLD_DB_PATH)]:
        cur = conn.execute("SELECT memory_type, COUNT(*) FROM memories GROUP BY memory_type ORDER BY COUNT(*) DESC")
        types = cur.fetchall()
        total_count = sum(r[1] for r in types)
        print(f"  {label} DB ({path}):")
        print(f"    Total: {total_count:,}")
        for mtype, cnt in types:
            print(f"    {mtype}: {cnt:,}")

    # VACUUM hot DB to reclaim space
    print("\n=== VACUUM hot DB ===")
    t0 = time.perf_counter()
    hot.execute("VACUUM")
    t1 = time.perf_counter()
    print(f"  VACUUM time: {t1 - t0:.1f}s")

    hot.close()
    cold.close()

    hot_size_after = os.path.getsize(DB_PATH)
    cold_size = os.path.getsize(COLD_DB_PATH)
    print(f"\n  Hot DB: {hot_size_before / 1e9:.2f} GB → {hot_size_after / 1e6:.0f} MB")
    print(f"  Cold DB: {cold_size / 1e6:.0f} MB")
    print(f"  Total saved from hot: {(hot_size_before - hot_size_after) / 1e9:.2f} GB")
    print("\n✓ C1 cold storage migration complete")


if __name__ == "__main__":
    main()
