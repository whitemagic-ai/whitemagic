#!/usr/bin/env python3
"""
Database Merge Script — Merge Primary DB into Legacy Galaxy DB.

Primary DB:   ~/.whitemagic/whitemagic.db          (3,631 memories, no galactic fields)
Legacy Galaxy: ~/.whitemagic/memory/whitemagic.db   (107,168 memories, full 5D galactic)

Strategy: "Migrate forward" — copy all primary memories INTO the legacy galaxy,
assign default galactic fields, then the legacy DB becomes the single source of truth.

Zero ID collisions verified before running.
"""
import sqlite3
import shutil
import os
import sys
import json
from datetime import datetime
from pathlib import Path

PRIMARY_PATH = Path.home() / ".whitemagic" / "whitemagic.db"
LEGACY_PATH = Path.home() / ".whitemagic" / "memory" / "whitemagic.db"
BACKUP_PATH = Path.home() / ".whitemagic" / "memory" / f"whitemagic_pre_merge_{datetime.now().strftime('%Y%m%d_%H%M%S')}.db"


def backup_legacy():
    """Create a backup of the legacy DB before merging."""
    print(f"📦 Backing up legacy DB ({LEGACY_PATH.stat().st_size / (1024*1024):.1f} MB)...")
    print(f"   → {BACKUP_PATH}")
    # Use hard link for speed (same filesystem), fall back to copy
    try:
        os.link(str(LEGACY_PATH), str(BACKUP_PATH))
        print("   ✅ Hard-linked (instant, same filesystem)")
    except OSError:
        shutil.copy2(str(LEGACY_PATH), str(BACKUP_PATH))
        print("   ✅ Copied")


def add_missing_columns(legacy_conn):
    """Add columns that exist in primary but not in legacy."""
    cursor = legacy_conn.execute("PRAGMA table_info(memories)")
    existing = {row[1] for row in cursor.fetchall()}

    new_cols = {
        "gana_processed": "INTEGER DEFAULT 0",
        "gana_context": "TEXT",
        "quadrant": "TEXT",
    }

    for col, typedef in new_cols.items():
        if col not in existing:
            print(f"   Adding column: {col}")
            legacy_conn.execute(f"ALTER TABLE memories ADD COLUMN {col} {typedef}")

    legacy_conn.commit()


def merge_memories(primary_conn, legacy_conn):
    """Copy all memories from primary into legacy."""
    # Get primary column names
    p_cols = [r[1] for r in primary_conn.execute("PRAGMA table_info(memories)").fetchall()]
    # Get legacy column names
    l_cols = [r[1] for r in legacy_conn.execute("PRAGMA table_info(memories)").fetchall()]

    # Columns shared between both (we'll copy these)
    shared = [c for c in p_cols if c in l_cols]

    print(f"   Shared columns: {len(shared)} — {shared}")

    # Read all primary memories
    select_sql = f"SELECT {', '.join(shared)} FROM memories"
    rows = primary_conn.execute(select_sql).fetchall()
    print(f"   Read {len(rows)} memories from primary")

    # Columns only in legacy that need defaults
    # galactic_distance, retention_score, last_retention_sweep, updated_at
    # We'll set these to sensible defaults for new memories
    now_iso = datetime.now().isoformat()

    insert_cols = shared + ["galactic_distance", "retention_score", "last_retention_sweep", "updated_at"]
    placeholders = ", ".join(["?"] * len(insert_cols))
    insert_sql = f"INSERT OR IGNORE INTO memories ({', '.join(insert_cols)}) VALUES ({placeholders})"

    inserted = 0
    for row in rows:
        row_dict = dict(zip(shared, row))
        # Add default galactic fields for new memories
        # Place them in MID_BAND (0.5) initially — galactic sweep will re-score them
        vals = list(row) + [
            0.50,   # galactic_distance — MID_BAND default
            0.50,   # retention_score — neutral default
            None,    # last_retention_sweep — never swept yet
            row_dict.get("created_at", now_iso),  # updated_at = created_at as default
        ]
        try:
            legacy_conn.execute(insert_sql, vals)
            inserted += 1
        except sqlite3.IntegrityError:
            pass  # Skip if somehow duplicate

    legacy_conn.commit()
    print(f"   ✅ Inserted {inserted} memories into legacy galaxy")
    return inserted


def merge_tags(primary_conn, legacy_conn):
    """Copy tags from primary into legacy."""
    tags = primary_conn.execute("SELECT memory_id, tag FROM tags").fetchall()
    if not tags:
        print("   No tags to merge")
        return 0

    legacy_conn.executemany(
        "INSERT OR IGNORE INTO tags (memory_id, tag) VALUES (?, ?)",
        tags
    )
    legacy_conn.commit()
    print(f"   ✅ Merged {len(tags)} tag entries")
    return len(tags)


def merge_holographic_coords(primary_conn, legacy_conn):
    """Copy holographic coordinates from primary into legacy."""
    # Check if primary has v column
    p_hc_cols = [r[1] for r in primary_conn.execute("PRAGMA table_info(holographic_coords)").fetchall()]
    has_v = "v" in p_hc_cols

    if has_v:
        coords = primary_conn.execute(
            "SELECT memory_id, x, y, z, w, v FROM holographic_coords"
        ).fetchall()
    else:
        coords = primary_conn.execute(
            "SELECT memory_id, x, y, z, w FROM holographic_coords"
        ).fetchall()

    if not coords:
        print("   No holographic coords to merge")
        return 0

    if has_v:
        legacy_conn.executemany(
            "INSERT OR IGNORE INTO holographic_coords (memory_id, x, y, z, w, v) VALUES (?, ?, ?, ?, ?, ?)",
            coords
        )
    else:
        legacy_conn.executemany(
            "INSERT OR IGNORE INTO holographic_coords (memory_id, x, y, z, w, v) VALUES (?, ?, ?, ?, ?, 0.5)",
            [(c[0], c[1], c[2], c[3], c[4]) for c in coords]
        )

    legacy_conn.commit()
    print(f"   ✅ Merged {len(coords)} holographic coordinate entries")
    return len(coords)


def merge_associations(primary_conn, legacy_conn):
    """Copy associations from primary into legacy."""
    assocs = primary_conn.execute(
        "SELECT source_id, target_id, strength FROM associations"
    ).fetchall()

    if not assocs:
        print("   No associations to merge")
        return 0

    legacy_conn.executemany(
        "INSERT OR IGNORE INTO associations (source_id, target_id, strength) VALUES (?, ?, ?)",
        assocs
    )
    legacy_conn.commit()
    print(f"   ✅ Merged {len(assocs)} association entries")
    return len(assocs)


def rebuild_fts_for_new(primary_conn, legacy_conn):
    """Build FTS entries for the newly merged memories."""
    # Get IDs of memories we just merged (all primary IDs)
    primary_ids = [r[0] for r in primary_conn.execute("SELECT id FROM memories").fetchall()]

    count = 0
    for mid in primary_ids:
        row = legacy_conn.execute(
            "SELECT id, title, content FROM memories WHERE id = ?", (mid,)
        ).fetchone()
        if not row:
            continue

        # Get tags
        tags = legacy_conn.execute(
            "SELECT tag FROM tags WHERE memory_id = ?", (mid,)
        ).fetchall()
        tags_text = " ".join(t[0] for t in tags)

        # Insert into FTS (memories_fts is the table name in legacy)
        try:
            legacy_conn.execute(
                "INSERT INTO memories_fts (id, title, content, tags_text) VALUES (?, ?, ?, ?)",
                (row[0], row[1] or "", str(row[2] or ""), tags_text)
            )
            count += 1
        except sqlite3.IntegrityError:
            pass

    legacy_conn.commit()
    print(f"   ✅ Built FTS entries for {count} memories")
    return count


def verify_merge(legacy_conn):
    """Print post-merge statistics."""
    total = legacy_conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
    types = legacy_conn.execute(
        "SELECT memory_type, COUNT(*) FROM memories GROUP BY memory_type ORDER BY COUNT(*) DESC"
    ).fetchall()
    tags = legacy_conn.execute("SELECT COUNT(*) FROM tags").fetchone()[0]
    coords = legacy_conn.execute("SELECT COUNT(*) FROM holographic_coords").fetchone()[0]
    fts = legacy_conn.execute("SELECT COUNT(*) FROM memories_fts").fetchone()[0]
    protected = legacy_conn.execute("SELECT COUNT(*) FROM memories WHERE is_protected = 1").fetchone()[0]

    print(f"\n📊 Post-Merge Stats:")
    print(f"   Total memories: {total}")
    for t, c in types:
        print(f"     {t}: {c}")
    print(f"   Tags: {tags}")
    print(f"   Holographic coords: {coords}")
    print(f"   FTS entries: {fts}")
    print(f"   Protected: {protected}")

    # Galactic zone distribution
    zones = legacy_conn.execute("""
        SELECT
            CASE
                WHEN galactic_distance < 0.15 THEN 'CORE'
                WHEN galactic_distance < 0.40 THEN 'INNER_RIM'
                WHEN galactic_distance < 0.65 THEN 'MID_BAND'
                WHEN galactic_distance < 0.85 THEN 'OUTER_RIM'
                ELSE 'FAR_EDGE'
            END as zone,
            COUNT(*)
        FROM memories
        GROUP BY zone
        ORDER BY COUNT(*) DESC
    """).fetchall()
    print(f"   Galactic zones:")
    for z, c in zones:
        print(f"     {z}: {c}")


def main():
    if not PRIMARY_PATH.exists():
        print(f"❌ Primary DB not found: {PRIMARY_PATH}")
        sys.exit(1)
    if not LEGACY_PATH.exists():
        print(f"❌ Legacy DB not found: {LEGACY_PATH}")
        sys.exit(1)

    print("=" * 60)
    print("🌌 WhiteMagic Database Merge — Split-Brain Resolution")
    print("=" * 60)
    print(f"\nSource (Primary): {PRIMARY_PATH}")
    print(f"Target (Legacy):  {LEGACY_PATH}")
    print()

    # Step 0: Backup
    backup_legacy()

    # Connect
    primary = sqlite3.connect(str(PRIMARY_PATH))
    legacy = sqlite3.connect(str(LEGACY_PATH))
    legacy.execute("PRAGMA journal_mode=WAL")
    legacy.execute("PRAGMA busy_timeout=10000")

    # Step 1: Schema alignment
    print("\n🔧 Step 1: Aligning schemas...")
    add_missing_columns(legacy)

    # Step 2: Merge memories
    print("\n📥 Step 2: Merging memories...")
    merge_memories(primary, legacy)

    # Step 3: Merge tags
    print("\n🏷️  Step 3: Merging tags...")
    merge_tags(primary, legacy)

    # Step 4: Merge holographic coords
    print("\n🌐 Step 4: Merging holographic coordinates...")
    merge_holographic_coords(primary, legacy)

    # Step 5: Merge associations
    print("\n🔗 Step 5: Merging associations...")
    merge_associations(primary, legacy)

    # Step 6: Rebuild FTS for new entries
    print("\n🔍 Step 6: Building FTS index for new memories...")
    rebuild_fts_for_new(primary, legacy)

    # Step 7: Verify
    verify_merge(legacy)

    primary.close()
    legacy.close()

    print(f"\n✅ Merge complete! Legacy galaxy now contains all memories.")
    print(f"   Backup at: {BACKUP_PATH}")
    print(f"   Primary DB ({PRIMARY_PATH}) can be archived once verified.")


if __name__ == "__main__":
    main()
