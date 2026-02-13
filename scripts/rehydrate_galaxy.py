#!/usr/bin/env python3
"""
Rehydrate Galaxy — Ingest all archive databases into the active WhiteMagic DB.

Sources:
  1. Hot archive:   ~/Desktop/whitemagic_memory_archive/whitemagic_hot.db
  2. Pre-merge:     ~/Desktop/wm_archive/phase6_dead_files/primary_db_pre_merge.db
  3. Cold archive:  ~/Desktop/whitemagic_memory_archive/whitemagic_cold.db

Target:
  ~/.whitemagic/memory/whitemagic.db

Usage:
    python scripts/rehydrate_galaxy.py                    # Hot + pre-merge only (recommended first)
    python scripts/rehydrate_galaxy.py --include-cold      # All three sources
    python scripts/rehydrate_galaxy.py --dry-run            # Preview only
    python scripts/rehydrate_galaxy.py --source hot         # Just hot archive
    python scripts/rehydrate_galaxy.py --source pre-merge   # Just pre-merge
    python scripts/rehydrate_galaxy.py --source cold        # Just cold archive
"""

import argparse
import sqlite3
import time
import os
from pathlib import Path

# Paths
HOME = Path.home()
ACTIVE_DB = HOME / ".whitemagic" / "memory" / "whitemagic.db"
HOT_DB = HOME / "Desktop" / "whitemagic_memory_archive" / "whitemagic_hot.db"
COLD_DB = HOME / "Desktop" / "whitemagic_memory_archive" / "whitemagic_cold.db"
PRE_MERGE_DB = HOME / "Desktop" / "wm_archive" / "phase6_dead_files" / "primary_db_pre_merge.db"

# Shared columns between active and archive memories tables
ACTIVE_MEMORY_COLS = [
    "id", "content", "memory_type", "created_at", "updated_at", "accessed_at",
    "access_count", "emotional_valence", "importance", "neuro_score",
    "novelty_score", "recall_count", "half_life_days", "is_protected",
    "metadata", "title", "galactic_distance", "retention_score",
    "last_retention_sweep", "content_hash", "event_time", "ingestion_time",
    "is_private", "model_exclude"
]

ACTIVE_ASSOC_COLS = [
    "source_id", "target_id", "strength", "last_traversed_at",
    "traversal_count", "created_at", "direction", "relation_type",
    "edge_type", "valid_from", "valid_until", "ingestion_time"
]

HOLO_COLS = ["memory_id", "x", "y", "z", "w", "v"]
EMBED_COLS = ["memory_id", "embedding", "model", "created_at"]
TAG_COLS = ["memory_id", "tag"]


def get_source_columns(conn, table):
    """Get column names from a source table."""
    cursor = conn.execute(f"PRAGMA table_info({table})")
    return [row[1] for row in cursor.fetchall()]


def safe_columns(source_cols, target_cols):
    """Return intersection of source and target columns, preserving target order."""
    source_set = set(source_cols)
    return [c for c in target_cols if c in source_set]


def count_table(conn, table):
    """Count rows in a table."""
    try:
        return conn.execute(f"SELECT COUNT(*) FROM {table}").fetchone()[0]
    except sqlite3.OperationalError:
        return 0


def ingest_memories(active_conn, source_path, dry_run=False):
    """Ingest memories from source DB into active DB."""
    source_conn = sqlite3.connect(str(source_path))
    source_cols = get_source_columns(source_conn, "memories")
    cols = safe_columns(source_cols, ACTIVE_MEMORY_COLS)

    col_list = ", ".join(cols)
    placeholders = ", ".join(["?"] * len(cols))

    # Count new
    active_ids = set(
        row[0] for row in active_conn.execute("SELECT id FROM memories").fetchall()
    )
    source_rows = source_conn.execute(f"SELECT {col_list} FROM memories").fetchall()
    new_rows = [r for r in source_rows if r[cols.index("id")] not in active_ids]

    print(f"  Memories: {len(source_rows)} total, {len(new_rows)} new")

    if not dry_run and new_rows:
        active_conn.executemany(
            f"INSERT OR IGNORE INTO memories ({col_list}) VALUES ({placeholders})",
            new_rows
        )
        active_conn.commit()
        print(f"  ✅ Inserted {len(new_rows)} memories")

    source_conn.close()
    return len(new_rows)


def ingest_associations(active_conn, source_path, batch_size=50000, dry_run=False):
    """Ingest associations from source DB into active DB."""
    source_conn = sqlite3.connect(str(source_path))
    source_cols = get_source_columns(source_conn, "associations")
    cols = safe_columns(source_cols, ACTIVE_ASSOC_COLS)

    col_list = ", ".join(cols)
    placeholders = ", ".join(["?"] * len(cols))

    source_count = count_table(source_conn, "associations")
    active_count_before = count_table(active_conn, "associations")
    print(f"  Associations: {source_count} in source, {active_count_before} in active")

    if dry_run:
        source_conn.close()
        return 0

    # Stream in batches to handle millions of rows
    cursor = source_conn.execute(f"SELECT {col_list} FROM associations")
    total_inserted = 0

    while True:
        batch = cursor.fetchmany(batch_size)
        if not batch:
            break
        active_conn.executemany(
            f"INSERT OR IGNORE INTO associations ({col_list}) VALUES ({placeholders})",
            batch
        )
        active_conn.commit()
        total_inserted += len(batch)
        if total_inserted % 200000 == 0 or len(batch) < batch_size:
            print(f"    ... processed {total_inserted:,} association rows")

    active_count_after = count_table(active_conn, "associations")
    actual_new = active_count_after - active_count_before
    print(f"  ✅ Net new associations: {actual_new:,}")

    source_conn.close()
    return actual_new


def ingest_holographic_coords(active_conn, source_path, dry_run=False):
    """Ingest holographic coordinates."""
    source_conn = sqlite3.connect(str(source_path))
    try:
        source_cols = get_source_columns(source_conn, "holographic_coords")
    except Exception:
        print("  Holographic coords: table not found in source")
        source_conn.close()
        return 0

    cols = safe_columns(source_cols, HOLO_COLS)
    col_list = ", ".join(cols)
    placeholders = ", ".join(["?"] * len(cols))

    source_rows = source_conn.execute(f"SELECT {col_list} FROM holographic_coords").fetchall()
    before = count_table(active_conn, "holographic_coords")
    print(f"  Holographic coords: {len(source_rows)} in source, {before} in active")

    if not dry_run and source_rows:
        active_conn.executemany(
            f"INSERT OR IGNORE INTO holographic_coords ({col_list}) VALUES ({placeholders})",
            source_rows
        )
        active_conn.commit()

    after = count_table(active_conn, "holographic_coords")
    actual_new = after - before
    if not dry_run:
        print(f"  ✅ Net new holo coords: {actual_new}")
    source_conn.close()
    return actual_new


def ingest_embeddings(active_conn, source_path, batch_size=5000, dry_run=False):
    """Ingest memory embeddings."""
    source_conn = sqlite3.connect(str(source_path))
    try:
        source_cols = get_source_columns(source_conn, "memory_embeddings")
    except Exception:
        print("  Embeddings: table not found in source")
        source_conn.close()
        return 0

    cols = safe_columns(source_cols, EMBED_COLS)
    if not cols:
        print("  Embeddings: no compatible columns in source")
        source_conn.close()
        return 0
    col_list = ", ".join(cols)
    placeholders = ", ".join(["?"] * len(cols))

    source_count = count_table(source_conn, "memory_embeddings")
    before = count_table(active_conn, "memory_embeddings")
    print(f"  Embeddings: {source_count} in source, {before} in active")

    if dry_run:
        source_conn.close()
        return 0

    cursor = source_conn.execute(f"SELECT {col_list} FROM memory_embeddings")
    total = 0
    while True:
        batch = cursor.fetchmany(batch_size)
        if not batch:
            break
        active_conn.executemany(
            f"INSERT OR IGNORE INTO memory_embeddings ({col_list}) VALUES ({placeholders})",
            batch
        )
        active_conn.commit()
        total += len(batch)
        if total % 20000 == 0 or len(batch) < batch_size:
            print(f"    ... processed {total:,} embedding rows")

    after = count_table(active_conn, "memory_embeddings")
    actual_new = after - before
    print(f"  ✅ Net new embeddings: {actual_new:,}")
    source_conn.close()
    return actual_new


def ingest_tags(active_conn, source_path, dry_run=False):
    """Ingest tags."""
    source_conn = sqlite3.connect(str(source_path))
    try:
        source_cols = get_source_columns(source_conn, "tags")
    except Exception:
        print("  Tags: table not found in source")
        source_conn.close()
        return 0

    cols = safe_columns(source_cols, TAG_COLS)
    col_list = ", ".join(cols)
    placeholders = ", ".join(["?"] * len(cols))

    source_rows = source_conn.execute(f"SELECT {col_list} FROM tags").fetchall()
    before = count_table(active_conn, "tags")
    print(f"  Tags: {len(source_rows)} in source, {before} in active")

    if not dry_run and source_rows:
        active_conn.executemany(
            f"INSERT OR IGNORE INTO tags ({col_list}) VALUES ({placeholders})",
            source_rows
        )
        active_conn.commit()

    after = count_table(active_conn, "tags")
    actual_new = after - before
    if not dry_run:
        print(f"  ✅ Net new tags: {actual_new}")
    source_conn.close()
    return actual_new


def ingest_source(active_conn, source_path, label, dry_run=False):
    """Ingest all tables from a single source DB."""
    print(f"\n{'='*60}")
    print(f"📦 Ingesting: {label}")
    print(f"   Source: {source_path}")
    print(f"{'='*60}")

    if not source_path.exists():
        print(f"  ❌ Source not found: {source_path}")
        return

    start = time.perf_counter()

    new_mems = ingest_memories(active_conn, source_path, dry_run)
    new_assocs = ingest_associations(active_conn, source_path, dry_run=dry_run)
    new_holo = ingest_holographic_coords(active_conn, source_path, dry_run)
    new_embeds = ingest_embeddings(active_conn, source_path, dry_run=dry_run)
    new_tags = ingest_tags(active_conn, source_path, dry_run)

    elapsed = time.perf_counter() - start
    print(f"\n  ⏱  {label} done in {elapsed:.1f}s")
    print(f"  📊 +{new_mems} memories, +{new_assocs:,} assocs, +{new_holo} holo, +{new_embeds:,} embeds, +{new_tags} tags")

    return {
        "memories": new_mems,
        "associations": new_assocs,
        "holographic": new_holo,
        "embeddings": new_embeds,
        "tags": new_tags,
        "elapsed": elapsed
    }


def print_db_stats(conn, label="Active DB"):
    """Print current DB statistics."""
    print(f"\n📊 {label} Statistics:")
    for table in ["memories", "associations", "holographic_coords", "memory_embeddings", "tags"]:
        count = count_table(conn, table)
        print(f"  {table}: {count:,}")


def main():
    parser = argparse.ArgumentParser(description="Rehydrate WhiteMagic galaxy from archives")
    parser.add_argument("--dry-run", action="store_true", help="Preview only, no writes")
    parser.add_argument("--include-cold", action="store_true",
                        help="Include cold archive (105K memories, 18.7M assocs — SLOW)")
    parser.add_argument("--source", choices=["hot", "pre-merge", "cold", "all"],
                        help="Ingest only a specific source")
    args = parser.parse_args()

    print("🌌 WhiteMagic Galaxy Rehydration")
    print("=" * 60)

    if args.dry_run:
        print("🔍 DRY RUN — no changes will be written")

    if not ACTIVE_DB.exists():
        print(f"❌ Active DB not found: {ACTIVE_DB}")
        return

    # Back up active DB first
    if not args.dry_run:
        backup_path = ACTIVE_DB.with_suffix(f".db.pre-rehydrate-backup")
        if not backup_path.exists():
            import shutil
            print(f"\n💾 Backing up active DB to {backup_path.name}...")
            shutil.copy2(ACTIVE_DB, backup_path)
            print(f"  ✅ Backup created ({backup_path.stat().st_size / 1024 / 1024:.1f} MB)")

    active_conn = sqlite3.connect(str(ACTIVE_DB))
    active_conn.execute("PRAGMA journal_mode=WAL")
    active_conn.execute("PRAGMA synchronous=NORMAL")
    active_conn.execute("PRAGMA cache_size=-64000")  # 64MB cache

    print_db_stats(active_conn, "Active DB (before)")

    sources = []
    if args.source:
        if args.source == "hot":
            sources = [("Hot Archive", HOT_DB)]
        elif args.source == "pre-merge":
            sources = [("Pre-Merge", PRE_MERGE_DB)]
        elif args.source == "cold":
            sources = [("Cold Archive", COLD_DB)]
        elif args.source == "all":
            sources = [
                ("Hot Archive", HOT_DB),
                ("Pre-Merge", PRE_MERGE_DB),
                ("Cold Archive", COLD_DB),
            ]
    else:
        # Default: hot + pre-merge (safe, fast)
        sources = [("Hot Archive", HOT_DB), ("Pre-Merge", PRE_MERGE_DB)]
        if args.include_cold:
            sources.append(("Cold Archive", COLD_DB))

    total_start = time.perf_counter()
    results = {}
    for label, path in sources:
        results[label] = ingest_source(active_conn, path, label, args.dry_run)

    # Rebuild FTS index
    if not args.dry_run:
        print("\n🔄 Rebuilding FTS index...")
        try:
            active_conn.execute("INSERT INTO memories_fts(memories_fts) VALUES('rebuild')")
            active_conn.commit()
            print("  ✅ FTS index rebuilt")
        except Exception as e:
            print(f"  ⚠ FTS rebuild: {e}")

    print_db_stats(active_conn, "Active DB (after)")

    total_elapsed = time.perf_counter() - total_start
    print(f"\n🎉 Rehydration complete in {total_elapsed:.1f}s")

    if args.dry_run:
        print("\n💡 Run without --dry-run to apply changes")

    active_conn.close()


if __name__ == "__main__":
    main()
