#!/usr/bin/env python3
"""
Rehydrate Cold Archive — SQL-native ingestion for the massive cold DB.

Uses ATTACH DATABASE + INSERT OR IGNORE directly in SQL,
avoiding loading 105K+ memories into Python memory.

Usage:
    python scripts/rehydrate_cold.py
    python scripts/rehydrate_cold.py --dry-run
"""

import argparse
import sqlite3
import time
import shutil
from pathlib import Path

HOME = Path.home()
ACTIVE_DB = HOME / ".whitemagic" / "memory" / "whitemagic.db"
COLD_DB = HOME / "Desktop" / "whitemagic_memory_archive" / "whitemagic_cold.db"


def count(conn, table, prefix="main"):
    try:
        return conn.execute(f"SELECT COUNT(*) FROM {prefix}.{table}").fetchone()[0]
    except sqlite3.OperationalError:
        return 0


def main():
    parser = argparse.ArgumentParser(description="Rehydrate cold archive via SQL ATTACH")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    print("🧊 Cold Archive Rehydration (SQL-native)")
    print("=" * 60)

    if not COLD_DB.exists():
        print(f"❌ Cold DB not found: {COLD_DB}")
        return

    if args.dry_run:
        print("🔍 DRY RUN mode")

    # Backup
    if not args.dry_run:
        backup = ACTIVE_DB.with_suffix(".db.pre-cold-backup")
        if not backup.exists():
            print(f"💾 Backing up to {backup.name}...")
            shutil.copy2(ACTIVE_DB, backup)
            print(f"  ✅ Backup: {backup.stat().st_size / 1024 / 1024:.1f} MB")

    conn = sqlite3.connect(str(ACTIVE_DB))
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")
    conn.execute("PRAGMA cache_size=-128000")  # 128MB cache
    conn.execute(f"ATTACH DATABASE '{COLD_DB}' AS cold")

    # --- Pre-counts ---
    print("\n📊 Before:")
    for t in ["memories", "associations", "holographic_coords", "memory_embeddings", "tags"]:
        mc = count(conn, t, "main")
        cc = count(conn, t, "cold")
        print(f"  {t}: active={mc:,}  cold={cc:,}")

    if args.dry_run:
        # Count what would be new
        new_mems = conn.execute(
            "SELECT COUNT(*) FROM cold.memories c WHERE c.id NOT IN (SELECT id FROM main.memories)"
        ).fetchone()[0]
        print(f"\n  Would insert ~{new_mems:,} new memories")
        conn.close()
        return

    total_start = time.perf_counter()

    # --- 1. Memories ---
    # The cold DB has a slightly different schema (missing some columns).
    # We select only columns that exist in both.
    cold_mem_cols = [r[1] for r in conn.execute("PRAGMA cold.table_info(memories)").fetchall()]
    active_mem_cols = [r[1] for r in conn.execute("PRAGMA main.table_info(memories)").fetchall()]
    shared_cols = [c for c in active_mem_cols if c in cold_mem_cols]
    col_list = ", ".join(shared_cols)

    print(f"\n📦 Ingesting memories ({len(shared_cols)} shared columns)...")
    t0 = time.perf_counter()
    conn.execute(f"""
        INSERT OR IGNORE INTO main.memories ({col_list})
        SELECT {col_list} FROM cold.memories
    """)
    conn.commit()
    dt = time.perf_counter() - t0
    print(f"  ✅ Memories done in {dt:.1f}s")

    # --- 2. Associations ---
    cold_assoc_cols = [r[1] for r in conn.execute("PRAGMA cold.table_info(associations)").fetchall()]
    active_assoc_cols = [r[1] for r in conn.execute("PRAGMA main.table_info(associations)").fetchall()]
    shared_assoc = [c for c in active_assoc_cols if c in cold_assoc_cols]
    acol_list = ", ".join(shared_assoc)

    cold_assoc_count = count(conn, "associations", "cold")
    print(f"\n📦 Ingesting associations ({cold_assoc_count:,} rows, {len(shared_assoc)} shared cols)...")
    print("   This may take several minutes for 18.7M rows...")
    t0 = time.perf_counter()

    # Use batched approach for huge tables
    batch_size = 500000
    offset = 0
    total_done = 0

    while True:
        result = conn.execute(f"""
            INSERT OR IGNORE INTO main.associations ({acol_list})
            SELECT {acol_list} FROM cold.associations
            LIMIT {batch_size} OFFSET {offset}
        """)
        rows_affected = result.rowcount if result.rowcount >= 0 else batch_size
        conn.commit()
        total_done += batch_size
        offset += batch_size

        elapsed = time.perf_counter() - t0
        pct = min(100, total_done / max(1, cold_assoc_count) * 100)
        rate = total_done / max(0.1, elapsed)
        remaining = max(0, cold_assoc_count - total_done) / max(1, rate)
        print(f"    ... {min(total_done, cold_assoc_count):,}/{cold_assoc_count:,} ({pct:.0f}%) "
              f"[{elapsed:.0f}s elapsed, ~{remaining:.0f}s remaining]")

        if total_done >= cold_assoc_count:
            break

    dt = time.perf_counter() - t0
    print(f"  ✅ Associations done in {dt:.1f}s")

    # --- 3. Holographic coords ---
    print("\n📦 Ingesting holographic coords...")
    t0 = time.perf_counter()
    cold_holo_cols = [r[1] for r in conn.execute("PRAGMA cold.table_info(holographic_coords)").fetchall()]
    active_holo_cols = [r[1] for r in conn.execute("PRAGMA main.table_info(holographic_coords)").fetchall()]
    shared_holo = [c for c in active_holo_cols if c in cold_holo_cols]
    hcol_list = ", ".join(shared_holo)
    conn.execute(f"""
        INSERT OR IGNORE INTO main.holographic_coords ({hcol_list})
        SELECT {hcol_list} FROM cold.holographic_coords
    """)
    conn.commit()
    print(f"  ✅ Holo coords done in {time.perf_counter() - t0:.1f}s")

    # --- 4. Embeddings ---
    print("\n📦 Ingesting embeddings...")
    t0 = time.perf_counter()
    try:
        cold_embed_cols = [r[1] for r in conn.execute("PRAGMA cold.table_info(memory_embeddings)").fetchall()]
        active_embed_cols = [r[1] for r in conn.execute("PRAGMA main.table_info(memory_embeddings)").fetchall()]
        shared_embed = [c for c in active_embed_cols if c in cold_embed_cols]
        if shared_embed:
            ecol_list = ", ".join(shared_embed)
            conn.execute(f"""
                INSERT OR IGNORE INTO main.memory_embeddings ({ecol_list})
                SELECT {ecol_list} FROM cold.memory_embeddings
            """)
            conn.commit()
            print(f"  ✅ Embeddings done in {time.perf_counter() - t0:.1f}s")
        else:
            print("  ⚠ No shared embedding columns")
    except Exception as e:
        print(f"  ⚠ Embeddings: {e}")

    # --- 5. Tags ---
    print("\n📦 Ingesting tags...")
    t0 = time.perf_counter()
    try:
        conn.execute("""
            INSERT OR IGNORE INTO main.tags (memory_id, tag)
            SELECT memory_id, tag FROM cold.tags
        """)
        conn.commit()
        print(f"  ✅ Tags done in {time.perf_counter() - t0:.1f}s")
    except Exception as e:
        print(f"  ⚠ Tags: {e}")

    # --- Rebuild FTS ---
    print("\n🔄 Rebuilding FTS index...")
    try:
        conn.execute("INSERT INTO memories_fts(memories_fts) VALUES('rebuild')")
        conn.commit()
        print("  ✅ FTS rebuilt")
    except Exception as e:
        print(f"  ⚠ FTS: {e}")

    # --- Final counts ---
    print("\n📊 After:")
    for t in ["memories", "associations", "holographic_coords", "memory_embeddings", "tags"]:
        mc = count(conn, t, "main")
        print(f"  {t}: {mc:,}")

    total_elapsed = time.perf_counter() - total_start
    print(f"\n🎉 Cold archive rehydration complete in {total_elapsed:.1f}s")

    conn.execute("DETACH DATABASE cold")
    conn.close()


if __name__ == "__main__":
    main()
