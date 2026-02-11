#!/usr/bin/env python3
"""
B1: Holographic re-encoding for LONG_TERM + CORE memories.
Fixes Y/Z centroid compression (-0.58/-0.63 → ~0.0).

Operates directly against the DB using the CoordinateEncoder.
Re-encodes the ~5,500 LONG_TERM + 42 CORE memories with fresh
coordinate calculations based on their actual content.
"""
import sqlite3
import os
import sys
import time
import json

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
os.environ["WM_SILENT_INIT"] = "1"

DB_PATH = os.path.expanduser("~/.whitemagic/memory/whitemagic.db")


def memory_row_to_dict(row, columns):
    """Convert a sqlite3 row to a dict suitable for the encoder."""
    d = {}
    for i, col in enumerate(columns):
        d[col] = row[i]
    # Parse tags from the tags table separately
    # Parse metadata JSON
    if d.get("metadata"):
        try:
            d["metadata"] = json.loads(d["metadata"])
        except (json.JSONDecodeError, TypeError):
            d["metadata"] = {}
    else:
        d["metadata"] = {}
    return d


def main():
    from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder

    if not os.path.exists(DB_PATH):
        print(f"ERROR: DB not found at {DB_PATH}")
        sys.exit(1)

    conn = sqlite3.connect(DB_PATH)
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")

    # Get column names
    cursor = conn.execute("PRAGMA table_info(memories)")
    mem_columns = [row[1] for row in cursor.fetchall()]

    # Pre-centroid check
    print("=== Before re-encoding ===")
    for label, where in [
        ("ALL", "1=1"),
        ("LONG_TERM", "m.memory_type = 'LONG_TERM'"),
        ("CORE (is_protected=1)", "m.is_protected = 1"),
    ]:
        cur = conn.execute(f"""
            SELECT AVG(hc.x), AVG(hc.y), AVG(hc.z), AVG(hc.w), AVG(hc.v), COUNT(*)
            FROM holographic_coords hc
            JOIN memories m ON hc.memory_id = m.id
            WHERE {where}
        """)
        r = cur.fetchone()
        if r and r[5] > 0:
            print(f"  {label:30s}  n={r[5]:>6,}  X={r[0]:+.3f} Y={r[1]:+.3f} Z={r[2]:+.3f} W={r[3]:.3f} V={r[4]:.3f}")

    encoder = CoordinateEncoder()

    # Fetch LONG_TERM + protected memories
    print("\n=== Re-encoding LONG_TERM + protected memories ===")
    cur = conn.execute(f"""
        SELECT {', '.join(mem_columns)} FROM memories
        WHERE memory_type = 'LONG_TERM' OR is_protected = 1
    """)
    rows = cur.fetchall()
    print(f"  Fetched {len(rows):,} memories to re-encode")

    # Also fetch tags for each memory
    tag_map = {}
    cur = conn.execute("""
        SELECT memory_id, tag FROM tags
        WHERE memory_id IN (
            SELECT id FROM memories
            WHERE memory_type = 'LONG_TERM' OR is_protected = 1
        )
    """)
    for mid, tag in cur.fetchall():
        tag_map.setdefault(mid, []).append(tag)

    # Re-encode in batches
    BATCH_SIZE = 500
    total_encoded = 0
    t0 = time.perf_counter()

    for batch_start in range(0, len(rows), BATCH_SIZE):
        batch = rows[batch_start:batch_start + BATCH_SIZE]
        updates = []

        for row in batch:
            mem = memory_row_to_dict(row, mem_columns)
            mem["tags"] = tag_map.get(mem["id"], [])

            coord = encoder.encode(mem)
            updates.append((
                mem["id"],
                coord.x, coord.y, coord.z, coord.w, coord.v,
            ))

        # Batch update
        conn.executemany(
            "INSERT OR REPLACE INTO holographic_coords (memory_id, x, y, z, w, v) VALUES (?, ?, ?, ?, ?, ?)",
            updates,
        )
        conn.commit()
        total_encoded += len(updates)

        if (batch_start // BATCH_SIZE) % 5 == 0:
            elapsed = time.perf_counter() - t0
            rate = total_encoded / elapsed if elapsed > 0 else 0
            print(f"  {total_encoded:,}/{len(rows):,} ({rate:.0f}/s)")

    elapsed = time.perf_counter() - t0
    print(f"  Done: {total_encoded:,} memories re-encoded in {elapsed:.1f}s ({total_encoded / elapsed:.0f}/s)")

    # Post-centroid check
    print("\n=== After re-encoding ===")
    for label, where in [
        ("ALL", "1=1"),
        ("LONG_TERM", "m.memory_type = 'LONG_TERM'"),
        ("CORE (is_protected=1)", "m.is_protected = 1"),
    ]:
        cur = conn.execute(f"""
            SELECT AVG(hc.x), AVG(hc.y), AVG(hc.z), AVG(hc.w), AVG(hc.v), COUNT(*)
            FROM holographic_coords hc
            JOIN memories m ON hc.memory_id = m.id
            WHERE {where}
        """)
        r = cur.fetchone()
        if r and r[5] > 0:
            print(f"  {label:30s}  n={r[5]:>6,}  X={r[0]:+.3f} Y={r[1]:+.3f} Z={r[2]:+.3f} W={r[3]:.3f} V={r[4]:.3f}")

    conn.close()
    print("\n✓ B1 complete")


if __name__ == "__main__":
    main()
