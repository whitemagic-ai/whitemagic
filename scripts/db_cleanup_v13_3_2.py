#!/usr/bin/env python3
"""
Database cleanup for v13.3.2:
- A1: Prune weak associations (strength < 0.30)
- A3: Remove structural tags (archive, scavenged, recovered)
- B3: Importance recalibration for merged memories
"""
import sqlite3
import os
import sys
import time
import shutil
import re

DB_PATH = os.path.expanduser("~/.whitemagic/memory/whitemagic.db")

# WM-relevant keywords for importance boosting (B3)
WM_KEYWORDS = {
    "whitemagic", "gana", "dharma", "harmony", "holographic", "galactic",
    "mcp", "prat", "tool", "dispatch", "pipeline", "resonance", "karma",
    "constellation", "association", "consolidation", "lifecycle", "retention",
    "maturity", "homeostasis", "bicameral", "salience", "temporal", "dream",
    "gnosis", "wu xing", "wuxing", "grimoire", "polyglot", "rust", "zig",
    "haskell", "elixir", "mojo", "nexus", "architecture", "session",
    "handoff", "benchmark", "accelerator", "simd", "cyberbrain", "mandala",
    "governor", "circuit breaker", "rate limiter", "input sanitizer",
    "agent", "swarm", "worker", "ensemble", "voting", "broker", "redis",
    "gan ying", "i ching", "hexagram", "zodiac", "garden", "memory",
}

SESSION_KEYWORDS = {"session handoff", "session_handoff", "handoff"}


def main():
    if not os.path.exists(DB_PATH):
        print(f"ERROR: DB not found at {DB_PATH}")
        sys.exit(1)

    db_size_before = os.path.getsize(DB_PATH)
    print(f"DB path: {DB_PATH}")
    print(f"DB size before: {db_size_before / 1e9:.2f} GB")

    conn = sqlite3.connect(DB_PATH)
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")

    # ── A1: Prune weak associations ─────────────────────────────────
    print("\n═══ A1: Prune weak associations (strength < 0.30) ═══")
    cur = conn.execute("SELECT COUNT(*) FROM associations WHERE strength < 0.30")
    to_delete = cur.fetchone()[0]
    cur = conn.execute("SELECT COUNT(*) FROM associations")
    total_before = cur.fetchone()[0]
    print(f"  Before: {total_before:,} associations")
    print(f"  Pruning: {to_delete:,} (strength < 0.30)")

    t0 = time.perf_counter()
    conn.execute("DELETE FROM associations WHERE strength < 0.30")
    conn.commit()
    t1 = time.perf_counter()

    cur = conn.execute("SELECT COUNT(*) FROM associations")
    total_after = cur.fetchone()[0]
    print(f"  After: {total_after:,} associations")
    print(f"  Removed: {total_before - total_after:,} ({(total_before - total_after) / total_before * 100:.1f}%)")
    print(f"  Time: {t1 - t0:.1f}s")

    # ── A3: Remove structural tags ──────────────────────────────────
    print("\n═══ A3: Remove structural tags ═══")
    cur = conn.execute("SELECT COUNT(*) FROM tags")
    tags_before = cur.fetchone()[0]

    for tag in ("archive", "scavenged", "recovered"):
        cur = conn.execute("SELECT COUNT(*) FROM tags WHERE tag = ?", (tag,))
        cnt = cur.fetchone()[0]
        conn.execute("DELETE FROM tags WHERE tag = ?", (tag,))
        print(f"  Removed '{tag}': {cnt:,}")

    conn.commit()
    cur = conn.execute("SELECT COUNT(*) FROM tags")
    tags_after = cur.fetchone()[0]
    print(f"  Tags before: {tags_before:,} → after: {tags_after:,}")

    # ── B3: Importance recalibration for merged memories ────────────
    print("\n═══ B3: Importance recalibration for merged memories ═══")
    # Find memories that still have default importance (0.5) and were
    # likely from the primary DB merge
    cur = conn.execute("""
        SELECT id, title, content, memory_type, importance
        FROM memories
        WHERE importance BETWEEN 0.49 AND 0.51
    """)
    candidates = cur.fetchall()
    print(f"  Candidates with default importance (0.49-0.51): {len(candidates):,}")

    boosted = 0
    dampened = 0
    protected = 0

    for mem_id, title, content, mem_type, importance in candidates:
        text = f"{title or ''} {content or ''}".lower()

        # Session handoffs → 0.95 + protect
        if any(kw in text for kw in SESSION_KEYWORDS):
            conn.execute(
                "UPDATE memories SET importance = 0.95, is_protected = 1 WHERE id = ?",
                (mem_id,),
            )
            protected += 1
            boosted += 1
            continue

        # Count WM-relevant keyword hits
        hits = sum(1 for kw in WM_KEYWORDS if kw in text)

        if hits >= 5:
            new_imp = min(0.90, 0.60 + hits * 0.03)
            conn.execute(
                "UPDATE memories SET importance = ? WHERE id = ?",
                (round(new_imp, 3), mem_id),
            )
            boosted += 1
        elif hits >= 2:
            conn.execute(
                "UPDATE memories SET importance = 0.65 WHERE id = ?",
                (mem_id,),
            )
            boosted += 1
        elif len(text.strip()) < 20:
            # Very short/empty → dampen
            conn.execute(
                "UPDATE memories SET importance = 0.30 WHERE id = ?",
                (mem_id,),
            )
            dampened += 1
        # else: leave at 0.5 (generic but not empty)

    conn.commit()
    print(f"  Boosted: {boosted:,} (including {protected} protected session handoffs)")
    print(f"  Dampened: {dampened:,} (short/empty content)")
    print(f"  Unchanged: {len(candidates) - boosted - dampened:,}")

    # ── Verification ────────────────────────────────────────────────
    print("\n═══ Verification ═══")
    cur = conn.execute("SELECT COUNT(*), AVG(strength) FROM associations")
    row = cur.fetchone()
    print(f"  Associations: {row[0]:,} (avg strength: {row[1]:.3f})")

    cur = conn.execute("SELECT tag, COUNT(*) FROM tags GROUP BY tag ORDER BY COUNT(*) DESC LIMIT 10")
    print("  Top 10 tags:")
    for tag, cnt in cur.fetchall():
        print(f"    {tag:<30} {cnt:,}")

    cur = conn.execute("""
        SELECT
            COUNT(CASE WHEN importance > 0.7 THEN 1 END) as high,
            COUNT(CASE WHEN importance BETWEEN 0.5 AND 0.7 THEN 1 END) as med,
            COUNT(CASE WHEN importance < 0.5 THEN 1 END) as low,
            AVG(importance),
            COUNT(CASE WHEN is_protected = 1 THEN 1 END) as prot
        FROM memories
    """)
    row = cur.fetchone()
    print(f"  Importance distribution: high(>0.7)={row[0]:,} mid(0.5-0.7)={row[1]:,} low(<0.5)={row[2]:,}")
    print(f"  Avg importance: {row[3]:.3f}, Protected: {row[4]:,}")

    # VACUUM to reclaim space
    print("\n═══ VACUUM ═══")
    t0 = time.perf_counter()
    conn.execute("VACUUM")
    t1 = time.perf_counter()
    print(f"  VACUUM time: {t1 - t0:.1f}s")

    conn.close()

    db_size_after = os.path.getsize(DB_PATH)
    saved = db_size_before - db_size_after
    print(f"\n  DB size after: {db_size_after / 1e9:.2f} GB (saved {saved / 1e6:.0f} MB)")
    print("\n✓ A1 + A3 + B3 complete")


if __name__ == "__main__":
    main()
