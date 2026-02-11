#!/usr/bin/env python3
"""B3: Importance recalibration for merged memories + VACUUM."""
import sqlite3
import os
import time

DB_PATH = os.path.expanduser("~/.whitemagic/memory/whitemagic.db")

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

conn = sqlite3.connect(DB_PATH)
conn.execute("PRAGMA journal_mode=WAL")
conn.execute("PRAGMA synchronous=NORMAL")

db_size_before = os.path.getsize(DB_PATH)
print(f"DB size before: {db_size_before / 1e9:.2f} GB")

# B3: Importance recalibration
print("\n=== B3: Importance recalibration ===")
cur = conn.execute("""
    SELECT id, title, content, memory_type, importance
    FROM memories WHERE importance BETWEEN 0.49 AND 0.51
""")
candidates = cur.fetchall()
print(f"  Candidates: {len(candidates):,}")

boosted = dampened = protected_count = 0
for mem_id, title, content, mem_type, importance in candidates:
    text = f"{title or ''} {content or ''}".lower()
    if any(kw in text for kw in SESSION_KEYWORDS):
        conn.execute("UPDATE memories SET importance = 0.95, is_protected = 1 WHERE id = ?", (mem_id,))
        protected_count += 1; boosted += 1; continue
    hits = sum(1 for kw in WM_KEYWORDS if kw in text)
    if hits >= 5:
        new_imp = min(0.90, 0.60 + hits * 0.03)
        conn.execute("UPDATE memories SET importance = ? WHERE id = ?", (round(new_imp, 3), mem_id))
        boosted += 1
    elif hits >= 2:
        conn.execute("UPDATE memories SET importance = 0.65 WHERE id = ?", (mem_id,))
        boosted += 1
    elif len(text.strip()) < 20:
        conn.execute("UPDATE memories SET importance = 0.30 WHERE id = ?", (mem_id,))
        dampened += 1

conn.commit()
print(f"  Boosted: {boosted:,} (including {protected_count} protected)")
print(f"  Dampened: {dampened:,}")
print(f"  Unchanged: {len(candidates) - boosted - dampened:,}")

# Verification
print("\n=== Verification ===")
cur = conn.execute("SELECT COUNT(*), AVG(strength) FROM associations")
r = cur.fetchone()
print(f"  Associations: {r[0]:,} (avg strength: {r[1]:.3f})")

cur = conn.execute("SELECT tag, COUNT(*) FROM tags GROUP BY tag ORDER BY COUNT(*) DESC LIMIT 10")
print("  Top 10 tags:")
for tag, cnt in cur.fetchall():
    print(f"    {tag:<30} {cnt:,}")

cur = conn.execute("""
    SELECT
        COUNT(CASE WHEN importance > 0.7 THEN 1 END),
        COUNT(CASE WHEN importance BETWEEN 0.5 AND 0.7 THEN 1 END),
        COUNT(CASE WHEN importance < 0.5 THEN 1 END),
        AVG(importance),
        COUNT(CASE WHEN is_protected = 1 THEN 1 END)
    FROM memories
""")
r = cur.fetchone()
print(f"  Importance: high(>0.7)={r[0]:,}  mid(0.5-0.7)={r[1]:,}  low(<0.5)={r[2]:,}")
print(f"  Avg importance: {r[3]:.3f}, Protected: {r[4]:,}")

# VACUUM
print("\n=== VACUUM ===")
t0 = time.perf_counter()
conn.execute("VACUUM")
t1 = time.perf_counter()
print(f"  VACUUM time: {t1 - t0:.1f}s")
conn.close()

db_size_after = os.path.getsize(DB_PATH)
print(f"\n  DB: {db_size_before / 1e9:.2f} GB -> {db_size_after / 1e9:.2f} GB (saved {(db_size_before - db_size_after) / 1e6:.0f} MB)")
print("\nDone.")
