#!/usr/bin/env python3
"""
Cold DB Embedding Indexer — Leap 2 (v13.6)
============================================
Encodes all 105K cold storage memories with MiniLM-L6-v2 embeddings.
Stores results in a `memory_embeddings` table inside the cold DB.

Designed to run as a long background process (~87 min on CPU at ~20/sec).
Supports resumption — skips already-encoded memories.

Usage:
    python scripts/cold_db_embed.py                  # Full run
    python scripts/cold_db_embed.py --batch-size 128 # Larger batches (more RAM)
    python scripts/cold_db_embed.py --limit 1000     # Test with subset
    python scripts/cold_db_embed.py --dry-run        # Just count candidates
"""

from __future__ import annotations

import argparse
import logging
import sqlite3
import struct
import sys
import time
from pathlib import Path

# Ensure project root is on path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)

EMBEDDING_DIM = 384
MODEL_NAME = "all-MiniLM-L6-v2"


def _pack_embedding(vec: list[float]) -> bytes:
    return struct.pack(f"{len(vec)}f", *vec)


def _init_cold_db(conn: sqlite3.Connection) -> None:
    """Create the memory_embeddings table in cold DB if it doesn't exist."""
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")
    conn.execute("PRAGMA mmap_size=268435456")
    conn.execute("PRAGMA cache_size=-65536")
    conn.execute("PRAGMA temp_store=MEMORY")
    conn.execute("""
        CREATE TABLE IF NOT EXISTS memory_embeddings (
            memory_id TEXT PRIMARY KEY,
            embedding BLOB,
            model TEXT,
            created_at TEXT DEFAULT CURRENT_TIMESTAMP
        )
    """)
    conn.commit()
    logger.info("memory_embeddings table ready in cold DB")


def get_candidates(conn: sqlite3.Connection, limit: int, skip_cached: bool = True) -> list[tuple]:
    """Get memories that need encoding."""
    sql = "SELECT id, title, content FROM memories"
    if skip_cached:
        sql += " WHERE id NOT IN (SELECT memory_id FROM memory_embeddings)"
    sql += " ORDER BY importance DESC"
    if limit > 0:
        sql += f" LIMIT {limit}"
    return conn.execute(sql).fetchall()


def run(
    cold_db_path: str,
    batch_size: int = 64,
    limit: int = 0,
    dry_run: bool = False,
    max_text_len: int = 2000,
) -> dict:
    """Main encoding loop."""
    conn = sqlite3.connect(cold_db_path)
    _init_cold_db(conn)

    # Count current state
    total_memories = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
    existing_embeddings = conn.execute("SELECT COUNT(*) FROM memory_embeddings").fetchone()[0]
    logger.info(f"Cold DB: {total_memories:,} memories, {existing_embeddings:,} already embedded")

    candidates = get_candidates(conn, limit=limit)
    logger.info(f"Candidates to encode: {len(candidates):,}")

    if dry_run:
        conn.close()
        return {"status": "dry_run", "candidates": len(candidates), "existing": existing_embeddings}

    if not candidates:
        conn.close()
        return {"status": "success", "indexed": 0, "reason": "all memories already cached"}

    # Load model
    try:
        from sentence_transformers import SentenceTransformer
    except ImportError:
        logger.error("sentence-transformers not installed. Run: pip install sentence-transformers")
        conn.close()
        return {"status": "error", "reason": "sentence-transformers not installed"}

    logger.info(f"Loading model: {MODEL_NAME}")
    model = SentenceTransformer(MODEL_NAME)
    logger.info(f"Model loaded ({EMBEDDING_DIM} dims)")

    # Process in batches
    t0 = time.perf_counter()
    total_encoded = 0
    total_batches = (len(candidates) + batch_size - 1) // batch_size

    for batch_idx in range(total_batches):
        batch_start = batch_idx * batch_size
        batch_end = min(batch_start + batch_size, len(candidates))
        batch = candidates[batch_start:batch_end]

        # Prepare texts: title + content, truncated
        texts = []
        ids = []
        for row in batch:
            mem_id, title, content = row[0], row[1] or "", row[2] or ""
            text = f"{title} {content}"[:max_text_len]
            texts.append(text)
            ids.append(mem_id)

        # Encode batch
        try:
            embeddings = model.encode(texts, batch_size=batch_size, show_progress_bar=False)
        except Exception as e:
            logger.warning(f"Batch {batch_idx + 1} encoding failed: {e}")
            continue

        # Store in DB
        insert_data = [
            (mid, _pack_embedding(emb.tolist()), MODEL_NAME)
            for mid, emb in zip(ids, embeddings)
        ]
        try:
            conn.executemany(
                "INSERT OR REPLACE INTO memory_embeddings (memory_id, embedding, model) VALUES (?, ?, ?)",
                insert_data,
            )
            conn.commit()
            total_encoded += len(insert_data)
        except Exception as e:
            logger.warning(f"Batch {batch_idx + 1} DB write failed: {e}")
            continue

        # Progress
        elapsed = time.perf_counter() - t0
        rate = total_encoded / elapsed if elapsed > 0 else 0
        eta_s = (len(candidates) - total_encoded) / rate if rate > 0 else 0
        eta_m = eta_s / 60

        if (batch_idx + 1) % 10 == 0 or batch_idx == total_batches - 1:
            logger.info(
                f"Batch {batch_idx + 1}/{total_batches} — "
                f"{total_encoded:,}/{len(candidates):,} encoded — "
                f"{rate:.1f}/sec — ETA {eta_m:.1f}min"
            )

    elapsed = time.perf_counter() - t0
    result = {
        "status": "success",
        "indexed": total_encoded,
        "total_candidates": len(candidates),
        "duration_s": round(elapsed, 1),
        "rate": round(total_encoded / elapsed, 1) if elapsed > 0 else 0,
        "model": MODEL_NAME,
        "dims": EMBEDDING_DIM,
    }
    logger.info(f"Done: {result}")
    conn.close()
    return result


def main():
    parser = argparse.ArgumentParser(description="Cold DB Embedding Indexer")
    parser.add_argument("--cold-db", default=str(Path.home() / ".whitemagic/memory/whitemagic_cold.db"))
    parser.add_argument("--batch-size", type=int, default=64, help="Encoding batch size")
    parser.add_argument("--limit", type=int, default=0, help="Max memories to encode (0 = all)")
    parser.add_argument("--max-text-len", type=int, default=2000, help="Max chars per memory text")
    parser.add_argument("--dry-run", action="store_true", help="Just count candidates")
    args = parser.parse_args()

    result = run(
        cold_db_path=args.cold_db,
        batch_size=args.batch_size,
        limit=args.limit,
        dry_run=args.dry_run,
        max_text_len=args.max_text_len,
    )
    print(f"\nResult: {result}")


if __name__ == "__main__":
    main()
