#!/usr/bin/env python3
"""
Archive Ingestion Script — Leap 2 (v13.6)
==========================================
Ingests session handoff .md files and other development history
documents into the hot memory DB as LONG_TERM memories.

These are real development conversations and decisions worth preserving
in the memory system for semantic search and knowledge retrieval.

Usage:
    python scripts/ingest_archives.py              # Full run
    python scripts/ingest_archives.py --dry-run    # Preview only
"""

from __future__ import annotations

import argparse
import hashlib
import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)

# Files to ingest: (path, title, tags)
PROJECT_ROOT = Path(__file__).resolve().parent.parent
ARCHIVE_ROOT = Path.home() / "Desktop" / "wm_archive" / "phase1_archived"

INGEST_TARGETS = [
    # Session handoffs (in docs/sessions/)
    (PROJECT_ROOT / "docs" / "sessions" / "SESSION_HANDOFF_2026_02_09_AFTERNOON.md",
     "Session Handoff — Feb 9, 2026 Afternoon",
     {"session_handoff", "v13", "distillation", "polyglot", "development_history"}),
    (PROJECT_ROOT / "docs" / "sessions" / "SESSION_HANDOFF_2026_02_09_EVENING.md",
     "Session Handoff — Feb 9, 2026 Evening",
     {"session_handoff", "v13.3.2", "cold_storage", "embeddings", "development_history"}),
    (PROJECT_ROOT / "docs" / "sessions" / "SESSION_HANDOFF_2026_02_09_POLYGLOT.md",
     "Session Handoff — Feb 9, 2026 Polyglot",
     {"session_handoff", "polyglot", "rust", "zig", "development_history"}),
    (PROJECT_ROOT / "docs" / "sessions" / "SESSION_HANDOFF_2026_02_10_P6P7.md",
     "Session Handoff — Feb 10, 2026 P6/P7 SQLite Optimization",
     {"session_handoff", "v13.3.3", "sqlite", "performance", "development_history"}),
    (PROJECT_ROOT / "docs" / "sessions" / "SESSION_HANDOFF_2026_02_10_V13_4.md",
     "Session Handoff — Feb 10, 2026 v13.4 Semantic Embeddings",
     {"session_handoff", "v13.4", "embeddings", "constellations", "development_history"}),
    (PROJECT_ROOT / "docs" / "sessions" / "SESSION_HANDOFF_2026_02_10_V13_5.md",
     "Session Handoff — Feb 10, 2026 v13.5 Semantic Memory Revolution",
     {"session_handoff", "v13.5", "semantic_memory", "hybrid_search", "development_history"}),
    # Archived session handoffs
    (ARCHIVE_ROOT / "SESSION_HANDOFF_2026_02_08.md",
     "Session Handoff — Feb 8, 2026",
     {"session_handoff", "polyglot", "dream_cycle", "nexus_api", "development_history"}),
    (ARCHIVE_ROOT / "SESSION_HANDOFF_2026_02_08_EVENING.md",
     "Session Handoff — Feb 8, 2026 Evening",
     {"session_handoff", "gana_analysis", "capability_matrix", "development_history"}),
    # Dream journal
    (ARCHIVE_ROOT / "dream_journal.md",
     "Dream Journal — WhiteMagic Development Dreams",
     {"dream_journal", "dreams", "development_history", "vision"}),
    # Key strategy and architecture docs
    (PROJECT_ROOT / "AI_PRIMARY.md",
     "AI Primary — WhiteMagic Core Architecture Guide",
     {"architecture", "ai_primary", "reference", "development_history"}),
    (PROJECT_ROOT / "SYSTEM_MAP.md",
     "System Map — WhiteMagic Component Overview",
     {"architecture", "system_map", "reference", "development_history"}),
    (PROJECT_ROOT / "docs" / "STRATEGIC_ROADMAP.md",
     "Strategic Roadmap — v13.4 to v14.0",
     {"roadmap", "strategy", "gratitude_architecture", "development_history"}),
    (PROJECT_ROOT / "docs" / "VISION.md",
     "Vision — WhiteMagic Long-term Vision",
     {"vision", "philosophy", "development_history"}),
    (PROJECT_ROOT / "docs" / "POLYGLOT_STATUS.md",
     "Polyglot Status — Multi-language Accelerator Status",
     {"polyglot", "rust", "zig", "haskell", "status", "development_history"}),
    (PROJECT_ROOT / "docs" / "POLYGLOT_EXPANSION_STRATEGY.md",
     "Polyglot Expansion Strategy",
     {"polyglot", "strategy", "rust", "zig", "haskell", "elixir", "development_history"}),
]


def generate_id(title: str, content: str) -> str:
    """Generate deterministic memory ID from title + content prefix."""
    key = f"ingest:{title}:{content[:200]}"
    return hashlib.sha256(key.encode()).hexdigest()[:16]


def run(dry_run: bool = False) -> dict:
    """Ingest archive files into memory system."""
    if not dry_run:
        import os
        os.environ["WM_SILENT_INIT"] = "1"
        os.environ["WM_SKIP_HOLO_INDEX"] = "1"
        from whitemagic.core.memory.unified import get_unified_memory
        from whitemagic.core.memory.unified_types import MemoryType
        memory = get_unified_memory()

    ingested = 0
    skipped = 0
    errors = 0

    for path, title, tags in INGEST_TARGETS:
        if not path.exists():
            logger.warning(f"SKIP (not found): {path.name}")
            skipped += 1
            continue

        content = path.read_text(encoding="utf-8", errors="replace")
        if len(content.strip()) < 50:
            logger.warning(f"SKIP (too short): {path.name} ({len(content)} chars)")
            skipped += 1
            continue

        mem_id = generate_id(title, content)

        if dry_run:
            logger.info(f"WOULD INGEST: {title} ({len(content):,} chars, id={mem_id})")
            ingested += 1
            continue

        # Check if already ingested
        existing = memory.backend.recall(mem_id)
        if existing:
            logger.info(f"ALREADY EXISTS: {title}")
            skipped += 1
            continue

        try:
            mem = memory.store(
                content=content[:50000],  # Cap at 50K chars
                memory_type=MemoryType.LONG_TERM,
                tags=tags,
                importance=0.80,
                title=title,
            )
            # Override the auto-generated ID with our deterministic one
            # (store already saved it, but we log the actual ID)
            logger.info(f"INGESTED: {title} ({len(content):,} chars) → {mem.id}")
            ingested += 1
        except Exception as e:
            logger.error(f"ERROR: {title}: {e}")
            errors += 1

    result = {
        "status": "success" if errors == 0 else "partial",
        "ingested": ingested,
        "skipped": skipped,
        "errors": errors,
        "dry_run": dry_run,
    }
    logger.info(f"Done: {result}")
    return result


def main():
    parser = argparse.ArgumentParser(description="Archive Ingestion Script")
    parser.add_argument("--dry-run", action="store_true", help="Preview only")
    args = parser.parse_args()
    run(dry_run=args.dry_run)


if __name__ == "__main__":
    main()
