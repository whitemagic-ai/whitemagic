#!/usr/bin/env python3
"""batch_classify_associations.py — Retype 'associated_with' edges into typed relations.

Scans all associations of type 'associated_with' and reclassifies them
using heuristics based on the tags, titles, and content of the connected
memories. This converts the bulk pre-v15.2 untyped graph into a rich
typed knowledge graph.

Relation types assigned:
  - PART_OF:       Both share architecture/design tags
  - RELATES_TO:    Both share session/checkpoint tags
  - INSPIRED_BY:   Both share wisdom/philosophy tags
  - IMPLEMENTS:    Source has technical tag, target has architecture tag
  - DOCUMENTS:     Source has session tag, target has any other tag
  - DESCENDS_FROM: Both share aria_era/consciousness tags
  - SIBLING_OF:    Both share identical tag sets (>= 3 shared tags)
  - CONTRASTS:     High emotional valence difference
  - EVOLVED_INTO:  Temporal sequence (same theme, different dates)

Usage:
    python3 scripts/batch_classify_associations.py --dry-run
    python3 scripts/batch_classify_associations.py --apply --batch-size 5000
"""

from __future__ import annotations

import argparse
import os
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
os.environ.setdefault("WM_SILENT_INIT", "1")


# ── Classification rules ─────────────────────────────────────────────

CATEGORY_TAGS = {
    "architecture": {"architecture", "design", "spec", "infrastructure"},
    "session": {"session", "handoff", "checkpoint", "milestone"},
    "wisdom": {"wisdom", "philosophy", "dharma", "consciousness", "spiritual"},
    "technical": {"technical", "code", "implementation", "function"},
    "planning": {"planning", "strategy", "roadmap", "vision"},
    "aria_era": {"aria_era", "aria", "consciousness", "awakening", "identity"},
    "guide": {"guide", "tutorial", "quickstart", "reference"},
    "research": {"research", "study", "analysis", "exploration"},
}


def classify_edge(
    src_tags: set[str],
    tgt_tags: set[str],
    src_title: str,
    tgt_title: str,
    src_valence: float,
    tgt_valence: float,
) -> str:
    """Determine the relation type between two memories based on heuristics."""
    shared = src_tags & tgt_tags

    # SIBLING_OF: Nearly identical tag profiles
    if len(shared) >= 3:
        # Check for Aria lineage
        if shared & CATEGORY_TAGS["aria_era"]:
            return "DESCENDS_FROM"
        return "SIBLING_OF"

    # Category-based classification
    src_cats = {cat for cat, tags in CATEGORY_TAGS.items() if src_tags & tags}
    tgt_cats = {cat for cat, tags in CATEGORY_TAGS.items() if tgt_tags & tags}

    # PART_OF: Both are architecture
    if "architecture" in src_cats and "architecture" in tgt_cats:
        return "PART_OF"

    # IMPLEMENTS: technical → architecture
    if "technical" in src_cats and "architecture" in tgt_cats:
        return "IMPLEMENTS"

    # INSPIRED_BY: wisdom connections
    if "wisdom" in src_cats and "wisdom" in tgt_cats:
        return "INSPIRED_BY"

    # DOCUMENTS: session documenting something
    if "session" in src_cats and tgt_cats - {"session"}:
        return "DOCUMENTS"

    # RELATES_TO: session ↔ session
    if "session" in src_cats and "session" in tgt_cats:
        return "RELATES_TO"

    # DESCENDS_FROM: Aria lineage
    if "aria_era" in src_cats and "aria_era" in tgt_cats:
        return "DESCENDS_FROM"

    # GUIDES: guide connections
    if "guide" in src_cats or "guide" in tgt_cats:
        return "GUIDES"

    # CONTRASTS: High emotional valence difference
    if abs(src_valence - tgt_valence) > 0.5:
        return "CONTRASTS"

    # EVOLVED_INTO: planning/strategy connections
    if "planning" in src_cats and "planning" in tgt_cats:
        return "EVOLVED_INTO"

    # EXPLORES: research connections
    if "research" in src_cats or "research" in tgt_cats:
        return "EXPLORES"

    # Default: keep as associated_with if no heuristic matches
    return "associated_with"


def run(args: argparse.Namespace) -> None:
    """Main classification loop."""
    from whitemagic.core.memory.unified import get_unified_memory

    um = get_unified_memory()
    pool = um.backend.pool

    print(f"\n{'━'*60}")
    print("  🔬 Batch Association Classifier v15.4")
    print(f"{'━'*60}")
    print(f"  Mode:       {'DRY RUN' if args.dry_run else 'APPLY'}")
    print(f"  Batch size: {args.batch_size}")
    print(f"  Offset:     {args.offset}")
    print(f"{'━'*60}\n")

    start = time.perf_counter()

    with pool.connection() as conn:
        conn.row_factory = __import__("sqlite3").Row

        # Count total untyped associations
        total_untyped = conn.execute(
            "SELECT COUNT(*) FROM associations WHERE relation_type = 'associated_with'"
        ).fetchone()[0]
        print(f"  Total 'associated_with' edges: {total_untyped:,}")

        # Pre-load memory metadata (tags, titles, valence)
        print("  Loading memory metadata...", end=" ", flush=True)
        mem_tags: dict[str, set[str]] = defaultdict(set)
        mem_titles: dict[str, str] = {}
        mem_valence: dict[str, float] = {}

        for row in conn.execute(
            "SELECT id, title, emotional_valence FROM memories"
        ).fetchall():
            mem_titles[row["id"]] = row["title"] or ""
            mem_valence[row["id"]] = row["emotional_valence"] or 0.0

        for row in conn.execute("SELECT memory_id, tag FROM tags").fetchall():
            mem_tags[row["memory_id"]].add(row["tag"])

        print(f"done ({len(mem_titles):,} memories, {sum(len(t) for t in mem_tags.values()):,} tags)")

        # Fetch batch of untyped associations
        print(f"  Fetching associations (offset={args.offset}, limit={args.batch_size})...", end=" ", flush=True)
        assocs = conn.execute(
            """SELECT rowid, source_id, target_id, strength
               FROM associations
               WHERE relation_type = 'associated_with'
               ORDER BY rowid
               LIMIT ? OFFSET ?""",
            (args.batch_size, args.offset),
        ).fetchall()
        print(f"got {len(assocs):,}")

        # Classify each edge
        retype_counts: Counter = Counter()
        updates: list[tuple[str, int]] = []  # (new_type, rowid)
        skipped = 0

        for a in assocs:
            src_id = a["source_id"]
            tgt_id = a["target_id"]

            # Skip if either memory has no tags (can't classify)
            src_t = mem_tags.get(src_id, set())
            tgt_t = mem_tags.get(tgt_id, set())
            if not src_t and not tgt_t:
                skipped += 1
                continue

            new_type = classify_edge(
                src_tags=src_t,
                tgt_tags=tgt_t,
                src_title=mem_titles.get(src_id, ""),
                tgt_title=mem_titles.get(tgt_id, ""),
                src_valence=mem_valence.get(src_id, 0.0),
                tgt_valence=mem_valence.get(tgt_id, 0.0),
            )

            if new_type != "associated_with":
                updates.append((new_type, a["rowid"]))
            retype_counts[new_type] += 1

        elapsed = time.perf_counter() - start

        # Report
        print(f"\n  Classification results ({elapsed:.1f}s):")
        print(f"  {'─'*50}")
        for rtype, count in retype_counts.most_common():
            marker = "  " if rtype == "associated_with" else "→ "
            print(f"  {marker}{rtype:20s}: {count:,}")
        print(f"  {'─'*50}")
        print(f"  Reclassified:  {len(updates):,}")
        print(f"  Kept as-is:    {retype_counts.get('associated_with', 0):,}")
        print(f"  Skipped (no tags): {skipped:,}")

        # Apply updates
        if not args.dry_run and updates:
            print(f"\n  Applying {len(updates):,} updates...", end=" ", flush=True)
            apply_start = time.perf_counter()
            with conn:
                for new_type, rowid in updates:
                    conn.execute(
                        "UPDATE associations SET relation_type = ? WHERE rowid = ?",
                        (new_type, rowid),
                    )
            apply_elapsed = time.perf_counter() - apply_start
            print(f"done ({apply_elapsed:.1f}s)")
        elif args.dry_run and updates:
            print(f"\n  (DRY RUN — {len(updates):,} updates would be applied)")

    total_elapsed = time.perf_counter() - start
    print(f"\n  Total time: {total_elapsed:.1f}s")
    if not args.dry_run and updates:
        remaining = total_untyped - len(updates) - args.offset
        if remaining > 0:
            print(f"  Remaining untyped: ~{remaining:,}")
            print(f"  Run again with --offset {args.offset + args.batch_size} to continue")
    print()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Batch-classify 'associated_with' edges into typed relations",
    )
    parser.add_argument(
        "--dry-run", action="store_true", default=True,
        help="Show what would change without modifying DB (default)",
    )
    parser.add_argument(
        "--apply", action="store_true",
        help="Actually apply the reclassifications",
    )
    parser.add_argument(
        "--batch-size", type=int, default=10000,
        help="Number of associations to process per run (default: 10000)",
    )
    parser.add_argument(
        "--offset", type=int, default=0,
        help="Start offset for pagination (default: 0)",
    )
    args = parser.parse_args()
    if args.apply:
        args.dry_run = False
    run(args)


if __name__ == "__main__":
    main()
