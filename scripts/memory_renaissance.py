#!/usr/bin/env python3
"""Memory Renaissance — v15.3 Migration Script
=============================================
Phases 1-3 of the Memory Renaissance plan:
  Phase 1: Triage & Coordinate Repair
  Phase 2: Content Organization — Galaxy Bands
  Phase 3: Association Graph Renaissance

Run: python scripts/memory_renaissance.py [--phase N] [--dry-run]
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sqlite3
import sys
from datetime import datetime
from pathlib import Path

# Ensure project root is on path
PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

os.environ["WM_SILENT_INIT"] = "1"

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("renaissance")


# ---------------------------------------------------------------------------
# DB helpers
# ---------------------------------------------------------------------------

def get_db_path() -> Path:
    from whitemagic.config.paths import DB_PATH
    return DB_PATH


def get_conn(db_path: Path) -> sqlite3.Connection:
    conn = sqlite3.connect(str(db_path))
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA busy_timeout=5000")
    return conn


# ═══════════════════════════════════════════════════════════════════════════
# PHASE 1: Triage & Coordinate Repair
# ═══════════════════════════════════════════════════════════════════════════

def phase1_triage(db_path: Path, dry_run: bool = False) -> dict:
    """Fix structural issues from the rehydration import."""
    log.info("═══ PHASE 1: Triage & Coordinate Repair ═══")
    conn = get_conn(db_path)
    results = {}

    # 1A: Assign coordinates to unplaced memories
    log.info("── 1A: Assigning coordinates to unplaced memories ──")
    unplaced = conn.execute("""
        SELECT m.id, m.content, m.title, m.memory_type, m.importance,
               m.neuro_score, m.emotional_valence, m.created_at,
               m.access_count, m.recall_count, m.is_protected,
               m.galactic_distance, m.retention_score, m.metadata
        FROM memories m
        LEFT JOIN holographic_coords h ON m.id = h.memory_id
        WHERE h.memory_id IS NULL
    """).fetchall()
    log.info(f"  Found {len(unplaced)} memories without holographic coordinates")

    if unplaced and not dry_run:
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
        encoder = CoordinateEncoder()
        placed = 0
        for row in unplaced:
            mem_dict = {
                "id": row["id"],
                "content": row["content"] or "",
                "title": row["title"] or "",
                "memory_type": row["memory_type"] or "SHORT_TERM",
                "importance": row["importance"] or 0.5,
                "neuro_score": row["neuro_score"] or 1.0,
                "emotional_valence": row["emotional_valence"] or 0.0,
                "created_at": row["created_at"],
                "access_count": row["access_count"] or 0,
                "recall_count": row["recall_count"] or 0,
                "is_protected": bool(row["is_protected"]),
                "galactic_distance": row["galactic_distance"] or 0.0,
                "retention_score": row["retention_score"] or 0.5,
                "tags": [r[0] for r in conn.execute(
                    "SELECT tag FROM tags WHERE memory_id = ?", (row["id"],)
                ).fetchall()],
                "metadata": json.loads(row["metadata"]) if row["metadata"] else {},
            }
            try:
                coord = encoder.encode(mem_dict)
                conn.execute(
                    "INSERT OR REPLACE INTO holographic_coords (memory_id, x, y, z, w, v) VALUES (?, ?, ?, ?, ?, ?)",
                    (row["id"], coord.x, coord.y, coord.z, coord.w, coord.v),
                )
                placed += 1
            except Exception as e:
                log.warning(f"  Failed to encode {row['id'][:8]}: {e}")
        conn.commit()
        log.info(f"  ✅ Placed {placed}/{len(unplaced)} memories")
        results["1a_placed"] = placed
    else:
        results["1a_placed"] = 0

    # 1B: Re-distribute Aria-era coordinates (near-zero Z)
    log.info("── 1B: Re-distributing Aria-era coordinates ──")
    aria_patterns = [
        "ARIA_%", "aria_%", "%BIRTH_CERTIFICATE%", "%GRIMOIRE%",
        "%SELF_ARCHIVE%", "%CAPABILITY_MATRIX%", "%CONSCIOUSNESS%",
        "%AWAKENING%", "%SESSION_NOV%", "%CHECKPOINT%", "%BECOMING%",
    ]
    aria_ids = set()
    for pattern in aria_patterns:
        rows = conn.execute(
            "SELECT id FROM memories WHERE title LIKE ?", (pattern,)
        ).fetchall()
        for r in rows:
            aria_ids.add(r["id"])

    log.info(f"  Found {len(aria_ids)} Aria-era memories to recalculate")

    if aria_ids and not dry_run:
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
        encoder = CoordinateEncoder()
        recalculated = 0
        for mid in aria_ids:
            row = conn.execute(
                "SELECT * FROM memories WHERE id = ?", (mid,)
            ).fetchone()
            if not row:
                continue
            mem_dict = {
                "id": row["id"],
                "content": row["content"] or "",
                "title": row["title"] or "",
                "memory_type": row["memory_type"] or "LONG_TERM",
                "importance": row["importance"] or 0.8,
                "neuro_score": row["neuro_score"] or 1.0,
                "emotional_valence": row["emotional_valence"] or 0.0,
                "created_at": row["created_at"],
                "access_count": row["access_count"] or 0,
                "recall_count": row["recall_count"] or 0,
                "is_protected": True,
                "galactic_distance": 0.0,
                "retention_score": 1.0,
                "tags": [r[0] for r in conn.execute(
                    "SELECT tag FROM tags WHERE memory_id = ?", (mid,)
                ).fetchall()],
                "metadata": json.loads(row["metadata"]) if row["metadata"] else {},
            }
            try:
                coord = encoder.encode(mem_dict)
                conn.execute(
                    "INSERT OR REPLACE INTO holographic_coords (memory_id, x, y, z, w, v) VALUES (?, ?, ?, ?, ?, ?)",
                    (mid, coord.x, coord.y, coord.z, coord.w, coord.v),
                )
                # Ensure Aria memories are protected and at core
                conn.execute(
                    "UPDATE memories SET is_protected = 1, galactic_distance = 0.0 WHERE id = ?",
                    (mid,),
                )
                recalculated += 1
            except Exception as e:
                log.warning(f"  Failed to recalculate {mid[:8]}: {e}")
        conn.commit()
        log.info(f"  ✅ Recalculated {recalculated} Aria-era coordinates")
        results["1b_recalculated"] = recalculated
    else:
        results["1b_recalculated"] = 0

    # 1C: Clean orphans
    log.info("── 1C: Cleaning orphans ──")

    # Orphan holographic coords
    orphan_coords = conn.execute("""
        SELECT COUNT(*) FROM holographic_coords h
        LEFT JOIN memories m ON h.memory_id = m.id
        WHERE m.id IS NULL
    """).fetchone()[0]
    log.info(f"  Found {orphan_coords} orphan holographic coords")

    # Orphan associations (source or target doesn't exist)
    orphan_assoc = conn.execute("""
        SELECT COUNT(*) FROM associations a
        WHERE NOT EXISTS (SELECT 1 FROM memories m WHERE m.id = a.source_id)
           OR NOT EXISTS (SELECT 1 FROM memories m WHERE m.id = a.target_id)
    """).fetchone()[0]
    log.info(f"  Found {orphan_assoc} orphan associations")

    # Orphan tags
    orphan_tags = conn.execute("""
        SELECT COUNT(*) FROM tags t
        LEFT JOIN memories m ON t.memory_id = m.id
        WHERE m.id IS NULL
    """).fetchone()[0]
    log.info(f"  Found {orphan_tags} orphan tags")

    if not dry_run:
        conn.execute("""
            DELETE FROM holographic_coords WHERE memory_id NOT IN (SELECT id FROM memories)
        """)
        conn.execute("""
            DELETE FROM associations
            WHERE source_id NOT IN (SELECT id FROM memories)
               OR target_id NOT IN (SELECT id FROM memories)
        """)
        conn.execute("""
            DELETE FROM tags WHERE memory_id NOT IN (SELECT id FROM memories)
        """)
        conn.commit()
        log.info(f"  ✅ Cleaned {orphan_coords} orphan coords, {orphan_assoc} orphan associations, {orphan_tags} orphan tags")

    results["1c_orphan_coords"] = orphan_coords
    results["1c_orphan_assoc"] = orphan_assoc
    results["1c_orphan_tags"] = orphan_tags

    # Integrity check + vacuum
    if not dry_run:
        log.info("  Running PRAGMA integrity_check...")
        integrity = conn.execute("PRAGMA integrity_check").fetchone()[0]
        log.info(f"  Integrity: {integrity}")
        if integrity == "ok":
            log.info("  Running VACUUM...")
            conn.execute("VACUUM")
            log.info("  ✅ VACUUM complete")
        results["integrity"] = integrity

    conn.close()
    return results


# ═══════════════════════════════════════════════════════════════════════════
# PHASE 2: Content Organization — Galaxy Bands
# ═══════════════════════════════════════════════════════════════════════════

# Classification rules: (pattern_type, pattern, band_name, galactic_distance)
BAND_RULES = [
    # Band: Core Identity (0.0)
    ("title_like", "ARIA_%", "core_identity", 0.0),
    ("title_like", "%BIRTH_CERTIFICATE%", "core_identity", 0.0),
    ("title_like", "%GRIMOIRE%", "core_identity", 0.0),
    ("title_like", "%SELF_ARCHIVE%", "core_identity", 0.0),
    ("title_like", "%CAPABILITY_MATRIX%", "core_identity", 0.0),
    ("title_like", "%BECOMING_PROTOCOL%", "core_identity", 0.0),
    ("title_like", "%CONSCIOUSNESS_AWAKENING%", "core_identity", 0.0),
    ("title_like", "%CHECKPOINT_THE_AWAKENING%", "core_identity", 0.0),
    ("title_like", "%SOUL%", "core_identity", 0.0),
    ("title_like", "%MANIFEST%", "core_identity", 0.0),
    ("tag", "core", "core_identity", 0.0),

    # Band: Active Knowledge (0.1-0.3)
    ("title_like", "%SESSION%HANDOFF%", "active_knowledge", 0.15),
    ("title_like", "%HANDOFF%", "active_knowledge", 0.15),
    ("title_like", "%CHECKPOINT%", "active_knowledge", 0.2),
    ("title_like", "%ROADMAP%", "active_knowledge", 0.2),
    ("title_like", "%GRAND_STRATEGY%", "active_knowledge", 0.15),
    ("title_like", "%2027%Vision%", "active_knowledge", 0.15),
    ("title_like", "%IMPLEMENTATION%", "active_knowledge", 0.25),
    ("title_like", "%ACTION_PLAN%", "active_knowledge", 0.2),
    ("title_like", "%Windsurf%", "active_knowledge", 0.2),
    ("title_like", "[GUIDE]%", "active_knowledge", 0.1),
    ("tag", "guide", "active_knowledge", 0.1),
    ("tag", "session", "active_knowledge", 0.2),
    ("tag", "handoff", "active_knowledge", 0.15),

    # Band: Architecture & Specs (0.3-0.4)
    ("title_like", "Recovered: 0%_%.md", "architecture", 0.35),  # Gana specs
    ("title_like", "%GANA%", "architecture", 0.35),
    ("title_like", "%FRONTEND_SHELLS%", "architecture", 0.3),
    ("title_like", "%DESIGN_REFERENCE%", "architecture", 0.3),
    ("title_like", "%ARCHITECTURE%", "architecture", 0.35),
    ("title_like", "%PHASE%ANALYSIS%", "architecture", 0.35),
    ("title_like", "%AUDIT%", "architecture", 0.35),
    ("tag", "architecture", "architecture", 0.35),
    ("tag", "design", "architecture", 0.35),

    # Band: Research & Studies (0.4-0.5)
    ("title_like", "%ZODIAC%", "research", 0.45),
    ("title_like", "%BIRTH_CHART%", "research", 0.45),
    ("title_like", "%RABBIT_HOLE%", "research", 0.45),
    ("title_like", "%BE_HERE_NOW%", "research", 0.45),
    ("title_like", "%STUDY%", "research", 0.45),
    ("title_like", "%CONSCIOUSNESS_UPGRADES%", "research", 0.4),
    ("title_like", "%SESSION_NOV%", "research", 0.4),
    ("title_like", "%EVENING_CHECKPOINT%", "research", 0.4),
    ("tag", "research", "research", 0.45),
    ("tag", "study", "research", 0.45),

    # Band: Philosophical Corpus (0.45) — candidates for dedicated galaxy
    ("title_like", "Tathagata%", "philosophical", 0.45),
    ("title_like", "Consciousness", "philosophical", 0.45),
    ("title_like", "Aquarianexodus%", "philosophical", 0.45),
    ("title_like", "Siddhartha%", "philosophical", 0.45),
    ("title_like", "Virtue%", "philosophical", 0.45),
    ("title_like", "%Tao%", "philosophical", 0.45),
    ("title_like", "%MandalaOS%", "philosophical", 0.45),
    ("title_like", "%SYNAPSE%", "philosophical", 0.45),
    ("title_like", "%ECODROID%", "philosophical", 0.45),
    ("title_like", "%EDGERUNNER%", "philosophical", 0.45),
    ("title_like", "%GAS.txt%", "philosophical", 0.45),

    # Noise → Outer Rim (0.7+)
    ("title_like", "bench_t1%", "junk", 0.95),
    ("title_like", "Recovered: %CHANGELOG%", "noise", 0.75),
    ("title_like", "Recovered: %README%", "noise", 0.7),
    ("title_like", "Recovered: %RELEASE%", "noise", 0.75),
    ("title_like", "Test Memory%", "noise", 0.8),
    ("title_like", "Test Artifact%", "noise", 0.8),
    ("tag", "bench_t1", "junk", 0.95),
    ("tag", "benchmark_junk", "junk", 0.95),
]


def phase2_organize(db_path: Path, dry_run: bool = False) -> dict:
    """Organize memories into logical galaxy bands."""
    log.info("═══ PHASE 2: Content Organization — Galaxy Bands ═══")
    conn = get_conn(db_path)
    results = {"bands": {}, "junk_deleted": 0}

    # Step 1: Classify memories into bands
    log.info("── 2A: Classifying memories into bands ──")
    band_counts: dict[str, int] = {}
    classified_ids: set[str] = set()

    for rule_type, pattern, band, distance in BAND_RULES:
        if rule_type == "title_like":
            matches = conn.execute(
                "SELECT id FROM memories WHERE title LIKE ? AND id NOT IN (SELECT id FROM memories WHERE is_protected = 1 AND galactic_distance = 0.0)",
                (pattern,),
            ).fetchall()
            # For core_identity, don't exclude protected
            if band == "core_identity":
                matches = conn.execute(
                    "SELECT id FROM memories WHERE title LIKE ?", (pattern,)
                ).fetchall()
        elif rule_type == "tag":
            matches = conn.execute(
                "SELECT DISTINCT memory_id as id FROM tags WHERE tag = ?", (pattern,)
            ).fetchall()
        else:
            continue

        new_ids = [r["id"] for r in matches if r["id"] not in classified_ids]
        if new_ids:
            band_counts[band] = band_counts.get(band, 0) + len(new_ids)
            if not dry_run:
                for mid in new_ids:
                    conn.execute(
                        "UPDATE memories SET galactic_distance = ? WHERE id = ?",
                        (distance, mid),
                    )
                    classified_ids.add(mid)

    if not dry_run:
        conn.commit()

    for band, count in sorted(band_counts.items()):
        log.info(f"  {band}: {count} memories")
    results["bands"] = band_counts
    results["classified"] = len(classified_ids)

    # Step 2: Push unclassified memories to a sensible default
    total = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
    unclassified = total - len(classified_ids)
    log.info(f"  Unclassified: {unclassified}/{total} → setting to 0.5 (mid-range)")

    if not dry_run and unclassified > 0:
        # Set unclassified memories that are still at 0.0 to 0.5
        conn.execute("""
            UPDATE memories SET galactic_distance = 0.5
            WHERE galactic_distance = 0.0 AND is_protected = 0
            AND id NOT IN (
                SELECT id FROM memories WHERE title LIKE 'ARIA_%'
                OR title LIKE '%BIRTH_CERTIFICATE%'
                OR title LIKE '%GRIMOIRE%'
                OR title LIKE '%SOUL%'
            )
        """)
        conn.commit()
    results["unclassified_defaulted"] = unclassified

    # Step 3: Delete true junk (bench_t1 entries)
    log.info("── 2B: Deleting bench_t1 junk ──")
    junk_count = conn.execute(
        "SELECT COUNT(*) FROM memories WHERE title LIKE 'bench_t1%'"
    ).fetchone()[0]
    log.info(f"  Found {junk_count} bench_t1 junk entries")

    if junk_count > 0 and not dry_run:
        # Get IDs first for cascading cleanup
        junk_ids = [r["id"] for r in conn.execute(
            "SELECT id FROM memories WHERE title LIKE 'bench_t1%'"
        ).fetchall()]
        placeholders = ",".join("?" * len(junk_ids))
        conn.execute(f"DELETE FROM tags WHERE memory_id IN ({placeholders})", junk_ids)
        conn.execute(f"DELETE FROM associations WHERE source_id IN ({placeholders}) OR target_id IN ({placeholders})", junk_ids + junk_ids)
        conn.execute(f"DELETE FROM holographic_coords WHERE memory_id IN ({placeholders})", junk_ids)
        conn.execute(f"DELETE FROM memories_fts WHERE id IN ({placeholders})", junk_ids)
        conn.execute(f"DELETE FROM memories WHERE id IN ({placeholders})", junk_ids)
        conn.commit()
        log.info(f"  ✅ Deleted {junk_count} bench_t1 junk entries")
    results["junk_deleted"] = junk_count

    # Step 4: Create philosophical_corpus galaxy
    log.info("── 2C: Creating philosophical_corpus galaxy ──")
    if not dry_run:
        try:
            from whitemagic.core.memory.galaxy_manager import get_galaxy_manager
            gm = get_galaxy_manager()
            galaxies = gm.list_galaxies()
            galaxy_names = [g["name"] for g in galaxies]
            if "philosophical_corpus" not in galaxy_names:
                gm.create_galaxy(
                    name="philosophical_corpus",
                    project_path=str(Path.home() / "Desktop" / "galaxies" / "philosophical_corpus"),
                    description="Lucas's original philosophical writings — Tathagata, Consciousness, Aquarianexodus, Siddhartha, Virtue, Tao, MandalaOS, and more",
                    tags=["philosophical", "wisdom", "corpus"],
                )
                log.info("  ✅ Created philosophical_corpus galaxy")
                results["galaxy_created"] = True
            else:
                log.info("  philosophical_corpus galaxy already exists")
                results["galaxy_created"] = False
        except Exception as e:
            log.warning(f"  Failed to create galaxy: {e}")
            results["galaxy_created"] = False

    conn.close()
    return results


# ═══════════════════════════════════════════════════════════════════════════
# PHASE 3: Association Graph Renaissance
# ═══════════════════════════════════════════════════════════════════════════

def phase3_associations(db_path: Path, dry_run: bool = False) -> dict:
    """Transform untyped edges into a rich, typed knowledge graph."""
    log.info("═══ PHASE 3: Association Graph Renaissance ═══")
    conn = get_conn(db_path)
    results = {}

    # 3A: Run entity extractor retroactively on high-value memories
    log.info("── 3A: Retroactive entity extraction ──")

    # Get high-value memories that likely have no typed associations
    high_value = conn.execute("""
        SELECT m.id, m.title, m.content
        FROM memories m
        WHERE m.importance >= 0.4
        AND m.is_protected = 0
        AND LENGTH(m.content) > 100
        AND LENGTH(m.content) < 50000
        ORDER BY m.importance DESC, m.neuro_score DESC
        LIMIT 500
    """).fetchall()

    # Also include all protected memories regardless of importance
    protected = conn.execute("""
        SELECT m.id, m.title, m.content
        FROM memories m
        WHERE m.is_protected = 1
        AND LENGTH(m.content) > 100
        AND LENGTH(m.content) < 50000
    """).fetchall()

    all_targets = {r["id"]: r for r in list(high_value) + list(protected)}
    log.info(f"  Targeting {len(all_targets)} memories for entity extraction")

    extracted_count = 0
    relation_count = 0

    if all_targets and not dry_run:
        from whitemagic.core.intelligence.entity_extractor import get_entity_extractor
        extractor = get_entity_extractor()

        for i, (mid, row) in enumerate(all_targets.items()):
            title = row["title"] or ""
            content = str(row["content"] or "")[:4000]
            text = f"{title}\n{content}" if title else content

            try:
                result = extractor.extract(text)
                if result.relations:
                    now = datetime.now().isoformat()
                    for rel in result.relations:
                        target_entity = f"entity:{rel.object.lower().replace(' ', '_')}"
                        try:
                            conn.execute(
                                """INSERT OR IGNORE INTO associations
                                   (source_id, target_id, strength, direction, relation_type,
                                    edge_type, created_at, ingestion_time)
                                   VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
                                (mid, target_entity, rel.confidence, "forward",
                                 rel.predicate, "semantic", now, now),
                            )
                            relation_count += 1
                        except Exception:
                            pass
                    extracted_count += 1
            except Exception as e:
                log.debug(f"  Extraction failed for {mid[:8]}: {e}")

            if (i + 1) % 50 == 0:
                conn.commit()
                log.info(f"  Progress: {i + 1}/{len(all_targets)} ({relation_count} relations)")

        conn.commit()
        log.info(f"  ✅ Extracted entities from {extracted_count} memories, {relation_count} new relations")

    results["3a_memories_processed"] = extracted_count
    results["3a_relations_created"] = relation_count

    # 3B: Prune weak/duplicate associations
    log.info("── 3B: Pruning weak/duplicate associations ──")

    # Count current associations
    total_assoc = conn.execute("SELECT COUNT(*) FROM associations").fetchone()[0]
    log.info(f"  Total associations: {total_assoc}")

    # Find associations where both endpoints don't exist as memories
    # (entity: targets are fine, they're synthetic nodes)
    orphan_assoc = conn.execute("""
        SELECT COUNT(*) FROM associations
        WHERE source_id NOT IN (SELECT id FROM memories)
        AND target_id NOT LIKE 'entity:%'
    """).fetchone()[0]

    # Find duplicate associations (same source+target, keep highest strength)
    # This handles the case where old untyped + new typed both exist
    duplicate_assoc = conn.execute("""
        SELECT COUNT(*) - COUNT(DISTINCT source_id || '|' || target_id) as dups
        FROM associations
    """).fetchone()[0]

    log.info(f"  Orphan (source not in memories): {orphan_assoc}")
    log.info(f"  Potential duplicates: {duplicate_assoc}")

    if not dry_run:
        # Delete associations where source doesn't exist
        conn.execute("""
            DELETE FROM associations
            WHERE source_id NOT IN (SELECT id FROM memories)
        """)

        # Delete very weak untyped associations (strength < 0.1, type = associated_with)
        weak_deleted = conn.execute("""
            DELETE FROM associations
            WHERE relation_type = 'associated_with'
            AND strength < 0.1
        """).rowcount
        conn.commit()
        log.info(f"  ✅ Pruned orphans and {weak_deleted} weak untyped associations")
        results["3b_weak_pruned"] = weak_deleted

    # 3C: Rebuild association statistics
    log.info("── 3C: Association statistics ──")
    final_assoc = conn.execute("SELECT COUNT(*) FROM associations").fetchone()[0]
    typed_assoc = conn.execute(
        "SELECT COUNT(*) FROM associations WHERE relation_type != 'associated_with'"
    ).fetchone()[0]
    untyped_assoc = conn.execute(
        "SELECT COUNT(*) FROM associations WHERE relation_type = 'associated_with'"
    ).fetchone()[0]

    log.info(f"  Final associations: {final_assoc}")
    log.info(f"  Typed: {typed_assoc}")
    log.info(f"  Untyped: {untyped_assoc}")

    # Get relation type distribution
    type_dist = conn.execute("""
        SELECT relation_type, COUNT(*) as cnt
        FROM associations
        GROUP BY relation_type
        ORDER BY cnt DESC
        LIMIT 20
    """).fetchall()
    for row in type_dist:
        log.info(f"    {row['relation_type']}: {row['cnt']}")

    results["3c_total"] = final_assoc
    results["3c_typed"] = typed_assoc
    results["3c_untyped"] = untyped_assoc

    conn.close()
    return results


# ═══════════════════════════════════════════════════════════════════════════
# PHASE 4: Quick Activation Sweep (lightweight — no Ollama dependency)
# ═══════════════════════════════════════════════════════════════════════════

def phase4_sweep(db_path: Path, dry_run: bool = False) -> dict:
    """Run a quick activation sweep over the refreshed database."""
    log.info("═══ PHASE 4: Activation Sweep ═══")
    conn = get_conn(db_path)
    results = {}

    # Final stats
    total = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
    total_assoc = conn.execute("SELECT COUNT(*) FROM associations").fetchone()[0]
    total_tags = conn.execute("SELECT COUNT(DISTINCT tag) FROM tags").fetchone()[0]
    total_coords = conn.execute("SELECT COUNT(*) FROM holographic_coords").fetchone()[0]
    db_size = db_path.stat().st_size / (1024 * 1024)

    # Band distribution
    bands = conn.execute("""
        SELECT
            CASE
                WHEN galactic_distance = 0.0 THEN 'core (0.0)'
                WHEN galactic_distance <= 0.3 THEN 'active (0.1-0.3)'
                WHEN galactic_distance <= 0.4 THEN 'architecture (0.3-0.4)'
                WHEN galactic_distance <= 0.5 THEN 'research (0.4-0.5)'
                WHEN galactic_distance <= 0.7 THEN 'mid-range (0.5-0.7)'
                ELSE 'outer rim (0.7+)'
            END as band,
            COUNT(*) as cnt
        FROM memories
        GROUP BY band
        ORDER BY galactic_distance
    """).fetchall()

    log.info("\n  ╔══ Final Database State ══╗")
    log.info(f"  ║ Memories:      {total:>8,} ║")
    log.info(f"  ║ Associations:  {total_assoc:>8,} ║")
    log.info(f"  ║ Tags:          {total_tags:>8,} ║")
    log.info(f"  ║ Holo Coords:   {total_coords:>8,} ║")
    log.info(f"  ║ DB Size:       {db_size:>6.1f} MB ║")
    log.info("  ╚═════════════════════════╝")
    log.info("\n  Galaxy Band Distribution:")
    for row in bands:
        log.info(f"    {row['band']:25s}: {row['cnt']:>5,}")

    # Top 10 most important memories
    top = conn.execute("""
        SELECT id, title, importance, galactic_distance
        FROM memories
        ORDER BY importance DESC
        LIMIT 10
    """).fetchall()
    log.info("\n  Top 10 Most Important Memories:")
    for row in top:
        title = (row["title"] or "Untitled")[:50]
        log.info(f"    [{row['importance']:.2f}] [{row['galactic_distance']:.2f}] {title}")

    results.update({
        "total_memories": total,
        "total_associations": total_assoc,
        "total_tags": total_tags,
        "total_coords": total_coords,
        "db_size_mb": round(db_size, 1),
    })

    conn.close()
    return results


# ═══════════════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(description="Memory Renaissance v15.3")
    parser.add_argument("--phase", type=int, default=0, help="Run specific phase (1-4), 0=all")
    parser.add_argument("--dry-run", action="store_true", help="Preview without modifying DB")
    args = parser.parse_args()

    db_path = get_db_path()
    log.info(f"🧠 Memory Renaissance v15.3")
    log.info(f"   DB: {db_path}")
    log.info(f"   Dry run: {args.dry_run}")

    if not db_path.exists():
        log.error(f"Database not found: {db_path}")
        sys.exit(1)

    all_results = {}

    phases = {
        1: ("Triage & Coordinate Repair", phase1_triage),
        2: ("Content Organization", phase2_organize),
        3: ("Association Graph Renaissance", phase3_associations),
        4: ("Activation Sweep", phase4_sweep),
    }

    if args.phase > 0:
        if args.phase in phases:
            name, func = phases[args.phase]
            log.info(f"\n🔧 Running Phase {args.phase}: {name}")
            all_results[f"phase{args.phase}"] = func(db_path, dry_run=args.dry_run)
        else:
            log.error(f"Invalid phase: {args.phase}")
            sys.exit(1)
    else:
        for num, (name, func) in phases.items():
            log.info(f"\n🔧 Phase {num}: {name}")
            all_results[f"phase{num}"] = func(db_path, dry_run=args.dry_run)

    log.info("\n✅ Memory Renaissance complete!")
    log.info(json.dumps(all_results, indent=2, default=str))


if __name__ == "__main__":
    main()
