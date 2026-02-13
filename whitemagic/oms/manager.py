"""OMS Manager — Optimized Memory State Export/Import (v15.2).
=============================================================
Handles the full lifecycle of .mem packages:
  export → inspect → verify → import → price

The .mem format is a ZIP archive containing JSONL data files
and a manifest with metadata, quality metrics, and Merkle verification.
"""

from __future__ import annotations

import hashlib
import json
import logging
import sqlite3
import threading
import time
import zipfile
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

OMS_VERSION = "1.0"


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------

@dataclass
class OMSManifest:
    """Manifest for a .mem package."""
    oms_version: str = OMS_VERSION
    format: str = "whitemagic-galaxy-export"
    topic: str = ""
    description: str = ""
    memory_count: int = 0
    association_count: int = 0
    entity_count: int = 0
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    whitemagic_version: str = ""
    author_did: str = ""
    price_xrp: float = 0.0
    license: str = "CC-BY-4.0"
    avg_importance: float = 0.0
    core_memory_count: int = 0
    source_data_merkle_root: str = ""
    memory_content_hash: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "oms_version": self.oms_version,
            "format": self.format,
            "meta": {
                "topic": self.topic,
                "description": self.description,
                "memory_count": self.memory_count,
                "association_count": self.association_count,
                "entity_count": self.entity_count,
                "created_at": self.created_at,
                "whitemagic_version": self.whitemagic_version,
                "author_did": self.author_did,
                "price_xrp": self.price_xrp,
                "license": self.license,
            },
            "quality": {
                "avg_importance": round(self.avg_importance, 4),
                "core_memory_count": self.core_memory_count,
            },
            "verification": {
                "source_data_merkle_root": self.source_data_merkle_root,
                "memory_content_hash": self.memory_content_hash,
            },
        }


# ---------------------------------------------------------------------------
# Merkle tree for verification
# ---------------------------------------------------------------------------

def _compute_merkle_root(hashes: list[str]) -> str:
    """Compute Merkle root from a list of hex digest strings."""
    if not hashes:
        return hashlib.sha256(b"empty").hexdigest()
    if len(hashes) == 1:
        return hashes[0]

    # Pad to even length
    if len(hashes) % 2 == 1:
        hashes.append(hashes[-1])

    next_level: list[str] = []
    for i in range(0, len(hashes), 2):
        combined = hashes[i] + hashes[i + 1]
        next_level.append(hashlib.sha256(combined.encode()).hexdigest())

    return _compute_merkle_root(next_level)


# ---------------------------------------------------------------------------
# OMS Manager
# ---------------------------------------------------------------------------

class OMSManager:
    """Manages OMS .mem package export, import, and verification."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._total_exports = 0
        self._total_imports = 0

    def export_galaxy(
        self,
        galaxy: str = "default",
        output_path: str = "",
        topic: str = "",
        description: str = "",
        price_xrp: float = 0.0,
        license_str: str = "CC-BY-4.0",
        include_embeddings: bool = False,
    ) -> dict[str, Any]:
        """Export a Galaxy as a .mem package.

        Args:
            galaxy: Galaxy name to export (default = active galaxy).
            output_path: Where to write the .mem file.
            topic: Topic description for the manifest.
            description: Detailed description.
            price_xrp: Suggested price in XRP.
            license_str: License identifier.
            include_embeddings: Include embedding vectors (larger file).

        Returns:
            Export result dict.
        """
        start = time.perf_counter()

        if not output_path:
            output_path = str(Path.home() / "exports" / f"{galaxy}_{int(time.time())}.mem")

        output = Path(output_path)
        output.parent.mkdir(parents=True, exist_ok=True)

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            pool = um.backend.pool
        except Exception as e:
            return {"status": "error", "reason": f"Cannot access memory system: {e}"}

        # Collect memories
        memory_hashes: list[str] = []
        memory_lines: list[str] = []
        assoc_lines: list[str] = []
        total_importance = 0.0
        core_count = 0

        try:
            with pool.connection() as conn:
                conn.row_factory = sqlite3.Row
                # Export memories
                rows = conn.execute(
                    """SELECT id, title, content, memory_type, importance,
                              emotional_valence, tags, metadata, created_at,
                              galactic_distance, x, y, z, w, v
                       FROM memories ORDER BY importance DESC"""
                ).fetchall()

                for row in rows:
                    entry = {
                        "id": row["id"],
                        "title": row["title"] or "",
                        "content": row["content"] or "",
                        "memory_type": row["memory_type"] or "short_term",
                        "importance": row["importance"] or 0.5,
                        "emotional_valence": row["emotional_valence"] or 0.0,
                        "tags": row["tags"] or "",
                        "galactic_distance": row["galactic_distance"] or 0.5,
                        "x": row["x"], "y": row["y"], "z": row["z"],
                        "w": row["w"], "v": row["v"],
                    }
                    line = json.dumps(entry, ensure_ascii=False)
                    memory_lines.append(line)
                    content_hash = hashlib.sha256(line.encode()).hexdigest()
                    memory_hashes.append(content_hash)
                    total_importance += entry["importance"]
                    if entry["galactic_distance"] < 0.2:
                        core_count += 1

                # Export associations
                assoc_rows = conn.execute(
                    """SELECT source_id, target_id, strength, direction,
                              relation_type, edge_type, created_at
                       FROM associations ORDER BY strength DESC"""
                ).fetchall()

                for row in assoc_rows:
                    entry = {
                        "source_id": row["source_id"],
                        "target_id": row["target_id"],
                        "strength": row["strength"],
                        "direction": row["direction"] or "undirected",
                        "relation_type": row["relation_type"] or "associated_with",
                        "edge_type": row["edge_type"] or "semantic",
                    }
                    assoc_lines.append(json.dumps(entry, ensure_ascii=False))

        except Exception as e:
            return {"status": "error", "reason": f"Database read failed: {e}"}

        if not memory_lines:
            return {"status": "error", "reason": "No memories to export"}

        # Compute verification
        merkle_root = _compute_merkle_root(memory_hashes)
        all_content = "\n".join(memory_lines)
        content_hash = hashlib.sha256(all_content.encode()).hexdigest()

        # Get version
        wm_version = ""
        try:
            from whitemagic import __version__
            wm_version = __version__
        except Exception:
            pass

        # Build manifest
        manifest = OMSManifest(
            topic=topic or galaxy,
            description=description or f"Export of galaxy '{galaxy}'",
            memory_count=len(memory_lines),
            association_count=len(assoc_lines),
            whitemagic_version=wm_version,
            price_xrp=price_xrp,
            license=license_str,
            avg_importance=total_importance / len(memory_lines) if memory_lines else 0,
            core_memory_count=core_count,
            source_data_merkle_root=merkle_root,
            memory_content_hash=content_hash,
        )

        # Write ZIP
        try:
            with zipfile.ZipFile(str(output), "w", zipfile.ZIP_DEFLATED) as zf:
                zf.writestr("manifest.json", json.dumps(manifest.to_dict(), indent=2))
                zf.writestr("memories.jsonl", "\n".join(memory_lines))
                zf.writestr("associations.jsonl", "\n".join(assoc_lines))
                zf.writestr("verification.json", json.dumps({
                    "merkle_root": merkle_root,
                    "content_hash": content_hash,
                    "memory_count": len(memory_lines),
                    "association_count": len(assoc_lines),
                }))
        except Exception as e:
            return {"status": "error", "reason": f"Failed to write .mem file: {e}"}

        elapsed = (time.perf_counter() - start) * 1000
        file_size = output.stat().st_size

        with self._lock:
            self._total_exports += 1

        logger.info(
            f"📦 OMS export: {len(memory_lines)} memories + {len(assoc_lines)} associations "
            f"→ {output.name} ({file_size / 1024:.0f}KB, {elapsed:.0f}ms)"
        )

        return {
            "status": "ok",
            "path": str(output),
            "memory_count": len(memory_lines),
            "association_count": len(assoc_lines),
            "merkle_root": merkle_root,
            "file_size_bytes": file_size,
            "duration_ms": round(elapsed, 1),
        }

    def inspect(self, path: str) -> dict[str, Any]:
        """Preview contents of a .mem package without importing.

        Args:
            path: Path to .mem file.

        Returns:
            Manifest and summary info.
        """
        mem_path = Path(path).expanduser()
        if not mem_path.exists():
            return {"status": "error", "reason": f"File not found: {path}"}

        try:
            with zipfile.ZipFile(str(mem_path), "r") as zf:
                manifest = json.loads(zf.read("manifest.json"))
                # Count lines in JSONL files
                mem_count = len(zf.read("memories.jsonl").decode().strip().split("\n"))
                assoc_count = len(zf.read("associations.jsonl").decode().strip().split("\n"))

                return {
                    "status": "ok",
                    "manifest": manifest,
                    "actual_memory_count": mem_count,
                    "actual_association_count": assoc_count,
                    "file_size_bytes": mem_path.stat().st_size,
                    "files": zf.namelist(),
                }
        except Exception as e:
            return {"status": "error", "reason": f"Failed to inspect: {e}"}

    def verify(self, path: str) -> dict[str, Any]:
        """Verify Merkle root and content hash of a .mem package.

        Args:
            path: Path to .mem file.

        Returns:
            Verification result.
        """
        mem_path = Path(path).expanduser()
        if not mem_path.exists():
            return {"status": "error", "verified": False, "reason": f"File not found: {path}"}

        try:
            with zipfile.ZipFile(str(mem_path), "r") as zf:
                verification = json.loads(zf.read("verification.json"))
                memories_data = zf.read("memories.jsonl").decode()

                # Recompute content hash
                actual_hash = hashlib.sha256(memories_data.encode()).hexdigest()
                hash_match = actual_hash == verification.get("content_hash", "")

                # Recompute Merkle root
                lines = memories_data.strip().split("\n")
                line_hashes = [hashlib.sha256(line.encode()).hexdigest() for line in lines]
                actual_root = _compute_merkle_root(line_hashes)
                root_match = actual_root == verification.get("merkle_root", "")

                verified = hash_match and root_match

                return {
                    "status": "ok",
                    "verified": verified,
                    "content_hash_match": hash_match,
                    "merkle_root_match": root_match,
                    "expected_hash": verification.get("content_hash", ""),
                    "actual_hash": actual_hash,
                    "expected_root": verification.get("merkle_root", ""),
                    "actual_root": actual_root,
                    "memory_count": len(lines),
                }
        except Exception as e:
            return {"status": "error", "verified": False, "reason": f"Verification failed: {e}"}

    def import_mem(
        self,
        path: str,
        galaxy: str = "",
        verify_first: bool = True,
    ) -> dict[str, Any]:
        """Import a .mem package into a new or existing Galaxy.

        Args:
            path: Path to .mem file.
            galaxy: Target galaxy name. If empty, derives from manifest topic.
            verify_first: Verify integrity before importing.

        Returns:
            Import result dict.
        """
        start = time.perf_counter()
        mem_path = Path(path).expanduser()

        if not mem_path.exists():
            return {"status": "error", "reason": f"File not found: {path}"}

        # Verify integrity
        if verify_first:
            v_result = self.verify(path)
            if not v_result.get("verified", False):
                return {"status": "error", "reason": "Verification failed", "details": v_result}

        try:
            with zipfile.ZipFile(str(mem_path), "r") as zf:
                manifest = json.loads(zf.read("manifest.json"))
                memories_data = zf.read("memories.jsonl").decode()
                assoc_data = zf.read("associations.jsonl").decode()
        except Exception as e:
            return {"status": "error", "reason": f"Failed to read .mem file: {e}"}

        if not galaxy:
            galaxy = manifest.get("meta", {}).get("topic", "imported").replace(" ", "_").lower()

        # Import into the active memory system
        imported_memories = 0
        imported_associations = 0

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()

            # Import memories
            for line in memories_data.strip().split("\n"):
                if not line.strip():
                    continue
                try:
                    entry = json.loads(line)
                    tags_raw = entry.get("tags", "")
                    if isinstance(tags_raw, str):
                        tags = set(t.strip() for t in tags_raw.split(",") if t.strip())
                    else:
                        tags = set(tags_raw) if tags_raw else set()
                    tags.add("oms_import")
                    tags.add(f"oms_galaxy:{galaxy}")

                    um.store(
                        content=entry.get("content", ""),
                        title=entry.get("title", ""),
                        importance=entry.get("importance", 0.5),
                        tags=tags,
                        metadata={
                            "oms_source": str(mem_path.name),
                            "oms_original_id": entry.get("id", ""),
                            "oms_galaxy": galaxy,
                        },
                    )
                    imported_memories += 1
                except Exception:
                    continue

            # Import associations (best-effort — IDs may not match)
            pool = um.backend.pool
            now = datetime.now().isoformat()
            with pool.connection() as conn:
                for line in assoc_data.strip().split("\n"):
                    if not line.strip():
                        continue
                    try:
                        entry = json.loads(line)
                        conn.execute(
                            """INSERT OR IGNORE INTO associations
                               (source_id, target_id, strength, direction,
                                relation_type, edge_type, created_at)
                               VALUES (?, ?, ?, ?, ?, ?, ?)""",
                            (
                                entry.get("source_id", ""),
                                entry.get("target_id", ""),
                                entry.get("strength", 0.5),
                                entry.get("direction", "undirected"),
                                entry.get("relation_type", "associated_with"),
                                entry.get("edge_type", "semantic"),
                                now,
                            ),
                        )
                        imported_associations += 1
                    except Exception:
                        continue
                conn.commit()

        except Exception as e:
            return {"status": "error", "reason": f"Import failed: {e}"}

        elapsed = (time.perf_counter() - start) * 1000

        with self._lock:
            self._total_imports += 1

        logger.info(
            f"📥 OMS import: {imported_memories} memories + {imported_associations} associations "
            f"from {mem_path.name} → galaxy '{galaxy}' ({elapsed:.0f}ms)"
        )

        return {
            "status": "ok",
            "galaxy": galaxy,
            "imported_memories": imported_memories,
            "imported_associations": imported_associations,
            "source": str(mem_path.name),
            "duration_ms": round(elapsed, 1),
        }

    def price(self, path: str) -> dict[str, Any]:
        """Estimate value of a .mem package based on quality metrics.

        Pricing heuristic:
            base = memory_count * 0.01 XRP
            quality_multiplier = avg_importance * 2
            core_bonus = core_memories * 0.1 XRP
        """
        info = self.inspect(path)
        if info.get("status") != "ok":
            return info

        manifest = info.get("manifest", {})
        meta = manifest.get("meta", {})
        quality = manifest.get("quality", {})

        mem_count = meta.get("memory_count", 0)
        avg_importance = quality.get("avg_importance", 0.5)
        core_count = quality.get("core_memory_count", 0)

        base = mem_count * 0.01
        quality_mult = max(1.0, avg_importance * 2)
        core_bonus = core_count * 0.1
        estimated = round(base * quality_mult + core_bonus, 2)

        return {
            "status": "ok",
            "estimated_xrp": estimated,
            "breakdown": {
                "base_value": round(base, 2),
                "quality_multiplier": round(quality_mult, 2),
                "core_bonus": round(core_bonus, 2),
            },
            "memory_count": mem_count,
            "avg_importance": round(avg_importance, 4),
            "core_memories": core_count,
        }

    def list_local(self, search_dir: str = "") -> dict[str, Any]:
        """List available .mem packages in a directory."""
        if not search_dir:
            search_dir = str(Path.home() / "exports")

        search_path = Path(search_dir)
        if not search_path.exists():
            return {"status": "ok", "packages": [], "directory": search_dir}

        packages = []
        for mem_file in search_path.glob("*.mem"):
            try:
                info = self.inspect(str(mem_file))
                if info.get("status") == "ok":
                    manifest = info.get("manifest", {})
                    packages.append({
                        "path": str(mem_file),
                        "name": mem_file.stem,
                        "size_bytes": mem_file.stat().st_size,
                        "topic": manifest.get("meta", {}).get("topic", ""),
                        "memory_count": manifest.get("meta", {}).get("memory_count", 0),
                    })
            except Exception:
                continue

        return {"status": "ok", "packages": packages, "directory": search_dir}

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return {
                "total_exports": self._total_exports,
                "total_imports": self._total_imports,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_manager: OMSManager | None = None
_manager_lock = threading.Lock()


def get_oms_manager() -> OMSManager:
    """Get the global OMSManager singleton."""
    global _manager
    if _manager is None:
        with _manager_lock:
            if _manager is None:
                _manager = OMSManager()
    return _manager
