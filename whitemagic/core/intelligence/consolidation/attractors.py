"""Black Hole Attractors - Intelligent Memory Consolidation.
========================================================

Implements gravitational attractors for the memory system.
High-gravity memories (Black Holes) pull in lower-gravity related memories
for consolidation, compression, or archival.

Concepts:
- Mass: Derived from importance, access count, and connectivity.
- Event Horizon: Radius within which memories are captured.
- Spaghettification: The process of compressing captured memories into the attractor.
"""

import math
from dataclasses import dataclass
from datetime import datetime
from typing import Any


@dataclass
class Attractor:
    memory_id: str
    center: tuple[float, float, float, float] # x, y, z, w
    mass: float
    event_horizon: float

    @property
    def density(self) -> float:
        return self.mass / (self.event_horizon ** 3 + 1e-6)

class AttractorManager:
    """Manages the physics of memory consolidation via attractors."""

    def __init__(self, memory_manager: Any) -> None:
        self.manager = memory_manager

    def identify_attractors(self, threshold_mass: float = 2.0) -> list[Attractor]:
        """Identify existing memories that have become massive enough to be attractors.
        Mass is primarily determined by the W-axis (Importance/Gravity).
        """
        # Get all memories with coordinates
        # For now, we iterate recent/active. In full prod, this would query the SpatialIndex.
        attractors = []

        # We need access to the holographic index to get coordinates efficiently
        # Or we rely on metadata if stored there.
        # Let's assume we can fetch high-importance memories via the manager.

        high_grav_memories = self.manager.search_memories(min_importance=0.8, limit=50)

        for mem_data in high_grav_memories:
            # Unwrap dict if needed (MemoryManager returns dicts usually)
            if isinstance(mem_data, dict) and "entry" in mem_data:
                mem = mem_data["entry"]
            else:
                mem = mem_data

            mem_id = mem.get("id")
            meta = mem.get("metadata", {})

            # Try to get coordinates from metadata or calculate them
            # For this MVP, we assume they might be in metadata or we recalculate
            # If we can't find coords, we skip
            # In v5.1, we should be able to query the Rust index for coords

            # Simple heuristic for mass without full physics engine yet:
            # Mass = Importance * (1 + 0.1 * link_count)
            # Check metadata for explicit gravity (e.g. from tests or manual set)
            if "gravity" in meta:
                importance = float(meta["gravity"])
            else:
                # Fallback to importance field (0.0 - 1.0 range usually, but can be higher)
                importance = float(mem.get("importance", 0.5))

            # Check if this qualifies as a black hole candidate
            if importance >= threshold_mass:
                # We need coordinates. If not in metadata, we can't simulate physics.
                # Placeholder: get from holographic index if available
                coords = self._get_coords(mem_id)
                if coords:
                    # Event horizon scales with mass
                    horizon = 0.2 * importance
                    attractors.append(Attractor(
                        memory_id=mem_id,
                        center=coords,
                        mass=importance,
                        event_horizon=horizon,
                    ))

        return attractors

    def _get_coords(self, memory_id: str) -> tuple[float, float, float, float] | None:
        """Helper to get coordinates from Holographic Index."""
        try:
            from whitemagic.core.memory.holographic import get_holographic_memory
            get_holographic_memory()
            # This is a bit of a hack since we don't have a direct 'get_coords' on the python wrapper yet
            # But we can try to re-encode or assume it's in the index.
            # Ideally, the Rust index supports `get_point(id)`.
            # For MVP, let's simulate or check if `find_clusters` logic exposes points.

            # If we can't get real coords efficiently, we return None
            # But wait! UnifiedMemory stores coords in SQLite if available.
            # Accessing backend directly is dirty but effective for now.
            if hasattr(self.manager, "unified") and hasattr(self.manager.unified.backend, "get_coords"):
                return self.manager.unified.backend.get_coords(memory_id)  # type: ignore[no-any-return]

        except Exception:
            pass
        return None

    def calculate_gravitational_pull(self, attractors: list[Attractor], candidates: list[dict]) -> list[dict]:
        """Calculate which candidates are pulled into which attractors.
        Returns a plan: list of { 'attractor': id, 'captured': [id, id, ...] }.
        """
        pull_plan = []

        for attractor in attractors:
            captured = []
            ax, ay, az, aw = attractor.center

            for mem in candidates:
                mem_id = mem.get("id")

                # Skip if it's the attractor itself
                if mem_id == attractor.memory_id:
                    continue

                # Skip high importance memories (they resist pull)
                # Check metadata for gravity, else top-level importance, else default
                meta = mem.get("metadata", {})
                gravity = float(meta.get("gravity", mem.get("importance", 0.5)))

                if gravity > 0.8:
                    continue

                # Get candidate coords
                c_coords = self._get_coords(str(mem_id)) if mem_id else None
                if not c_coords:
                    continue

                cx, cy, cz, cw = c_coords

                # Euclidean distance in 3D spatial (ignore W for capture radius)
                # We want to capture things spatially close (semantically/temporally),
                # regardless of importance delta. W delta is expected to be large.
                dist = math.sqrt(
                    (ax-cx)**2 + (ay-cy)**2 + (az-cz)**2,
                )

                if dist < attractor.event_horizon:
                    captured.append(mem_id)

            if captured:
                pull_plan.append({
                    "attractor_id": attractor.memory_id,
                    "captured_ids": captured,
                    "mass": attractor.mass,
                })

        return pull_plan

    def execute_spaghettification(self, plan: list[dict], dry_run: bool = True) -> dict[str, Any]:
        """Compress captured memories into the attractor (link + archive).
        """
        results = {
            "attractors_active": len(plan),
            "memories_absorbed": 0,
            "dry_run": dry_run,
        }

        if dry_run:
            for p in plan:
                results["memories_absorbed"] += len(p["captured_ids"])
            return results

        for event in plan:
            attractor_id = event["attractor_id"]
            # Load attractor
            attractor_mem = self.manager.get_memory(attractor_id)
            if not attractor_mem or "error" in attractor_mem:
                continue

            absorbed_count = 0
            summary_buffer = []

            for victim_id in event["captured_ids"]:
                victim = self.manager.get_memory(victim_id)
                if not victim or "error" in victim:
                    continue

                # Link victim to attractor
                self.manager.associate(attractor_id, victim_id, strength=0.9)

                # Add victim content to summary buffer
                summary_buffer.append(f"- {victim.get('title')}: {str(victim.get('content'))[:100]}...")

                # Archive victim (Soft Delete / Demote)
                # We tag it as 'absorbed' and lower its priority
                self.manager.update_memory(
                    victim_id,
                    add_tags=["absorbed", f"absorbed_by_{attractor_id}"],
                    # In real impl, we might set status='archived' or memory_type='archived'
                )
                absorbed_count += 1

            # Append summary to attractor?
            # Or just strengthen the attractor.
            if summary_buffer:
                timestamp = datetime.now().isoformat()
                append_text = f"\n\n## Absorbed Memories ({timestamp})\n" + "\n".join(summary_buffer)
                new_content = attractor_mem.get("content", "") + append_text
                self.manager.update_memory(attractor_id, content=new_content)

            results["memories_absorbed"] += absorbed_count

        return results
