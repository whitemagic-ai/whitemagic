"""Immune Memory System.

Stores and recalls past immune responses, enabling the system
to respond faster and more effectively to previously encountered threats.

Like biological immune memory (memory B cells and T cells), this system:
- Remembers which antibodies worked for which antigens
- Tracks encounter frequency
- Enables rapid recall for known threats
- Improves over time through learning
"""
from __future__ import annotations

import json
import logging
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT

logger = logging.getLogger(__name__)


@dataclass
class ImmuneMemoryRecord:
    """Single immune memory record."""

    antigen: str
    antibody: str
    first_encounter: str
    last_encounter: str
    encounter_count: int
    success_count: int
    failure_count: int
    metadata: dict[str, Any]


class ImmuneMemory:
    """Immune memory storage and recall.

    Maintains a persistent memory of past threats and responses,
    enabling faster and more effective future responses.
    """

    def __init__(self, storage_path: Path | None = None) -> None:
        self.storage_path = storage_path or (WM_ROOT / "immune" / "immune_memory.json")
        self.storage_path.parent.mkdir(parents=True, exist_ok=True)
        self.memories: dict[str, ImmuneMemoryRecord] = {}
        self._load()

    def remember(self, antigen: str, antibody: str, success: bool, metadata: dict[str, Any] | None = None) -> None:
        """Remember a threat-response pair.

        Args:
            antigen: The threat pattern
            antibody: The antibody that was used
            success: Whether the response was successful
            metadata: Additional context about the encounter

        """
        now = datetime.now(timezone.utc).isoformat()

        if antigen in self.memories:
            # Update existing memory
            memory = self.memories[antigen]
            memory.last_encounter = now
            memory.encounter_count += 1

            if success:
                memory.success_count += 1
            else:
                memory.failure_count += 1

            # Update metadata
            if metadata:
                memory.metadata.update(metadata)
        else:
            # Create new memory
            self.memories[antigen] = ImmuneMemoryRecord(
                antigen=antigen,
                antibody=antibody,
                first_encounter=now,
                last_encounter=now,
                encounter_count=1,
                success_count=1 if success else 0,
                failure_count=0 if success else 1,
                metadata=metadata or {},
            )

        self._save()

    def recall(self, antigen: str) -> dict[str, Any] | None:
        """Recall memory for a specific antigen.

        Returns:
            Memory record or None if not found

        """
        if antigen not in self.memories:
            return None

        memory = self.memories[antigen]
        return {
            "antigen": memory.antigen,
            "antibody": memory.antibody,
            "encounter_count": memory.encounter_count,
            "success_rate": (memory.success_count / memory.encounter_count * 100) if memory.encounter_count > 0 else 0,
            "last_seen": memory.last_encounter,
            "metadata": memory.metadata,
        }

    def has_memory(self, antigen: str) -> bool:
        """Check if we have memory for this antigen."""
        return antigen in self.memories

    def get_frequent_threats(self, limit: int = 10) -> list[dict[str, Any]]:
        """Get most frequently encountered threats."""
        sorted_memories = sorted(
            self.memories.values(),
            key=lambda m: m.encounter_count,
            reverse=True,
        )

        return [
            {
                "antigen": m.antigen,
                "antibody": m.antibody,
                "encounters": m.encounter_count,
                "success_rate": f"{(m.success_count / m.encounter_count * 100) if m.encounter_count > 0 else 0:.1f}%",
            }
            for m in sorted_memories[:limit]
        ]

    def get_problematic_threats(self) -> list[dict[str, Any]]:
        """Get threats with low success rates."""
        problematic = []

        for memory in self.memories.values():
            if memory.encounter_count >= 3:  # Only consider if seen at least 3 times
                success_rate = (memory.success_count / memory.encounter_count * 100) if memory.encounter_count > 0 else 0
                if success_rate < 50:
                    problematic.append({
                        "antigen": memory.antigen,
                        "antibody": memory.antibody,
                        "encounters": memory.encounter_count,
                        "success_rate": f"{success_rate:.1f}%",
                        "needs_improvement": True,
                    })

        return sorted(problematic, key=lambda x: x["encounters"], reverse=True)

    def _load(self) -> None:
        """Load immune memory from storage."""
        if not self.storage_path.exists():
            return

        try:
            data = json.loads(self.storage_path.read_text())
            for antigen, record_data in data.items():
                self.memories[antigen] = ImmuneMemoryRecord(**record_data)
        except Exception as e:
            logger.info(f"Warning: Failed to load immune memory: {e}")

    def _save(self) -> None:
        """Save immune memory to storage."""
        try:
            data = {
                antigen: asdict(memory)
                for antigen, memory in self.memories.items()
            }
            self.storage_path.write_text(json.dumps(data, indent=2))
        except Exception as e:
            logger.info(f"Warning: Failed to save immune memory: {e}")

    def clear(self) -> None:
        """Clear all immune memory (use with caution!)."""
        self.memories = {}
        self._save()

    def export_statistics(self) -> dict[str, Any]:
        """Export immune memory statistics."""
        total_memories = len(self.memories)
        total_encounters = sum(m.encounter_count for m in self.memories.values())
        total_successes = sum(m.success_count for m in self.memories.values())

        return {
            "total_memories": total_memories,
            "total_encounters": total_encounters,
            "overall_success_rate": f"{(total_successes / total_encounters * 100) if total_encounters > 0 else 0:.1f}%",
            "most_common_threats": self.get_frequent_threats(5),
            "problematic_threats": self.get_problematic_threats(),
            "memory_age": {
                "oldest": min((m.first_encounter for m in self.memories.values()), default="N/A"),
                "newest": max((m.last_encounter for m in self.memories.values()), default="N/A"),
            },
        }
