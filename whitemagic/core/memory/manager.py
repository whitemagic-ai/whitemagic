"""WhiteMagic Core - Memory Management System (Unified Adapter).

This module provides the MemoryManager class that adapts the legacy file-based API
to the new UnifiedMemory (SQLite) system. Maintains backward compatibility for
CLI, MCP, and other consumers.
"""

from __future__ import annotations

from collections.abc import Sequence
from datetime import datetime
from pathlib import Path
from typing import Any, Union, cast

from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.core.memory.unified_types import Memory, MemoryType
from whitemagic.core.resonance.gan_ying_enhanced import EventType


class MemoryManager:
    """Wrapper around UnifiedMemory for backward compatibility.
    Adapts legacy method signatures to UnifiedMemory calls.
    """

    def __init__(self, base_dir: Union[str, Path] = "."):
        self.base_path = Path(base_dir)
        # Initialize a dedicated UnifiedMemory instance for this manager
        # if a custom path is provided (for test isolation)
        if str(base_dir) != ".":
            from whitemagic.core.memory.unified import UnifiedMemory
            self.unified = UnifiedMemory(base_path=self.base_path)
        else:
            self.unified = get_unified_memory()

        # Internal index emulation for legacy tests
        self._index_cache: dict[str, dict[str, Any]] = {}

    @property
    def _index(self) -> dict[str, dict[str, Any]]:
        """Emulate the legacy _index attribute."""
        # This is expensive but needed for legacy test compatibility
        all_mems = self.unified.list_recent(limit=1000)
        return {m.id: self._memory_to_dict(m) for m in all_mems}

    def create_memory(
        self,
        title: str,
        content: str,
        memory_type: str = "short_term",
        tags: Sequence[str] | None = None,
        extra_fields: dict[str, Any] | None = None,
    ) -> Path:
        """Create a new memory. Returns a virtual Path for legacy compatibility."""
        mem_type_enum = self._parse_memory_type(memory_type)
        # Normalize tags to lowercase for legacy compatibility
        tags_set = {t.lower() for t in tags} if tags else set()
        metadata = extra_fields or {}

        # Add Wu Xing metadata if not present (legacy compatibility)
        if "wu_xing_phase" not in metadata:
            try:
                from whitemagic.core.intelligence.wisdom.wu_xing import get_wu_xing
                element = get_wu_xing().detect_current_phase()
                metadata["wu_xing_phase"] = element.value
                metadata["wu_xing_timestamp"] = datetime.now().isoformat()
            except (ImportError, AttributeError):
                metadata["wu_xing_phase"] = "earth"
                metadata["wu_xing_timestamp"] = datetime.now().isoformat()

        # Extract top-level fields from metadata if present
        importance = float(metadata.get("importance", 0.5))
        valence = float(metadata.get("emotional_valence", 0.0))

        memory = self.unified.store(
            content=content,
            memory_type=mem_type_enum,
            tags=tags_set,
            title=title,
            metadata=metadata,
            importance=importance,
            emotional_valence=valence,
        )

        # Return a Path object as expected by legacy tests
        # We use a virtual path that includes the ID
        return Path(f"{memory.id}.md")

    def search_memories(
        self,
        query: str | None = None,
        memory_type: str | None = None,
        tags: Sequence[str] | None = None,
        limit: int = 20,
        min_importance: float = 0.0,
        *,
        include_archived: bool = False,
        include_content: bool = True,
    ) -> list[dict[str, Any]]:
        """Search memories with relevance scoring."""
        mem_type_enum = self._parse_memory_type(memory_type) if memory_type else None
        tags_set = set(tags) if tags else None

        results = self.unified.search(
            query=query,
            tags=tags_set,
            memory_type=mem_type_enum,
            min_importance=min_importance,
            limit=limit,
        )

        # Emit search event for Gan Ying integration
        try:
            from whitemagic.core.resonance.gan_ying import emit_event
            emit_event(
                source="memory_manager_adapter",
                event_type=EventType.SEARCH_COMPLETED,
                data={
                    "query": query,
                    "memory_type": memory_type,
                    "tags": list(tags) if tags else [],
                    "result_count": len(results),
                    "timestamp": datetime.now().isoformat(),
                },
            )
        except Exception:
            # Don't fail search if event emission fails
            pass

        # Convert to legacy search result format with real scores
        search_results = []
        for mem in results:
            # Calculate relevance score from FTS rank (if available)
            # FTS5 rank is negative (lower = better), normalize to 0-1 scale
            fts_rank = mem.metadata.get("fts_rank", 0.0)
            if query and fts_rank < 0:
                # Normalize FTS rank to 0-1 (higher = better)
                # Typical FTS ranks: -10 to -0.1, map to 0.9-0.1
                score = max(0.1, min(1.0, 1.0 + (fts_rank / 10.0)))
            else:
                # No FTS rank: use importance as fallback
                score = mem.importance

            search_results.append({
                "entry": self._memory_to_dict(mem),
                "preview": str(mem.content)[:200] if mem.content else "",
                "score": score,
            })

        return search_results

    def read_recent_memories(
        self,
        memory_type: str = "short_term",
        limit: int = 5,
        include_archived: bool = False,
    ) -> list[dict[str, Any]]:
        """Read recent memories."""
        mem_type_enum = self._parse_memory_type(memory_type)
        memories = self.unified.list_recent(limit=limit * 2, memory_type=mem_type_enum)

        # Filter out archived memory for legacy compatibility
        if not include_archived:
            memories = [m for m in memories if m.metadata.get("status") != "archived"]

        # Trim to requested limit
        memories = memories[:limit]

        return [
            {
                "entry": self._memory_to_dict(mem),
                "frontmatter": {"title": mem.title, "tags": list(mem.tags), **mem.metadata},
                "body": str(mem.content),
            }
            for mem in memories
        ]

    def update_memory(
        self,
        filename: str, # Treated as memory_id or filename
        title: str | None = None,
        content: str | None = None,
        tags: list[str] | None = None,
        add_tags: list[str] | None = None,
        remove_tags: list[str] | None = None,
    ) -> dict[str, Any]:
        """Update a memory."""
        memory_id = filename.replace(".md", "") if filename.endswith(".md") else filename
        memory = self.unified.recall(memory_id)
        if not memory:
            return {"success": False, "error": "Memory not found"}

        if title:
            memory.title = title
        if content:
            memory.content = content
        if tags:
            memory.tags = {t.lower() for t in tags}
        if add_tags:
            memory.tags.update({t.lower() for t in add_tags})
        if remove_tags:
            memory.tags.difference_update({t.lower() for t in remove_tags})

        # Persist update
        self.unified.backend.store(memory)

        # Re-index holographic coords after update
        if self.unified.holographic:
            coords = self.unified.holographic.index_memory(memory.id, memory.to_dict())
            if coords:
                self.unified.backend.store_coords(memory.id, *coords)

        return {"success": True, **self._memory_to_dict(memory)}

    def delete_memory(self, filename: str, permanent: bool = False) -> dict[str, Any]:
        """Delete a memory. Supports soft-delete (archiving) for legacy compatibility."""
        memory_id = filename.replace(".md", "") if filename.endswith(".md") else filename

        if not permanent:
            # Soft delete: update metadata and importance
            memory = self.unified.recall(memory_id)
            if memory:
                memory.metadata["status"] = "archived"
                memory.importance = 0.0
                self.unified.backend.store(memory)
                return {"status": "deleted", "id": memory_id, "success": True, "action": "archived"}
            return {"error": "Memory not found"}

        # Permanent delete
        success = self.unified.backend.delete(memory_id)
        if success:
            return {"status": "deleted", "id": memory_id, "success": True, "action": "permanently_deleted"}
        else:
            return {"error": "Failed to delete"}

    def get_memory(self, memory_id: str, include_metadata: bool = True) -> dict[str, Any]:
        """Get a single memory and update access metrics."""
        real_id = memory_id.replace(".md", "") if memory_id.endswith(".md") else memory_id
        memory = self.unified.recall(real_id)
        if not memory:
            return {"error": "Memory not found"}

        # Don't return archived memories via this method for legacy compatibility
        if memory.metadata.get("status") == "archived":
            return {"error": "Memory not found"}

        # Update access metrics
        memory.access()
        self.unified.backend.store(memory)

        return self._memory_to_dict(memory)

    def consolidate(self) -> int:
        """Consolidate memories - strengthen important, decay unimportant."""
        return self.unified.consolidate()

    def get_stats(self) -> dict[str, Any]:
        """Get memory system statistics."""
        return self.unified.get_stats()

    def _parse_memory_type(self, type_str: str) -> MemoryType:
        try:
            return cast("MemoryType", getattr(MemoryType, type_str.upper()))
        except AttributeError:
            return MemoryType.SHORT_TERM

    def list_all_memories(self, include_archived: bool = False, sort_by: str = "created") -> dict[str, list[dict[str, Any]]]:
        """List all memories grouped by type."""
        all_mems = self.unified.list_recent(limit=1000)

        if sort_by == "accessed":
            all_mems.sort(key=lambda x: x.accessed_at, reverse=True)

        result: dict[str, list[dict[str, Any]]] = {
            "short_term": [],
            "long_term": [],
            "archived": [],
        }

        for m in all_mems:
            if not include_archived and m.metadata.get("status") == "archived":
                continue

            m_dict = self._memory_to_dict(m)
            m_type = m.memory_type.name.lower()
            if m_type in result:
                result[m_type].append(m_dict)

            # Map archived memories to the archived list even if include_archived is False
            # but we won't show them in short/long term lists
            if m.metadata.get("status") == "archived" and include_archived:
                result["archived"].append(m_dict)

        return result

    def list_all_tags(self) -> dict[str, Any]:
        """Get all tags and their counts."""
        tag_counts = self.unified.get_tag_counts(limit=100)
        total_usages = sum(count for _, count in tag_counts)

        # Get count of memories with tags
        all_mems = self.unified.list_recent(limit=1000)
        mems_with_tags = sum(1 for m in all_mems if m.tags)

        return {
            "total_unique_tags": len(tag_counts),
            "total_tag_usages": total_usages,
            "total_memories_with_tags": mems_with_tags,
            "tags": [{"tag": tag, "count": count} for tag, count in tag_counts],
        }

    def generate_context_summary(self, tier: int = 1) -> str:
        """Emulate legacy context summary generation."""
        limits = {0: 3, 1: 10, 2: 50}
        limit = limits.get(tier, 10)
        recent = self.unified.list_recent(limit=limit)

        summary = []
        for m in recent:
            summary.append(f"Title: {m.title}\nContent: {m.content}\n")

        return "\n---\n".join(summary)

    def consolidate_short_term(self, dry_run: bool = False) -> dict[str, int]:
        """Emulate legacy consolidation."""
        count = self.unified.consolidate()
        return {
            "archived": count,
            "auto_promoted": 0,
            "decayed": 0,
        }

    def normalize_legacy_tags(self, dry_run: bool = True) -> dict[str, Any]:
        """Emulate legacy tag normalization."""
        # Modern UnifiedMemory already normalizes tags to lowercase on store
        return {
            "dry_run": dry_run,
            "affected_memories": 0,
            "changes": [],
        }

    def restore_memory(self, memory_id: str, memory_type: str = "short_term") -> dict[str, Any]:
        """Emulate legacy restore."""
        real_id = memory_id.replace(".md", "") if memory_id.endswith(".md") else memory_id
        memory = self.unified.recall(real_id)
        if not memory:
            return {"success": False, "error": "Memory not found"}

        # Check if actually archived
        if memory.metadata.get("status") != "archived":
            return {"success": False, "error": "Memory is not archived"}

        # Restore
        memory.metadata.pop("status", None)
        memory.importance = 0.5
        memory.memory_type = self._parse_memory_type(memory_type)
        self.unified.backend.store(memory)
        return {"success": True, "memory_type": memory_type}

    def _memory_to_dict(self, memory: Memory) -> dict[str, Any]:
        status = memory.metadata.get("status", "active")
        return {
            "id": memory.id,
            "filename": f"{memory.id}.md", # Legacy compatibility
            "title": memory.title,
            "content": memory.content,
            "type": memory.memory_type.name.lower(),
            "status": status, # Legacy compatibility
            "created": memory.created_at.isoformat(),
            "updated": memory.accessed_at.isoformat(),
            "tags": list(memory.tags),
            "path": f"virtual://{memory.memory_type.name.lower()}/{memory.id}",
            "importance": memory.importance,
            "metadata": memory.metadata,
        }
