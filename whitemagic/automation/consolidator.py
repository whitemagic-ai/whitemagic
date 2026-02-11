"""Automatic Memory Consolidation System

Implements the Episodic → Semantic memory flow from the vision document.
Runs at session end to extract lasting knowledge from temporary experiences.

Inspired by human sleep consolidation: noisy experiences → usable knowledge.
"""

import logging
import re
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from whitemagic.core.memory.neural_system import NeuralMemory, get_neural_system

logger = logging.getLogger(__name__)


@dataclass
class ExtractedKnowledge:
    """Knowledge extracted from episodic memories."""

    content: str
    type: str  # "decision", "preference", "fact", "todo", "insight"
    tags: list[str]
    confidence: float  # 0.0 to 1.0
    source_memory_ids: list[str]


class MemoryConsolidator:
    """Automatically consolidate episodic → semantic memory."""

    def __init__(self, memory_system: Any | None = None):
        self.memory = memory_system or get_neural_system()

        # Extraction patterns
        self.patterns = {
            "decision": [
                r"(?:we|I) (?:decided|chose|selected|opted for) (.+)",
                r"decision:? (.+)",
                r"(?:going with|settled on) (.+)",
            ],
            "preference": [
                r"(?:I|Lucas) (?:prefer|like|love|favor)s? (.+)",
                r"(?:always|usually) (?:use|choose|go with) (.+)",
                r"favorite .+ is (.+)",
            ],
            "fact": [
                r"(?:note|remember) that (.+)",
                r"(?:important|key) fact:? (.+)",
                r"(.+) is (?:always|never|required|necessary)",
            ],
            "todo": [
                r"(?:TODO|todo):? (.+)",
                r"(?:need to|must|should) (.+)",
                r"(?:task|action item):? (.+)",
            ],
            "insight": [
                r"(?:realized|learned|discovered) (?:that )?(.+)",
                r"(?:key insight|breakthrough):? (.+)",
                r"(?:this means|implication:?) (.+)",
            ],
        }

    def consolidate_session(
        self,
        session_id: str | None = None,
        minutes: int = 60,
    ) -> dict[str, Any]:
        """Consolidate memories from recent session.

        Args:
            session_id: Optional session identifier
            minutes: Look back this many minutes (if no session_id)

        Returns:
            Consolidation report with stats

        """
        logger.info(f"Starting consolidation for session: {session_id or 'recent'}")

        # Get episodic memories to consolidate
        episodic = self._get_episodic_memories(session_id, minutes)

        if not episodic:
            logger.info("No episodic memories to consolidate")
            return {"status": "no_memories", "consolidated": 0}

        # Extract semantic knowledge
        extracted = self._extract_semantic_content(episodic)

        # Create semantic memories
        created = []
        for knowledge in extracted:
            mem = self._create_semantic_memory(knowledge)
            if mem:
                created.append(mem)

        # Archive episodic memories
        archived_count = self._archive_episodic(episodic)

        logger.info(
            f"Consolidation complete: {len(created)} semantic memories created, "
            f"{archived_count} episodic archived",
        )

        return {
            "status": "success",
            "episodic_processed": len(episodic),
            "semantic_created": len(created),
            "episodic_archived": archived_count,
            "knowledge_types": self._count_types(extracted),
            "timestamp": datetime.now().isoformat(),
        }

    def _get_episodic_memories(
        self,
        session_id: str | None,
        minutes: int,
    ) -> list[NeuralMemory]:
        """Get episodic memories to consolidate."""
        if session_id:
            # Get by session ID
            memories = self.memory.search(
                f"session:{session_id}",
                top_k=1000,
            )
        else:
            # Get recent memories
            memories = self.memory.get_recent_memories(
                minutes=minutes,
                limit=1000,
            )

        # Filter for episodic type (or untyped recent ones)
        episodic = [
            m for m in memories
            if "episodic" in getattr(m, "tags", [])
            or ("session" in getattr(m, "tags", []) and getattr(m, "recall_count", 0) < 3)
        ]

        return episodic  # type: ignore[return-value]

    def _extract_semantic_content(
        self,
        memories: list[NeuralMemory],
    ) -> list[ExtractedKnowledge]:
        """Extract lasting knowledge from episodic memories."""
        extracted = []

        for mem in memories:
            content = mem.content.lower()

            # Try each pattern type
            for knowledge_type, patterns in self.patterns.items():
                for pattern in patterns:
                    matches = re.finditer(pattern, content, re.IGNORECASE)
                    for match in matches:
                        knowledge_content = match.group(1).strip()

                        # Only extract meaningful knowledge (not too short)
                        if len(knowledge_content) > 10:
                            extracted.append(ExtractedKnowledge(
                                content=knowledge_content,
                                type=knowledge_type,
                                tags=[knowledge_type, "consolidated"] + list(mem.tags),
                                confidence=self._calculate_confidence(mem, knowledge_type),
                                source_memory_ids=[mem.id],
                            ))

            # Also extract high-importance memories as-is
            if mem.emotional_weight > 0.7 or mem.recall_count > 2:
                extracted.append(ExtractedKnowledge(
                    content=mem.content,
                    type="important_event",
                    tags=["important", "consolidated"] + list(mem.tags),
                    confidence=min(mem.emotional_weight, 0.9),
                    source_memory_ids=[mem.id],
                ))

        # Deduplicate similar knowledge
        extracted = self._deduplicate(extracted)

        return extracted

    def _calculate_confidence(self, memory: NeuralMemory, knowledge_type: str) -> float:
        """Calculate confidence score for extracted knowledge."""
        confidence = 0.5  # Base confidence

        # Boost for repeated access
        if memory.recall_count > 0:
            confidence += min(memory.recall_count * 0.1, 0.3)

        # Boost for emotional weight
        confidence += memory.emotional_weight * 0.2

        # Boost for strong language
        strong_words = ["always", "never", "must", "decided", "critical", "important"]
        content_lower = memory.content.lower()
        for word in strong_words:
            if word in content_lower:
                confidence += 0.05

        # Type-specific adjustments
        if knowledge_type == "decision":
            confidence += 0.1  # Decisions are usually important
        elif knowledge_type == "todo":
            confidence += 0.05  # TODOs are actionable

        return float(min(confidence, 1.0))

    def _deduplicate(self, extracted: list[ExtractedKnowledge]) -> list[ExtractedKnowledge]:
        """Remove duplicate or very similar knowledge."""
        unique = []
        seen_content: set[str] = set()

        for knowledge in extracted:
            # Normalize content for comparison
            normalized = knowledge.content.lower().strip()
            normalized = re.sub(r"\s+", " ", normalized)

            # Check if we've seen similar content
            is_duplicate = False
            for seen in seen_content:
                # Simple similarity check (more than 80% overlap)
                overlap = len(set(normalized.split()) & set(seen.split()))
                total = len(set(normalized.split()) | set(seen.split()))
                if total > 0 and overlap / total > 0.8:
                    is_duplicate = True
                    break

            if not is_duplicate:
                unique.append(knowledge)
                seen_content.add(normalized)

        return unique

    def _create_semantic_memory(self, knowledge: ExtractedKnowledge) -> NeuralMemory | None:
        """Create a semantic memory from extracted knowledge."""
        try:
            # Format title based on type
            title_prefixes = {
                "decision": "Decision:",
                "preference": "Preference:",
                "fact": "Fact:",
                "todo": "TODO:",
                "insight": "Insight:",
                "important_event": "Important:",
            }

            title = f"{title_prefixes.get(knowledge.type, '')} {knowledge.content[:50]}..."

            mem = self.memory.create_memory(
                content=knowledge.content,
                title=title,
                tags=knowledge.tags + ["semantic", knowledge.type],
                source="consolidator",
            )

            # Set initial emotional weight based on confidence
            mem.emotional_weight = knowledge.confidence

            # Note: Linking handled automatically by neural system

            return mem

        except Exception as e:
            logger.warning(f"Failed to create semantic memory: {e}")
            return None

    def _archive_episodic(self, memories: list[NeuralMemory]) -> int:
        """Archive episodic memories after consolidation."""
        archived = 0

        for mem in memories:
            try:
                # Don't archive if it's been recalled recently or has high weight
                if mem.recall_count > 5 or mem.emotional_weight > 0.8:
                    continue

                # Mark as archived by adding tag
                if "archived" not in mem.tags:
                    if isinstance(mem.tags, set):
                        mem.tags.add("archived")
                    else:
                        mem.tags.append("archived")

                # Reduce neuro-score to move to background
                mem.neuro_score = max(mem.neuro_score * 0.5, 0.2)

                archived += 1

            except Exception as e:
                logger.warning(f"Failed to archive memory {mem.id}: {e}")

        return archived

    def _count_types(self, extracted: list[ExtractedKnowledge]) -> dict[str, int]:
        """Count knowledge by type."""
        counts: dict[str, int] = {}
        for knowledge in extracted:
            counts[knowledge.type] = counts.get(knowledge.type, 0) + 1
        return counts


# Global instance
_consolidator: MemoryConsolidator | None = None


def get_consolidator() -> MemoryConsolidator:
    """Get the global memory consolidator."""
    global _consolidator
    if _consolidator is None:
        _consolidator = MemoryConsolidator()
    return _consolidator


def consolidate_now(session_id: str | None = None, minutes: int = 60) -> dict[str, Any]:
    """Convenience function to consolidate immediately."""
    consolidator = get_consolidator()
    return consolidator.consolidate_session(session_id, minutes)
