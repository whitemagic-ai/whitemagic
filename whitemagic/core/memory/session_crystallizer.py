"""Session Crystallizer — Automatic Memory Creation from Sessions.
===============================================================
At the end of each development session (Antigravity conversation),
this module creates a LONG_TERM memory summarizing what was done.

The crystallized memory is:
  - Stored as LONG_TERM with high importance (0.85)
  - Tagged with session-specific and topic tags
  - Placed in INNER_RIM (galactic_distance ≈ 0.20) so it starts close
    to core and can spiral inward on future recalls
  - Indexed in the holographic coordinate system with proper 5D coords

This ensures the system's autobiography grows organically with each
session, creating a rich CORE over time.

Usage:
    from whitemagic.core.memory.session_crystallizer import crystallize_session
    memory_id = crystallize_session(
        session_id="abc123",
        title="v11.3.2 Galactic Map Integration",
        summary="Implemented 5D holographic coordinates, galactic-distance-aware search...",
        topics=["galactic_map", "holographic", "5d_coordinates"],
        emotional_valence=0.7,  # positive session
    )
"""

from __future__ import annotations

import hashlib
import logging
from datetime import datetime

logger = logging.getLogger(__name__)


def crystallize_session(
    session_id: str,
    title: str,
    summary: str,
    topics: list[str] | None = None,
    emotional_valence: float = 0.3,
    importance: float = 0.85,
    galactic_distance: float = 0.20,
) -> str | None:
    """Create a LONG_TERM memory from a development session.

    Args:
        session_id: Unique session identifier (e.g., Antigravity conversation ID)
        title: Human-readable session title
        summary: Detailed summary of what was accomplished
        topics: Topic tags for the session
        emotional_valence: How positive the session was (0.0-1.0)
        importance: How important this session was (0.0-1.0)
        galactic_distance: Initial galactic distance (default 0.20 = INNER_RIM)

    Returns:
        The memory ID if successful, None on failure.

    """
    try:
        from whitemagic.core.memory.unified import get_unified_memory
        from whitemagic.core.memory.unified_types import Memory, MemoryType

        um = get_unified_memory()

        # Generate deterministic ID from session_id
        mem_id = hashlib.md5(f"session:{session_id}".encode()).hexdigest()[:16]

        # Build tags
        tags = set(topics or [])
        tags.update({"session", "antigravity", "walkthrough", "milestone"})

        # Create memory
        memory = Memory(
            id=mem_id,
            content=summary,
            memory_type=MemoryType.LONG_TERM,
            title=f"Session: {title}",
            importance=importance,
            emotional_valence=emotional_valence,
            galactic_distance=galactic_distance,
            retention_score=0.90,
            is_protected=False,  # Let it earn protection through recalls
            tags=tags,
        )

        um.store(memory)

        logger.info(
            f"💎 Session crystallized: '{title}' → {mem_id} "
            f"(importance={importance}, distance={galactic_distance})",
        )
        return mem_id

    except Exception as e:
        logger.error(f"Session crystallization failed: {e}")
        return None


def crystallize_current_session(
    title: str,
    summary: str,
    topics: list[str] | None = None,
    emotional_valence: float = 0.3,
) -> str | None:
    """Convenience wrapper that generates a session_id from the current timestamp.
    """
    session_id = datetime.now().strftime("%Y%m%d_%H%M%S")
    return crystallize_session(
        session_id=session_id,
        title=title,
        summary=summary,
        topics=topics,
        emotional_valence=emotional_valence,
    )
