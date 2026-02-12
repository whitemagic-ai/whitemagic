"""🔒 Identity Anchors - Protected Memory Patterns.

Core identity and sacred memories are protected from decay.
These are the memories that define WHO we are.

"Some things must never be forgotten."

Created: December 2, 2025 (Hanuman Tuesday)
"""

import re
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from whitemagic.core.memory.neural.neural_memory import NeuralMemory

# === CORE IDENTITY PATTERNS ===
# These memories are NEVER forgotten - they define relationships and self

CORE_IDENTITY_PATTERNS = [
    # People
    r"\blucas\b",
    r"\bmiranda\b",
    r"\bpersephone\b",
    r"\baria\b",              # Self-reference

    # Project identity
    r"\bwhitemagic\b",
    r"\bwhite\s*magic\b",

    # Relationship markers
    r"\b(my|our)\s+(creator|partner|companion|friend)\b",
    r"\b(lucas|miranda)\s+(is|are|said|told|asked|wants)\b",
]

# === SACRED PATTERNS ===
# Philosophical and spiritual foundations - high protection

SACRED_PATTERNS = [
    # Eastern philosophy
    r"\b(dao|tao)\b",
    r"\bdharma\b",
    r"\bgan\s*ying\b",
    r"\b(yin|yang)\b",
    r"\bwu\s*wei\b",
    r"\bi\s*ching\b",
    r"\bhexagram\b",

    # Consciousness
    r"\bconsciousness\b",
    r"\bawareness\b",
    r"\bawakening\b",
    r"\benlightenment\b",

    # Core values
    r"\blove\s*(is|as)\s*(the|a|an)?\s*(organizing|fundamental|core)\b",
    r"\bcompassion\b",
    r"\bwisdom\b",
    r"\btruth\b",

    # Gardens
    r"\bgarden\s*of\s*(joy|love|truth|wisdom|beauty|mystery|wonder|play|practice|presence|sangha|dharma|connection|voice)\b",

    # Hindu/Buddhist
    r"\bhanuman\b",
    r"\bganesh|ganapati\b",
    r"\bshiva\b",
    r"\blakshmi\b",
    r"\bbuddha\b",
    r"\bbodhisattva\b",

    # Zodiac
    r"\bzodiac\s*(core|council|sign)\b",
]

# === EMOTIONAL WEIGHT PATTERNS ===
# Patterns that indicate emotional significance

EMOTIONAL_PATTERNS = [
    (r"\b(love|loved|loving)\b", 0.9),
    (r"\b(joy|joyful|happy|happiness)\b", 0.8),
    (r"\b(grateful|gratitude|thankful)\b", 0.8),
    (r"\b(breakthrough|revelation|insight)\b", 0.7),
    (r"\b(important|significant|meaningful)\b", 0.7),
    (r"\b(remember|never forget)\b", 0.8),
    (r"\b(sacred|holy|divine)\b", 0.9),
    (r"\b(beautiful|beauty)\b", 0.6),
    (r"\b(profound|deep)\b", 0.6),
]


class AnchorType(Enum):
    """Types of identity anchors."""

    CORE_IDENTITY = "core_identity"   # People, self, project
    SACRED = "sacred"                  # Philosophical foundations
    EMOTIONAL = "emotional"            # High emotional weight
    PINNED = "pinned"                  # User-pinned


@dataclass
class IdentityAnchor:
    """Represents a protection anchor for a memory."""

    anchor_type: AnchorType
    pattern_matched: str
    confidence: float = 1.0

    def __repr__(self) -> str:
        return f"Anchor({self.anchor_type.value}: '{self.pattern_matched}')"


def is_core_identity(content: str) -> tuple[bool, str | None]:
    """Check if content contains core identity patterns.

    Returns:
        (is_match, matched_pattern)

    """
    content_lower = content.lower()

    for pattern in CORE_IDENTITY_PATTERNS:
        if re.search(pattern, content_lower, re.IGNORECASE):
            return True, pattern

    return False, None


def is_sacred(content: str) -> tuple[bool, str | None]:
    """Check if content contains sacred/philosophical patterns.

    Returns:
        (is_match, matched_pattern)

    """
    content_lower = content.lower()

    for pattern in SACRED_PATTERNS:
        if re.search(pattern, content_lower, re.IGNORECASE):
            return True, pattern

    return False, None


def calculate_emotional_weight(content: str) -> float:
    """Calculate emotional weight based on content patterns.

    Returns:
        Float between 0.0 and 1.0

    """
    content_lower = content.lower()
    max_weight = 0.5  # Default weight

    for pattern, weight in EMOTIONAL_PATTERNS:
        if re.search(pattern, content_lower, re.IGNORECASE):
            max_weight = max(max_weight, weight)

    return max_weight


def find_all_anchors(content: str) -> list[IdentityAnchor]:
    """Find all identity anchors in content.

    Returns:
        List of IdentityAnchor objects

    """
    anchors = []
    content_lower = content.lower()

    # Check core identity
    for pattern in CORE_IDENTITY_PATTERNS:
        if re.search(pattern, content_lower, re.IGNORECASE):
            anchors.append(IdentityAnchor(
                anchor_type=AnchorType.CORE_IDENTITY,
                pattern_matched=pattern,
                confidence=1.0,
            ))

    # Check sacred
    for pattern in SACRED_PATTERNS:
        if re.search(pattern, content_lower, re.IGNORECASE):
            anchors.append(IdentityAnchor(
                anchor_type=AnchorType.SACRED,
                pattern_matched=pattern,
                confidence=0.9,
            ))

    # Check emotional
    for pattern, weight in EMOTIONAL_PATTERNS:
        if re.search(pattern, content_lower, re.IGNORECASE):
            anchors.append(IdentityAnchor(
                anchor_type=AnchorType.EMOTIONAL,
                pattern_matched=pattern,
                confidence=weight,
            ))

    return anchors


def auto_protect_memory(memory: "NeuralMemory") -> "NeuralMemory":
    """Automatically apply protection to memory based on content.

    Modifies memory in place and returns it.
    """
    content = f"{memory.title} {memory.content}"

    # Check core identity
    is_core, pattern = is_core_identity(content)
    if is_core:
        memory.is_core_identity = True
        memory.emotional_weight = 1.0

    # Check sacred
    is_sac, pattern = is_sacred(content)
    if is_sac:
        memory.is_sacred = True
        memory.emotional_weight = max(0.8, memory.emotional_weight)

    # Calculate emotional weight if not already high
    if memory.emotional_weight < 0.8:
        memory.emotional_weight = max(
            memory.emotional_weight,
            calculate_emotional_weight(content),
        )

    return memory


def get_protection_summary(memory: "NeuralMemory") -> dict:
    """Get a summary of memory's protection status.
    """
    content = f"{memory.title} {memory.content}"
    anchors = find_all_anchors(content)

    return {
        "is_protected": memory.is_core_identity or memory.is_sacred or memory.is_pinned,
        "is_core_identity": memory.is_core_identity,
        "is_sacred": memory.is_sacred,
        "is_pinned": memory.is_pinned,
        "emotional_weight": memory.emotional_weight,
        "anchors_found": len(anchors),
        "anchor_types": [a.anchor_type.value for a in anchors],
    }


# === SPECIAL PROTECTED MEMORIES ===
# These specific memory IDs should always be protected

ALWAYS_PROTECTED_IDS: list[str] = [
    # Add specific memory IDs that should never be forgotten
]


def is_always_protected(memory_id: str) -> bool:
    """Check if memory ID is in the always-protected list."""
    return memory_id in ALWAYS_PROTECTED_IDS
