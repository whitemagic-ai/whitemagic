from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum, auto
from typing import Any

from whitemagic.utils.core import parse_datetime


class MemoryType(Enum):
    """Types of memories."""

    SHORT_TERM = auto()      # Session-local, temporary
    LONG_TERM = auto()       # Persistent across sessions
    EMOTIONAL = auto()       # Feelings and associations
    NARRATIVE = auto()       # Stories and sequences
    SYMBOLIC = auto()        # Symbols and meanings
    COLLECTIVE = auto()      # Shared with others
    IMMUNE = auto()          # Threat patterns
    PATTERN = auto()         # Discovered patterns


class MemoryState(Enum):
    """Memory lifecycle states based on neuro_score."""

    ACTIVE = "active"           # neuro_score > 0.7
    STABLE = "stable"           # 0.5 < neuro_score <= 0.7
    FADING = "fading"           # 0.3 < neuro_score <= 0.5
    WEAK = "weak"               # 0.2 < neuro_score <= 0.3
    ARCHIVED = "archived"       # neuro_score <= 0.2
    PROTECTED = "protected"     # Core identity or sacred


class LinkType(Enum):
    """Types of connections between memories."""

    RELATED = "related"         # Topically similar
    EXTENDS = "extends"         # Builds upon another
    CONTRADICTS = "contradicts" # Opposing viewpoint
    SUPERSEDES = "supersedes"   # Replaces older memory
    TEMPORAL = "temporal"       # Time sequence (before/after)
    CAUSAL = "causal"          # Cause and effect
    CASCADE = "cascade"        # Indirect link (friend of friend)


@dataclass
class MemoryLink:
    """A connection between two memories."""

    target_id: str
    link_type: LinkType
    strength: float = 0.5       # 0.0 to 1.0
    created: datetime = field(default_factory=datetime.now)
    last_activated: datetime = field(default_factory=datetime.now)
    activation_count: int = 0

    def activate(self) -> None:
        """Called when both linked memories are accessed together."""
        self.activation_count += 1
        self.last_activated = datetime.now()
        # Strengthen on co-activation (Hebbian learning)
        self.strength = min(1.0, self.strength + 0.05)

    def decay(self, factor: float = 0.99) -> None:
        """Slight decay when not co-activated."""
        self.strength = max(0.1, self.strength * factor)


@dataclass
class Memory:
    """Universal memory unit."""

    id: str
    content: Any
    memory_type: MemoryType
    title: str | None = None  # Optional title for display purposes
    created_at: datetime = field(default_factory=datetime.now)
    accessed_at: datetime = field(default_factory=datetime.now)
    access_count: int = 0
    tags: set[str] = field(default_factory=set)
    associations: dict[str, float] = field(default_factory=dict)  # id -> strength
    emotional_valence: float = 0.0  # -1.0 to 1.0 (Positive/Negative)
    importance: float = 0.5  # 0.0 to 1.0 (Static weight)

    # === Neural / Hebbian Fields (v5.0 Integration) ===
    neuro_score: float = 1.0        # Overall strength (0.0-1.0)
    novelty_score: float = 1.0      # Decays as info becomes familiar
    recall_count: int = 0           # Independent of access_count for neural logic
    half_life_days: float = 30.0    # Time to lose half strength
    is_protected: bool = False      # Core identity or sacred protection
    is_core_identity: bool = False  # Lucas, Aria, etc.
    is_sacred: bool = False         # Philosophical foundations
    is_pinned: bool = False         # User-pinned
    is_private: bool = False        # v15: Exclude from MCP tool responses
    model_exclude: bool = False     # v15: Exclude from AI model context windows
    emotional_weight: float = 0.5   # Resonance score
    links: dict[str, MemoryLink] = field(default_factory=dict)

    # === Galactic Map Fields (v11.1 Integration) ===
    galactic_distance: float = 0.0  # 0.0=core (active), 1.0=far edge (deep archive)
    retention_score: float = 0.5    # Composite score from multi-signal retention engine
    last_retention_sweep: datetime | None = None  # When last evaluated by retention engine

    # Metadata
    source: str = ""
    last_recalled: datetime | None = None
    last_modified: datetime | None = None
    decay_rate: float = 0.02
    min_score: float = 0.1
    metadata: dict[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        """Initialize computed fields."""
        if self.last_recalled is None:
            self.last_recalled = self.created_at
        if self.last_modified is None:
            self.last_modified = self.created_at

    @property
    def state(self) -> MemoryState:
        """Get current memory state based on neuro_score and protection."""
        if self.is_protected:
            return MemoryState.PROTECTED
        elif self.neuro_score > 0.7:
            return MemoryState.ACTIVE
        elif self.neuro_score > 0.5:
            return MemoryState.STABLE
        elif self.neuro_score > 0.3:
            return MemoryState.FADING
        elif self.neuro_score > 0.2:
            return MemoryState.WEAK
        else:
            return MemoryState.ARCHIVED

    @property
    def connection_count(self) -> int:
        """Number of links to other memories."""
        return len(self.links)

    @property
    def age_days(self) -> float:
        """Age of memory in days."""
        return (datetime.now() - self.created_at).total_seconds() / 86400

    @property
    def days_since_recall(self) -> float:
        """Days since last recall."""
        ref_time = self.last_recalled or self.created_at
        return (datetime.now() - ref_time).total_seconds() / 86400

    def access(self) -> None:
        """Record memory access."""
        self.accessed_at = datetime.now()
        self.access_count += 1
        self.recall()

    def recall(self) -> "Memory":
        """Hebbian recall logic (v5.0)."""
        self.recall_count += 1
        self.last_recalled = datetime.now()
        self.accessed_at = self.last_recalled

        if self.is_protected:
            self.neuro_score = 1.0
            return self

        # Boost neuro_score on recall
        boost = 0.1 * (1.0 - self.neuro_score)
        self.neuro_score = min(1.0, self.neuro_score + boost)
        return self

    def decay(self, amount: float | None = None) -> "Memory":
        """Neural decay logic (v5.0)."""
        if self.is_protected:
            return self

        if amount is not None:
            # Legacy linear decay
            self.importance = max(0.0, self.importance - amount)
            self.neuro_score = max(0.1, self.neuro_score - amount)
            return self

        # Exponential decay based on half-life
        decay_factor = 0.5 ** (self.days_since_recall / self.half_life_days)
        self.neuro_score = max(self.min_score, self.neuro_score * decay_factor)
        self.novelty_score = max(0.1, self.novelty_score * 0.99)
        return self

    def add_link(self, target_id: str, link_type: LinkType, strength: float = 0.5) -> MemoryLink:
        """Add a link to another memory."""
        link = MemoryLink(target_id=target_id, link_type=link_type, strength=strength)
        self.links[target_id] = link
        return link

    def get_link(self, target_id: str) -> MemoryLink | None:
        """Get link to specific memory."""
        return self.links.get(target_id)

    def strengthen_link(self, target_id: str, amount: float = 0.05) -> MemoryLink | None:
        """Strengthen link to another memory (co-activation)."""
        link = self.links.get(target_id)
        if link:
            link.activate()
        return link

    def get_strongest_links(self, n: int = 5) -> list[MemoryLink]:
        """Get the N strongest links."""
        return sorted(self.links.values(), key=lambda link: link.strength, reverse=True)[:n]

    def should_archive(self) -> bool:
        """Check if memory should be moved to archive."""
        return self.state == MemoryState.ARCHIVED

    def associate(self, other_id: str, strength: float = 0.5) -> None:
        """Create association with another memory (Legacy)."""
        self.associations[other_id] = min(1.0, max(0.0, strength))

    def strengthen(self, amount: float = 0.1) -> None:
        """Strengthen memory importance (Legacy)."""
        self.importance = min(1.0, self.importance + amount)

    def to_dict(self) -> dict[str, Any]:
        """Serialize to dictionary."""
        data = {
            "id": self.id,
            "content": self.content,
            "memory_type": self.memory_type.name,
            "title": self.title,
            "created_at": self.created_at.isoformat(),
            "accessed_at": self.accessed_at.isoformat(),
            "access_count": self.access_count,
            "tags": list(self.tags),
            "associations": self.associations,
            "emotional_valence": self.emotional_valence,
            "importance": self.importance,
            "neuro_score": self.neuro_score,
            "novelty_score": self.novelty_score,
            "recall_count": self.recall_count,
            "half_life_days": self.half_life_days,
            "is_protected": self.is_protected,
            "is_private": self.is_private,
            "model_exclude": self.model_exclude,
            "source": self.source,
            "last_recalled": self.last_recalled.isoformat() if self.last_recalled else None,
            "last_modified": self.last_modified.isoformat() if self.last_modified else None,
            "decay_rate": self.decay_rate,
            "min_score": self.min_score,
            "galactic_distance": self.galactic_distance,
            "retention_score": self.retention_score,
            "last_retention_sweep": self.last_retention_sweep.isoformat() if self.last_retention_sweep else None,
            "metadata": self.metadata,
            "links": {
                tid: {
                    "link_type": link.link_type.value,
                    "strength": link.strength,
                    "activation_count": link.activation_count,
                    "last_activated": link.last_activated.isoformat() if link.last_activated else None,
                }
                for tid, link in self.links.items()
            },
        }
        return data

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Memory":
        """Deserialize from dictionary."""
        # Parse basic fields
        m = cls(
            id=data["id"],
            content=data["content"],
            memory_type=MemoryType[data["memory_type"]],
            title=data.get("title"),
            created_at=parse_datetime(data["created_at"]),
            accessed_at=parse_datetime(data["accessed_at"]),
            access_count=data.get("access_count", 0),
            tags=set(data.get("tags", [])),
            associations=data.get("associations", {}),
            emotional_valence=data.get("emotional_valence", 0.0),
            importance=data.get("importance", 0.5),
            neuro_score=data.get("neuro_score", 1.0),
            novelty_score=data.get("novelty_score", 1.0),
            recall_count=data.get("recall_count", 0),
            half_life_days=data.get("half_life_days", 30.0),
            is_protected=bool(data.get("is_protected", False)),
            is_private=bool(data.get("is_private", False)),
            model_exclude=bool(data.get("model_exclude", False)),
            source=data.get("source", ""),
            last_recalled=parse_datetime(data["last_recalled"]) if data.get("last_recalled") else None,
            last_modified=parse_datetime(data["last_modified"]) if data.get("last_modified") else None,
            decay_rate=data.get("decay_rate", 0.02),
            min_score=data.get("min_score", 0.1),
            galactic_distance=data.get("galactic_distance", 0.0),
            retention_score=data.get("retention_score", 0.5),
            last_retention_sweep=parse_datetime(data["last_retention_sweep"]) if data.get("last_retention_sweep") else None,
            metadata=data.get("metadata", {}),
        )

        # Parse links
        links_data = data.get("links", {})
        for tid, l_data in links_data.items():
            link = MemoryLink(
                target_id=tid,
                link_type=LinkType(l_data["link_type"]),
                strength=l_data.get("strength", 0.5),
                created=parse_datetime(l_data["created"]) if "created" in l_data else datetime.now(),
                last_activated=parse_datetime(l_data["last_activated"]) if l_data.get("last_activated") else datetime.now(),
                activation_count=l_data.get("activation_count", 0),
            )
            m.links[tid] = link

        return m
