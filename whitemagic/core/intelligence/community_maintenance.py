"""Incremental Community Maintenance — Label propagation on new memories.

Source: LIVING_MEMORY_GAP_ANALYSIS.md (Zep/Graphiti comparison)

Current state: Louvain + HDBSCAN run as batch operations during dream cycles.
This module adds incremental community updates — when a new memory is stored,
it propagates community labels from its neighbors rather than requiring a
full batch re-clustering.

Algorithm: Modified Label Propagation
1. New memory arrives with associations to existing memories
2. Collect community labels from neighbors (weighted by association strength)
3. Assign the majority label (or create a new community if no strong match)
4. Optionally merge communities that have grown too close

This keeps community structure up-to-date between dream cycles without
the O(N²) cost of full re-clustering.
"""

from __future__ import annotations

import logging
import time
from collections import Counter
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# Minimum association weight to propagate labels
MIN_PROPAGATION_WEIGHT = 0.3

# Minimum fraction of neighbors that must share a label to assign it
MAJORITY_THRESHOLD = 0.4

# Maximum community size before suggesting a split
MAX_COMMUNITY_SIZE = 200

# Minimum number of neighbors needed to assign a community
MIN_NEIGHBORS_FOR_ASSIGNMENT = 2


@dataclass
class Community:
    """A community (cluster) of related memories."""

    community_id: str
    label: str
    member_count: int = 0
    created_at: float = field(default_factory=time.time)
    updated_at: float = field(default_factory=time.time)
    representative_tags: list[str] = field(default_factory=list)
    cohesion_score: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "community_id": self.community_id,
            "label": self.label,
            "member_count": self.member_count,
            "cohesion_score": round(self.cohesion_score, 3),
            "representative_tags": self.representative_tags[:5],
            "created_at": self.created_at,
            "updated_at": self.updated_at,
        }


@dataclass
class PropagationResult:
    """Result of a label propagation operation."""

    memory_id: str
    assigned_community: str | None
    action: str  # "assigned", "created_new", "no_neighbors", "below_threshold"
    neighbor_count: int = 0
    label_distribution: dict[str, int] = field(default_factory=dict)
    confidence: float = 0.0


class CommunityMaintainer:
    """Incremental community maintenance via label propagation.

    Maintains a lightweight community registry and updates it
    incrementally as new memories are added.

    Usage:
        maintainer = CommunityMaintainer()

        # When a new memory is stored:
        result = maintainer.propagate_label(
            memory_id="new_mem",
            neighbors=[("neighbor_1", 0.8), ("neighbor_2", 0.6)],
            memory_tags=["python", "architecture"],
        )

        # Get community for a memory:
        community = maintainer.get_community("new_mem")

        # Periodic health check:
        health = maintainer.check_health()
    """

    def __init__(self) -> None:
        self._communities: dict[str, Community] = {}
        self._member_to_community: dict[str, str] = {}
        self._next_community_id = 0
        self._stats = {
            "total_propagations": 0,
            "total_assignments": 0,
            "total_new_communities": 0,
            "total_merges": 0,
        }

    def propagate_label(
        self,
        memory_id: str,
        neighbors: list[tuple[str, float]],
        memory_tags: list[str] | None = None,
    ) -> PropagationResult:
        """Propagate community label from neighbors to a new memory.

        Args:
            memory_id: The new memory's ID
            neighbors: List of (neighbor_id, association_weight) tuples
            memory_tags: Tags of the new memory (used for new community labeling)

        Returns:
            PropagationResult with assignment details

        """
        self._stats["total_propagations"] += 1

        # Filter by minimum weight
        strong_neighbors = [(nid, w) for nid, w in neighbors if w >= MIN_PROPAGATION_WEIGHT]

        if len(strong_neighbors) < MIN_NEIGHBORS_FOR_ASSIGNMENT:
            return PropagationResult(
                memory_id=memory_id,
                assigned_community=None,
                action="no_neighbors",
                neighbor_count=len(strong_neighbors),
            )

        # Collect community labels from neighbors, weighted by association strength
        label_votes: Counter[str] = Counter()
        for nid, weight in strong_neighbors:
            community_id = self._member_to_community.get(nid)
            if community_id:
                label_votes[community_id] += weight

        if not label_votes:
            # No neighbors have communities — create a new one
            return self._create_new_community(memory_id, memory_tags or [])

        # Find majority label
        total_weight = sum(label_votes.values())
        best_label, best_weight = label_votes.most_common(1)[0]
        confidence = best_weight / total_weight if total_weight > 0 else 0.0

        if confidence < MAJORITY_THRESHOLD:
            # No strong majority — create a new community
            return self._create_new_community(memory_id, memory_tags or [])

        # Assign to the majority community
        self._assign_to_community(memory_id, best_label)
        self._stats["total_assignments"] += 1

        return PropagationResult(
            memory_id=memory_id,
            assigned_community=best_label,
            action="assigned",
            neighbor_count=len(strong_neighbors),
            label_distribution=dict(label_votes),
            confidence=confidence,
        )

    def _create_new_community(
        self,
        memory_id: str,
        tags: list[str],
    ) -> PropagationResult:
        """Create a new community for a memory that doesn't fit existing ones."""
        self._next_community_id += 1
        community_id = f"community_{self._next_community_id}"

        # Generate label from tags
        label = ", ".join(tags[:3]) if tags else f"cluster_{self._next_community_id}"

        community = Community(
            community_id=community_id,
            label=label,
            member_count=1,
            representative_tags=tags[:5],
            cohesion_score=1.0,
        )
        self._communities[community_id] = community
        self._member_to_community[memory_id] = community_id
        self._stats["total_new_communities"] += 1

        logger.debug(f"Created new community {community_id} ({label}) for {memory_id}")

        return PropagationResult(
            memory_id=memory_id,
            assigned_community=community_id,
            action="created_new",
            confidence=1.0,
        )

    def _assign_to_community(self, memory_id: str, community_id: str) -> None:
        """Assign a memory to an existing community."""
        self._member_to_community[memory_id] = community_id
        community = self._communities.get(community_id)
        if community:
            community.member_count += 1
            community.updated_at = time.time()

    def get_community(self, memory_id: str) -> Community | None:
        """Get the community a memory belongs to."""
        community_id = self._member_to_community.get(memory_id)
        if community_id:
            return self._communities.get(community_id)
        return None

    def get_community_members(self, community_id: str) -> list[str]:
        """Get all member IDs in a community."""
        return [mid for mid, cid in self._member_to_community.items() if cid == community_id]

    def merge_communities(self, community_a: str, community_b: str) -> str | None:
        """Merge two communities into one.

        The larger community absorbs the smaller one.

        Args:
            community_a: First community ID
            community_b: Second community ID

        Returns:
            ID of the surviving community, or None if invalid

        """
        ca = self._communities.get(community_a)
        cb = self._communities.get(community_b)
        if not ca or not cb:
            return None

        # Larger absorbs smaller
        if ca.member_count >= cb.member_count:
            survivor, absorbed = ca, cb
            survivor_id, absorbed_id = community_a, community_b
        else:
            survivor, absorbed = cb, ca
            survivor_id, absorbed_id = community_b, community_a

        # Move all members
        for mid in list(self._member_to_community.keys()):
            if self._member_to_community[mid] == absorbed_id:
                self._member_to_community[mid] = survivor_id

        survivor.member_count += absorbed.member_count
        survivor.updated_at = time.time()
        # Merge representative tags
        merged_tags = list(set(survivor.representative_tags + absorbed.representative_tags))[:5]
        survivor.representative_tags = merged_tags

        del self._communities[absorbed_id]
        self._stats["total_merges"] += 1

        logger.info(f"Merged community {absorbed_id} into {survivor_id} ({survivor.member_count} members)")
        return survivor_id

    def check_health(self) -> dict[str, Any]:
        """Check community health — detect oversized or orphaned communities."""
        oversized = []
        empty = []
        for cid, community in self._communities.items():
            actual_count = sum(1 for v in self._member_to_community.values() if v == cid)
            community.member_count = actual_count  # Correct any drift
            if actual_count > MAX_COMMUNITY_SIZE:
                oversized.append(cid)
            if actual_count == 0:
                empty.append(cid)

        # Clean up empty communities
        for cid in empty:
            del self._communities[cid]

        return {
            "total_communities": len(self._communities),
            "total_members": len(self._member_to_community),
            "oversized_communities": oversized,
            "empty_cleaned": len(empty),
            "avg_community_size": (
                len(self._member_to_community) / max(len(self._communities), 1)
            ),
        }

    def import_batch_communities(
        self,
        assignments: dict[str, int],
        labels: dict[int, str] | None = None,
    ) -> int:
        """Import community assignments from a batch clustering run (Louvain/HDBSCAN).

        This bridges the gap between batch dream-cycle clustering and
        incremental maintenance.

        Args:
            assignments: {memory_id: community_label_int}
            labels: Optional {community_label_int: human_readable_label}

        Returns:
            Number of assignments imported

        """
        count = 0
        for memory_id, cluster_label in assignments.items():
            community_id = f"batch_{cluster_label}"

            if community_id not in self._communities:
                label = (labels or {}).get(cluster_label, f"cluster_{cluster_label}")
                self._communities[community_id] = Community(
                    community_id=community_id,
                    label=label,
                    member_count=0,
                )

            self._member_to_community[memory_id] = community_id
            self._communities[community_id].member_count += 1
            count += 1

        logger.info(f"Imported {count} batch community assignments across {len(self._communities)} communities")
        return count

    def get_status(self) -> dict[str, Any]:
        """Get community maintenance status."""
        return {
            "total_communities": len(self._communities),
            "total_members": len(self._member_to_community),
            **self._stats,
            "communities": [c.to_dict() for c in sorted(
                self._communities.values(),
                key=lambda c: c.member_count,
                reverse=True,
            )[:20]],
        }


# Global singleton
_maintainer: CommunityMaintainer | None = None


def get_community_maintainer() -> CommunityMaintainer:
    """Get the global community maintainer."""
    global _maintainer
    if _maintainer is None:
        _maintainer = CommunityMaintainer()
    return _maintainer
