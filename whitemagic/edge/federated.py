"""Federated Learning for WhiteMagic Edge AI
Version: 3.0.0

Enables collective intelligence without sharing private data.
Each node learns locally and shares only patterns/rules.

Philosophy: Learn together, keep data private.
"""

from __future__ import annotations

import hashlib
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.utils.fileio import atomic_write, file_lock

logger = logging.getLogger(__name__)


@dataclass
class SharedPattern:
    """A pattern learned locally that can be shared."""

    id: str
    pattern: str  # Keywords for matching
    response_hash: str  # Hash of response (not actual response)
    confidence: float
    source_node: str  # Anonymized node ID
    votes: int = 1  # How many nodes have validated this
    created: str = ""

    def __post_init__(self) -> None:
        if not self.created:
            self.created = datetime.now().isoformat()


@dataclass
class FederatedNode:
    """Represents this node in the federated network."""

    node_id: str
    learned_patterns: list[SharedPattern] = field(default_factory=list)
    received_patterns: list[SharedPattern] = field(default_factory=list)
    contribution_score: float = 0.0


class FederatedLearning:
    """Federated learning coordinator.

    How it works:
    1. Each node learns patterns from local queries
    2. Patterns are anonymized (no raw data shared)
    3. Nodes share pattern signatures (not responses)
    4. Other nodes validate patterns locally
    5. Validated patterns get higher confidence
    6. Result: Collective intelligence without data sharing
    """

    def __init__(self, data_dir: Path | None = None, node_id: str | None = None) -> None:
        from whitemagic.config.paths import DATA_DIR
        self.data_dir = data_dir or (DATA_DIR / "federated")
        self.data_dir.mkdir(parents=True, exist_ok=True)

        # Generate anonymous node ID
        self.node_id = node_id or self._generate_node_id()

        self.node = FederatedNode(node_id=self.node_id)
        self._load()

    def _generate_node_id(self) -> str:
        """Generate anonymous node ID."""
        import uuid
        return hashlib.sha256(str(uuid.uuid4()).encode()).hexdigest()[:16]

    def _load(self) -> None:
        """Load node state."""
        state_file = self.data_dir / "node_state.json"
        if state_file.exists():
            try:
                with file_lock(state_file):
                    data = json.loads(state_file.read_text())
                self.node_id = data.get("node_id", self.node_id)
                self.node = FederatedNode(
                    node_id=self.node_id,
                    learned_patterns=[
                        SharedPattern(**p) for p in data.get("learned_patterns", [])
                    ],
                    received_patterns=[
                        SharedPattern(**p) for p in data.get("received_patterns", [])
                    ],
                    contribution_score=data.get("contribution_score", 0.0),
                )
            except Exception:
                pass

    def _save(self) -> None:
        """Save node state."""
        # Ensure directory exists
        self.data_dir.mkdir(parents=True, exist_ok=True)

        state_file = self.data_dir / "node_state.json"
        data = {
            "node_id": self.node_id,
            "learned_patterns": [
                {
                    "id": p.id,
                    "pattern": p.pattern,
                    "response_hash": p.response_hash,
                    "confidence": p.confidence,
                    "source_node": p.source_node,
                    "votes": p.votes,
                    "created": p.created,
                }
                for p in self.node.learned_patterns
            ],
            "received_patterns": [
                {
                    "id": p.id,
                    "pattern": p.pattern,
                    "response_hash": p.response_hash,
                    "confidence": p.confidence,
                    "source_node": p.source_node,
                    "votes": p.votes,
                    "created": p.created,
                }
                for p in self.node.received_patterns
            ],
            "contribution_score": self.node.contribution_score,
        }
        with file_lock(state_file):
            atomic_write(state_file, json.dumps(data, indent=2))

    def learn_pattern(self, pattern: str, response: str, confidence: float = 0.8) -> SharedPattern:
        """Learn a new pattern locally.

        The response is hashed - we share the pattern but not the actual answer.
        Other nodes can validate if they have similar patterns.
        """
        response_hash = hashlib.sha256(response.encode()).hexdigest()[:32]

        shared = SharedPattern(
            id=f"pat_{len(self.node.learned_patterns)}_{self.node_id[:8]}",
            pattern=pattern,
            response_hash=response_hash,
            confidence=confidence,
            source_node=self.node_id,
        )

        self.node.learned_patterns.append(shared)
        self.node.contribution_score += 1.0
        self._save()

        return shared

    def export_patterns(self) -> list[dict[str, Any]]:
        """Export patterns for sharing with other nodes.

        This is what gets shared - no actual response data!
        """
        return [
            {
                "id": p.id,
                "pattern": p.pattern,
                "response_hash": p.response_hash,
                "confidence": p.confidence,
                "source_node": p.source_node[:8] + "...",  # Truncate for privacy
                "votes": p.votes,
            }
            for p in self.node.learned_patterns
            if p.confidence >= 0.7  # Only share high-confidence patterns
        ]

    def import_patterns(self, patterns: list[dict[str, Any]]) -> int:
        """Import patterns from other nodes.

        We don't import the actual responses - just the patterns.
        Local validation happens when queries match.
        """
        imported = 0
        existing_hashes = {p.response_hash for p in self.node.received_patterns}

        for p in patterns:
            if p["response_hash"] not in existing_hashes:
                shared = SharedPattern(
                    id=p["id"],
                    pattern=p["pattern"],
                    response_hash=p["response_hash"],
                    confidence=p["confidence"] * 0.8,  # Reduce confidence for imported
                    source_node=p["source_node"],
                    votes=p.get("votes", 1),
                )
                self.node.received_patterns.append(shared)
                imported += 1

        self._save()
        return imported

    def validate_pattern(self, pattern_id: str, is_valid: bool) -> None:
        """Validate a received pattern based on local experience.

        This is how collective intelligence grows:
        - If pattern works locally, vote up
        - If pattern fails locally, vote down
        - High-vote patterns become trusted
        """
        for p in self.node.received_patterns:
            if p.id == pattern_id:
                if is_valid:
                    p.votes += 1
                    p.confidence = min(1.0, p.confidence + 0.1)
                else:
                    p.votes -= 1
                    p.confidence = max(0.1, p.confidence - 0.1)
                break

        self._save()

    def get_trusted_patterns(self, min_votes: int = 3) -> list[SharedPattern]:
        """Get patterns that have been validated by multiple nodes."""
        all_patterns = self.node.learned_patterns + self.node.received_patterns
        return [p for p in all_patterns if p.votes >= min_votes]

    def merge_with_local(self) -> list[dict[str, Any]]:
        """Merge trusted federated patterns with local edge rules.

        Returns rules that can be added to EdgeInference.
        """
        trusted = self.get_trusted_patterns(min_votes=2)

        return [
            {
                "id": f"federated_{p.id}",
                "pattern": p.pattern,
                "confidence": p.confidence,
                "metadata": {
                    "source": "federated",
                    "votes": p.votes,
                    "origin": p.source_node[:8],
                },
            }
            for p in trusted
        ]

    def stats(self) -> dict[str, Any]:
        """Get federated learning statistics."""
        return {
            "node_id": self.node_id[:8] + "...",
            "learned_patterns": len(self.node.learned_patterns),
            "received_patterns": len(self.node.received_patterns),
            "trusted_patterns": len(self.get_trusted_patterns()),
            "contribution_score": self.node.contribution_score,
            "total_votes_given": sum(p.votes for p in self.node.received_patterns),
        }


# === Sync Protocol ===

class FederatedSync:
    """Protocol for syncing patterns between nodes.

    Can use various transports:
    - HTTP API (central coordinator)
    - P2P (direct node-to-node)
    - File-based (sneakernet)
    """

    def __init__(self, learning: FederatedLearning) -> None:
        self.learning = learning

    def prepare_sync_package(self) -> dict[str, Any]:
        """Prepare patterns for syncing."""
        return {
            "version": "3.0.0",
            "node_id": self.learning.node_id[:8],
            "timestamp": datetime.now().isoformat(),
            "patterns": self.learning.export_patterns(),
            "stats": self.learning.stats(),
        }

    def process_sync_package(self, package: dict[str, Any]) -> dict[str, Any]:
        """Process incoming sync package."""
        if package.get("node_id") == self.learning.node_id[:8]:
            return {"imported": 0, "reason": "own_package"}

        imported = self.learning.import_patterns(package.get("patterns", []))
        return {
            "imported": imported,
            "from_node": package.get("node_id"),
            "timestamp": datetime.now().isoformat(),
        }

    def save_for_transfer(self, path: Path) -> None:
        """Save sync package to file for manual transfer."""
        package = self.prepare_sync_package()
        path.write_text(json.dumps(package, indent=2))

    def load_from_transfer(self, path: Path) -> dict[str, Any]:
        """Load and process sync package from file."""
        package = json.loads(path.read_text())
        return self.process_sync_package(package)


# === Singleton ===

_federated: FederatedLearning | None = None


def get_federated_learning() -> FederatedLearning:
    """Get or create federated learning instance."""
    global _federated
    if _federated is None:
        _federated = FederatedLearning()
    return _federated


if __name__ == "__main__":
    logger.info("🌐 FEDERATED LEARNING TEST")
    logger.info("=" * 50)

    # Create two nodes
    import tempfile
    from pathlib import Path

    with tempfile.TemporaryDirectory() as tmpdir:
        # Node A learns a pattern
        node_a = FederatedLearning(Path(tmpdir) / "node_a")
        pattern_a = node_a.learn_pattern(
            "project status|what's the status",
            "Project is 80% complete",
            confidence=0.9,
        )
        logger.info(f"Node A learned: {pattern_a.pattern}")

        # Node A exports
        sync_a = FederatedSync(node_a)
        package = sync_a.prepare_sync_package()
        logger.info(f"Node A exporting {len(package['patterns'])} patterns")

        # Node B imports
        node_b = FederatedLearning(Path(tmpdir) / "node_b")
        sync_b = FederatedSync(node_b)
        result = sync_b.process_sync_package(package)
        logger.info(f"Node B imported {result['imported']} patterns")

        # Node B validates
        node_b.validate_pattern(pattern_a.id, is_valid=True)
        logger.info("Node B validated pattern")

        # Check stats
        logger.info(f"\nNode A stats: {node_a.stats()}")
        logger.info(f"Node B stats: {node_b.stats()}")
