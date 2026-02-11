"""Surprise-Gated Memory Ingestion (v14.0 Living Graph).
======================================================
Balances plasticity (learning new concepts) vs stability (reinforcing
existing knowledge) at memory store time.

Surprise is computed as: S = -log₂(max_cosine_similarity_to_existing)

Decision logic:
  - High Surprise (S > 3.0): Novel concept → create + boost importance + emit NOVEL_CONCEPT
  - Medium (1.0 < S ≤ 3.0): Normal → store as usual
  - Low (S ≤ 1.0): Redundant → reinforce nearest neighbor instead of creating new memory

This prevents the Data Sea from filling with near-duplicate memories
while ensuring genuinely novel information gets priority treatment.

Requires the embedding engine (sentence-transformers). Gracefully degrades
to pass-through when embeddings are unavailable.

Usage:
    from whitemagic.core.memory.surprise_gate import get_surprise_gate
    gate = get_surprise_gate()

    verdict = gate.evaluate(content="some new memory content")
    # verdict.action: 'create', 'reinforce', or 'create_boosted'
"""

from __future__ import annotations

import logging
import math
import threading
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class SurpriseAction(Enum):
    """What to do with incoming memory content."""

    CREATE = "create"               # Normal: store as new memory
    CREATE_BOOSTED = "create_boosted"  # Novel: store with boosted importance
    REINFORCE = "reinforce"         # Redundant: strengthen existing instead


@dataclass
class SurpriseVerdict:
    """Result of surprise evaluation."""

    action: SurpriseAction
    surprise_score: float           # -log₂(max_similarity)
    max_similarity: float           # Cosine similarity to nearest existing
    nearest_memory_id: str | None   # ID of most similar existing memory
    reason: str
    evaluation_ms: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "action": self.action.value,
            "surprise_score": round(self.surprise_score, 3),
            "max_similarity": round(self.max_similarity, 4),
            "nearest_memory_id": self.nearest_memory_id,
            "reason": self.reason,
            "evaluation_ms": round(self.evaluation_ms, 1),
        }


class SurpriseGate:
    """Evaluates incoming content for novelty before storage.

    Uses embedding similarity to determine if content is:
    - Novel (high surprise): boost and store
    - Normal (medium surprise): store normally
    - Redundant (low surprise): reinforce existing memory
    """

    def __init__(
        self,
        high_threshold: float = 3.0,
        low_threshold: float = 1.0,
        importance_boost: float = 0.15,
        reinforcement_strength: float = 0.05,
    ) -> None:
        self._high_threshold = high_threshold
        self._low_threshold = low_threshold
        self._importance_boost = importance_boost
        self._reinforcement_strength = reinforcement_strength
        self._lock = threading.Lock()
        self._total_evaluations = 0
        self._total_novel = 0
        self._total_redundant = 0
        self._total_normal = 0

    def evaluate(self, content: str, limit: int = 20) -> SurpriseVerdict:
        """Evaluate surprise of incoming content.

        Args:
            content: Text content to evaluate.
            limit: Max existing memories to compare against.

        Returns:
            SurpriseVerdict with action recommendation.
        """
        start = time.perf_counter()

        # Try to get embedding engine
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            if not engine.available():
                raise RuntimeError("Embeddings unavailable")
        except Exception:
            # No embeddings → pass through as normal create
            with self._lock:
                self._total_evaluations += 1
                self._total_normal += 1
            return SurpriseVerdict(
                action=SurpriseAction.CREATE,
                surprise_score=2.0,  # Assume medium
                max_similarity=0.0,
                nearest_memory_id=None,
                reason="Embeddings unavailable — defaulting to create",
                evaluation_ms=(time.perf_counter() - start) * 1000,
            )

        # Encode incoming content
        try:
            query_vec = engine.encode(content)
            if query_vec is None:
                raise RuntimeError("Encoding failed")
        except Exception as e:
            return SurpriseVerdict(
                action=SurpriseAction.CREATE,
                surprise_score=2.0,
                max_similarity=0.0,
                nearest_memory_id=None,
                reason=f"Encoding failed: {e}",
                evaluation_ms=(time.perf_counter() - start) * 1000,
            )

        # Find most similar existing memory
        try:
            hits = engine.search_similar(
                content, limit=limit, min_similarity=0.0,
            )
        except Exception:
            hits = []

        if not hits:
            # No existing memories to compare → novel by definition
            elapsed = (time.perf_counter() - start) * 1000
            with self._lock:
                self._total_evaluations += 1
                self._total_novel += 1
            return SurpriseVerdict(
                action=SurpriseAction.CREATE_BOOSTED,
                surprise_score=10.0,
                max_similarity=0.0,
                nearest_memory_id=None,
                reason="No existing memories — novel by default",
                evaluation_ms=elapsed,
            )

        # Get max similarity
        max_sim = max(h.get("similarity", 0.0) for h in hits)
        nearest_id = hits[0].get("memory_id") if hits else None

        # Clamp to avoid log(0)
        max_sim_clamped = max(0.001, min(0.999, max_sim))

        # Surprise score: -log₂(max_similarity)
        surprise = -math.log2(max_sim_clamped)

        elapsed = (time.perf_counter() - start) * 1000

        # Decision
        if surprise > self._high_threshold:
            action = SurpriseAction.CREATE_BOOSTED
            reason = f"Novel concept (S={surprise:.2f} > {self._high_threshold})"
            with self._lock:
                self._total_novel += 1
        elif surprise <= self._low_threshold:
            action = SurpriseAction.REINFORCE
            reason = f"Redundant (S={surprise:.2f} ≤ {self._low_threshold}, sim={max_sim:.3f})"
            with self._lock:
                self._total_redundant += 1
        else:
            action = SurpriseAction.CREATE
            reason = f"Normal novelty (S={surprise:.2f})"
            with self._lock:
                self._total_normal += 1

        with self._lock:
            self._total_evaluations += 1

        return SurpriseVerdict(
            action=action,
            surprise_score=surprise,
            max_similarity=max_sim,
            nearest_memory_id=nearest_id,
            reason=reason,
            evaluation_ms=elapsed,
        )

    def apply(self, verdict: SurpriseVerdict, memory_kwargs: dict[str, Any]) -> dict[str, Any]:
        """Apply surprise verdict to memory store kwargs.

        Modifies the kwargs dict in-place based on the verdict:
        - CREATE_BOOSTED: increases importance
        - REINFORCE: sets a flag for the caller to reinforce instead of create
        - CREATE: no modification

        Returns:
            Modified kwargs with surprise metadata.
        """
        memory_kwargs.setdefault("metadata", {})
        memory_kwargs["metadata"]["surprise_score"] = round(verdict.surprise_score, 3)
        memory_kwargs["metadata"]["surprise_action"] = verdict.action.value

        if verdict.action == SurpriseAction.CREATE_BOOSTED:
            current_importance = memory_kwargs.get("importance", 0.5)
            memory_kwargs["importance"] = min(1.0, current_importance + self._importance_boost)
            memory_kwargs["metadata"]["surprise_boosted"] = True

        elif verdict.action == SurpriseAction.REINFORCE:
            memory_kwargs["metadata"]["reinforce_target"] = verdict.nearest_memory_id
            memory_kwargs["metadata"]["reinforce_similarity"] = round(verdict.max_similarity, 4)

        return memory_kwargs

    def _emit_novel_event(self, verdict: SurpriseVerdict) -> None:
        """Emit NOVEL_CONCEPT event to Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            get_bus().emit(ResonanceEvent(
                event_type=EventType.PATTERN_DETECTED,
                source="surprise_gate",
                data={
                    "type": "novel_concept",
                    "surprise_score": round(verdict.surprise_score, 3),
                    "nearest_memory_id": verdict.nearest_memory_id,
                },
            ))
        except Exception:
            pass

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return {
                "total_evaluations": self._total_evaluations,
                "total_novel": self._total_novel,
                "total_redundant": self._total_redundant,
                "total_normal": self._total_normal,
                "high_threshold": self._high_threshold,
                "low_threshold": self._low_threshold,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_gate: SurpriseGate | None = None
_gate_lock = threading.Lock()


def get_surprise_gate(**kwargs: Any) -> SurpriseGate:
    """Get the global SurpriseGate singleton."""
    global _gate
    if _gate is None:
        with _gate_lock:
            if _gate is None:
                _gate = SurpriseGate(**kwargs)
    return _gate
