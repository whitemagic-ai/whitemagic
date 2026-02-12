"""Holographic Reduced Representations — Circular Convolution Binding (v14.2).
=============================================================================
Implements HRR (Plate, 1995) for compositional memory retrieval.

HRR allows binding concepts to roles without increasing dimensionality:
  - bind(A, B) = circular_convolution(A, B)  — "A in the role of B"
  - unbind(bound, B) = circular_correlation(bound, B)  — recover A from bound

Use cases in WhiteMagic:
  1. Look-ahead projection in graph walks:
     projected = bind(current_embedding, relation_vector)
     → "What would the next node look like if we follow this relation?"

  2. Compositional queries:
     query = unbind(concept_X, CAUSED_BY)
     → "What caused X?" — recovers the approximate embedding of the cause

  3. Role-filler binding:
     event = bind(bind(AGENT, "Lucas"), bind(ACTION, "created"), bind(OBJECT, "WhiteMagic"))
     → A single vector representing the entire event

Usage:
    from whitemagic.core.memory.hrr import get_hrr_engine
    hrr = get_hrr_engine(dim=384)

    # Bind two concepts
    bound = hrr.bind(embedding_a, embedding_b)

    # Unbind to recover concept A
    recovered = hrr.unbind(bound, embedding_b)

    # Project next hop in graph walk
    projected = hrr.project(current_embedding, "CAUSES")

    # Compositional query
    query = hrr.compose_query("What caused X?", concept_x_embedding)
"""

from __future__ import annotations

import logging
import threading
from typing import Any

import numpy as np

logger = logging.getLogger(__name__)


class HRREngine:
    """Holographic Reduced Representations engine.

    Uses FFT-based circular convolution for efficient binding/unbinding.
    All operations preserve dimensionality: bind(D, D) → D.
    """

    def __init__(self, dim: int = 384) -> None:
        self._dim = dim
        self._rng = np.random.default_rng(seed=42)
        self._lock = threading.Lock()

        # Pre-computed relation vectors (deterministic from seed)
        self._relation_vectors: dict[str, np.ndarray] = {}
        self._init_relation_vectors()

    def _init_relation_vectors(self) -> None:
        """Initialize canonical relation vectors.

        Each relation gets a fixed random unit vector.
        Using a seeded RNG ensures reproducibility across sessions.
        """
        relations = [
            "CAUSES", "CAUSED_BY",
            "FOLLOWS", "PRECEDED_BY",
            "PART_OF", "CONTAINS",
            "SIMILAR_TO", "OPPOSITE_OF",
            "EXTENDS", "EXTENDED_BY",
            "USES", "USED_BY",
            "CREATES", "CREATED_BY",
            "IMPLEMENTS", "IMPLEMENTED_BY",
            "AGENT", "ACTION", "OBJECT", "LOCATION", "TIME",
        ]
        rng = np.random.default_rng(seed=12345)
        for rel in relations:
            vec = rng.standard_normal(self._dim).astype(np.float32)
            vec /= np.linalg.norm(vec)
            self._relation_vectors[rel] = vec

    @property
    def dim(self) -> int:
        return self._dim

    def _to_array(self, vec: list[float] | np.ndarray) -> np.ndarray:
        """Convert input to numpy float32 array of correct dimension."""
        arr = np.asarray(vec, dtype=np.float32)
        if arr.shape != (self._dim,):
            raise ValueError(f"Expected dim={self._dim}, got shape={arr.shape}")
        return arr

    # ------------------------------------------------------------------
    # Core HRR operations
    # ------------------------------------------------------------------

    def bind(
        self, a: list[float] | np.ndarray, b: list[float] | np.ndarray,
    ) -> np.ndarray:
        """Circular convolution: bind A to role B.

        bind(A, B) = IFFT(FFT(A) * FFT(B))

        This is the core HRR operation. The result is a vector of the
        same dimensionality that "encodes" A-in-the-role-of-B.
        """
        a_arr = self._to_array(a)
        b_arr = self._to_array(b)
        result = np.real(np.fft.ifft(np.fft.fft(a_arr) * np.fft.fft(b_arr)))
        return result.astype(np.float32)

    def unbind(
        self, bound: list[float] | np.ndarray, b: list[float] | np.ndarray,
    ) -> np.ndarray:
        """Circular correlation: unbind B from bound vector.

        unbind(bound, B) ≈ A  (approximate recovery of the bound concept)

        Uses the inverse of B (conjugate in frequency domain).
        """
        bound_arr = self._to_array(bound)
        b_arr = self._to_array(b)
        # Correlation = convolution with the inverse
        # In frequency domain: conj(FFT(B)) * FFT(bound)
        result = np.real(np.fft.ifft(
            np.conj(np.fft.fft(b_arr)) * np.fft.fft(bound_arr),
        ))
        return result.astype(np.float32)

    def superpose(self, *vectors: list[float] | np.ndarray) -> np.ndarray:
        """Superposition: element-wise sum of multiple HRR vectors.

        Used to combine multiple bindings into a single composite vector.
        The result can be probed with unbind() to recover individual fillers.
        """
        arrays = [self._to_array(v) for v in vectors]
        result = np.sum(arrays, axis=0)
        # Normalize to unit length
        norm = np.linalg.norm(result)
        if norm > 0:
            result = result / norm
        ret: np.ndarray = result.astype(np.float32)
        return ret

    def similarity(
        self, a: list[float] | np.ndarray, b: list[float] | np.ndarray,
    ) -> float:
        """Cosine similarity between two HRR vectors."""
        a_arr = self._to_array(a)
        b_arr = self._to_array(b)
        dot = np.dot(a_arr, b_arr)
        norm_a = np.linalg.norm(a_arr)
        norm_b = np.linalg.norm(b_arr)
        if norm_a == 0 or norm_b == 0:
            return 0.0
        return float(dot / (norm_a * norm_b))

    # ------------------------------------------------------------------
    # Relation operations
    # ------------------------------------------------------------------

    def get_relation_vector(self, relation: str) -> np.ndarray:
        """Get the canonical vector for a named relation.

        If the relation is unknown, generates a deterministic vector
        from the relation name.
        """
        rel_upper = relation.upper()
        if rel_upper in self._relation_vectors:
            return self._relation_vectors[rel_upper]

        # Generate a deterministic vector from the relation name
        seed = hash(rel_upper) % (2**31)
        rng = np.random.default_rng(seed=seed)
        vec = rng.standard_normal(self._dim).astype(np.float32)
        vec /= np.linalg.norm(vec)
        self._relation_vectors[rel_upper] = vec
        return vec

    def project(
        self,
        embedding: list[float] | np.ndarray,
        relation: str,
    ) -> np.ndarray:
        """Project an embedding through a relation.

        project(E, "CAUSES") → "What does something caused by E look like?"

        This is the key operation for semantic walk steering:
        instead of comparing neighbors to the raw query, compare them
        to the projected query — what the next hop *should* look like
        given the current context and the relation being traversed.
        """
        rel_vec = self.get_relation_vector(relation)
        return self.bind(embedding, rel_vec)

    def inverse_project(
        self,
        embedding: list[float] | np.ndarray,
        relation: str,
    ) -> np.ndarray:
        """Inverse projection: "What was the source that led to E via relation?"

        inverse_project(E, "CAUSES") → "What caused E?"
        """
        rel_vec = self.get_relation_vector(relation)
        return self.unbind(embedding, rel_vec)

    # ------------------------------------------------------------------
    # Composite event encoding
    # ------------------------------------------------------------------

    def encode_event(
        self,
        agent: list[float] | np.ndarray | None = None,
        action: list[float] | np.ndarray | None = None,
        obj: list[float] | np.ndarray | None = None,
        location: list[float] | np.ndarray | None = None,
        time_vec: list[float] | np.ndarray | None = None,
    ) -> np.ndarray:
        """Encode a structured event as a single HRR vector.

        event = bind(agent, AGENT) + bind(action, ACTION) + bind(obj, OBJECT) + ...

        The result can be probed to recover individual fillers:
            recovered_agent = unbind(event, AGENT)
        """
        components: list[np.ndarray] = []

        if agent is not None:
            components.append(self.bind(agent, self.get_relation_vector("AGENT")))
        if action is not None:
            components.append(self.bind(action, self.get_relation_vector("ACTION")))
        if obj is not None:
            components.append(self.bind(obj, self.get_relation_vector("OBJECT")))
        if location is not None:
            components.append(self.bind(location, self.get_relation_vector("LOCATION")))
        if time_vec is not None:
            components.append(self.bind(time_vec, self.get_relation_vector("TIME")))

        if not components:
            return np.zeros(self._dim, dtype=np.float32)

        return self.superpose(*components)

    def decode_event_role(
        self,
        event: list[float] | np.ndarray,
        role: str,
    ) -> np.ndarray:
        """Decode a role filler from an event vector.

        decode_event_role(event, "AGENT") → approximate agent embedding
        """
        return self.unbind(event, self.get_relation_vector(role))

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        return {
            "dim": self._dim,
            "num_relation_vectors": len(self._relation_vectors),
            "relations": sorted(self._relation_vectors.keys()),
        }

    def available_relations(self) -> list[str]:
        return sorted(self._relation_vectors.keys())


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_engine: HRREngine | None = None
_engine_lock = threading.Lock()


def get_hrr_engine(dim: int = 384, **kwargs: Any) -> HRREngine:
    """Get the global HRREngine singleton."""
    global _engine
    if _engine is None:
        with _engine_lock:
            if _engine is None:
                _engine = HRREngine(dim=dim, **kwargs)
    return _engine
