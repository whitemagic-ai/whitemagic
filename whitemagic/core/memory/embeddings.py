"""Semantic Embedding Layer (v14.1).
=================================
Provides sentence-level semantic embeddings for memory search and
association mining. Replaces keyword-overlap Jaccard with true
semantic similarity using sentence-transformers.

Design:
  - Lazy-loaded model (MiniLM-L6-v2, 384 dims, ~100MB)
  - Embeddings cached in `memory_embeddings` table (SQLite)
  - Batch encode via model.encode() with progress
  - HNSW approximate nearest-neighbor index (O(log N) queries)
  - Falls back to brute-force numpy cosine when hnswlib not available
  - Graceful fallback when sentence-transformers not installed

Usage:
    from whitemagic.core.memory.embeddings import get_embedding_engine
    engine = get_embedding_engine()

    # Encode a single text
    vec = engine.encode("holographic coordinate system")

    # Find similar memories (hot DB only)
    results = engine.search_similar("memory consolidation", limit=10)

    # Find similar memories across hot + cold DBs
    results = engine.search_similar("memory consolidation", limit=10, include_cold=True)

    # Batch encode all LONG_TERM memories
    engine.index_memories(memory_type="LONG_TERM")
"""

from __future__ import annotations

import logging
import math
import sqlite3
import struct
import threading
import time
from typing import Any, cast

import numpy as np

logger = logging.getLogger(__name__)

# Embedding dimension for MiniLM-L6-v2
EMBEDDING_DIM = 384
MODEL_NAME = "all-MiniLM-L6-v2"


def _cosine_similarity(a: list[float], b: list[float]) -> float:
    """Cosine similarity — delegates to Zig SIMD when available, else pure Python."""
    try:
        from whitemagic.core.acceleration.simd_cosine import (
            cosine_similarity as _simd_cos,
        )
        return float(_simd_cos(a, b))
    except Exception:
        pass
    dot = sum(x * y for x, y in zip(a, b))
    norm_a = math.sqrt(sum(x * x for x in a))
    norm_b = math.sqrt(sum(x * x for x in b))
    if norm_a == 0 or norm_b == 0:
        return 0.0
    return dot / (norm_a * norm_b)


def _batch_cosine_similarity_numpy(query_vec: np.ndarray, matrix: np.ndarray, pre_normalized: bool = False) -> np.ndarray:
    """Vectorized batch cosine similarity using numpy.

    Args:
        query_vec: 1D numpy array of shape (D,).
        matrix: 2D numpy array of shape (N, D) — contiguous, float32.
        pre_normalized: If True, matrix rows are already unit-length.
            Skips the per-row norm computation — saves ~30% of FLOPS.

    Returns:
        1D numpy array of shape (N,) with cosine similarities.

    For 5,500 vectors × 384 dims this is one BLAS matrix-vector multiply
    instead of 5,500 Python loops — typically 20-50× faster.
    With pre_normalized=True, it's a single matmul: O(N*D) instead of O(2*N*D).
    """
    # Normalize query
    query_norm = np.linalg.norm(query_vec)
    if query_norm == 0:
        return cast(np.ndarray, np.zeros(matrix.shape[0], dtype=np.float32))
    q = query_vec / query_norm

    if pre_normalized:
        # Matrix rows already unit-length: dot product == cosine similarity
        return cast(np.ndarray, matrix @ q)

    # Normalize matrix rows
    norms = np.linalg.norm(matrix, axis=1)
    norms[norms == 0] = 1.0  # avoid division by zero
    dots = matrix @ q
    return cast(np.ndarray, dots / norms)


def _batch_cosine_similarity(query: list[float] | np.ndarray, vectors: list[list[float]] | np.ndarray) -> list[float] | np.ndarray:
    """Batch cosine similarity — numpy fast path, Zig SIMD fallback, then pure Python."""
    # Fast path: if vectors is already a numpy array, use vectorized ops
    if isinstance(vectors, np.ndarray) and vectors.ndim == 2:
        q = np.asarray(query, dtype=np.float32) if not isinstance(query, np.ndarray) else query
        return _batch_cosine_similarity_numpy(q, vectors)

    # Zig SIMD path for list inputs
    try:
        from whitemagic.core.acceleration.simd_cosine import batch_cosine as _simd_batch
        return cast(list[float] | np.ndarray, _simd_batch(query, vectors))  # type: ignore[arg-type]
    except Exception:
        pass
    query_list = cast(list[float], query.tolist()) if isinstance(query, np.ndarray) else query
    if isinstance(vectors, np.ndarray):
        vector_list = [cast(list[float], row.tolist()) for row in vectors]
    else:
        vector_list = vectors
    return [_cosine_similarity(query_list, v) for v in vector_list]


def _pack_embedding(vec: list[float]) -> bytes:
    """Pack a float list into compact bytes for SQLite storage."""
    return struct.pack(f"{len(vec)}f", *vec)


def _unpack_embedding(data: bytes) -> list[float]:
    """Unpack bytes back to float list."""
    n = len(data) // 4
    return list(struct.unpack(f"{n}f", data))


class EmbeddingEngine:
    """Semantic embedding engine for memory search."""

    def __init__(self) -> None:
        self._model: Any | None = None
        self._model_lock = threading.Lock()
        self._available: bool | None = None  # None = unchecked
        self._db_conn: sqlite3.Connection | None = None
        self._cold_db_conn: sqlite3.Connection | None = None
        self._cold_db_checked = False
        # In-memory vector cache for fast repeated searches (hot DB)
        # Vectors stored as contiguous numpy array (N, 384) for SIMD-friendly search
        # Pre-normalized at load time: each row is unit-length, so dot product == cosine
        self._vec_cache_ids: list[str] | None = None
        self._vec_cache_vecs: np.ndarray | None = None  # shape (N, EMBEDDING_DIM), float32, PRE-NORMALIZED
        self._vec_cache_lock = threading.Lock()
        self._vec_cache_count: int = 0  # DB count at cache time
        # HNSW index (optional, O(log N) search)
        self._hnsw_index: Any | None = None
        self._hnsw_ids: list[str] | None = None
        self._hnsw_count: int = 0
        self._hnsw_available: bool | None = None  # None = unchecked
        # Cold HNSW index
        self._cold_hnsw_index: Any | None = None
        self._cold_hnsw_ids: list[str] | None = None
        self._cold_hnsw_count: int = 0
        # Separate vector cache for cold DB
        self._cold_vec_cache_ids: list[str] | None = None
        self._cold_vec_cache_vecs: np.ndarray | None = None  # shape (N, EMBEDDING_DIM), float32
        self._cold_vec_cache_lock = threading.Lock()
        self._cold_vec_cache_count: int = 0

    def available(self) -> bool:
        """Check if sentence-transformers is installed."""
        if self._available is not None:
            return self._available
        try:
            import sentence_transformers  # noqa: F401
            self._available = True
        except ImportError:
            self._available = False
            logger.debug("sentence-transformers not installed — embedding engine unavailable")
        return self._available

    def _get_model(self) -> Any:
        """Lazy-load the sentence-transformer model."""
        if self._model is not None:
            return self._model
        with self._model_lock:
            if self._model is not None:
                return self._model
            if not self.available():
                return None
            try:
                from sentence_transformers import SentenceTransformer
                logger.info(f"Loading embedding model: {MODEL_NAME}")
                self._model = SentenceTransformer(MODEL_NAME)
                logger.info(f"Embedding model loaded ({EMBEDDING_DIM} dims)")
            except Exception as e:
                logger.warning(f"Failed to load embedding model: {e}")
                self._available = False
                return None
        return self._model

    def _get_db(self) -> sqlite3.Connection | None:
        """Get or create the embedding cache DB connection."""
        if self._db_conn is not None:
            return self._db_conn
        try:
            from whitemagic.config.paths import DB_PATH
            conn = sqlite3.connect(str(DB_PATH))
            conn.execute("PRAGMA journal_mode=WAL")
            conn.execute("PRAGMA synchronous=NORMAL")
            conn.execute("PRAGMA mmap_size=268435456")
            conn.execute("PRAGMA cache_size=-65536")
            conn.execute("PRAGMA temp_store=MEMORY")
            conn.execute("""
                CREATE TABLE IF NOT EXISTS memory_embeddings (
                    memory_id TEXT PRIMARY KEY,
                    embedding BLOB,
                    model TEXT,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP
                )
            """)
            conn.commit()
            self._db_conn = conn
        except Exception as e:
            logger.debug(f"Embedding DB init failed: {e}")
            return None
        return self._db_conn

    def _get_cold_db(self) -> sqlite3.Connection | None:
        """Get or create the cold DB connection for embedding search."""
        if self._cold_db_conn is not None:
            return self._cold_db_conn
        if self._cold_db_checked:
            return None  # Already tried, not available
        self._cold_db_checked = True
        try:
            from whitemagic.config.paths import COLD_DB_PATH
            if not COLD_DB_PATH.exists():
                return None
            conn = sqlite3.connect(str(COLD_DB_PATH))
            conn.execute("PRAGMA journal_mode=WAL")
            conn.execute("PRAGMA synchronous=NORMAL")
            conn.execute("PRAGMA mmap_size=268435456")
            conn.execute("PRAGMA cache_size=-65536")
            conn.execute("PRAGMA temp_store=MEMORY")
            # Check if memory_embeddings table exists
            tables = conn.execute(
                "SELECT name FROM sqlite_master WHERE type='table' AND name='memory_embeddings'",
            ).fetchall()
            if not tables:
                logger.debug("Cold DB has no memory_embeddings table yet")
                conn.close()
                self._cold_db_checked = False  # Retry later (might be encoding)
                return None
            self._cold_db_conn = conn
            logger.debug("Cold DB embedding connection established")
        except Exception as e:
            logger.debug(f"Cold DB embedding init failed: {e}")
            return None
        return self._cold_db_conn

    def _load_cold_vec_cache(self) -> tuple[list[str], np.ndarray]:
        """Load or return cached vectors from cold DB as contiguous numpy array."""
        cold_db = self._get_cold_db()
        if cold_db is None:
            return [], np.empty((0, EMBEDDING_DIM), dtype=np.float32)

        with self._cold_vec_cache_lock:
            try:
                current_count = cold_db.execute(
                    "SELECT COUNT(*) FROM memory_embeddings",
                ).fetchone()[0]
            except Exception:
                current_count = 0

            if current_count == 0:
                return [], np.empty((0, EMBEDDING_DIM), dtype=np.float32)

            if (self._cold_vec_cache_ids is not None
                    and self._cold_vec_cache_vecs is not None
                    and self._cold_vec_cache_count == current_count):
                return self._cold_vec_cache_ids, self._cold_vec_cache_vecs

            # Reload from cold DB into contiguous numpy array
            try:
                rows = cold_db.execute(
                    "SELECT memory_id, embedding FROM memory_embeddings",
                ).fetchall()
            except Exception:
                return [], np.empty((0, EMBEDDING_DIM), dtype=np.float32)

            ids = [r[0] for r in rows]
            vecs = np.array(
                [_unpack_embedding(r[1]) for r in rows],
                dtype=np.float32,
            )
            self._cold_vec_cache_ids = ids
            self._cold_vec_cache_vecs = vecs
            self._cold_vec_cache_count = current_count
            logger.debug(f"Cold vector cache loaded: {len(ids)} embeddings ({vecs.nbytes / 1024:.0f} KB contiguous)")
            return ids, vecs

    def encode(self, text: str) -> list[float] | None:
        """Encode a single text into an embedding vector."""
        model = self._get_model()
        if model is None:
            return None
        try:
            vec = model.encode(text, show_progress_bar=False)
            return cast(list[float], vec.tolist())
        except Exception as e:
            logger.debug(f"Encoding failed: {e}")
            return None

    def encode_batch(self, texts: list[str], batch_size: int = 64) -> list[list[float]] | None:
        """Batch encode texts into embedding vectors."""
        model = self._get_model()
        if model is None:
            return None
        try:
            vecs = model.encode(texts, batch_size=batch_size, show_progress_bar=False)
            return [v.tolist() for v in vecs]
        except Exception as e:
            logger.debug(f"Batch encoding failed: {e}")
            return None

    def get_cached_embedding(self, memory_id: str) -> list[float] | None:
        """Retrieve a cached embedding for a memory."""
        db = self._get_db()
        if db is None:
            return None
        try:
            row = db.execute(
                "SELECT embedding FROM memory_embeddings WHERE memory_id = ?",
                (memory_id,),
            ).fetchone()
            if row and row[0]:
                return _unpack_embedding(row[0])
        except Exception:
            pass
        return None

    def cache_embedding(self, memory_id: str, embedding: list[float]) -> bool:
        """Cache an embedding for a memory."""
        db = self._get_db()
        if db is None:
            return False
        try:
            db.execute(
                "INSERT OR REPLACE INTO memory_embeddings (memory_id, embedding, model) VALUES (?, ?, ?)",
                (memory_id, _pack_embedding(embedding), MODEL_NAME),
            )
            db.commit()
            self._invalidate_vec_cache()
            return True
        except Exception:
            return False

    def _invalidate_vec_cache(self) -> None:
        """Invalidate the in-memory vector cache and HNSW index."""
        with self._vec_cache_lock:
            self._vec_cache_ids = None
            self._vec_cache_vecs = None
            self._vec_cache_count = 0
            self._hnsw_index = None
            self._hnsw_ids = None
            self._hnsw_count = 0

    def _hnsw_is_available(self) -> bool:
        """Check if hnswlib is installed."""
        if self._hnsw_available is not None:
            return self._hnsw_available
        try:
            import hnswlib  # noqa: F401
            self._hnsw_available = True
        except ImportError:
            self._hnsw_available = False
        return self._hnsw_available

    def _build_hnsw_index(self, ids: list[str], vectors: np.ndarray) -> Any:
        """Build an HNSW index from vectors.

        Parameters tuned for memory workloads:
          - ef_construction=200: higher quality graph (default 100)
          - M=32: connections per node (good for 384-dim, default 16)
          - ef=100: search-time beam width (recall ~99% at this level)
        """
        import hnswlib
        n, dim = vectors.shape
        index = hnswlib.Index(space="cosine", dim=dim)
        # max_elements with 20% headroom for incremental adds
        index.init_index(max_elements=max(n + 100, int(n * 1.2)), ef_construction=200, M=32)
        index.add_items(vectors, list(range(n)))
        index.set_ef(100)
        return index

    def _get_hnsw_index(self) -> tuple[Any, list[str]] | None:
        """Get or build the hot HNSW index. Returns (index, id_list) or None."""
        if not self._hnsw_is_available():
            return None
        ids, vectors = self._load_vec_cache()
        if not ids or not hasattr(vectors, 'shape') or vectors.shape[0] < 5:
            return None  # Too few vectors for HNSW to be useful
        with self._vec_cache_lock:
            if (self._hnsw_index is not None
                    and self._hnsw_ids is not None
                    and self._hnsw_count == len(ids)):
                return self._hnsw_index, self._hnsw_ids
        try:
            index = self._build_hnsw_index(ids, vectors)
            self._hnsw_index = index
            self._hnsw_ids = ids
            self._hnsw_count = len(ids)
            logger.debug(f"HNSW index built: {len(ids)} vectors, dim={vectors.shape[1]}")
            return index, ids
        except Exception as e:
            logger.debug(f"HNSW build failed: {e}")
            self._hnsw_available = False
            return None

    def _get_cold_hnsw_index(self) -> tuple[Any, list[str]] | None:
        """Get or build the cold HNSW index. Returns (index, id_list) or None."""
        if not self._hnsw_is_available():
            return None
        cold_ids, cold_vectors = self._load_cold_vec_cache()
        if not cold_ids or not hasattr(cold_vectors, 'shape') or cold_vectors.shape[0] < 5:
            return None
        if (self._cold_hnsw_index is not None
                and self._cold_hnsw_ids is not None
                and self._cold_hnsw_count == len(cold_ids)):
            return self._cold_hnsw_index, self._cold_hnsw_ids
        try:
            index = self._build_hnsw_index(cold_ids, cold_vectors)
            self._cold_hnsw_index = index
            self._cold_hnsw_ids = cold_ids
            self._cold_hnsw_count = len(cold_ids)
            logger.debug(f"Cold HNSW index built: {len(cold_ids)} vectors")
            return index, cold_ids
        except Exception as e:
            logger.debug(f"Cold HNSW build failed: {e}")
            return None

    def _hnsw_search(self, query_vec: np.ndarray, index: Any, ids: list[str],
                     limit: int, min_similarity: float) -> list[dict[str, Any]]:
        """Search an HNSW index. Returns list of {memory_id, similarity, source}."""
        q = np.asarray(query_vec, dtype=np.float32).reshape(1, -1)
        k = min(limit * 3, len(ids))  # Over-fetch then filter
        labels, distances = index.knn_query(q, k=k)
        results = []
        for idx, dist in zip(labels[0], distances[0]):
            sim = 1.0 - dist  # hnswlib cosine returns distance = 1 - similarity
            if sim >= min_similarity:
                results.append({"memory_id": ids[idx], "similarity": round(float(sim), 4)})
        return results

    def _load_vec_cache(self) -> tuple[list[str], np.ndarray]:
        """Load or return cached vectors as contiguous numpy array.

        Returns (ids, vectors) where vectors is shape (N, 384) float32.
        One contiguous allocation, cache-line friendly, BLAS-ready.
        """
        db = self._get_db()
        if db is None:
            return [], np.empty((0, EMBEDDING_DIM), dtype=np.float32)

        with self._vec_cache_lock:
            # Check if cache is still valid
            try:
                current_count = db.execute(
                    "SELECT COUNT(*) FROM memory_embeddings",
                ).fetchone()[0]
            except Exception:
                current_count = 0

            if (self._vec_cache_ids is not None
                    and self._vec_cache_vecs is not None
                    and self._vec_cache_count == current_count):
                return self._vec_cache_ids, self._vec_cache_vecs

            # Reload from DB into contiguous numpy array
            try:
                rows = db.execute(
                    "SELECT memory_id, embedding FROM memory_embeddings",
                ).fetchall()
            except Exception:
                return [], np.empty((0, EMBEDDING_DIM), dtype=np.float32)

            if not rows:
                return [], np.empty((0, EMBEDDING_DIM), dtype=np.float32)
            ids = [r[0] for r in rows]
            # Unpack all blobs into a single contiguous float32 array
            vecs = np.array(
                [_unpack_embedding(r[1]) for r in rows],
                dtype=np.float32,
            )
            if vecs.ndim == 1:
                vecs = vecs.reshape(-1, EMBEDDING_DIM)
            # Pre-normalize: each row becomes unit-length
            # This saves ~30% FLOPS per search (no per-row norm at query time)
            norms = np.linalg.norm(vecs, axis=1, keepdims=True)
            norms[norms == 0] = 1.0
            vecs = vecs / norms
            self._vec_cache_ids = ids
            self._vec_cache_vecs = vecs
            self._vec_cache_count = current_count
            logger.debug(f"Vector cache loaded: {len(ids)} embeddings ({vecs.nbytes / 1024:.0f} KB contiguous, pre-normalized)")
            return ids, vecs

    def index_memories(
        self,
        memory_type: str | None = None,
        limit: int = 10000,
        skip_cached: bool = True,
    ) -> dict[str, Any]:
        """Batch-encode and cache embeddings for memories.

        Returns a dict with indexing stats.
        """
        if not self.available():
            return {"status": "unavailable", "reason": "sentence-transformers not installed"}

        db = self._get_db()
        if db is None:
            return {"status": "error", "reason": "DB unavailable"}

        t0 = time.perf_counter()

        # Find memories to encode
        sql = "SELECT id, title, content FROM memories"
        params: list = []
        conditions = []

        if memory_type:
            conditions.append("memory_type = ?")
            params.append(memory_type)

        if skip_cached:
            conditions.append("id NOT IN (SELECT memory_id FROM memory_embeddings)")

        if conditions:
            sql += " WHERE " + " AND ".join(conditions)

        sql += " ORDER BY importance DESC LIMIT ?"
        params.append(limit)

        rows = db.execute(sql, params).fetchall()

        if not rows:
            return {"status": "success", "indexed": 0, "reason": "all memories already cached"}

        # Prepare texts
        texts = [f"{row[1] or ''} {row[2] or ''}" for row in rows]
        ids = [row[0] for row in rows]

        # Batch encode
        embeddings = self.encode_batch(texts)
        if embeddings is None:
            return {"status": "error", "reason": "encoding failed"}

        # Cache (batch insert without per-row cache invalidation)
        cached = 0
        for mid, emb in zip(ids, embeddings):
            try:
                db.execute(
                    "INSERT OR REPLACE INTO memory_embeddings (memory_id, embedding, model) VALUES (?, ?, ?)",
                    (mid, _pack_embedding(emb), MODEL_NAME),
                )
                cached += 1
            except Exception:
                pass
        db.commit()
        self._invalidate_vec_cache()

        elapsed = time.perf_counter() - t0
        return {
            "status": "success",
            "indexed": cached,
            "total_candidates": len(rows),
            "duration_s": round(elapsed, 1),
            "rate": round(cached / elapsed, 1) if elapsed > 0 else 0,
            "model": MODEL_NAME,
            "dims": EMBEDDING_DIM,
        }

    def search_similar(
        self, query: str, limit: int = 10, min_similarity: float = 0.3,
        include_cold: bool = False,
    ) -> list[dict[str, Any]]:
        """Search for memories semantically similar to a query.

        Args:
            query: Search query text.
            limit: Maximum results to return.
            min_similarity: Minimum cosine similarity threshold.
            include_cold: If True, also search cold DB embeddings.
                Hot results are returned first; cold results fill remaining slots.

        Returns a list of dicts with memory_id, similarity, and source ('hot'/'cold').
        Sorted by similarity descending.

        """
        query_vec = self.encode(query)
        if query_vec is None:
            return []

        # --- Hot DB search ---
        results = []

        # HNSW fast path (O(log N) per query)
        hnsw_result = self._get_hnsw_index()
        if hnsw_result is not None:
            q_np = np.asarray(query_vec, dtype=np.float32)
            hnsw_index, hnsw_ids = hnsw_result
            hits = self._hnsw_search(q_np, hnsw_index, hnsw_ids, limit, min_similarity)
            for hit in hits:
                hit["source"] = "hot"
                results.append(hit)
        else:
            # Brute-force fallback (O(N) per query)
            ids, vectors = self._load_vec_cache()
            if ids:
                q_np = np.asarray(query_vec, dtype=np.float32)
                scores = _batch_cosine_similarity_numpy(q_np, vectors, pre_normalized=True)
                if isinstance(scores, np.ndarray):
                    mask = scores >= min_similarity
                    for idx in np.where(mask)[0]:
                        results.append({"memory_id": ids[idx], "similarity": round(float(scores[idx]), 4), "source": "hot"})
                else:
                    for mid, sim in zip(ids, scores):
                        if sim >= min_similarity:
                            results.append({"memory_id": mid, "similarity": round(sim, 4), "source": "hot"})

        # --- Cold DB search (optional) ---
        if include_cold:
            hot_id_set = {r["memory_id"] for r in results}

            # HNSW fast path for cold
            cold_hnsw = self._get_cold_hnsw_index()
            if cold_hnsw is not None:
                q_np = np.asarray(query_vec, dtype=np.float32)
                cold_index, cold_hnsw_ids = cold_hnsw
                cold_hits = self._hnsw_search(q_np, cold_index, cold_hnsw_ids, limit, min_similarity)
                for hit in cold_hits:
                    if hit["memory_id"] not in hot_id_set:
                        hit["source"] = "cold"
                        results.append(hit)
            else:
                # Brute-force fallback for cold
                cold_ids, cold_vectors = self._load_cold_vec_cache()
                if cold_ids:
                    q_np = np.asarray(query_vec, dtype=np.float32) if not isinstance(query_vec, np.ndarray) else query_vec
                    cold_scores = _batch_cosine_similarity(q_np, cold_vectors)
                    if isinstance(cold_scores, np.ndarray):
                        mask = cold_scores >= min_similarity
                        for idx in np.where(mask)[0]:
                            mid = cold_ids[idx]
                            if mid not in hot_id_set:
                                results.append({"memory_id": mid, "similarity": round(float(cold_scores[idx]), 4), "source": "cold"})
                    else:
                        for mid, sim in zip(cold_ids, cold_scores):
                            if sim >= min_similarity and mid not in hot_id_set:
                                results.append({"memory_id": mid, "similarity": round(sim, 4), "source": "cold"})

        # Sort by similarity descending
        results.sort(key=lambda r: r["similarity"], reverse=True)
        return results[:limit]

    def find_similar_pairs(
        self,
        min_similarity: float = 0.50,
        max_pairs: int = 200,
    ) -> list[dict[str, Any]]:
        """Find all memory pairs above a cosine similarity threshold.

        Used by semantic association mining (Leap 1a) to replace keyword
        Jaccard with true semantic similarity.

        Returns list of {source_id, target_id, similarity} sorted descending.
        """
        ids, vectors = self._load_vec_cache()
        if len(ids) < 2:
            return []

        pairs: list[dict[str, Any]] = []
        n = len(ids)

        # Batch approach: for each vector, compute cosine against all subsequent
        # With numpy arrays, vectors[i] is a view and vectors[i+1:] is a slice
        # — no copying, just pointer arithmetic on the contiguous buffer
        for i in range(n):
            if len(pairs) >= max_pairs * 3:  # collect extra, trim later
                break
            remaining = vectors[i + 1:]
            if len(remaining) == 0:
                break
            scores = _batch_cosine_similarity(vectors[i], remaining)
            if isinstance(scores, np.ndarray):
                mask = scores >= min_similarity
                for j_offset in np.where(mask)[0]:
                    pairs.append({
                        "source_id": ids[i],
                        "target_id": ids[i + 1 + int(j_offset)],
                        "similarity": round(float(scores[j_offset]), 4),
                    })
            else:
                for j_offset, sim in enumerate(scores):
                    if sim >= min_similarity:
                        pairs.append({
                            "source_id": ids[i],
                            "target_id": ids[i + 1 + j_offset],
                            "similarity": round(sim, 4),
                        })

        pairs.sort(key=lambda p: p["similarity"], reverse=True)
        return pairs[:max_pairs]

    def find_duplicates(
        self,
        threshold: float = 0.95,
        max_results: int = 100,
    ) -> list[dict[str, Any]]:
        """Find near-duplicate memory pairs via embedding cosine similarity.

        Used by Leap 1b for embedding-powered deduplication.
        Threshold ≥0.95 catches semantic duplicates (same meaning, different wording).

        Returns list of {source_id, target_id, similarity} sorted descending.
        """
        return self.find_similar_pairs(
            min_similarity=threshold,
            max_pairs=max_results,
        )

    def embedding_stats(self) -> dict[str, Any]:
        """Get stats about the embedding cache (hot + cold)."""
        db = self._get_db()
        if db is None:
            return {"status": "unavailable"}

        try:
            hot_total = db.execute("SELECT COUNT(*) FROM memory_embeddings").fetchone()[0]
            cold_total = 0
            cold_db = self._get_cold_db()
            if cold_db:
                try:
                    cold_total = cold_db.execute("SELECT COUNT(*) FROM memory_embeddings").fetchone()[0]
                except Exception:
                    pass
            hnsw_status = "active" if self._hnsw_index is not None else (
                "available" if self._hnsw_is_available() else "not_installed"
            )
            return {
                "status": "ok",
                "hot_embeddings": hot_total,
                "cold_embeddings": cold_total,
                "total_embeddings": hot_total + cold_total,
                "model": MODEL_NAME,
                "dims": EMBEDDING_DIM,
                "engine_available": self.available(),
                "hnsw_index": hnsw_status,
                "hnsw_hot_count": self._hnsw_count,
                "hnsw_cold_count": self._cold_hnsw_count,
            }
        except Exception as e:
            return {"status": "error", "error": str(e)}


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_engine_instance: EmbeddingEngine | None = None
_engine_lock = threading.Lock()


def get_embedding_engine() -> EmbeddingEngine:
    """Get or create the global EmbeddingEngine singleton."""
    global _engine_instance
    with _engine_lock:
        if _engine_instance is None:
            _engine_instance = EmbeddingEngine()
        return _engine_instance
