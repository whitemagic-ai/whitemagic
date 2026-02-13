"""🧠 Unified Memory System - WhiteMagic v12.3
Consolidates all memory implementations into one coherent system using SQLite backend.
"""

import hashlib
import logging
import os
from datetime import datetime
from pathlib import Path
from typing import Any, cast

from whitemagic.config.paths import DB_PATH, MEMORY_DIR
from whitemagic.core.memory.sqlite_backend import SQLiteBackend

# Re-export Memory and MemoryType for compatibility
from whitemagic.core.memory.unified_types import Memory, MemoryType
from whitemagic.utils.rust_helper import get_rust_module, is_rust_available
from whitemagic.utils.time import now_iso

logger = logging.getLogger(__name__)

try:
    from whitemagic.core.memory.holographic import get_holographic_memory
    _HOLO_AVAILABLE = True
except ImportError:
    _HOLO_AVAILABLE = False

try:
    from whitemagic.core.resonance.gan_ying_enhanced import EventType
    _GAN_YING_AVAILABLE = True
except ImportError:
    _GAN_YING_AVAILABLE = False

# Note: RefiningFire is imported inside prune() to avoid circular dependency

class UnifiedMemory:
    """Unified Memory System - The central memory hub.

    Consolidates storage into a SQLite backend while maintaining
    integration with the Holographic spatial index.
    """

    def __init__(self, base_path: Path | None = None) -> None:
        # Use canonical Data Sea database location
        if base_path is None:
            self.db_path = DB_PATH
            self.base_path = MEMORY_DIR
            self.base_path.mkdir(parents=True, exist_ok=True)
        else:
            self.base_path = base_path
            self.base_path.mkdir(parents=True, exist_ok=True)
            self.db_path = self.base_path / "whitemagic.db"

        # Initialize SQLite Backend
        self.backend = SQLiteBackend(self.db_path)

        # Holographic Memory
        _skip_holo = os.getenv("WM_SKIP_HOLO_INDEX", "").strip()
        self.holographic = get_holographic_memory() if (_HOLO_AVAILABLE and not _skip_holo) else None

        # Load Holographic Index from DB
        count = 0
        if self.holographic:
            try:
                coords_map = self.backend.get_all_coords()
                for mem_id, coords in coords_map.items():
                    x, y, z, w = coords[0], coords[1], coords[2], coords[3]
                    v = coords[4] if len(coords) > 4 else 0.5
                    if self.holographic.add_memory_with_coords(mem_id, x, y, z, w, v):
                        count += 1
            except Exception as e:
                logger.info(f"⚠️  Failed to load holographic index: {e}")
                count = 0

        if not os.getenv("WM_SILENT_INIT"):
            stats = self.backend.get_stats()
            logger.info(f"🧠 Unified Memory initialized: {stats['total_memories']} memories (SQLite)")
            if self.holographic and count > 0:
                logger.info(f"🌌 Holographic Index loaded: {count} points")

    def store(self, content: Any, memory_type: MemoryType = MemoryType.SHORT_TERM,
              tags: set[str] | None = None, emotional_valence: float = 0.0,
              importance: float = 0.5, metadata: dict | None = None, title: str | None = None, **kwargs: Any) -> Memory:
        """Store a new memory.

        v14.0: Surprise-gated ingestion evaluates novelty before storage.
        High surprise → boosted importance. Low surprise → reinforce existing.
        """
        metadata = metadata or {}

        # v14.1.1: Content hash dedup — check for exact duplicates before anything else
        content_hash = hashlib.sha256(str(content).encode()).hexdigest()
        try:
            existing_id = self.backend.find_by_content_hash(content_hash)
            if existing_id:
                existing = self.backend.recall(existing_id)
                if existing:
                    existing.access_count += 1
                    existing.accessed_at = datetime.now()
                    existing.metadata["last_reinforced"] = now_iso()
                    existing.metadata["reinforcement_count"] = existing.metadata.get("reinforcement_count", 0) + 1
                    self.backend.store(existing, content_hash=content_hash)
                    logger.debug(f"Content hash dedup: reinforced {existing_id} instead of creating duplicate")
                    return existing
        except Exception:
            pass  # Dedup check failed — proceed normally

        # v14.0: Surprise-gated ingestion
        surprise_verdict = None
        try:
            from whitemagic.core.memory.surprise_gate import get_surprise_gate, SurpriseAction
            gate = get_surprise_gate()
            content_str_for_eval = str(content)[:2000]
            surprise_verdict = gate.evaluate(content_str_for_eval)

            if surprise_verdict.action == SurpriseAction.REINFORCE:
                # Redundant content: reinforce nearest neighbor instead
                target_id = surprise_verdict.nearest_memory_id
                if target_id:
                    try:
                        existing = self.backend.recall(target_id)
                        if existing:
                            existing.access_count += 1
                            existing.importance = min(1.0, existing.importance + 0.03)
                            existing.metadata["last_reinforced"] = now_iso()
                            existing.metadata["reinforcement_count"] = existing.metadata.get("reinforcement_count", 0) + 1
                            self.backend.store(existing)
                            logger.debug(f"Surprise gate: reinforced {target_id} instead of creating duplicate")
                            return existing
                    except Exception:
                        pass  # Fall through to normal create

            elif surprise_verdict.action == SurpriseAction.CREATE_BOOSTED:
                importance = min(1.0, importance + 0.15)
                metadata["surprise_boosted"] = True
                metadata["surprise_score"] = round(surprise_verdict.surprise_score, 3)

        except Exception:
            pass  # Surprise gate unavailable — proceed normally

        # Generate ID
        content_str = str(content)[:1000]
        timestamp = now_iso()
        memory_id = hashlib.sha256(f"{content_str}{timestamp}".encode()).hexdigest()[:16]

        memory = Memory(
            id=memory_id,
            content=content,
            memory_type=memory_type,
            tags=tags or set(),
            emotional_valence=emotional_valence,
            importance=importance,
            metadata=metadata,
            title=title,
            **kwargs,
        )

        # Store in SQLite (with content hash for dedup)
        self.backend.store(memory, content_hash=content_hash)

        # Index in Holographic Memory (5D Spatial: x, y, z, w, v)
        if self.holographic:
            coords = self.holographic.index_memory(memory.id, memory.to_dict())
            if coords:
                self.backend.store_coords(memory.id, *coords)

        # Auto-index embedding if sentence-transformers is available
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            if engine and engine.model_available:
                engine.index_single(memory.id, memory.content)
        except Exception:
            pass  # Embedding unavailable — skip silently

        # v15.2: Auto-extract entities and relations into knowledge graph
        try:
            from whitemagic.core.intelligence.entity_extractor import get_entity_extractor
            extractor = get_entity_extractor()
            content_for_extraction = str(content)[:4000]
            if title:
                content_for_extraction = f"{title}\n{content_for_extraction}"
            extractor.extract_and_store(memory.id, content_for_extraction)
        except Exception:
            pass  # Entity extraction unavailable — skip silently

        return memory

    def recall(self, memory_id: str) -> Memory | None:
        """Recall a specific memory by ID. Promotes it inward on the Galactic Map."""
        memory = self.backend.recall(memory_id)
        if memory and memory.galactic_distance > 0.0:
            # Spiral inward: reduce galactic distance by 5% on each recall
            # A memory at the far edge (0.95) would need ~60 recalls to reach core
            new_distance = max(0.0, memory.galactic_distance * 0.95)
            if new_distance != memory.galactic_distance:
                memory.galactic_distance = new_distance
                try:
                    self.backend.update_galactic_distance(memory_id, new_distance)
                except Exception:
                    pass  # Don't fail recall if distance update fails
        return memory

    def search(self, query: str | None = None, tags: set[str] | None = None,
               memory_type: MemoryType | None = None, min_importance: float = 0.0,
               limit: int = 10) -> list[Memory]:
        """Search memories with various filters."""
        results = self.backend.search(query=query, tags=tags, memory_type=memory_type,
                                 min_importance=min_importance, limit=limit)

        # Annotate results with constellation context (B2 v13.3.2)
        if results:
            try:
                from whitemagic.core.memory.constellations import (
                    get_constellation_detector,
                )
                detector = get_constellation_detector()
                detector.annotate_memories(results, backend=self.backend)
            except Exception:
                pass

        # Emit search event for Gan Ying integration
        if _GAN_YING_AVAILABLE:
            try:
                from whitemagic.core.resonance.gan_ying import emit_event
                emit_event(
                    source="memory_manager",
                    event_type=EventType.SEARCH_COMPLETED,
                    data={
                        "query": query,
                        "memory_type": memory_type.value if memory_type else None,
                        "tags": list(tags) if tags else [],
                        "result_count": len(results),
                        "min_importance": min_importance,
                        "timestamp": datetime.now().isoformat(),
                    },
                )
            except Exception:
                pass  # Don't fail search if event emission fails

        return cast(list[Memory], results)

    def search_similar(self, query: str, memory_type: MemoryType | None = None,
                       threshold: float = 0.1, limit: int = 10) -> list[Memory]:
        """Search memories by similarity using Rust acceleration if available.
        """
        if is_rust_available():
            rs = get_rust_module()
            # Try to use the batch search function if available
            if rs and hasattr(rs, "rust_search_memories"):
                try:
                    # 1. Get candidate memories from backend
                    candidates = self.backend.search(query=None, memory_type=memory_type, limit=limit * 10)  # type: ignore[no-redef]
                    if candidates:
                        # 2. Prepare data for Rust: list of (id, content)
                        mem_tuples = [(c.id, str(c.content)) for c in candidates]

                        # 3. Use Rust SIMD for high-speed parallel search
                        # Returns list of (id, score)
                        rust_results = rs.rust_search_memories(query, mem_tuples, threshold, limit)

                        # 4. Map back to Memory objects
                        results = []
                        # Create a lookup map for candidates
                        candidate_map = {c.id: c for c in candidates}

                        for mem_id, score in rust_results:
                            if mem_id in candidate_map:
                                mem = candidate_map[mem_id]
                                mem.metadata["similarity_score"] = float(score)
                                results.append(mem)

                        # Emit specific pattern detection event
                        if _GAN_YING_AVAILABLE:
                            try:
                                from whitemagic.core.resonance.gan_ying import (
                                    emit_event,
                                )
                                emit_event(
                                    source="memory_manager",
                                    event_type=EventType.PATTERN_DETECTED,
                                    data={
                                        "type": "rust_simd_batch_search",
                                        "query": query,
                                        "result_count": len(results),
                                        "top_score": results[0].metadata["similarity_score"] if results else 0.0,
                                        "timestamp": datetime.now().isoformat(),
                                    },
                                )
                            except Exception:
                                pass

                        return results
                except Exception as e:
                    logger.info(f"⚠️ Rust batch search failed: {e}, falling back to FTS")

            # Fallback to single-pair similarity if batch is not available but fast_similarity is
            elif rs and hasattr(rs, "fast_similarity"):
                try:
                    candidates = self.backend.search(query=None, memory_type=memory_type, limit=limit * 5)
                    scored_results = []
                    for cand in candidates:
                        score = rs.fast_similarity(query, str(cand.content))
                        if score >= threshold:
                            cand.metadata["similarity_score"] = float(score)
                            scored_results.append(cand)

                    scored_results.sort(key=lambda x: x.metadata.get("similarity_score", 0.0), reverse=True)
                    results = scored_results[:limit]
                    return results
                except Exception as e:
                    logger.info(f"⚠️ Rust fast_similarity failed: {e}, falling back to FTS")

        # Fallback to standard FTS search with optional Rust pipeline re-ranking
        results = self.search(query=query, memory_type=memory_type, limit=limit * 3)

        # Try Rust multi-pass retrieval pipeline for re-ranking (v13.3.2)
        if results and len(results) > 1:
            try:
                from whitemagic.optimization.rust_accelerators import retrieval_pipeline
                candidates: list[dict[str, Any]] = []  # type: ignore[no-redef]
                for mem in results:
                    coords = None
                    try:
                        c = self.backend.get_coords(mem.id)
                        if c:
                            coords = list(c)
                    except Exception:
                        pass
                    age_days = 0.0
                    if mem.created_at:
                        try:
                            age_days = max(0.0, (datetime.now() - mem.created_at).total_seconds() / 86400.0)
                        except Exception:
                            pass
                    candidates.append({  # type: ignore[arg-type]
                        "id": mem.id,
                        "score": mem.metadata.get("similarity_score", 0.5),
                        "importance": mem.importance,
                        "memory_type": mem.memory_type.name if mem.memory_type else "",
                        "tags": list(mem.tags) if mem.tags else [],
                        "age_days": age_days,
                        "coords": coords,
                    })

                pipeline_result = retrieval_pipeline(candidates, {  # type: ignore[arg-type]
                    "query": query,
                    "limit": limit,
                    "enable_importance_rerank": True,
                    "importance_weight": 0.3,
                    "recency_weight": 0.1,
                    "enable_holographic_boost": bool(candidates[0].get("coords")),  # type: ignore[union-attr,attr-defined]
                    "enable_dedup": True,
                    "dedup_threshold": 0.85,
                })

                if pipeline_result:
                    # Re-order results by pipeline ranking
                    mem_map = {m.id: m for m in results}
                    reranked = []
                    for item in pipeline_result:
                        mid = item.get("id", "")
                        if mid in mem_map:
                            mem = mem_map[mid]
                            mem.metadata["pipeline_score"] = item.get("score", 0.0)
                            reranked.append(mem)
                    if reranked:
                        return reranked[:limit]
            except Exception:
                pass

        return results[:limit]

    def search_hybrid(
        self,
        query: str,
        limit: int = 10,
        memory_type: MemoryType | None = None,
        rrf_k: int = 60,
        semantic_weight: float = 1.0,
        lexical_weight: float = 1.0,
        include_cold: bool = False,
    ) -> list[Memory]:
        """Hybrid retrieval combining BM25 lexical search + embedding semantic
        search via Reciprocal Rank Fusion (RRF).

        RRF score = Σ weight_i / (k + rank_i)

        This is the modern retrieval standard used by Pinecone, Weaviate, etc.
        Falls back gracefully: embedding-only if no BM25, FTS-only if no embeddings.

        Args:
            query: Search query text.
            limit: Maximum results to return.
            memory_type: Optional filter by memory type.
            rrf_k: RRF constant (default 60, standard value).
            semantic_weight: Weight for semantic (embedding) rankings.
            lexical_weight: Weight for lexical (BM25/FTS) rankings.
            include_cold: If True, also search cold DB embeddings (v13.6).

        """
        from collections import defaultdict

        rrf_scores: dict[str, float] = defaultdict(float)
        all_memories: dict[str, "Memory"] = {}

        # --- Channel 1: Lexical (BM25 via Rust, FTS fallback) ---
        lexical_results = []
        try:
            from whitemagic.optimization.rust_accelerators import (
                rust_search_available,
                search_build_index,
                search_query,
            )
            if rust_search_available():
                # Build index from memories if needed
                candidates = self.backend.search(
                    query=None, memory_type=memory_type, limit=limit * 10,
                )
                if candidates:
                    docs = [
                        {"id": m.id, "title": m.title or "", "content": str(m.content)[:2000]}
                        for m in candidates
                    ]
                    search_build_index(docs)
                    bm25_hits = search_query(query, limit=limit * 3)
                    if bm25_hits:
                        mem_map = {m.id: m for m in candidates}
                        for hit in bm25_hits:
                            mid = hit.get("id", "")
                            if mid in mem_map:
                                lexical_results.append(mem_map[mid])
                                all_memories[mid] = mem_map[mid]
        except Exception:
            pass

        # Fallback: standard FTS search
        if not lexical_results:
            lexical_results = self.backend.search(
                query=query, memory_type=memory_type, limit=limit * 3,
            )
            for m in lexical_results:
                all_memories[m.id] = m

        # Assign RRF scores for lexical channel
        for rank, mem in enumerate(lexical_results):
            rrf_scores[mem.id] += lexical_weight / (rrf_k + rank + 1)

        # --- Channel 2: Semantic (Embedding cosine, hot + optional cold) ---
        semantic_results = []
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            if engine.available():
                hits = engine.search_similar(
                    query, limit=limit * 3, min_similarity=0.25,
                    include_cold=include_cold,
                )
                for hit in hits:
                    mid = hit["memory_id"]
                    if mid not in all_memories:
                        # Hydrate memory from backend (hot-first, cold-fallback)
                        recalled = self.backend.recall(mid)
                        if recalled:
                            if hit.get("source") == "cold":
                                recalled.metadata["storage_tier"] = "cold"
                            all_memories[mid] = recalled
                    semantic_results.append(hit)
        except Exception:
            pass

        # Assign RRF scores for semantic channel
        for rank, hit in enumerate(semantic_results):
            mid = hit["memory_id"]
            rrf_scores[mid] += semantic_weight / (rrf_k + rank + 1)

        # --- Fuse and rank ---
        if not rrf_scores:
            return []

        # --- Channel 3: Constellation Boost (v14.3) ---
        # Find query's closest constellation, then boost results from same cluster
        query_constellation_name: str | None = None
        constellation_boost = 0.3
        diversity_bonus = 0.05
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            if engine.available():
                closest = engine.closest_constellation(query, max_results=1)
                if closest and closest[0]["similarity"] >= 0.25:
                    query_constellation_name = closest[0]["name"]
        except Exception:
            pass

        if query_constellation_name:
            for mid in rrf_scores:
                try:
                    membership = self.backend.get_constellation_membership(mid)
                    if membership:
                        confidence = membership.get("membership_confidence", 0.5)
                        if membership["constellation_name"] == query_constellation_name:
                            # Same constellation as query → multiplicative boost
                            rrf_scores[mid] *= (1.0 + constellation_boost * confidence)
                        else:
                            # Different constellation → small diversity bonus
                            rrf_scores[mid] += diversity_bonus * (1.0 - confidence)
                except Exception:
                    pass

        ranked_ids = sorted(rrf_scores.keys(), key=lambda mid: rrf_scores[mid], reverse=True)
        results = []
        for mid in ranked_ids[:limit]:
            mem = all_memories.get(mid)  # type: ignore[assignment]
            if mem:
                mem.metadata["rrf_score"] = round(rrf_scores[mid], 6)
                # Tag which channels contributed
                in_lexical = any(m.id == mid for m in lexical_results)
                in_semantic = any(h["memory_id"] == mid for h in semantic_results)
                if in_lexical and in_semantic:
                    mem.metadata["retrieval_channels"] = "lexical+semantic"
                elif in_semantic:
                    mem.metadata["retrieval_channels"] = "semantic"
                else:
                    mem.metadata["retrieval_channels"] = "lexical"
                if query_constellation_name:
                    mem.metadata["query_constellation"] = query_constellation_name
                results.append(mem)

        # Constellation annotation
        if results:
            try:
                from whitemagic.core.memory.constellations import (
                    get_constellation_detector,
                )
                detector = get_constellation_detector()
                detector.annotate_memories(results, backend=self.backend)
            except Exception:
                pass

        return results

    def hybrid_recall(
        self,
        query: str,
        hops: int = 2,
        anchor_limit: int = 5,
        final_limit: int = 10,
    ) -> list[dict[str, Any]]:
        """Multi-hop graph-aware recall (v14.0 Living Graph).

        Combines anchor search (BM25 + embedding) with graph walk expansion
        to discover memories connected via the association graph that would
        never appear in a stateless single-hop search.

        Returns list of dicts with memory data + reasoning paths.
        """
        try:
            from whitemagic.core.memory.graph_walker import get_graph_walker
            walker = get_graph_walker()
            return cast(list[dict[str, Any]], walker.hybrid_recall(
                query=query,
                hops=hops,
                anchor_limit=anchor_limit,
                final_limit=final_limit,
            ))
        except Exception as e:
            logger.debug(f"hybrid_recall: graph walker unavailable, falling back to search_hybrid: {e}")
            # Graceful fallback to standard hybrid search
            results = self.search_hybrid(query=query, limit=final_limit)
            return [
                {
                    "memory_id": m.id,
                    "title": m.title,
                    "content": str(m.content)[:500],
                    "importance": m.importance,
                    "source": "fallback_hybrid",
                }
                for m in results
            ]

    def associate(self, memory_id1: str, memory_id2: str, strength: float = 0.5) -> None:
        """Create bidirectional association between memories."""
        m1 = self.recall(memory_id1)
        m2 = self.recall(memory_id2)

        if m1 and m2:
            m1.associate(memory_id2, strength)
            m2.associate(memory_id1, strength)
            self.backend.store(m1)
            self.backend.store(m2)

    def consolidate(self) -> int:
        """Consolidate memories - strengthen important, decay unimportant."""
        count: int = self.backend.consolidate()
        # Automate pruning of weak/archived memories after consolidation
        self.prune()
        return count

    def prune(self, threshold: float = 0.2) -> int:
        """Rotate weak memories to the galactic edge.
        No memory is ever truly forgotten — only archived.

        This implements the 'No-Waste' Tikkun model: extract sparks of
        wisdom, then rotate the memory outward from the galactic core
        toward the far edge of the plane. It remains searchable but
        deprioritized.
        """
        try:
            from whitemagic.core.intelligence.synthesis.refining_fire import (
                get_refining_fire,
            )
            refiner = get_refining_fire()
        except ImportError:
            refiner = None

        # 1. Identify candidates (WEAK) using neuro_score sorting
        candidates = self.backend.get_weakest_memories(limit=100)
        weak_candidates = [m for m in candidates if m.neuro_score <= threshold and not m.is_protected]

        if not weak_candidates:
            return 0

        logger.info(f"🌌 Starting Galactic Rotation for {len(weak_candidates)} memories.")

        rotated_count = 0
        for mem in weak_candidates:
            # 2. Extract Sparks (if refiner available)
            if refiner:
                try:
                    sol_id = refiner.refine_memory(mem)
                    if sol_id:
                        logger.info(f"✨ Memory {mem.id} refined into spark {sol_id}.")
                except Exception:
                    pass

            # 3. Rotate to galactic edge (NEVER delete)
            self.backend.archive_to_edge(mem.id, galactic_distance=0.95)
            rotated_count += 1

        if rotated_count > 0:
            logger.info(f"🌌 Galactic Rotation complete: {rotated_count} memories rotated to edge.")

        return rotated_count

    def arrow_export(
        self,
        memory_type: MemoryType | None = None,
        limit: int = 10000,
    ) -> bytes | None:
        """Export memories as Arrow IPC bytes (v14.7 — 32× faster than JSON).

        Uses Rust Arrow bridge for zero-copy columnar serialization.
        Falls back to None if Arrow/Rust unavailable (caller should use JSON).

        Returns:
            Arrow IPC file bytes, or None if unavailable.
        """
        try:
            from whitemagic.optimization.rust_accelerators import (
                arrow_available,
                arrow_encode_memories,
            )
            if not arrow_available():
                return None

            memories = self.backend.search(
                query=None, memory_type=memory_type, limit=limit,
            )
            if not memories:
                return None

            import json as _json
            docs = []
            for m in memories:
                coords = self.backend.get_coords(m.id) if self.holographic else None
                docs.append({
                    "id": m.id,
                    "title": m.title or "",
                    "content": str(m.content)[:10000],
                    "importance": m.importance,
                    "memory_type": m.memory_type.name if m.memory_type else "SHORT_TERM",
                    "x": coords[0] if coords else 0.0,
                    "y": coords[1] if coords else 0.0,
                    "z": coords[2] if coords else 0.0,
                    "w": coords[3] if coords else 0.5,
                    "v": coords[4] if coords and len(coords) > 4 else 0.5,
                    "tags": sorted(m.tags) if m.tags else [],
                })
            return arrow_encode_memories(_json.dumps(docs))
        except Exception as e:
            logger.debug(f"Arrow export failed: {e}")
            return None

    def arrow_import(self, ipc_bytes: bytes) -> int:
        """Import memories from Arrow IPC bytes (v14.7 — 32× faster than JSON).

        Decodes Arrow IPC → JSON → stores each memory via normal pipeline
        (with dedup, surprise gate, holographic indexing).

        Returns:
            Number of memories imported.
        """
        try:
            from whitemagic.optimization.rust_accelerators import (
                arrow_available,
                arrow_decode_memories,
            )
            if not arrow_available():
                return 0

            json_str = arrow_decode_memories(ipc_bytes)
            if not json_str:
                return 0

            import json as _json
            docs = _json.loads(json_str)
            count = 0
            for doc in docs:
                try:
                    mt = MemoryType[doc.get("memory_type", "SHORT_TERM")]
                except (KeyError, ValueError):
                    mt = MemoryType.SHORT_TERM
                raw_tags = doc.get("tags", [])
                if isinstance(raw_tags, list):
                    tags = set(raw_tags)
                elif isinstance(raw_tags, str) and raw_tags:
                    tags = set(raw_tags.split(","))
                else:
                    tags = set()
                tags.discard("")
                self.store(
                    content=doc.get("content", ""),
                    memory_type=mt,
                    title=doc.get("title"),
                    importance=doc.get("importance", 0.5),
                    tags=tags,
                )
                count += 1
            logger.info(f"Arrow IPC import: {count} memories imported")
            return count
        except Exception as e:
            logger.debug(f"Arrow import failed: {e}")
            return 0

    def save(self, memory_type: MemoryType | None = None) -> None:
        """Save memories to disk - No-op for SQLite (auto-save)."""
        pass

    def get_stats(self) -> dict[str, Any]:
        """Get memory system statistics."""
        return cast(dict[str, Any], self.backend.get_stats())

    def list_recent(self, limit: int = 10, memory_type: MemoryType | None = None) -> list[Memory]:
        """List recent memories."""
        return cast(list[Memory], self.backend.list_recent(limit=limit, memory_type=memory_type))

    def fetch_all_contents(self, memory_type: str | None = None, limit: int = 10000) -> list[str]:
        """Fetch all memory contents efficiently."""
        return cast(list[str], self.backend.fetch_memory_contents(memory_type, limit))

    def list_accessed(self, limit: int = 10) -> list[Memory]:
        """List recently accessed memories."""
        return cast(list[Memory], self.backend.list_accessed(limit=limit))

    def get_tag_counts(self, limit: int = 10) -> list[tuple[str, int]]:
        """Get most common tags."""
        return cast(list[tuple[str, int]], self.backend.get_tag_counts(limit=limit))


# Singleton instance
_unified_memory: UnifiedMemory | None = None

def get_unified_memory() -> UnifiedMemory:
    """Get the singleton unified memory instance."""
    global _unified_memory
    if _unified_memory is None:
        _unified_memory = UnifiedMemory()
    return _unified_memory


# Convenience functions
def remember(content: Any, **kwargs: Any) -> Memory:
    """Quick store function."""
    return get_unified_memory().store(content, **kwargs)

def recall(query: str | None = None, **kwargs: Any) -> list[Memory]:
    """Quick search function."""
    return get_unified_memory().search(query=query, **kwargs)

def consolidate() -> int:
    """Quick consolidate function."""
    return get_unified_memory().consolidate()
