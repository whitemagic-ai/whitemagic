"""
Tests for Leap 1 — Semantic Memory Revolution (v13.5)

Tests cover:
  1a. Embedding-based association re-mining (mine_semantic)
  1b. Embedding-powered deduplication (find_duplicates)
  1c. Hybrid retrieval pipeline (search_hybrid with RRF)
"""

import unittest
from unittest.mock import MagicMock, patch


# ---------------------------------------------------------------------------
# 1a: Embedding-based association re-mining
# ---------------------------------------------------------------------------

class TestSemanticMining(unittest.TestCase):
    """Test the mine_semantic() method on AssociationMiner."""

    def test_mine_semantic_falls_back_when_no_embeddings(self):
        """When embeddings are unavailable, mine_semantic falls back to keyword mine()."""
        from whitemagic.core.memory.association_miner import AssociationMiner

        miner = AssociationMiner(persist=False)

        with patch(
            "whitemagic.core.memory.embeddings.get_embedding_engine"
        ) as mock_get:
            engine = MagicMock()
            engine.available.return_value = False
            mock_get.return_value = engine

            # Patch mine() to avoid needing a real DB
            with patch.object(miner, "mine") as mock_mine:
                mock_mine.return_value = MagicMock(links_proposed=0)
                miner.mine_semantic(persist=False)
                mock_mine.assert_called_once()

    def test_mine_semantic_returns_report_on_no_pairs(self):
        """When no pairs above threshold, returns empty report."""
        from whitemagic.core.memory.association_miner import AssociationMiner

        miner = AssociationMiner(persist=False)

        with patch(
            "whitemagic.core.memory.embeddings.get_embedding_engine"
        ) as mock_get:
            engine = MagicMock()
            engine.available.return_value = True
            engine.find_similar_pairs.return_value = []
            mock_get.return_value = engine

            result = miner.mine_semantic(persist=False)
            self.assertEqual(result.links_proposed, 0)
            self.assertEqual(result.pairs_evaluated, 0)

    def test_mine_semantic_produces_proposals(self):
        """When pairs are found, proposals are generated correctly."""
        from whitemagic.core.memory.association_miner import AssociationMiner

        miner = AssociationMiner(persist=False)

        pairs = [
            {"source_id": "mem_a", "target_id": "mem_b", "similarity": 0.85},
            {"source_id": "mem_c", "target_id": "mem_d", "similarity": 0.55},
        ]

        with patch(
            "whitemagic.core.memory.embeddings.get_embedding_engine"
        ) as mock_get:
            engine = MagicMock()
            engine.available.return_value = True
            engine.find_similar_pairs.return_value = pairs
            mock_get.return_value = engine

            # Mock unified memory to avoid DB access
            with patch(
                "whitemagic.core.memory.unified.get_unified_memory"
            ) as mock_um:
                um = MagicMock()
                conn = MagicMock()
                conn.execute.return_value.fetchall.return_value = []
                um.backend.pool.connection.return_value.__enter__ = MagicMock(return_value=conn)
                um.backend.pool.connection.return_value.__exit__ = MagicMock(return_value=False)
                mock_um.return_value = um

                result = miner.mine_semantic(persist=False)
                self.assertEqual(result.links_proposed, 2)
                self.assertEqual(result.pairs_evaluated, 2)

                # Check strong vs weak labeling
                strong = [p for p in result.top_proposals if "strong" in p.reason]
                weak = [p for p in result.top_proposals if "weak" in p.reason]
                self.assertEqual(len(strong), 1)  # 0.85 >= 0.70
                self.assertEqual(len(weak), 1)    # 0.55 < 0.70

    def test_mine_semantic_filters_existing_associations(self):
        """Existing associations are skipped."""
        from whitemagic.core.memory.association_miner import AssociationMiner

        miner = AssociationMiner(persist=False)

        pairs = [
            {"source_id": "mem_a", "target_id": "mem_b", "similarity": 0.90},
        ]

        with patch(
            "whitemagic.core.memory.embeddings.get_embedding_engine"
        ) as mock_get:
            engine = MagicMock()
            engine.available.return_value = True
            engine.find_similar_pairs.return_value = pairs
            mock_get.return_value = engine

            with patch(
                "whitemagic.core.memory.unified.get_unified_memory"
            ) as mock_um:
                um = MagicMock()
                conn = MagicMock()
                # Return existing association
                conn.execute.return_value.fetchall.return_value = [("mem_a", "mem_b")]
                um.backend.pool.connection.return_value.__enter__ = MagicMock(return_value=conn)
                um.backend.pool.connection.return_value.__exit__ = MagicMock(return_value=False)
                mock_um.return_value = um

                result = miner.mine_semantic(persist=False)
                self.assertEqual(result.links_proposed, 0)

    def test_mine_semantic_report_to_dict(self):
        """MiningReport.to_dict() works for semantic results."""
        from whitemagic.core.memory.association_miner import MiningReport, ProposedLink

        report = MiningReport(
            memories_sampled=100,
            pairs_evaluated=50,
            links_proposed=3,
            links_created=2,
        )
        report.top_proposals = [
            ProposedLink(
                source_id="a", target_id="b", overlap_score=0.85,
                shared_keywords=set(), reason="Semantic similarity (strong, cosine=0.850)",
            )
        ]
        d = report.to_dict()
        self.assertEqual(d["links_proposed"], 3)
        self.assertEqual(len(d["top_proposals"]), 1)
        self.assertIn("Semantic similarity", d["top_proposals"][0]["reason"])


# ---------------------------------------------------------------------------
# 1b: Embedding-powered deduplication
# ---------------------------------------------------------------------------

class TestEmbeddingDeduplication(unittest.TestCase):
    """Test find_duplicates() and find_similar_pairs() on EmbeddingEngine."""

    def _make_engine_with_vectors(self, ids, vectors):
        """Create an EmbeddingEngine with pre-loaded vector cache."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        engine = EmbeddingEngine()
        engine._vec_cache_ids = ids
        engine._vec_cache_vecs = vectors
        engine._vec_cache_count = len(ids)
        # Bypass DB by making _load_vec_cache return our cached data directly
        engine._get_db = MagicMock(return_value=MagicMock())
        def patched_load():
            return ids, vectors
        engine._load_vec_cache = patched_load
        return engine

    def test_find_similar_pairs_basic(self):
        """Identical vectors produce similarity 1.0."""
        vec = [1.0, 0.0, 0.0, 0.0]
        engine = self._make_engine_with_vectors(
            ["a", "b"], [vec, vec]
        )
        pairs = engine.find_similar_pairs(min_similarity=0.9)
        self.assertEqual(len(pairs), 1)
        self.assertAlmostEqual(pairs[0]["similarity"], 1.0, places=3)

    def test_find_similar_pairs_orthogonal(self):
        """Orthogonal vectors produce similarity 0.0, below threshold."""
        engine = self._make_engine_with_vectors(
            ["a", "b"],
            [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
        )
        pairs = engine.find_similar_pairs(min_similarity=0.5)
        self.assertEqual(len(pairs), 0)

    def test_find_similar_pairs_respects_max(self):
        """max_pairs limits output."""
        vec = [1.0, 0.5, 0.3]
        engine = self._make_engine_with_vectors(
            ["a", "b", "c"],
            [vec, vec, vec],
        )
        pairs = engine.find_similar_pairs(min_similarity=0.5, max_pairs=1)
        self.assertEqual(len(pairs), 1)

    def test_find_duplicates_uses_high_threshold(self):
        """find_duplicates() defaults to 0.95 threshold."""
        vec = [1.0, 0.0, 0.0]
        # Very similar vector — cosine ≈ 0.995
        slightly_different = [0.99, 0.1, 0.0]
        engine = self._make_engine_with_vectors(
            ["a", "b"],
            [vec, slightly_different],
        )
        dupes = engine.find_duplicates(threshold=0.95)
        self.assertGreaterEqual(len(dupes), 1)

    def test_find_duplicates_empty_cache(self):
        """Empty cache returns empty list."""
        engine = self._make_engine_with_vectors([], [])
        dupes = engine.find_duplicates()
        self.assertEqual(len(dupes), 0)

    def test_find_similar_pairs_sorted_descending(self):
        """Results are sorted by similarity descending."""
        engine = self._make_engine_with_vectors(
            ["a", "b", "c"],
            [[1.0, 0.0, 0.0], [0.8, 0.6, 0.0], [1.0, 0.01, 0.0]],
        )
        pairs = engine.find_similar_pairs(min_similarity=0.3)
        if len(pairs) > 1:
            for i in range(len(pairs) - 1):
                self.assertGreaterEqual(pairs[i]["similarity"], pairs[i + 1]["similarity"])


# ---------------------------------------------------------------------------
# 1c: Hybrid retrieval pipeline (RRF)
# ---------------------------------------------------------------------------

class TestHybridRetrieval(unittest.TestCase):
    """Test search_hybrid() on UnifiedMemory."""

    def _make_mock_memory(self, mid, title="test", content="test content"):
        """Create a mock Memory object."""
        mem = MagicMock()
        mem.id = mid
        mem.title = title
        mem.content = content
        mem.tags = set()
        mem.importance = 0.5
        mem.metadata = {}
        mem.memory_type = MagicMock()
        mem.memory_type.name = "LONG_TERM"
        return mem

    def test_hybrid_search_lexical_only(self):
        """When embeddings unavailable, falls back to lexical only."""
        from whitemagic.core.memory.unified import UnifiedMemory

        with patch.object(UnifiedMemory, "__init__", lambda self, *a, **kw: None):
            um = UnifiedMemory.__new__(UnifiedMemory)
            um.backend = MagicMock()
            um.holographic = None

            mem1 = self._make_mock_memory("m1")
            mem2 = self._make_mock_memory("m2")
            um.backend.search.return_value = [mem1, mem2]

            with patch(
                "whitemagic.core.memory.embeddings.get_embedding_engine"
            ) as mock_emb:
                engine = MagicMock()
                engine.available.return_value = False
                mock_emb.return_value = engine

                results = um.search_hybrid("test query", limit=5)
                self.assertGreaterEqual(len(results), 1)
                # All should be lexical channel
                for r in results:
                    self.assertEqual(r.metadata.get("retrieval_channels"), "lexical")

    def test_hybrid_search_both_channels(self):
        """When both channels return results, RRF fuses them."""
        from whitemagic.core.memory.unified import UnifiedMemory

        with patch.object(UnifiedMemory, "__init__", lambda self, *a, **kw: None):
            um = UnifiedMemory.__new__(UnifiedMemory)
            um.backend = MagicMock()
            um.holographic = None

            mem1 = self._make_mock_memory("m1")
            mem2 = self._make_mock_memory("m2")
            mem3 = self._make_mock_memory("m3")
            um.backend.search.return_value = [mem1, mem2]
            um.backend.recall.return_value = mem3

            with patch(
                "whitemagic.core.memory.embeddings.get_embedding_engine"
            ) as mock_emb:
                engine = MagicMock()
                engine.available.return_value = True
                # Return m1 (overlap) and m3 (semantic only)
                engine.search_similar.return_value = [
                    {"memory_id": "m1", "similarity": 0.85},
                    {"memory_id": "m3", "similarity": 0.70},
                ]
                mock_emb.return_value = engine

                results = um.search_hybrid("test query", limit=5)
                self.assertGreaterEqual(len(results), 1)

                # m1 should be in both channels
                m1_result = next((r for r in results if r.id == "m1"), None)
                if m1_result:
                    self.assertEqual(
                        m1_result.metadata.get("retrieval_channels"),
                        "lexical+semantic",
                    )

    def test_hybrid_search_rrf_scores_present(self):
        """All results have rrf_score in metadata."""
        from whitemagic.core.memory.unified import UnifiedMemory

        with patch.object(UnifiedMemory, "__init__", lambda self, *a, **kw: None):
            um = UnifiedMemory.__new__(UnifiedMemory)
            um.backend = MagicMock()
            um.holographic = None

            mem1 = self._make_mock_memory("m1")
            um.backend.search.return_value = [mem1]

            with patch(
                "whitemagic.core.memory.embeddings.get_embedding_engine"
            ) as mock_emb:
                engine = MagicMock()
                engine.available.return_value = False
                mock_emb.return_value = engine

                results = um.search_hybrid("test query", limit=5)
                for r in results:
                    self.assertIn("rrf_score", r.metadata)
                    self.assertGreater(r.metadata["rrf_score"], 0)

    def test_hybrid_search_empty_query_returns_empty(self):
        """Empty results from both channels returns empty list."""
        from whitemagic.core.memory.unified import UnifiedMemory

        with patch.object(UnifiedMemory, "__init__", lambda self, *a, **kw: None):
            um = UnifiedMemory.__new__(UnifiedMemory)
            um.backend = MagicMock()
            um.holographic = None

            um.backend.search.return_value = []

            with patch(
                "whitemagic.core.memory.embeddings.get_embedding_engine"
            ) as mock_emb:
                engine = MagicMock()
                engine.available.return_value = True
                engine.search_similar.return_value = []
                mock_emb.return_value = engine

                results = um.search_hybrid("nonexistent", limit=5)
                self.assertEqual(len(results), 0)

    def test_hybrid_search_respects_limit(self):
        """Limit parameter caps results."""
        from whitemagic.core.memory.unified import UnifiedMemory

        with patch.object(UnifiedMemory, "__init__", lambda self, *a, **kw: None):
            um = UnifiedMemory.__new__(UnifiedMemory)
            um.backend = MagicMock()
            um.holographic = None

            mems = [self._make_mock_memory(f"m{i}") for i in range(20)]
            um.backend.search.return_value = mems

            with patch(
                "whitemagic.core.memory.embeddings.get_embedding_engine"
            ) as mock_emb:
                engine = MagicMock()
                engine.available.return_value = False
                mock_emb.return_value = engine

                results = um.search_hybrid("test", limit=3)
                self.assertLessEqual(len(results), 3)


# ---------------------------------------------------------------------------
# Cosine similarity helpers
# ---------------------------------------------------------------------------

class TestCosineHelpers(unittest.TestCase):
    """Test the cosine similarity helper functions."""

    def test_cosine_identical(self):
        from whitemagic.core.memory.embeddings import _cosine_similarity
        v = [1.0, 2.0, 3.0]
        self.assertAlmostEqual(_cosine_similarity(v, v), 1.0, places=5)

    def test_cosine_orthogonal(self):
        from whitemagic.core.memory.embeddings import _cosine_similarity
        a = [1.0, 0.0, 0.0]
        b = [0.0, 1.0, 0.0]
        self.assertAlmostEqual(_cosine_similarity(a, b), 0.0, places=5)

    def test_cosine_opposite(self):
        from whitemagic.core.memory.embeddings import _cosine_similarity
        a = [1.0, 0.0]
        b = [-1.0, 0.0]
        self.assertAlmostEqual(_cosine_similarity(a, b), -1.0, places=5)

    def test_batch_cosine(self):
        from whitemagic.core.memory.embeddings import _batch_cosine_similarity
        query = [1.0, 0.0, 0.0]
        vectors = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.5, 0.5, 0.0]]
        scores = _batch_cosine_similarity(query, vectors)
        self.assertEqual(len(scores), 3)
        self.assertAlmostEqual(scores[0], 1.0, places=3)
        self.assertAlmostEqual(scores[1], 0.0, places=3)

    def test_cosine_zero_vector(self):
        from whitemagic.core.memory.embeddings import _cosine_similarity
        a = [0.0, 0.0, 0.0]
        b = [1.0, 0.0, 0.0]
        self.assertAlmostEqual(_cosine_similarity(a, b), 0.0, places=5)


if __name__ == "__main__":
    unittest.main()
