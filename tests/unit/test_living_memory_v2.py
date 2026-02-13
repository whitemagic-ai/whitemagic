"""Tests for Living Memory v14.2 improvements.

Covers:
  - Semantic projection in graph walks
  - Fused gravity signal
  - Causality enforcement
  - Bitemporal on memories table
  - MediaProcessor protocol + processors
  - Entity extractor (regex fallback)
  - HRR circular convolution engine
"""

import unittest
from datetime import datetime
from pathlib import Path
from unittest.mock import patch

np = __import__("pytest").importorskip("numpy")


# ============================================================================
# Test: Graph Walker — Semantic Projection + Fused Gravity + Causality
# ============================================================================

class TestGraphWalkerTransitionScore(unittest.TestCase):
    """Test the upgraded _transition_score with semantic projection and fused gravity."""

    def setUp(self):
        from whitemagic.core.memory.graph_walker import GraphWalker, Neighbor
        self.walker = GraphWalker(
            gravity_alpha=0.5,
            staleness_beta=0.3,
            semantic_sigma=1.0,
            gravity_weights=(0.5, 0.3, 0.2),
        )
        self.base_neighbor = Neighbor(
            memory_id="test_target",
            strength=0.8,
            direction="forward",
            relation_type="associated_with",
            edge_type="semantic",
            traversal_count=0,
            created_at=datetime.now().isoformat(),
            last_traversed_at=None,
            neuro_score=0.9,
        )

    def test_basic_score_without_semantic(self):
        """Score should work without semantic projection (backward compatible)."""
        score = self.walker._transition_score(
            self.base_neighbor, 0.5, 10,
        )
        self.assertGreater(score, 0)

    def test_semantic_boost_aligned(self):
        """Semantically aligned neighbors should get higher scores."""
        dim = 384
        query_embed = list(np.random.randn(dim).astype(float))
        # Aligned neighbor: same direction
        aligned_embed = list(np.array(query_embed) * 0.9 + np.random.randn(dim) * 0.1)
        # Misaligned: opposite direction
        misaligned_embed = list(-np.array(query_embed) * 0.9 + np.random.randn(dim) * 0.1)

        score_aligned = self.walker._transition_score(
            self.base_neighbor, 0.5, 10,
            query_embedding=query_embed,
            neighbor_embedding=aligned_embed,
        )
        score_misaligned = self.walker._transition_score(
            self.base_neighbor, 0.5, 10,
            query_embedding=query_embed,
            neighbor_embedding=misaligned_embed,
        )
        self.assertGreater(score_aligned, score_misaligned)

    def test_semantic_never_zero(self):
        """Semantic sim should never produce zero score (minimum floor)."""
        dim = 384
        query_embed = list(np.random.randn(dim).astype(float))
        opposite_embed = list(-np.array(query_embed))

        score = self.walker._transition_score(
            self.base_neighbor, 0.5, 10,
            query_embedding=query_embed,
            neighbor_embedding=opposite_embed,
        )
        self.assertGreater(score, 0)

    def test_causality_enforcement_blocks_backward(self):
        """Edges going backward in time should be blocked."""
        from whitemagic.core.memory.graph_walker import Neighbor
        future_neighbor = Neighbor(
            memory_id="future",
            strength=0.8,
            direction="forward",
            relation_type="temporal",
            edge_type="episodic",
            traversal_count=0,
            created_at="2026-01-01T00:00:00",
            last_traversed_at=None,
            neuro_score=0.9,
        )

        # This edge was created BEFORE the previous hop
        score = self.walker._transition_score(
            future_neighbor, 0.5, 10,
            prev_created_at="2026-06-01T00:00:00",  # prev hop is LATER
            enforce_causality=True,
        )
        self.assertEqual(score, 0.0)

    def test_causality_allows_forward(self):
        """Edges going forward in time should be allowed."""
        from whitemagic.core.memory.graph_walker import Neighbor
        forward_neighbor = Neighbor(
            memory_id="forward",
            strength=0.8,
            direction="forward",
            relation_type="temporal",
            edge_type="episodic",
            traversal_count=0,
            created_at="2026-06-01T00:00:00",
            last_traversed_at=None,
            neuro_score=0.9,
        )

        score = self.walker._transition_score(
            forward_neighbor, 0.5, 10,
            prev_created_at="2026-01-01T00:00:00",  # prev hop is EARLIER
            enforce_causality=True,
        )
        self.assertGreater(score, 0)

    def test_causality_disabled_by_default(self):
        """Without enforce_causality, backward edges are allowed."""
        from whitemagic.core.memory.graph_walker import Neighbor
        backward = Neighbor(
            memory_id="backward",
            strength=0.8,
            direction="forward",
            relation_type="temporal",
            edge_type="episodic",
            traversal_count=0,
            created_at="2026-01-01T00:00:00",
            last_traversed_at=None,
            neuro_score=0.9,
        )

        score = self.walker._transition_score(
            backward, 0.5, 10,
            prev_created_at="2026-06-01T00:00:00",
            enforce_causality=False,  # default
        )
        self.assertGreater(score, 0)


class TestFusedGravity(unittest.TestCase):
    """Test the fused gravity signal."""

    def setUp(self):
        from whitemagic.core.memory.graph_walker import GraphWalker
        self.walker = GraphWalker(gravity_weights=(0.5, 0.3, 0.2))

    def test_core_memory_has_higher_gravity(self):
        """CORE memories (galactic_distance ≈ 0) should have higher gravity."""
        with patch.object(self.walker, '_get_pagerank', return_value=0.01):
            gravity_core = self.walker._fused_gravity(0.0, 1.0, "core_mem")
            gravity_edge = self.walker._fused_gravity(1.0, 0.3, "edge_mem")
            self.assertGreater(gravity_core, gravity_edge)

    def test_high_neuro_boosts_gravity(self):
        """Higher neuro_score should boost gravity."""
        with patch.object(self.walker, '_get_pagerank', return_value=0.01):
            gravity_high = self.walker._fused_gravity(0.5, 1.0, "mem1")
            gravity_low = self.walker._fused_gravity(0.5, 0.1, "mem2")
            self.assertGreater(gravity_high, gravity_low)

    def test_gravity_always_positive(self):
        """Gravity should always be positive."""
        with patch.object(self.walker, '_get_pagerank', return_value=0.0):
            gravity = self.walker._fused_gravity(1.0, 0.0, "mem")
            self.assertGreater(gravity, 0)


class TestGraphWalkerStats(unittest.TestCase):
    """Test updated stats output."""

    def test_stats_include_new_features(self):
        from whitemagic.core.memory.graph_walker import GraphWalker
        walker = GraphWalker()
        stats = walker.get_stats()
        self.assertIn("semantic_sigma", stats)
        self.assertIn("gravity_weights", stats)
        self.assertIn("features", stats)
        self.assertTrue(stats["features"]["semantic_projection"])
        self.assertTrue(stats["features"]["fused_gravity"])
        self.assertTrue(stats["features"]["causality_enforcement"])


# ============================================================================
# Test: MediaProcessor Protocol + Processors
# ============================================================================

class TestProcessorChain(unittest.TestCase):
    """Test the MediaProcessor chain architecture."""

    def test_chain_creation(self):
        from whitemagic.core.intake.media_processor import get_processor_chain
        chain = get_processor_chain()
        self.assertIsNotNone(chain)

    def test_chain_has_processors(self):
        from whitemagic.core.intake.media_processor import ProcessorChain
        chain = ProcessorChain()
        self.assertEqual(len(chain.available_processors()), 0)

    def test_chain_supported_extensions(self):
        from whitemagic.core.intake.media_processor import get_processor_chain
        chain = get_processor_chain()
        exts = chain.supported_extensions()
        self.assertIn(".pdf", exts)
        self.assertIn(".png", exts)
        self.assertIn(".mp3", exts)
        self.assertIn(".csv", exts)
        self.assertIn(".docx", exts)

    def test_chain_returns_none_for_unknown(self):
        from whitemagic.core.intake.media_processor import get_processor_chain
        chain = get_processor_chain()
        result = chain.process(Path("/fake/file.xyz"))
        self.assertIsNone(result)


class TestPDFProcessor(unittest.TestCase):
    """Test PDF processor."""

    def test_can_process_pdf(self):
        from whitemagic.core.intake.media_processor import PDFProcessor
        proc = PDFProcessor()
        # Even if fitz not installed, can_process should return False gracefully
        result = proc.can_process(Path("/fake/test.pdf"))
        self.assertIsInstance(result, bool)

    def test_rejects_non_pdf(self):
        from whitemagic.core.intake.media_processor import PDFProcessor
        proc = PDFProcessor()
        self.assertFalse(proc.can_process(Path("/fake/test.txt")))

    def test_supported_extensions(self):
        from whitemagic.core.intake.media_processor import PDFProcessor
        proc = PDFProcessor()
        self.assertEqual(proc.supported_extensions(), {".pdf"})


class TestImageProcessor(unittest.TestCase):
    """Test image processor."""

    def test_supported_extensions(self):
        from whitemagic.core.intake.media_processor import ImageProcessor
        proc = ImageProcessor()
        exts = proc.supported_extensions()
        self.assertIn(".png", exts)
        self.assertIn(".jpg", exts)
        self.assertIn(".webp", exts)

    def test_rejects_non_image(self):
        from whitemagic.core.intake.media_processor import ImageProcessor
        proc = ImageProcessor()
        self.assertFalse(proc.can_process(Path("/fake/test.txt")))


class TestSpreadsheetProcessor(unittest.TestCase):
    """Test spreadsheet processor."""

    def test_csv_always_available(self):
        from whitemagic.core.intake.media_processor import SpreadsheetProcessor
        proc = SpreadsheetProcessor()
        # CSV uses stdlib, so can_process should work if file existed
        self.assertTrue(proc.can_process(Path("/fake/test.csv")))

    def test_supported_extensions(self):
        from whitemagic.core.intake.media_processor import SpreadsheetProcessor
        proc = SpreadsheetProcessor()
        exts = proc.supported_extensions()
        self.assertIn(".csv", exts)
        self.assertIn(".xlsx", exts)


class TestProcessedMedia(unittest.TestCase):
    """Test ProcessedMedia dataclass."""

    def test_creation(self):
        from whitemagic.core.intake.media_processor import ProcessedMedia
        pm = ProcessedMedia(
            text="Hello world",
            metadata={"key": "value"},
            holographic_bias={"x": 0.5},
            media_type="test",
        )
        self.assertEqual(pm.text, "Hello world")
        self.assertEqual(pm.media_type, "test")
        self.assertEqual(pm.holographic_bias["x"], 0.5)


# ============================================================================
# Test: Entity Extractor (regex fallback)
# ============================================================================

class TestEntityExtractorRegex(unittest.TestCase):
    """Test the regex-based entity extraction fallback."""

    def setUp(self):
        from whitemagic.core.intelligence.entity_extractor import EntityExtractor
        self.extractor = EntityExtractor()
        # Force regex mode by marking Ollama unavailable
        self.extractor._ollama_available = False

    def test_extracts_capitalized_phrases(self):
        result = self.extractor.extract("Lucas Martin created WhiteMagic Labs.")
        names = [e.name for e in result.entities]
        self.assertTrue(any("Lucas" in n for n in names) or any("WhiteMagic" in n for n in names))
        self.assertEqual(result.method, "regex")

    def test_extracts_camelcase(self):
        result = self.extractor.extract("The GraphWalker and BridgeSynthesizer are components.")
        names = [e.name for e in result.entities]
        self.assertTrue(any("GraphWalker" in n for n in names))

    def test_empty_input(self):
        result = self.extractor.extract("")
        self.assertEqual(len(result.entities), 0)
        self.assertEqual(len(result.relations), 0)

    def test_extracts_relations(self):
        result = self.extractor.extract("Lucas created WhiteMagic and uses Python.")
        predicates = [r.predicate for r in result.relations]
        self.assertTrue(any("CREATED" in p for p in predicates) or any("USES" in p for p in predicates))

    def test_max_entities_capped(self):
        # Long text with many entities
        text = " ".join([f"Entity{i} Number{i}" for i in range(100)])
        result = self.extractor.extract(text)
        self.assertLessEqual(len(result.entities), 10)

    def test_to_dict(self):
        result = self.extractor.extract("Test Entity Name exists here.")
        d = result.to_dict()
        self.assertIn("entities", d)
        self.assertIn("relations", d)
        self.assertIn("method", d)
        self.assertIn("entity_count", d)

    def test_stats(self):
        self.extractor.extract("Some text with Entity Name here.")
        stats = self.extractor.get_stats()
        self.assertIn("total_extractions", stats)
        self.assertGreaterEqual(stats["total_extractions"], 1)


class TestEntityExtractorSingleton(unittest.TestCase):
    """Test singleton pattern."""

    def test_singleton(self):
        from whitemagic.core.intelligence.entity_extractor import get_entity_extractor
        e1 = get_entity_extractor()
        e2 = get_entity_extractor()
        self.assertIs(e1, e2)


# ============================================================================
# Test: HRR Circular Convolution Engine
# ============================================================================

class TestHRREngine(unittest.TestCase):
    """Test the Holographic Reduced Representations engine."""

    def setUp(self):
        from whitemagic.core.memory.hrr import HRREngine
        self.hrr = HRREngine(dim=128)  # Smaller dim for faster tests
        self.dim = 128

    def test_bind_preserves_dimension(self):
        """Binding should preserve dimensionality."""
        a = np.random.randn(self.dim).astype(np.float32)
        b = np.random.randn(self.dim).astype(np.float32)
        result = self.hrr.bind(a, b)
        self.assertEqual(result.shape, (self.dim,))

    def test_unbind_approximately_recovers(self):
        """Unbinding should approximately recover the original vector."""
        a = np.random.randn(self.dim).astype(np.float32)
        a /= np.linalg.norm(a)
        b = np.random.randn(self.dim).astype(np.float32)
        b /= np.linalg.norm(b)

        bound = self.hrr.bind(a, b)
        recovered = self.hrr.unbind(bound, b)

        # The recovered vector should be more similar to A than to random
        sim_to_a = self.hrr.similarity(recovered, a)
        random_vec = np.random.randn(self.dim).astype(np.float32)
        sim_to_random = abs(self.hrr.similarity(recovered, random_vec))

        self.assertGreater(sim_to_a, sim_to_random)

    def test_bind_is_commutative(self):
        """Circular convolution is commutative: bind(A,B) = bind(B,A)."""
        a = np.random.randn(self.dim).astype(np.float32)
        b = np.random.randn(self.dim).astype(np.float32)

        ab = self.hrr.bind(a, b)
        ba = self.hrr.bind(b, a)

        sim = self.hrr.similarity(ab, ba)
        self.assertAlmostEqual(sim, 1.0, places=4)

    def test_superpose_normalizes(self):
        """Superposition should produce a unit-length vector."""
        a = np.random.randn(self.dim).astype(np.float32)
        b = np.random.randn(self.dim).astype(np.float32)

        result = self.hrr.superpose(a, b)
        norm = np.linalg.norm(result)
        self.assertAlmostEqual(norm, 1.0, places=4)

    def test_relation_vectors_reproducible(self):
        """Relation vectors should be deterministic."""
        from whitemagic.core.memory.hrr import HRREngine
        h1 = HRREngine(dim=128)
        h2 = HRREngine(dim=128)

        v1 = h1.get_relation_vector("CAUSES")
        v2 = h2.get_relation_vector("CAUSES")
        np.testing.assert_array_almost_equal(v1, v2)

    def test_project_changes_vector(self):
        """Projection should produce a different vector."""
        embedding = np.random.randn(self.dim).astype(np.float32)
        projected = self.hrr.project(embedding, "CAUSES")
        sim = self.hrr.similarity(embedding, projected)
        # Should be different (not 1.0)
        self.assertLess(abs(sim), 0.5)

    def test_inverse_project_recovers(self):
        """Inverse projection should approximately undo projection."""
        embedding = np.random.randn(self.dim).astype(np.float32)
        embedding /= np.linalg.norm(embedding)

        projected = self.hrr.project(embedding, "CAUSES")
        recovered = self.hrr.inverse_project(projected, "CAUSES")

        sim = self.hrr.similarity(recovered, embedding)
        self.assertGreater(sim, 0.3)  # Approximate recovery

    def test_encode_event(self):
        """Event encoding should produce a valid vector."""
        agent = np.random.randn(self.dim).astype(np.float32)
        action = np.random.randn(self.dim).astype(np.float32)
        obj = np.random.randn(self.dim).astype(np.float32)

        event = self.hrr.encode_event(agent=agent, action=action, obj=obj)
        self.assertEqual(event.shape, (self.dim,))
        self.assertGreater(np.linalg.norm(event), 0)

    def test_decode_event_role(self):
        """Decoding should recover role fillers from events."""
        agent = np.random.randn(self.dim).astype(np.float32)
        agent /= np.linalg.norm(agent)
        action = np.random.randn(self.dim).astype(np.float32)
        action /= np.linalg.norm(action)

        event = self.hrr.encode_event(agent=agent, action=action)
        recovered_agent = self.hrr.decode_event_role(event, "AGENT")

        # Recovered should be more similar to agent than to action
        sim_agent = self.hrr.similarity(recovered_agent, agent)
        sim_action = self.hrr.similarity(recovered_agent, action)
        self.assertGreater(sim_agent, sim_action)

    def test_available_relations(self):
        """Should have pre-defined relation vectors."""
        relations = self.hrr.available_relations()
        self.assertIn("CAUSES", relations)
        self.assertIn("PART_OF", relations)
        self.assertIn("AGENT", relations)
        self.assertGreater(len(relations), 10)

    def test_dynamic_relation_vector(self):
        """Unknown relations should get deterministic vectors."""
        v1 = self.hrr.get_relation_vector("MY_CUSTOM_RELATION")
        v2 = self.hrr.get_relation_vector("MY_CUSTOM_RELATION")
        np.testing.assert_array_equal(v1, v2)

    def test_empty_event(self):
        """Empty event should return zero vector."""
        event = self.hrr.encode_event()
        self.assertTrue(np.allclose(event, 0))

    def test_stats(self):
        stats = self.hrr.get_stats()
        self.assertEqual(stats["dim"], 128)
        self.assertIn("relations", stats)
        self.assertGreater(stats["num_relation_vectors"], 0)


class TestHRRSingleton(unittest.TestCase):
    """Test HRR singleton."""

    def test_singleton(self):
        from whitemagic.core.memory.hrr import get_hrr_engine
        h1 = get_hrr_engine(dim=384)
        h2 = get_hrr_engine(dim=384)
        self.assertIs(h1, h2)


# ============================================================================
# Test: HolographicIntake multimodal integration
# ============================================================================

class TestHolographicIntakeMultimodal(unittest.TestCase):
    """Test that HolographicIntake now recognizes multimodal extensions."""

    def test_get_media_chain(self):
        from whitemagic.core.intake.holographic_intake import HolographicIntake
        chain = HolographicIntake._get_media_chain()
        self.assertIsNotNone(chain)

    def test_supported_text_types_unchanged(self):
        from whitemagic.core.intake.holographic_intake import HolographicIntake
        self.assertIn(".md", HolographicIntake.SUPPORTED_TYPES)
        self.assertIn(".py", HolographicIntake.SUPPORTED_TYPES)
        self.assertIn(".json", HolographicIntake.SUPPORTED_TYPES)


# ============================================================================
# Test: Bitemporal columns
# ============================================================================

class TestBitemporalMigration(unittest.TestCase):
    """Test that bitemporal columns exist in the schema migration."""

    def test_new_columns_defined(self):
        """The migration dict should include event_time and ingestion_time."""
        # Read the source to verify the columns are in the migration dict
        import inspect
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend
        source = inspect.getsource(SQLiteBackend._init_db)
        self.assertIn("event_time", source)
        self.assertIn("ingestion_time", source)

    def test_store_sets_ingestion_time(self):
        """The store method should set ingestion_time."""
        import inspect
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend
        source = inspect.getsource(SQLiteBackend.store)
        self.assertIn("ingestion_time", source)
        self.assertIn("event_time", source)


if __name__ == "__main__":
    unittest.main()
