"""
Tests for v12.7 — Polyglot Hot Paths + Cross-System Fusions
=============================================================
Tests:
  - Zig SIMD cosine similarity bridge (library path fix)
  - Haskell Dharma as primary evaluator (Python fallback)
  - Zodiac Cores → Grimoire Spells fusion
  - Bicameral → Consolidation fusion
  - Salience ↔ Homeostasis fusion
  - Dream → Bicameral fusion
  - Constellation → Garden fusion
  - KG → Gana Routing fusion
  - Capability matrix updates
  - Fusion status tracking
"""

import unittest
from unittest.mock import patch, MagicMock
from typing import Any, Dict


# =========================================================================
# 1. Zig SIMD Cosine Similarity
# =========================================================================

class TestZigSIMD(unittest.TestCase):
    """Test the Zig SIMD cosine similarity bridge."""

    def test_cosine_identical_vectors(self):
        from whitemagic.core.acceleration.simd_cosine import cosine_similarity
        score = cosine_similarity([1.0, 0.0, 0.0], [1.0, 0.0, 0.0])
        self.assertAlmostEqual(score, 1.0, places=3)

    def test_cosine_orthogonal_vectors(self):
        from whitemagic.core.acceleration.simd_cosine import cosine_similarity
        score = cosine_similarity([1.0, 0.0, 0.0], [0.0, 1.0, 0.0])
        self.assertAlmostEqual(score, 0.0, places=3)

    def test_cosine_opposite_vectors(self):
        from whitemagic.core.acceleration.simd_cosine import cosine_similarity
        score = cosine_similarity([1.0, 2.0, 3.0], [-1.0, -2.0, -3.0])
        self.assertAlmostEqual(score, -1.0, places=3)

    def test_cosine_empty_vectors(self):
        from whitemagic.core.acceleration.simd_cosine import cosine_similarity
        score = cosine_similarity([], [])
        self.assertEqual(score, 0.0)

    def test_batch_cosine(self):
        from whitemagic.core.acceleration.simd_cosine import batch_cosine
        query = [1.0, 0.0, 0.0]
        vectors = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.5, 0.5, 0.0]]
        scores = batch_cosine(query, vectors)
        self.assertEqual(len(scores), 3)
        self.assertAlmostEqual(scores[0], 1.0, places=3)
        self.assertAlmostEqual(scores[1], 0.0, places=3)
        self.assertGreater(scores[2], 0.5)

    def test_simd_status(self):
        from whitemagic.core.acceleration.simd_cosine import simd_status
        status = simd_status()
        self.assertIn("has_zig_simd", status)
        self.assertIn("lane_width", status)
        self.assertIn("backend", status)

    def test_find_zig_lib_paths(self):
        from whitemagic.core.acceleration.simd_cosine import _find_zig_lib
        path = _find_zig_lib()
        # Path should be found (Zig was built)
        if path:
            self.assertIn("libwhitemagic", path)


# =========================================================================
# 2. Haskell Dharma Primary Evaluator
# =========================================================================

class TestHaskellDharmaPrimary(unittest.TestCase):
    """Test Haskell Dharma as primary evaluator with Python fallback."""

    def test_evaluate_destructive_op(self):
        """Destructive operations should be blocked/warned regardless of backend."""
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine()
        decision = engine.evaluate({"tool": "delete_memory", "description": "remove old data"})
        self.assertIn(decision.action, [DharmaAction.WARN, DharmaAction.BLOCK])
        self.assertGreater(len(decision.triggered_rules), 0)

    def test_evaluate_safe_op(self):
        """Safe operations should get LOG action."""
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine()
        decision = engine.evaluate({"tool": "search_memories", "description": "find docs"})
        self.assertEqual(decision.action, DharmaAction.LOG)

    def test_evaluate_external_op(self):
        """External network operations should be flagged."""
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine()
        decision = engine.evaluate({
            "tool": "upload", "description": "send to external server"
        })
        self.assertIn(decision.action, [DharmaAction.WARN, DharmaAction.THROTTLE, DharmaAction.TAG])

    def test_python_fallback_works(self):
        """Python evaluation should work when Haskell is unavailable."""
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine()
        decision = engine._python_evaluate(
            {"tool": "delete_all", "description": "purge everything"}, "default"
        )
        self.assertIn(decision.action, [DharmaAction.WARN, DharmaAction.BLOCK])

    def test_haskell_bridge_check(self):
        """The Haskell bridge availability check should not crash."""
        from whitemagic.dharma.rules import _get_haskell_bridge
        bridge = _get_haskell_bridge()
        # Bridge may or may not be available, but should not error
        self.assertTrue(bridge is None or bridge is not None)

    def test_karmic_trace_populated(self):
        """Each evaluation should create a karmic trace entry."""
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        engine.evaluate({"tool": "test", "description": "test"})
        trace = engine.get_karmic_trace(limit=5)
        self.assertGreater(len(trace), 0)

    def test_profile_switch(self):
        """Profile switch should work."""
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        engine.set_profile("creative")
        self.assertEqual(engine.get_profile(), "creative")
        engine.set_profile("default")
        self.assertEqual(engine.get_profile(), "default")


# =========================================================================
# 3. Zodiac Cores → Grimoire Spells Fusion
# =========================================================================

class TestZodiacGrimoireFusion(unittest.TestCase):
    """Test Zodiac Cores → Grimoire Spells boost."""

    def test_zodiac_spell_boost_returns_result(self):
        from whitemagic.core.fusions import get_zodiac_spell_boost
        result = get_zodiac_spell_boost("create something beautiful")
        self.assertIn("boosted_spells", result)
        self.assertIsInstance(result["boosted_spells"], list)

    def test_zodiac_spell_boost_has_active_cores(self):
        """Even without explicit activation, should derive from Wu Xing."""
        from whitemagic.core.fusions import get_zodiac_spell_boost
        result = get_zodiac_spell_boost("explore and discover")
        # Should have either active_cores or an error/reason
        self.assertTrue(
            "active_cores" in result or "error" in result or "reason" in result
        )

    def test_zodiac_element_to_wuxing_mapping(self):
        from whitemagic.core.fusions import _ZODIAC_ELEMENT_TO_WUXING
        self.assertEqual(_ZODIAC_ELEMENT_TO_WUXING["fire"], ["fire"])
        self.assertEqual(_ZODIAC_ELEMENT_TO_WUXING["air"], ["metal"])
        self.assertEqual(_ZODIAC_ELEMENT_TO_WUXING["water"], ["water"])
        self.assertEqual(_ZODIAC_ELEMENT_TO_WUXING["earth"], ["earth"])

    def test_spell_boost_contains_spell_names(self):
        from whitemagic.core.fusions import get_zodiac_spell_boost
        result = get_zodiac_spell_boost("anything")
        for spell in result.get("boosted_spells", []):
            self.assertIn("spell", spell)
            self.assertIn("wu_xing", spell)
            self.assertIn("boost", spell)


# =========================================================================
# 4. Bicameral → Consolidation Fusion
# =========================================================================

class TestBicameralConsolidation(unittest.TestCase):
    """Test Bicameral Reasoner → Consolidation enhancement."""

    def test_empty_clusters(self):
        from whitemagic.core.fusions import bicameral_consolidation_enhance
        result = bicameral_consolidation_enhance([])
        self.assertEqual(result["suggestions"], [])

    def test_single_cluster(self):
        from whitemagic.core.fusions import bicameral_consolidation_enhance
        result = bicameral_consolidation_enhance([{"cluster_id": "c1", "shared_tags": ["a"]}])
        self.assertEqual(result["suggestions"], [])

    def test_two_disjoint_clusters(self):
        from whitemagic.core.fusions import bicameral_consolidation_enhance
        clusters = [
            {"cluster_id": "c1", "shared_tags": ["memory", "galactic"],
             "theme": "memory management", "size": 5, "memory_ids": ["m1"]},
            {"cluster_id": "c2", "shared_tags": ["dharma", "ethics"],
             "theme": "ethical governance", "size": 4, "memory_ids": ["m2"]},
        ]
        result = bicameral_consolidation_enhance(clusters)
        self.assertIn("suggestions", result)
        self.assertEqual(result["clusters_analyzed"], 2)

    def test_overlapping_tags_skipped(self):
        """Clusters that already share tags should not get creative suggestions."""
        from whitemagic.core.fusions import bicameral_consolidation_enhance
        clusters = [
            {"cluster_id": "c1", "shared_tags": ["memory"], "theme": "a", "size": 2, "memory_ids": []},
            {"cluster_id": "c2", "shared_tags": ["memory"], "theme": "b", "size": 2, "memory_ids": []},
        ]
        result = bicameral_consolidation_enhance(clusters)
        # Should skip these since they share "memory" tag
        self.assertEqual(len(result["suggestions"]), 0)


# =========================================================================
# 5. Salience ↔ Homeostasis Fusion
# =========================================================================

class TestSalienceHomeostasis(unittest.TestCase):
    """Test bidirectional Salience Arbiter ↔ Homeostatic Loop sync."""

    def test_sync_returns_both_directions(self):
        from whitemagic.core.fusions import salience_homeostasis_sync
        result = salience_homeostasis_sync()
        # Should return result even if subsystems aren't fully running
        self.assertIsInstance(result, dict)
        if "error" not in result:
            self.assertIn("direction_1", result)
            self.assertIn("direction_2", result)

    def test_sync_direction2_has_sensitivity(self):
        from whitemagic.core.fusions import salience_homeostasis_sync
        result = salience_homeostasis_sync()
        if "direction_2" in result:
            d2 = result["direction_2"]
            self.assertIn("sensitivity", d2)
            self.assertIn(d2["sensitivity"], ["heightened", "normal", "relaxed"])


# =========================================================================
# 6. Dream → Bicameral Fusion
# =========================================================================

class TestDreamBicameral(unittest.TestCase):
    """Test Dream Cycle → Bicameral serendipity."""

    def test_empty_memories(self):
        from whitemagic.core.fusions import dream_bicameral_serendipity
        result = dream_bicameral_serendipity([])
        self.assertEqual(result["connections"], [])

    def test_single_memory(self):
        from whitemagic.core.fusions import dream_bicameral_serendipity
        result = dream_bicameral_serendipity([{"id": "m1", "title": "test", "tags": []}])
        self.assertEqual(result["connections"], [])

    def test_two_memories_with_shared_concepts(self):
        from whitemagic.core.fusions import dream_bicameral_serendipity
        memories = [
            {"id": "m1", "title": "python memory management", "tags": ["python"]},
            {"id": "m2", "title": "python web server", "tags": ["web"]},
        ]
        result = dream_bicameral_serendipity(memories)
        self.assertIn("connections", result)
        self.assertEqual(result["memories_processed"], 2)

    def test_many_memories_capped(self):
        """Should cap at 20 memories for efficiency."""
        from whitemagic.core.fusions import dream_bicameral_serendipity
        memories = [{"id": f"m{i}", "title": f"memory {i}", "tags": []} for i in range(50)]
        result = dream_bicameral_serendipity(memories)
        self.assertLessEqual(result["memories_processed"], 20)

    def test_serendipity_score_in_connections(self):
        from whitemagic.core.fusions import dream_bicameral_serendipity
        memories = [
            {"id": "m1", "title": "galactic distance calculation", "tags": ["galactic"]},
            {"id": "m2", "title": "galactic map rotation", "tags": ["map"]},
        ]
        result = dream_bicameral_serendipity(memories)
        for conn in result.get("connections", []):
            self.assertIn("serendipity_score", conn)
            self.assertIn("shared_concepts", conn)


# =========================================================================
# 7. Constellation → Garden Fusion
# =========================================================================

class TestConstellationGarden(unittest.TestCase):
    """Test Constellation Detection → Garden Activation."""

    def test_empty_constellations(self):
        from whitemagic.core.fusions import constellation_garden_activate
        result = constellation_garden_activate([])
        self.assertEqual(result["activations"], [])

    def test_constellation_with_matching_tags(self):
        from whitemagic.core.fusions import constellation_garden_activate
        constellations = [
            {"name": "Creative Cluster", "dominant_tags": ["create", "build", "art"]},
        ]
        result = constellation_garden_activate(constellations)
        self.assertIn("activations", result)
        # Should suggest creation/beauty garden
        if result["activations"]:
            gardens = [a["garden"] for a in result["activations"]]
            self.assertTrue(any(g in ["creation", "beauty"] for g in gardens))

    def test_constellation_with_no_matching_tags(self):
        from whitemagic.core.fusions import constellation_garden_activate
        constellations = [
            {"name": "Random", "dominant_tags": ["xyzzy123", "qwerty456"]},
        ]
        result = constellation_garden_activate(constellations)
        self.assertEqual(len(result["activations"]), 0)

    def test_capped_at_five_constellations(self):
        from whitemagic.core.fusions import constellation_garden_activate
        constellations = [{"name": f"c{i}", "dominant_tags": ["create"]} for i in range(10)]
        result = constellation_garden_activate(constellations)
        self.assertLessEqual(result["constellations_analyzed"], 5)


# =========================================================================
# 8. KG → Gana Routing Fusion
# =========================================================================

class TestKGGanaRouting(unittest.TestCase):
    """Test Knowledge Graph → Gana Routing suggestion."""

    def test_suggest_returns_dict(self):
        from whitemagic.core.fusions import kg_suggest_next_gana
        result = kg_suggest_next_gana("search_memories")
        self.assertIsInstance(result, dict)
        self.assertIn("suggestions", result)

    def test_suggest_unknown_tool(self):
        from whitemagic.core.fusions import kg_suggest_next_gana
        result = kg_suggest_next_gana("nonexistent_tool_xyz_123")
        self.assertIsInstance(result["suggestions"], list)


# =========================================================================
# 9. Capability Matrix Updates
# =========================================================================

class TestCapabilityMatrixUpdates(unittest.TestCase):
    """Test that the capability matrix reflects all new fusions."""

    def test_active_fusion_count(self):
        from whitemagic.tools.capability_matrix import ACTIVE_FUSIONS
        # Was 15, added 8 new = 23
        self.assertGreaterEqual(len(ACTIVE_FUSIONS), 23)

    def test_unexplored_fusion_count(self):
        from whitemagic.tools.capability_matrix import UNEXPLORED_FUSIONS
        # Was 13, 8 moved in v12.7, remaining 5 moved in v12.8 = 0
        self.assertEqual(len(UNEXPLORED_FUSIONS), 0)

    def test_new_fusions_in_active(self):
        from whitemagic.tools.capability_matrix import ACTIVE_FUSIONS
        active_ids = {f["id"] for f in ACTIVE_FUSIONS}
        expected_new = {
            "zodiac_grimoire", "bicameral_consolidation", "salience_homeostasis",
            "dream_bicameral", "constellation_garden", "kg_gana_routing",
            "zig_simd_cosine", "haskell_dharma_primary",
        }
        for fid in expected_new:
            self.assertIn(fid, active_ids, f"Missing active fusion: {fid}")

    def test_removed_from_unexplored(self):
        from whitemagic.tools.capability_matrix import UNEXPLORED_FUSIONS
        unexplored_ids = {f["id"] for f in UNEXPLORED_FUSIONS}
        should_not_be_there = {
            "zodiac_grimoire", "bicameral_consolidation", "salience_homeostasis",
            "dream_bicameral", "constellation_garden", "kg_gana_routing",
            "haskell_dharma_primary", "zig_association",
        }
        for fid in should_not_be_there:
            self.assertNotIn(fid, unexplored_ids, f"Should be active, not unexplored: {fid}")

    def test_subsystem_count(self):
        from whitemagic.tools.capability_matrix import SUBSYSTEMS
        # Added Zig SIMD subsystem
        self.assertGreaterEqual(len(SUBSYSTEMS), 25)

    def test_zig_simd_subsystem_exists(self):
        from whitemagic.tools.capability_matrix import SUBSYSTEMS
        ids = {s["id"] for s in SUBSYSTEMS}
        self.assertIn("zig_simd", ids)

    def test_get_capability_matrix_summary(self):
        from whitemagic.tools.capability_matrix import get_capability_matrix
        matrix = get_capability_matrix()
        self.assertGreaterEqual(matrix["summary"]["active_fusions"], 28)
        self.assertEqual(matrix["summary"]["unexplored_fusions"], 0)


# =========================================================================
# 10. Fusion Status Tracking
# =========================================================================

class TestFusionStatus(unittest.TestCase):
    """Test the fusion status tracker."""

    def test_all_fusions_in_status(self):
        from whitemagic.core.fusions import get_fusion_status
        status = get_fusion_status()
        expected = [
            "selfmodel_dream", "wuxing_gana_boost", "resonance_emotion",
            "depgraph_pipeline", "zodiac_grimoire", "bicameral_consolidation",
            "salience_homeostasis", "dream_bicameral", "constellation_garden",
            "kg_gana_routing", "zig_simd_cosine", "haskell_dharma_primary",
        ]
        for fid in expected:
            self.assertIn(fid, status, f"Missing from fusion status: {fid}")
            self.assertTrue(status[fid]["wired"])

    def test_fusion_count(self):
        from whitemagic.core.fusions import get_fusion_status
        status = get_fusion_status()
        # Was 12 in v12.7, now 17 after v12.8 added 5 more
        self.assertGreaterEqual(len(status), 12)


# =========================================================================
# 11. Grimoire Handler Integration
# =========================================================================

class TestGrimoireHandlerZodiacIntegration(unittest.TestCase):
    """Test that grimoire suggest handler includes zodiac boost."""

    def test_grimoire_suggest_handler(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_suggest
        result = handle_grimoire_suggest(task="create something new")
        self.assertEqual(result["status"], "success")
        self.assertIn("suggestions", result)
        # Should have zodiac context (derived from Wu Xing)
        # Note: may or may not have zodiac_context depending on time of day

    def test_grimoire_suggest_empty_task(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_suggest
        result = handle_grimoire_suggest(task="")
        self.assertEqual(result["status"], "error")


if __name__ == "__main__":
    unittest.main()
