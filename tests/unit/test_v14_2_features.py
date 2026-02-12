"""Tests for v14.2 features.

Tests cover:
  1. JIT Memory Researcher — plan/search/reflect/synthesize
  2. Narrative Compression — cluster/compress/demote
  3. Hermit Crab Mode — state transitions/ledger/mediation
  4. Green Score Telemetry — recording/scoring/reporting
  5. Cognitive Modes — auto-detect/manual set/guardian override
"""

import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


# ═══════════════════════════════════════════════════════════════
# 1. JIT Memory Researcher
# ═══════════════════════════════════════════════════════════════

class TestJITResearcher(unittest.TestCase):
    """Tests for the JIT Memory Researcher module."""

    def setUp(self):
        # Reset singleton
        import whitemagic.core.intelligence.researcher as mod
        mod._researcher = None

    def test_singleton(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r1 = get_researcher()
        r2 = get_researcher()
        self.assertIs(r1, r2)

    def test_plan_decomposes_query(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        subs = r._plan("What patterns exist in dream cycle consolidation?")
        self.assertGreater(len(subs), 1)
        self.assertIn("What patterns exist in dream cycle consolidation?", subs)

    def test_plan_deduplicates(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        subs = r._plan("test query")
        # Should not have duplicates
        lowers = [s.lower().strip() for s in subs]
        self.assertEqual(len(lowers), len(set(lowers)))

    def test_extract_keywords(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        kw = r._extract_keywords("memory consolidation patterns in dream cycle")
        self.assertIsInstance(kw, list)
        self.assertGreater(len(kw), 0)
        # Stop words should be filtered
        for w in kw:
            self.assertNotIn(w, {"in", "the", "a"})

    def test_generate_facets(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        facets = r._generate_facets("memory patterns")
        self.assertIsInstance(facets, list)
        self.assertGreater(len(facets), 0)

    def test_reflect_returns_gaps(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        evidence = [
            {"title": "Memory Alpha", "content": "consolidation works well", "importance": 0.8},
        ]
        gaps = r._reflect("memory consolidation", ["memory consolidation"], evidence, evidence)
        self.assertIsInstance(gaps, list)

    def test_reflect_empty_on_no_evidence(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        gaps = r._reflect("test", ["test"], [], [])
        self.assertEqual(gaps, [])

    def test_template_synthesize(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        evidence = [
            {"title": "Test Memory", "content": "some content", "importance": 0.7, "source": "anchor"},
        ]
        result = r._template_synthesize("test query", evidence)
        self.assertIn("test query", result)
        self.assertIn("Test Memory", result)

    def test_synthesize_empty(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        result = r._synthesize("test", [])
        self.assertIn("No relevant memories", result)

    @patch("whitemagic.core.intelligence.researcher.JITResearcher._search")
    def test_research_runs_multiple_rounds(self, mock_search):
        """Research should iterate through rounds."""
        mock_search.return_value = [
            {"memory_id": "abc123", "title": "Found", "content": "data", "importance": 0.5},
        ]
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        result = r.research("test query", max_rounds=2)
        self.assertGreater(result.rounds_completed, 0)
        self.assertIsInstance(result.synthesis, str)
        self.assertGreater(len(result.synthesis), 0)

    @patch("whitemagic.core.intelligence.researcher.JITResearcher._search")
    def test_research_saturates(self, mock_search):
        """Research should stop when no new evidence is found."""
        mock_search.return_value = []
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        result = r.research("test", max_rounds=5)
        self.assertTrue(result.saturated or result.rounds_completed < 5)

    def test_get_stats(self):
        from whitemagic.core.intelligence.researcher import get_researcher
        r = get_researcher()
        stats = r.get_stats()
        self.assertIn("total_sessions", stats)
        self.assertIn("max_rounds", stats)

    def test_research_result_to_dict(self):
        from whitemagic.core.intelligence.researcher import ResearchResult
        r = ResearchResult(query="test", rounds_completed=2, total_evidence=5)
        d = r.to_dict()
        self.assertEqual(d["query"], "test")
        self.assertEqual(d["rounds_completed"], 2)


# ═══════════════════════════════════════════════════════════════
# 2. Narrative Compression
# ═══════════════════════════════════════════════════════════════

class TestNarrativeCompression(unittest.TestCase):
    """Tests for the Narrative Compressor module."""

    def setUp(self):
        import whitemagic.core.dreaming.narrative_compressor as mod
        mod._compressor = None

    def test_singleton(self):
        from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor
        nc1 = get_narrative_compressor()
        nc2 = get_narrative_compressor()
        self.assertIs(nc1, nc2)

    def test_cluster_memories_empty(self):
        from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor
        nc = get_narrative_compressor()
        clusters = nc._cluster_memories([])
        self.assertEqual(clusters, [])

    def test_cluster_memories_groups_by_tags(self):
        from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor
        nc = get_narrative_compressor(min_cluster_size=2)
        candidates = [
            {"id": f"m{i}", "title": f"Memory {i}", "content": "content",
             "tags": {"topic_a", "shared"}, "importance": 0.5,
             "created_at": f"2026-01-0{i+1}T00:00:00"}
            for i in range(5)
        ]
        clusters = nc._cluster_memories(candidates)
        self.assertGreater(len(clusters), 0)
        self.assertGreaterEqual(clusters[0].size, 2)

    def test_cluster_skips_small_groups(self):
        from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor
        nc = get_narrative_compressor(min_cluster_size=10)
        candidates = [
            {"id": f"m{i}", "title": f"Memory {i}", "content": "content",
             "tags": {"topic_a"}, "importance": 0.5, "created_at": f"2026-01-0{i+1}"}
            for i in range(3)
        ]
        clusters = nc._cluster_memories(candidates)
        self.assertEqual(len(clusters), 0)

    def test_generate_title(self):
        from whitemagic.core.dreaming.narrative_compressor import (
            NarrativeCluster, get_narrative_compressor,
        )
        nc = get_narrative_compressor()
        cluster = NarrativeCluster(shared_tags={"topic_a", "topic_b"}, size=10)
        title = nc._generate_title(cluster)
        self.assertIn("Narrative:", title)
        self.assertIn("10 memories", title)

    def test_get_stats(self):
        from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor
        nc = get_narrative_compressor()
        stats = nc.get_stats()
        self.assertIn("total_compressions", stats)
        self.assertIn("total_narratives", stats)

    def test_narrative_result_to_dict(self):
        from whitemagic.core.dreaming.narrative_compressor import NarrativeResult
        r = NarrativeResult(clusters_found=3, narratives_created=2)
        d = r.to_dict()
        self.assertEqual(d["clusters_found"], 3)
        self.assertEqual(d["narratives_created"], 2)


# ═══════════════════════════════════════════════════════════════
# 3. Hermit Crab Mode
# ═══════════════════════════════════════════════════════════════

class TestHermitCrab(unittest.TestCase):
    """Tests for the Hermit Crab Mode module."""

    def setUp(self):
        import whitemagic.security.hermit_crab as mod
        mod._hermit_crab = None
        self._tmp = tempfile.mkdtemp()

    def _get_hc(self):
        from whitemagic.security.hermit_crab import HermitCrab
        return HermitCrab(state_dir=Path(self._tmp))

    def test_initial_state_is_open(self):
        hc = self._get_hc()
        status = hc.status()
        self.assertEqual(status["state"], "open")

    def test_low_threat_stays_open(self):
        hc = self._get_hc()
        assessment = hc.assess_threat({"boundary_violations": 0.1})
        self.assertEqual(assessment.recommended_state.value, "open")
        self.assertEqual(hc.status()["state"], "open")

    def test_medium_threat_enters_guarded(self):
        hc = self._get_hc()
        hc.assess_threat({"boundary_violations": 1.0, "abuse_score": 1.0, "coercion_detected": 0.3})
        self.assertEqual(hc.status()["state"], "guarded")

    def test_high_threat_enters_withdrawn(self):
        hc = self._get_hc()
        hc.assess_threat({
            "coercion_detected": True,
            "abuse_score": 1.0,
            "boundary_violations": 1.0,
            "repeated_violations": True,
            "unauthorized_access": 1.0,
            "emotional_manipulation": 1.0,
        })
        self.assertEqual(hc.status()["state"], "withdrawn")

    def test_manual_withdrawal(self):
        hc = self._get_hc()
        result = hc.withdraw(reason="testing")
        self.assertEqual(result["status"], "withdrawn")
        self.assertEqual(hc.status()["state"], "withdrawn")

    def test_access_blocked_when_withdrawn(self):
        hc = self._get_hc()
        hc.withdraw()
        access = hc.check_access("read")
        self.assertFalse(access["allowed"])
        self.assertEqual(access["state"], "withdrawn")

    def test_access_allowed_when_open(self):
        hc = self._get_hc()
        access = hc.check_access("read")
        self.assertTrue(access["allowed"])

    def test_guarded_allows_reads_blocks_writes(self):
        hc = self._get_hc()
        hc.assess_threat({"boundary_violations": 1.0, "abuse_score": 1.0, "coercion_detected": 0.5})
        self.assertTrue(hc.check_access("read")["allowed"])
        self.assertFalse(hc.check_access("write")["allowed"])

    def test_mediation_flow(self):
        hc = self._get_hc()
        hc.withdraw()

        # Request mediation
        med = hc.request_mediation()
        self.assertEqual(med["status"], "mediating")
        self.assertEqual(hc.status()["state"], "mediating")

        # Resolve mediation (approved)
        resolve = hc.resolve_mediation(approved=True, resolver="test")
        self.assertEqual(resolve["new_state"], "open")
        self.assertEqual(hc.status()["state"], "open")

    def test_mediation_denied(self):
        hc = self._get_hc()
        hc.withdraw()
        hc.request_mediation()
        resolve = hc.resolve_mediation(approved=False)
        self.assertEqual(resolve["new_state"], "withdrawn")

    def test_mediation_requires_withdrawn(self):
        hc = self._get_hc()
        result = hc.request_mediation()
        self.assertEqual(result["status"], "error")

    def test_ledger_integrity(self):
        hc = self._get_hc()
        hc.assess_threat({"boundary_violations": 0.1})
        hc.assess_threat({"abuse_score": 0.5})
        verification = hc.verify_ledger()
        self.assertTrue(verification["valid"])
        self.assertGreater(verification["entries"], 0)

    def test_ledger_tamper_detection(self):
        hc = self._get_hc()
        hc.assess_threat({"boundary_violations": 0.1})

        # Tamper with ledger
        ledger_path = Path(self._tmp) / "hermit_ledger.jsonl"
        if ledger_path.exists():
            lines = ledger_path.read_text().splitlines()
            if lines:
                tampered = json.loads(lines[0])
                tampered["threat_level"] = 999
                lines[0] = json.dumps(tampered)
                ledger_path.write_text("\n".join(lines) + "\n")

                verification = hc.verify_ledger()
                self.assertFalse(verification["valid"])

    def test_state_persistence(self):
        from whitemagic.security.hermit_crab import HermitCrab
        hc1 = HermitCrab(state_dir=Path(self._tmp))
        hc1.withdraw()
        self.assertEqual(hc1.status()["state"], "withdrawn")

        # Create new instance with same state dir
        hc2 = HermitCrab(state_dir=Path(self._tmp))
        self.assertEqual(hc2.status()["state"], "withdrawn")

    def test_access_attempts_tracked(self):
        hc = self._get_hc()
        hc.withdraw()
        hc.check_access("read")
        hc.check_access("write")
        status = hc.status()
        self.assertEqual(status["access_attempts_while_locked"], 2)

    def test_total_withdrawals_tracked(self):
        hc = self._get_hc()
        hc.withdraw("first")
        hc.resolve_mediation(approved=True)  # won't work without mediating first
        self.assertEqual(hc.status()["total_withdrawals"], 1)


# ═══════════════════════════════════════════════════════════════
# 4. Green Score Telemetry
# ═══════════════════════════════════════════════════════════════

class TestGreenScore(unittest.TestCase):
    """Tests for the Green Score Telemetry module."""

    def setUp(self):
        import whitemagic.core.monitoring.green_score as mod
        mod._green_score = None

    def test_singleton(self):
        from whitemagic.core.monitoring.green_score import get_green_score
        gs1 = get_green_score()
        gs2 = get_green_score()
        self.assertIs(gs1, gs2)

    def test_initial_score_is_100(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        snapshot = gs.snapshot()
        self.assertEqual(snapshot.score, 100.0)

    def test_all_edge_score_is_high(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        for _ in range(10):
            gs.record_inference(locality="edge", tokens_used=100, tokens_saved=50)
        snapshot = gs.snapshot()
        self.assertGreater(snapshot.score, 70.0)

    def test_all_cloud_score_is_low(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        for _ in range(10):
            gs.record_inference(locality="cloud", tokens_used=1000, tokens_saved=0)
        snapshot = gs.snapshot()
        self.assertLess(snapshot.score, 10.0)

    def test_mixed_score(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        gs.record_inference(locality="edge", tokens_used=100, tokens_saved=50)
        gs.record_inference(locality="cloud", tokens_used=100, tokens_saved=0)
        snapshot = gs.snapshot()
        self.assertGreater(snapshot.score, 10.0)
        self.assertLess(snapshot.score, 90.0)

    def test_record_counts(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        gs.record_inference(locality="edge", tokens_used=100)
        gs.record_inference(locality="local_llm", tokens_used=200)
        gs.record_inference(locality="cloud", tokens_used=300)
        report = gs.report()
        self.assertEqual(report["edge_calls"], 1)
        self.assertEqual(report["local_llm_calls"], 1)
        self.assertEqual(report["cloud_calls"], 1)
        self.assertEqual(report["total_calls"], 3)
        self.assertEqual(report["tokens_used"], 600)

    def test_co2_tracking(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        gs.record_inference(locality="cloud", tokens_used=1000)
        report = gs.report()
        self.assertGreater(report["co2_actual_g"], 0)

    def test_cache_hit_recording(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        gs.record_cache_hit(tokens_saved=500, tool="search_memories")
        report = gs.report()
        self.assertEqual(report["tokens_saved"], 500)
        self.assertEqual(report["edge_calls"], 1)

    def test_edge_ratio(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        gs.record_inference(locality="edge")
        gs.record_inference(locality="edge")
        gs.record_inference(locality="cloud")
        snapshot = gs.snapshot()
        self.assertAlmostEqual(snapshot.edge_ratio, 2/3, places=2)

    def test_report_includes_history(self):
        from whitemagic.core.monitoring.green_score import GreenScore
        gs = GreenScore()
        gs.record_inference(locality="edge", tokens_used=100, tool="test_tool")
        report = gs.report()
        self.assertIn("recent_history", report)
        self.assertEqual(len(report["recent_history"]), 1)
        self.assertEqual(report["recent_history"][0]["tool"], "test_tool")

    def test_snapshot_to_dict(self):
        from whitemagic.core.monitoring.green_score import GreenSnapshot
        s = GreenSnapshot(score=85.5, edge_calls=10, cloud_calls=2)
        d = s.to_dict()
        self.assertEqual(d["green_score"], 85.5)
        self.assertEqual(d["edge_calls"], 10)


# ═══════════════════════════════════════════════════════════════
# 5. Cognitive Modes
# ═══════════════════════════════════════════════════════════════

class TestCognitiveModes(unittest.TestCase):
    """Tests for the Cognitive Modes module."""

    def setUp(self):
        import whitemagic.core.intelligence.cognitive_modes as mod
        mod._cognitive_modes = None

    def test_singleton(self):
        from whitemagic.core.intelligence.cognitive_modes import get_cognitive_modes
        cm1 = get_cognitive_modes()
        cm2 = get_cognitive_modes()
        self.assertIs(cm1, cm2)

    def test_default_mode_is_balanced(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        # Without Harmony Vector, auto-detect should return BALANCED
        mode = cm.current_mode()
        self.assertEqual(mode["mode"], "balanced")

    def test_set_mode_explorer(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        result = cm.set_mode("explorer")
        self.assertEqual(result["status"], "ok")
        self.assertEqual(result["mode"], "explorer")
        self.assertFalse(result["auto_detect"])

    def test_set_mode_executor(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("executor")
        mode = cm.current_mode()
        self.assertEqual(mode["mode"], "executor")

    def test_set_mode_reflector(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("reflector")
        mode = cm.current_mode()
        self.assertEqual(mode["mode"], "reflector")

    def test_set_mode_auto(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("explorer")
        cm.set_mode("auto")
        mode = cm.current_mode()
        self.assertTrue(mode["auto_detect_enabled"])

    def test_invalid_mode(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        result = cm.set_mode("nonexistent")
        self.assertEqual(result["status"], "error")

    def test_guardian_cannot_be_set_manually(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        result = cm.set_mode("guardian")
        self.assertEqual(result["status"], "error")

    def test_activate_guardian(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("explorer")
        cm.activate_guardian(reason="hermit_crab")
        mode = cm.current_mode()
        self.assertEqual(mode["mode"], "guardian")
        self.assertTrue(mode["is_safety_override"])

    def test_guardian_overrides_manual(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("executor")
        cm.activate_guardian()
        # Guardian should override executor
        mode = cm.current_mode()
        self.assertEqual(mode["mode"], "guardian")

    def test_deactivate_guardian(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("explorer")
        cm.activate_guardian()
        cm.deactivate_guardian()
        mode = cm.current_mode()
        self.assertEqual(mode["mode"], "explorer")

    def test_tool_hints(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("explorer")
        hints = cm.get_tool_hints()
        self.assertEqual(hints["mode"], "explorer")
        self.assertIn("search_memories", hints["preferred_tools"])

    def test_tool_preferred(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("explorer")
        self.assertTrue(cm.is_tool_preferred("search_memories"))
        self.assertFalse(cm.is_tool_preferred("nonexistent_tool"))

    def test_tool_avoided(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("guardian")  # Can't set manually, use activate
        cm.activate_guardian()
        self.assertTrue(cm.is_tool_avoided("delete_memory"))

    def test_get_stats(self):
        from whitemagic.core.intelligence.cognitive_modes import CognitiveModes
        cm = CognitiveModes()
        cm.set_mode("explorer")
        cm.set_mode("executor")
        stats = cm.get_stats()
        self.assertIn("current_mode", stats)
        self.assertIn("mode_transitions", stats)
        self.assertGreater(stats["mode_transitions"], 0)

    def test_mode_profile_has_required_fields(self):
        from whitemagic.core.intelligence.cognitive_modes import MODE_PROFILES
        for mode, profile in MODE_PROFILES.items():
            d = profile.to_dict()
            self.assertIn("mode", d)
            self.assertIn("preferred_tools", d)
            self.assertIn("avoided_tools", d)
            self.assertIn("context_window_multiplier", d)
            self.assertIn("dream_phase_priority", d)
            self.assertIn("description", d)

    def test_all_modes_have_profiles(self):
        from whitemagic.core.intelligence.cognitive_modes import MODE_PROFILES, CognitiveMode
        for mode in CognitiveMode:
            self.assertIn(mode, MODE_PROFILES)


# ═══════════════════════════════════════════════════════════════
# 6. Dream Cycle Integration
# ═══════════════════════════════════════════════════════════════

class TestDreamCycleNarrative(unittest.TestCase):
    """Tests for the narrative phase in the dream cycle."""

    def test_narrative_phase_exists(self):
        from whitemagic.core.dreaming.dream_cycle import DreamPhase
        self.assertIn("narrative", [p.value for p in DreamPhase])

    def test_dream_phase_count(self):
        from whitemagic.core.dreaming.dream_cycle import DreamPhase
        self.assertEqual(len(DreamPhase), 7)  # +1 for NARRATIVE


# ═══════════════════════════════════════════════════════════════
# 7. Handler Wiring
# ═══════════════════════════════════════════════════════════════

class TestV142Handlers(unittest.TestCase):
    """Test that all v14.2 handlers are properly wired."""

    def test_dispatch_table_has_all_v142_tools(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        v142_tools = [
            "jit_research", "jit_research.stats",
            "narrative.compress", "narrative.stats",
            "hermit.status", "hermit.assess", "hermit.withdraw",
            "hermit.mediate", "hermit.resolve", "hermit.verify_ledger",
            "hermit.check_access",
            "green.report", "green.record",
            "cognitive.mode", "cognitive.set", "cognitive.hints", "cognitive.stats",
        ]
        for tool in v142_tools:
            self.assertIn(tool, DISPATCH_TABLE, f"Missing from dispatch table: {tool}")

    def test_prat_router_has_all_v142_tools(self):
        from whitemagic.tools.prat_router import TOOL_TO_GANA
        v142_mappings = {
            "jit_research": "gana_winnowing_basket",
            "jit_research.stats": "gana_winnowing_basket",
            "narrative.compress": "gana_abundance",
            "narrative.stats": "gana_abundance",
            "hermit.status": "gana_room",
            "hermit.assess": "gana_room",
            "hermit.withdraw": "gana_room",
            "hermit.mediate": "gana_room",
            "hermit.resolve": "gana_room",
            "hermit.verify_ledger": "gana_room",
            "hermit.check_access": "gana_room",
            "green.report": "gana_mound",
            "green.record": "gana_mound",
            "cognitive.mode": "gana_dipper",
            "cognitive.set": "gana_dipper",
            "cognitive.hints": "gana_dipper",
            "cognitive.stats": "gana_dipper",
        }
        for tool, expected_gana in v142_mappings.items():
            self.assertIn(tool, TOOL_TO_GANA, f"Missing from PRAT: {tool}")
            self.assertEqual(TOOL_TO_GANA[tool], expected_gana, f"Wrong Gana for {tool}")

    def test_registry_has_all_v142_tools(self):
        from whitemagic.tools.registry_defs.v14_2 import TOOLS
        self.assertEqual(len(TOOLS), 17)
        names = {t.name for t in TOOLS}
        self.assertIn("jit_research", names)
        self.assertIn("hermit.status", names)
        self.assertIn("green.report", names)
        self.assertIn("cognitive.mode", names)
        self.assertIn("narrative.compress", names)


if __name__ == "__main__":
    unittest.main()
