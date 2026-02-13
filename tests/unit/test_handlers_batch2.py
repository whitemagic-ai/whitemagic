"""
Test expansion batch 2 — coverage for untested handler modules:
balance, garden, learning, edge, ganying, galactic_dashboard, context_optimizer, synthesis.

Uses mocks for subsystem singletons to isolate handler logic.
"""
import unittest
from unittest.mock import MagicMock, patch


# =========================================================================
# Balance / Harmony handlers
# =========================================================================

class TestBalanceHandlers(unittest.TestCase):

    @patch('whitemagic.harmony.yin_yang_tracker.get_tracker')
    def test_record_yin_yang_activity(self, mock_tracker_fn):
        mock_tracker = MagicMock()
        mock_metrics = MagicMock()
        mock_metrics.balance_score = 0.7
        mock_metrics.burnout_risk = 0.2
        mock_metrics.recommendation = "balanced"
        mock_metrics.yang_ratio = 0.6
        mock_metrics.yin_ratio = 0.4
        mock_tracker.record_activity.return_value = mock_metrics
        mock_tracker_fn.return_value = mock_tracker

        from whitemagic.tools.handlers.balance import handle_record_yin_yang_activity
        result = handle_record_yin_yang_activity(activity="WRITE")
        self.assertTrue(result["success"])
        self.assertEqual(result["balance_score"], 0.7)
        mock_tracker.record_activity.assert_called_once_with("WRITE")

    @patch('whitemagic.harmony.yin_yang_tracker.get_tracker')
    def test_get_yin_yang_balance(self, mock_tracker_fn):
        mock_tracker = MagicMock()
        mock_tracker.get_report.return_value = {"balance": 0.5, "status": "ok"}
        mock_tracker_fn.return_value = mock_tracker

        from whitemagic.tools.handlers.balance import handle_get_yin_yang_balance
        result = handle_get_yin_yang_balance()
        self.assertIn("balance", result)

    @patch('whitemagic.harmony.vector.get_harmony_vector')
    def test_harmony_vector(self, mock_hv_fn):
        mock_hv = MagicMock()
        mock_snap = MagicMock()
        mock_snap.to_dict.return_value = {"balance": 1.0, "energy": 0.8}
        mock_hv.snapshot.return_value = mock_snap
        mock_hv_fn.return_value = mock_hv

        from whitemagic.tools.handlers.balance import handle_harmony_vector
        result = handle_harmony_vector()
        self.assertEqual(result["status"], "success")
        self.assertIn("harmony_vector", result)


# =========================================================================
# Garden handlers
# =========================================================================

class TestGardenHandlers(unittest.TestCase):

    @patch('whitemagic.tools.handlers.garden._emit')
    @patch('whitemagic.gardens.get_garden')
    def test_garden_activate(self, mock_get_garden, mock_emit):
        from whitemagic.tools.handlers.garden import handle_garden_activate
        result = handle_garden_activate(garden="water")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["garden"], "water")
        self.assertTrue(result["active"])
        mock_emit.assert_called()

    @patch('whitemagic.gardens.get_all_gardens')
    def test_garden_status(self, mock_all):
        mock_all.return_value = {"water": MagicMock(), "fire": MagicMock()}

        from whitemagic.tools.handlers.garden import handle_garden_status
        result = handle_garden_status()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["count"], 2)

    @patch('whitemagic.gardens.get_all_gardens')
    def test_garden_health(self, mock_all):
        mock_all.return_value = {"water": MagicMock(), "earth": MagicMock()}

        from whitemagic.tools.handlers.garden import handle_garden_health
        result = handle_garden_health()
        self.assertEqual(result["status"], "success")
        self.assertIn("health", result)
        self.assertEqual(len(result["health"]), 2)


# =========================================================================
# Learning handlers
# =========================================================================

class TestLearningHandlers(unittest.TestCase):

    @patch('whitemagic.core.learning.get_session_learner')
    def test_learning_patterns(self, mock_learner_fn):
        mock_learner = MagicMock()
        mock_learner.get_patterns.return_value = {"sequences": [], "total": 0}
        mock_learner_fn.return_value = mock_learner

        from whitemagic.tools.handlers.learning import handle_learning_patterns
        result = handle_learning_patterns()
        self.assertEqual(result["status"], "success")

    def test_learning_suggest_missing_tool(self):
        from whitemagic.tools.handlers.learning import handle_learning_suggest
        result = handle_learning_suggest()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.core.learning.get_session_learner')
    def test_learning_suggest_success(self, mock_learner_fn):
        mock_learner = MagicMock()
        mock_learner.suggest_next.return_value = ["search_memories", "create_memory"]
        mock_learner_fn.return_value = mock_learner

        from whitemagic.tools.handlers.learning import handle_learning_suggest
        result = handle_learning_suggest(current_tool="gnosis")
        self.assertEqual(result["status"], "success")
        self.assertEqual(len(result["suggestions"]), 2)

    @patch('whitemagic.core.learning.get_session_learner')
    def test_learning_status(self, mock_learner_fn):
        mock_learner = MagicMock()
        mock_learner.status.return_value = {"sessions": 5, "patterns": 12}
        mock_learner_fn.return_value = mock_learner

        from whitemagic.tools.handlers.learning import handle_learning_status
        result = handle_learning_status()
        self.assertEqual(result["status"], "success")


# =========================================================================
# Edge inference handlers
# =========================================================================

class TestEdgeHandlers(unittest.TestCase):

    def test_edge_infer_missing_query(self):
        from whitemagic.tools.handlers.edge import handle_edge_infer
        result = handle_edge_infer()
        self.assertEqual(result["status"], "error")
        self.assertIn("query", result["message"])

    @patch('whitemagic.edge.inference.get_edge_inference')
    def test_edge_infer_success(self, mock_edge_fn):
        mock_engine = MagicMock()
        mock_result = MagicMock()
        mock_result.query = "what is python"
        mock_result.answer = "A programming language"
        mock_result.confidence = 0.95
        mock_result.method = "rule"
        mock_result.latency_ms = 1.2
        mock_result.tokens_equivalent = 100
        mock_result.from_cache = False
        mock_engine.infer.return_value = mock_result
        mock_edge_fn.return_value = mock_engine

        from whitemagic.tools.handlers.edge import handle_edge_infer
        result = handle_edge_infer(query="what is python")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["confidence"], 0.95)

    def test_edge_add_rule_missing_fields(self):
        from whitemagic.tools.handlers.edge import handle_edge_add_rule
        result = handle_edge_add_rule()
        self.assertEqual(result["status"], "error")

    def test_edge_batch_infer_empty_queries(self):
        from whitemagic.tools.handlers.edge import handle_edge_batch_infer
        result = handle_edge_batch_infer(queries=[])
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.edge.inference.get_edge_inference')
    def test_edge_stats(self, mock_edge_fn):
        mock_engine = MagicMock()
        mock_engine.stats = {"queries": 100, "cache_hits": 42}
        mock_edge_fn.return_value = mock_engine

        from whitemagic.tools.handlers.edge import handle_edge_stats
        result = handle_edge_stats()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["queries"], 100)


# =========================================================================
# Gan Ying handlers
# =========================================================================

class TestGanYingHandlers(unittest.TestCase):

    @patch('whitemagic.tools.handlers.ganying._emit')
    def test_ganying_emit(self, mock_emit):
        from whitemagic.tools.handlers.ganying import handle_ganying_emit
        result = handle_ganying_emit(event_type="TEST_EVENT", data={"key": "val"})
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["event_emitted"], "TEST_EVENT")
        mock_emit.assert_called_once_with("TEST_EVENT", {"key": "val"})

    @patch('whitemagic.core.resonance.gan_ying.get_bus')
    def test_ganying_history(self, mock_bus_fn):
        mock_bus = MagicMock()
        mock_bus.get_history.return_value = []
        mock_bus_fn.return_value = mock_bus

        from whitemagic.tools.handlers.ganying import handle_ganying_history
        result = handle_ganying_history(limit=10)
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["count"], 0)

    @patch('whitemagic.core.resonance.gan_ying.get_bus')
    def test_ganying_listeners(self, mock_bus_fn):
        mock_bus = MagicMock()
        mock_bus._listeners = {}
        mock_bus_fn.return_value = mock_bus

        from whitemagic.tools.handlers.ganying import handle_ganying_listeners
        result = handle_ganying_listeners()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["total"], 0)


# =========================================================================
# Context Optimizer handlers
# =========================================================================

class TestContextOptimizerHandlers(unittest.TestCase):

    def test_context_pack_missing_query(self):
        from whitemagic.tools.handlers.context_optimizer import handle_context_pack
        result = handle_context_pack()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.ai.context_optimizer.get_context_optimizer')
    def test_context_pack_success(self, mock_opt_fn):
        mock_opt = MagicMock()
        mock_packed = MagicMock()
        mock_packed.total_tokens = 500
        mock_packed.budget = 8000
        mock_packed.utilization = 0.0625
        mock_packed.items = []
        mock_packed.dropped_count = 0
        mock_packed.strategy = "relevance"
        mock_opt.pack_memories.return_value = mock_packed
        mock_opt.render.return_value = "packed context"
        mock_opt_fn.return_value = mock_opt

        from whitemagic.tools.handlers.context_optimizer import handle_context_pack
        result = handle_context_pack(query="test query", token_budget=4000)
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["total_tokens"], 500)

    @patch('whitemagic.ai.context_optimizer.get_context_optimizer')
    def test_context_status(self, mock_opt_fn):
        mock_opt = MagicMock()
        mock_opt.status.return_value = {"ready": True, "memories": 100}
        mock_opt_fn.return_value = mock_opt

        from whitemagic.tools.handlers.context_optimizer import handle_context_status
        result = handle_context_status()
        self.assertEqual(result["status"], "success")


# =========================================================================
# Synthesis handlers
# =========================================================================

class TestSynthesisHandlers(unittest.TestCase):

    def test_kaizen_apply_fixes_invalid_fix_ids(self):
        from whitemagic.tools.handlers.synthesis import handle_kaizen_apply_fixes
        result = handle_kaizen_apply_fixes(fix_ids="not_a_list")
        self.assertEqual(result["status"], "error")

    def test_kaizen_apply_fixes_non_string_ids(self):
        from whitemagic.tools.handlers.synthesis import handle_kaizen_apply_fixes
        result = handle_kaizen_apply_fixes(fix_ids=[1, 2, 3])
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.core.intelligence.synthesis.kaizen_engine.get_kaizen_engine')
    def test_kaizen_apply_fixes_dry_run(self, mock_engine_fn):
        mock_engine = MagicMock()
        mock_report = MagicMock()
        mock_report.proposals = []
        mock_engine.analyze.return_value = mock_report
        mock_engine_fn.return_value = mock_engine

        from whitemagic.tools.handlers.synthesis import handle_kaizen_apply_fixes
        result = handle_kaizen_apply_fixes(fix_ids=["fix_1"], dry_run=True)
        self.assertEqual(result["status"], "success")
        self.assertTrue(result["dry_run"])
        self.assertIn("fix_1", result["missing"])


if __name__ == "__main__":
    unittest.main()
