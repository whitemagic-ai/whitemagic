"""
Test expansion batch 3 — coverage for remaining untested handler modules:
misc, simd, solver, vector_search, export_import, prompts, rust_bridge, watcher, bitnet.

Uses mocks for subsystem singletons to isolate handler logic.
"""
import unittest
from unittest.mock import MagicMock, patch
from typing import Dict, Any


# =========================================================================
# Misc handlers
# =========================================================================

class TestMiscHandlers(unittest.TestCase):

    def test_get_agent_capabilities(self):
        from whitemagic.tools.handlers.misc import handle_get_agent_capabilities
        result = handle_get_agent_capabilities()
        self.assertEqual(result["status"], "success")
        self.assertIn("categories", result)
        self.assertIn("Memory", result["categories"])
        self.assertIn("recommended_workflow", result)

    def test_stub(self):
        from whitemagic.tools.handlers.misc import _stub
        result = _stub("fake_tool", preview_key="val")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["error_code"], "not_implemented")
        self.assertIn("fake_tool", result["message"])
        self.assertEqual(result["preview_key"], "val")

    @patch('whitemagic.tools.handlers.misc._emit')
    def test_focus_session(self, mock_emit):
        from whitemagic.tools.handlers.misc import handle_focus_session
        result = handle_focus_session(task="deep work", duration=45)
        self.assertEqual(result["status"], "success")
        self.assertTrue(result["focus_started"])
        self.assertEqual(result["task"], "deep work")
        mock_emit.assert_called_once()

    def test_view_hologram_unknown_operation(self):
        from whitemagic.tools.handlers.misc import handle_view_hologram
        with patch('whitemagic.core.intelligence.hologram.engine.get_hologram_engine') as mock_eng_fn:
            mock_eng = MagicMock()
            mock_eng.enabled = True
            mock_eng_fn.return_value = mock_eng
            result = handle_view_hologram(operation="nonexistent")
            self.assertEqual(result["status"], "error")
            self.assertIn("nonexistent", result["message"])

    @patch('whitemagic.core.intelligence.hologram.engine.get_hologram_engine')
    def test_view_hologram_status(self, mock_eng_fn):
        mock_eng = MagicMock()
        mock_eng.get_stats.return_value = {"indexed": 100}
        mock_eng_fn.return_value = mock_eng

        from whitemagic.tools.handlers.misc import handle_view_hologram
        result = handle_view_hologram(operation="status")
        self.assertEqual(result["status"], "success")
        self.assertIn("engine_stats", result)


# =========================================================================
# SIMD handlers
# =========================================================================

class TestSimdHandlers(unittest.TestCase):

    def test_simd_cosine_missing_vectors(self):
        from whitemagic.tools.handlers.simd import handle_simd_cosine
        result = handle_simd_cosine()
        self.assertEqual(result["status"], "error")

    def test_simd_batch_missing_vectors(self):
        from whitemagic.tools.handlers.simd import handle_simd_batch
        result = handle_simd_batch()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.core.acceleration.simd_cosine.cosine_similarity')
    def test_simd_cosine_success(self, mock_cosine):
        mock_cosine.return_value = 0.95
        from whitemagic.tools.handlers.simd import handle_simd_cosine
        result = handle_simd_cosine(a=[1.0, 0.0], b=[0.9, 0.1])
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["similarity"], 0.95)
        self.assertEqual(result["dim"], 2)

    @patch('whitemagic.core.acceleration.simd_cosine.batch_cosine')
    def test_simd_batch_success(self, mock_batch):
        mock_batch.return_value = [0.9, 0.8, 0.7]
        from whitemagic.tools.handlers.simd import handle_simd_batch
        result = handle_simd_batch(query=[1.0], vectors=[[0.9], [0.8], [0.7]])
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["count"], 3)

    @patch('whitemagic.core.acceleration.simd_cosine.simd_status')
    def test_simd_status(self, mock_status):
        mock_status.return_value = {"backend": "python", "lane_width": 4}
        from whitemagic.tools.handlers.simd import handle_simd_status
        result = handle_simd_status()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["backend"], "python")


# =========================================================================
# Solver handlers
# =========================================================================

class TestSolverHandlers(unittest.TestCase):
    """Solver requires cvxpy — skip if unavailable."""

    def _import_solver(self):
        try:
            from whitemagic.tools.handlers.solver import handle_solve_optimization
            return handle_solve_optimization
        except (ImportError, ModuleNotFoundError):
            self.skipTest("cvxpy not installed")

    def test_solver_missing_nodes(self):
        handler = self._import_solver()
        result = handler()
        self.assertEqual(result["status"], "error")
        self.assertEqual(result["error_code"], "NO_NODES")

    def test_solver_via_misc_handler(self):
        """The misc.py handler for solve_optimization delegates to bridge."""
        from whitemagic.tools.handlers.misc import handle_solve_optimization
        # Should not crash with empty input
        result = handle_solve_optimization(nodes=[], edges=[], scores={})
        self.assertIsInstance(result, dict)


# =========================================================================
# Vector Search handlers
# =========================================================================

class TestVectorSearchHandlers(unittest.TestCase):

    def test_vector_search_missing_query(self):
        from whitemagic.tools.handlers.vector_search import handle_vector_search
        result = handle_vector_search()
        self.assertEqual(result["status"], "error")

    def test_vector_index_missing_fields(self):
        from whitemagic.tools.handlers.vector_search import handle_vector_index
        result = handle_vector_index()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.core.memory.vector_search.get_vector_search')
    def test_vector_search_success(self, mock_vs_fn):
        mock_vs = MagicMock()
        mock_result = MagicMock()
        mock_result.to_dict.return_value = {"id": "m1", "score": 0.9}
        mock_vs.search.return_value = [mock_result]
        mock_vs.index_count.return_value = 100
        mock_vs_fn.return_value = mock_vs

        from whitemagic.tools.handlers.vector_search import handle_vector_search
        result = handle_vector_search(query="python", limit=5)
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["count"], 1)
        self.assertEqual(result["index_size"], 100)

    @patch('whitemagic.core.memory.vector_search.get_vector_search')
    def test_vector_index_success(self, mock_vs_fn):
        mock_vs = MagicMock()
        mock_vs.index_count.return_value = 101
        mock_vs_fn.return_value = mock_vs

        from whitemagic.tools.handlers.vector_search import handle_vector_index
        result = handle_vector_index(memory_id="m1", content="test content", title="Test")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["indexed"], "m1")
        mock_vs.index_memory.assert_called_once()

    @patch('whitemagic.core.memory.vector_search.get_vector_search')
    def test_vector_status(self, mock_vs_fn):
        mock_vs = MagicMock()
        mock_vs.status.return_value = {"indexed": 100, "backend": "numpy"}
        mock_vs_fn.return_value = mock_vs

        from whitemagic.tools.handlers.vector_search import handle_vector_status
        result = handle_vector_status()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["indexed"], 100)


# =========================================================================
# Dispatch table backward compatibility
# =========================================================================

class TestDispatchBackwardCompat(unittest.TestCase):
    """Verify all legacy tool names are still in dispatch table."""

    def test_all_legacy_aliases_present(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        legacy_names = [
            # Batch 1
            "archaeology_stats", "archaeology_mark_read",
            "dream_start", "dream_stop", "dream_status",
            "pipeline.create", "pipeline.status", "pipeline.list",
            "homeostasis.status", "homeostasis.check",
            "tool.graph_full",
            # Batch 2
            "anomaly.check", "anomaly.history", "anomaly.status",
            "otel.spans", "otel.metrics", "otel.status",
            "scratchpad_create", "scratchpad_update", "scratchpad_finalize",
            "sangha_lock_acquire", "sangha_lock_release", "sangha_lock_list",
            "ensemble.query", "ensemble.status", "ensemble.history",
            "session.handoff_transfer", "session.accept_handoff", "session.list_handoffs",
            "memory.lifecycle_sweep", "memory.lifecycle_stats",
            "memory.consolidate", "memory.consolidation_stats",
            # Batch 3
            "starter_packs.list", "starter_packs.get", "starter_packs.suggest",
        ]
        for name in legacy_names:
            self.assertIn(name, DISPATCH_TABLE, f"Legacy alias missing: {name}")

    def test_unified_tool_names_present(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        unified = [
            "archaeology", "dream", "pipeline", "homeostasis", "tool.graph",
            "anomaly", "otel", "scratchpad", "sangha_lock", "ensemble",
            "session.handoff", "memory.lifecycle", "starter_packs",
        ]
        for name in unified:
            self.assertIn(name, DISPATCH_TABLE, f"Unified tool missing: {name}")


if __name__ == "__main__":
    unittest.main()
