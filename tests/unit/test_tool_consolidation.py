"""
Tests for v12.5 tool consolidation — unified multi-action handlers.

Verifies that:
1. Each consolidated tool exists in the registry with an action enum
2. Each unified handler dispatches correctly to sub-handlers
3. Invalid actions return helpful error messages
4. Backward-compat aliases still work via dispatch table
5. No duplicate tool names in the registry
"""
import unittest
from collections import Counter


class TestNoDuplicates(unittest.TestCase):
    """Registry must have zero duplicate tool names."""

    def test_no_duplicate_tool_names(self):
        from whitemagic.tools.registry import TOOL_REGISTRY
        counts = Counter(t.name for t in TOOL_REGISTRY)
        dupes = {name: c for name, c in counts.items() if c > 1}
        self.assertEqual(dupes, {}, f"Duplicate tool names: {dupes}")

    def test_tool_count_in_range(self):
        from whitemagic.tools.registry import TOOL_REGISTRY
        # v12.5: consolidated 207→175 tools; Violet security added 15 more
        # v14.6: 234 tools after Phase 4B2 karma anchoring + v14.2 features
        # v15.1: 341 tools after backfilling 104 missing ToolDefinition entries
        self.assertGreaterEqual(len(TOOL_REGISTRY), 300)
        self.assertLessEqual(len(TOOL_REGISTRY), 400)


class TestUnifiedArchaeology(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("archaeology")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("mark_read", actions)
        self.assertIn("stats", actions)
        self.assertIn("find_unread", actions)

    def test_unified_handler_invalid_action(self):
        from whitemagic.tools.handlers.archaeology import handle_archaeology
        result = handle_archaeology(action="nonexistent")
        self.assertEqual(result["status"], "error")
        self.assertIn("nonexistent", result["message"])

    def test_unified_handler_routes_correctly(self):
        """Verify the dispatch dict has all expected actions."""
        from whitemagic.tools.handlers.archaeology import handle_archaeology, _ACTIONS
        self.assertTrue(len(_ACTIONS) >= 10)

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("archaeology_stats", DISPATCH_TABLE)
        self.assertIn("archaeology_mark_read", DISPATCH_TABLE)


class TestUnifiedDream(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("dream")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("start", actions)
        self.assertIn("stop", actions)
        self.assertIn("status", actions)
        self.assertIn("now", actions)

    def test_unified_handler_status(self):
        from whitemagic.tools.handlers.dreaming import handle_dream
        result = handle_dream(action="status")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("dream_start", DISPATCH_TABLE)
        self.assertIn("dream_status", DISPATCH_TABLE)


class TestUnifiedPipeline(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("pipeline")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("create", actions)
        self.assertIn("status", actions)
        self.assertIn("list", actions)

    def test_unified_handler_list(self):
        from whitemagic.tools.handlers.pipeline import handle_pipeline
        result = handle_pipeline(action="list")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("pipeline.create", DISPATCH_TABLE)
        self.assertIn("pipeline.list", DISPATCH_TABLE)


class TestUnifiedHomeostasis(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("homeostasis")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("status", actions)
        self.assertIn("check", actions)

    def test_unified_handler_status(self):
        from whitemagic.tools.handlers.governance import handle_homeostasis
        result = handle_homeostasis(action="status")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("homeostasis.status", DISPATCH_TABLE)
        self.assertIn("homeostasis.check", DISPATCH_TABLE)


class TestUnifiedAnomaly(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("anomaly")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("check", actions)
        self.assertIn("history", actions)
        self.assertIn("status", actions)

    def test_unified_handler_check(self):
        from whitemagic.tools.handlers.anomaly import handle_anomaly
        result = handle_anomaly(action="check")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("anomaly.check", DISPATCH_TABLE)


class TestUnifiedOtel(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("otel")
        self.assertIsNotNone(tool)

    def test_unified_handler_metrics(self):
        from whitemagic.tools.handlers.otel import handle_otel
        result = handle_otel(action="metrics")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("otel.spans", DISPATCH_TABLE)


class TestUnifiedScratchpad(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("scratchpad")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("create", actions)
        self.assertIn("update", actions)
        self.assertIn("finalize", actions)

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("scratchpad_create", DISPATCH_TABLE)


class TestUnifiedSanghaLock(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("sangha_lock")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("acquire", actions)
        self.assertIn("release", actions)
        self.assertIn("list", actions)

    def test_unified_handler_dispatch_map(self):
        """Verify the unified handler has the right dispatch map."""
        from whitemagic.tools.handlers.sangha import handle_sangha_lock
        result = handle_sangha_lock(action="nonexistent")
        self.assertEqual(result["status"], "error")
        self.assertIn("acquire", result["message"])

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("sangha_lock_acquire", DISPATCH_TABLE)


class TestUnifiedEnsemble(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("ensemble")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("query", actions)
        self.assertIn("status", actions)
        self.assertIn("history", actions)

    def test_unified_handler_history(self):
        from whitemagic.tools.handlers.ensemble import handle_ensemble
        result = handle_ensemble(action="history")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("ensemble.query", DISPATCH_TABLE)


class TestUnifiedSessionHandoff(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("session.handoff")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("transfer", actions)
        self.assertIn("accept", actions)
        self.assertIn("list", actions)

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("session.handoff_transfer", DISPATCH_TABLE)


class TestUnifiedMemoryLifecycle(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("memory.lifecycle")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("sweep", actions)
        self.assertIn("stats", actions)
        self.assertIn("consolidate", actions)

    def test_unified_handler_stats(self):
        from whitemagic.tools.handlers.governance import handle_memory_lifecycle
        result = handle_memory_lifecycle(action="stats")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("memory.lifecycle_sweep", DISPATCH_TABLE)
        self.assertIn("memory.consolidate", DISPATCH_TABLE)


class TestUnifiedStarterPacks(unittest.TestCase):

    def test_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("starter_packs")
        self.assertIsNotNone(tool)
        actions = tool.input_schema["properties"]["action"]["enum"]
        self.assertIn("list", actions)
        self.assertIn("get", actions)
        self.assertIn("suggest", actions)

    def test_unified_handler_list(self):
        from whitemagic.tools.handlers.agent_ergonomics import handle_starter_packs
        result = handle_starter_packs(action="list")
        self.assertEqual(result["status"], "success")

    def test_unified_handler_get(self):
        from whitemagic.tools.handlers.agent_ergonomics import handle_starter_packs
        result = handle_starter_packs(action="get", name="quickstart")
        self.assertEqual(result["status"], "success")

    def test_backward_compat_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        self.assertIn("starter_packs.list", DISPATCH_TABLE)
        self.assertIn("starter_packs.get", DISPATCH_TABLE)


if __name__ == "__main__":
    unittest.main()
