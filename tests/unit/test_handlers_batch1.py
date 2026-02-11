"""
Test expansion batch 1 — coverage for untested handler modules:
dharma, governor, memory, scratchpad, sandbox, knowledge_graph, grimoire, swarm.

Uses mocks for subsystem singletons to isolate handler logic.
"""
import unittest
from unittest.mock import MagicMock, patch, PropertyMock
from typing import Dict, Any


# =========================================================================
# Dharma handlers
# =========================================================================

class TestDharmaHandlers(unittest.TestCase):

    @patch('whitemagic.dharma.karma_ledger.get_karma_ledger')
    def test_karma_report(self, mock_ledger_fn):
        mock_ledger = MagicMock()
        mock_ledger.report.return_value = {"entries": [], "total": 0}
        mock_ledger_fn.return_value = mock_ledger

        from whitemagic.tools.handlers.dharma import handle_karma_report
        result = handle_karma_report(limit=10)
        self.assertEqual(result["status"], "success")
        self.assertIn("karma", result)
        mock_ledger.report.assert_called_once_with(limit=10)

    @patch('whitemagic.dharma.rules.get_rules_engine')
    def test_karmic_trace(self, mock_engine_fn):
        mock_engine = MagicMock()
        mock_engine.get_karmic_trace.return_value = [{"rule": "test", "action": "LOG"}]
        mock_engine_fn.return_value = mock_engine

        from whitemagic.tools.handlers.dharma import handle_karmic_trace
        result = handle_karmic_trace(limit=5)
        self.assertEqual(result["status"], "success")
        self.assertIn("trace", result)

    @patch('whitemagic.dharma.rules.get_rules_engine')
    def test_dharma_rules(self, mock_engine_fn):
        mock_engine = MagicMock()
        mock_engine.get_profile.return_value = "default"
        mock_engine.get_rules.return_value = [{"name": "no_harm"}]
        mock_engine_fn.return_value = mock_engine

        from whitemagic.tools.handlers.dharma import handle_dharma_rules
        result = handle_dharma_rules()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["active_profile"], "default")
        self.assertIn("rules", result)

    @patch('whitemagic.dharma.rules.get_rules_engine')
    def test_set_dharma_profile(self, mock_engine_fn):
        mock_engine = MagicMock()
        mock_engine.get_profile.return_value = "secure"
        mock_engine_fn.return_value = mock_engine

        from whitemagic.tools.handlers.dharma import handle_set_dharma_profile
        result = handle_set_dharma_profile(profile="secure")
        self.assertEqual(result["status"], "success")
        self.assertIn("secure", result["message"])
        mock_engine.set_profile.assert_called_once_with("secure")

    @patch('whitemagic.dharma.karma_ledger.get_karma_ledger')
    def test_karma_verify_chain(self, mock_ledger_fn):
        mock_ledger = MagicMock()
        mock_ledger.verify_chain.return_value = {"valid": True, "entries": 42}
        mock_ledger_fn.return_value = mock_ledger

        from whitemagic.tools.handlers.dharma import handle_karma_verify_chain
        result = handle_karma_verify_chain()
        self.assertEqual(result["status"], "success")
        self.assertTrue(result["valid"])


# =========================================================================
# Governor handlers
# =========================================================================

class TestGovernorHandlers(unittest.TestCase):

    @patch('whitemagic.core.governor.get_governor')
    def test_governor_validate(self, mock_gov_fn):
        mock_gov = MagicMock()
        mock_result = MagicMock()
        mock_result.to_dict.return_value = {"allowed": True, "reason": "ok"}
        mock_gov.validate_command.return_value = mock_result
        mock_gov_fn.return_value = mock_gov

        from whitemagic.tools.handlers.governor import handle_governor_validate
        result = handle_governor_validate(command="ls -la")
        self.assertEqual(result["status"], "success")
        self.assertTrue(result["allowed"])

    @patch('whitemagic.core.governor.get_governor')
    def test_governor_set_goal(self, mock_gov_fn):
        mock_gov = MagicMock()
        mock_gov_fn.return_value = mock_gov

        from whitemagic.tools.handlers.governor import handle_governor_set_goal
        result = handle_governor_set_goal(goal="explore memory")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["goal_set"], "explore memory")
        mock_gov.set_goal.assert_called_once_with("explore memory")

    @patch('whitemagic.core.governor.get_governor')
    def test_governor_check_drift(self, mock_gov_fn):
        mock_gov = MagicMock()
        mock_result = MagicMock()
        mock_result.to_dict.return_value = {"drifted": False, "distance": 0.1}
        mock_gov.check_drift.return_value = mock_result
        mock_gov_fn.return_value = mock_gov

        from whitemagic.tools.handlers.governor import handle_governor_check_drift
        result = handle_governor_check_drift(action="search_memories", goal="explore")
        self.assertEqual(result["status"], "success")

    @patch('whitemagic.core.governor.get_governor')
    def test_governor_stats(self, mock_gov_fn):
        mock_gov = MagicMock()
        mock_gov.stats.return_value = {"checks": 100, "blocks": 2}
        mock_gov_fn.return_value = mock_gov

        from whitemagic.tools.handlers.governor import handle_governor_stats
        result = handle_governor_stats()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["checks"], 100)


# =========================================================================
# Memory handlers
# =========================================================================

class TestMemoryHandlers(unittest.TestCase):

    def test_create_memory_missing_title(self):
        from whitemagic.tools.handlers.memory import handle_create_memory
        result = handle_create_memory(content="test content")
        self.assertEqual(result["status"], "error")
        self.assertIn("title", result["message"])

    def test_create_memory_missing_content(self):
        from whitemagic.tools.handlers.memory import handle_create_memory
        result = handle_create_memory(title="test title")
        self.assertEqual(result["status"], "error")
        self.assertIn("content", result["message"])

    def test_create_memory_empty_title(self):
        from whitemagic.tools.handlers.memory import handle_create_memory
        result = handle_create_memory(title="   ", content="some content")
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.tools.handlers.memory._emit')
    @patch('whitemagic.core.memory.unified.remember')
    def test_create_memory_success(self, mock_remember, mock_emit):
        mock_mem = MagicMock()
        mock_mem.id = "test-id-123"
        mock_remember.return_value = mock_mem

        from whitemagic.tools.handlers.memory import handle_create_memory
        result = handle_create_memory(
            title="Test Memory",
            content="Test content here",
            tags=["test", "unit"],
            type="short_term",
        )
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["memory_id"], "test-id-123")
        mock_emit.assert_called()

    @patch('whitemagic.core.memory.unified.recall')
    def test_search_memories(self, mock_recall):
        mock_mem = MagicMock()
        mock_mem.id = "mem-1"
        mock_mem.content = "Test memory content"
        mock_recall.return_value = [mock_mem]

        from whitemagic.tools.handlers.memory import handle_search_memories
        result = handle_search_memories(query="test", limit=5)
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["count"], 1)
        mock_recall.assert_called_once_with(query="test", limit=5)

    def test_batch_read_too_many(self):
        """Batch read should reject >50 files."""
        try:
            from whitemagic.tools.handlers.memory import handle_batch_read_memories
            result = handle_batch_read_memories(filenames=["f"] * 51)
            self.assertEqual(result["status"], "error")
        except ModuleNotFoundError:
            # archaeology module not available in test env
            self.skipTest("archaeology module not importable")


# =========================================================================
# Scratchpad handlers
# =========================================================================

class TestScratchpadHandlers(unittest.TestCase):

    def test_unified_scratchpad_invalid_action(self):
        from whitemagic.tools.handlers.scratchpad import handle_scratchpad
        result = handle_scratchpad(action="destroy")
        self.assertEqual(result["status"], "error")
        self.assertIn("destroy", result["message"])

    def test_unified_scratchpad_valid_actions(self):
        from whitemagic.tools.handlers.scratchpad import handle_scratchpad
        dispatch = {"create", "update", "finalize"}
        # Verify the dispatch map contains all expected actions
        result = handle_scratchpad(action="nonexistent")
        for action in dispatch:
            self.assertIn(action, result["message"])


# =========================================================================
# Sandbox handlers
# =========================================================================

class TestSandboxHandlers(unittest.TestCase):

    @patch('whitemagic.tools.sandbox.get_sandbox')
    def test_sandbox_status(self, mock_sandbox_fn):
        mock_sandbox = MagicMock()
        mock_sandbox.status.return_value = {"tools": 10, "violations": 0}
        mock_sandbox_fn.return_value = mock_sandbox

        from whitemagic.tools.handlers.sandbox import handle_sandbox_status
        result = handle_sandbox_status()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["tools"], 10)

    @patch('whitemagic.tools.sandbox.get_sandbox')
    def test_sandbox_violations(self, mock_sandbox_fn):
        mock_sandbox = MagicMock()
        mock_sandbox.get_violations.return_value = [{"tool": "test", "type": "timeout"}]
        mock_sandbox_fn.return_value = mock_sandbox

        from whitemagic.tools.handlers.sandbox import handle_sandbox_violations
        result = handle_sandbox_violations(limit=5)
        self.assertEqual(result["status"], "success")
        self.assertEqual(len(result["violations"]), 1)
        mock_sandbox.get_violations.assert_called_once_with(limit=5)

    def test_sandbox_set_limits_missing_tool(self):
        from whitemagic.tools.handlers.sandbox import handle_sandbox_set_limits
        result = handle_sandbox_set_limits()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.tools.sandbox.get_sandbox')
    def test_sandbox_set_limits_success(self, mock_sandbox_fn):
        mock_sandbox = MagicMock()
        mock_sandbox_fn.return_value = mock_sandbox

        from whitemagic.tools.handlers.sandbox import handle_sandbox_set_limits
        result = handle_sandbox_set_limits(
            target_tool="test_tool",
            timeout_s=60,
            max_memory_mb=256,
        )
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["tool"], "test_tool")
        mock_sandbox.set_limits.assert_called_once()


# =========================================================================
# Knowledge Graph handlers
# =========================================================================

class TestKnowledgeGraphHandlers(unittest.TestCase):

    def test_kg_extract_missing_text(self):
        from whitemagic.tools.handlers.knowledge_graph import handle_kg_extract
        result = handle_kg_extract()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.core.intelligence.knowledge_graph.get_knowledge_graph')
    def test_kg_extract_success(self, mock_kg_fn):
        mock_kg = MagicMock()
        mock_kg.extract_from_text.return_value = {"entities": 3, "relations": 2}
        mock_kg_fn.return_value = mock_kg

        from whitemagic.tools.handlers.knowledge_graph import handle_kg_extract
        result = handle_kg_extract(text="Python is a programming language", source_id="test")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["entities"], 3)

    def test_kg_query_missing_name(self):
        from whitemagic.tools.handlers.knowledge_graph import handle_kg_query
        result = handle_kg_query()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.core.intelligence.knowledge_graph.get_knowledge_graph')
    def test_kg_query_success(self, mock_kg_fn):
        mock_kg = MagicMock()
        mock_kg.query_entity.return_value = {"name": "Python", "relations": []}
        mock_kg_fn.return_value = mock_kg

        from whitemagic.tools.handlers.knowledge_graph import handle_kg_query
        result = handle_kg_query(name="Python")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["name"], "Python")

    @patch('whitemagic.core.intelligence.knowledge_graph.get_knowledge_graph')
    def test_kg_top(self, mock_kg_fn):
        mock_kg = MagicMock()
        mock_kg.top_entities.return_value = [{"name": "Python", "count": 5}]
        mock_kg_fn.return_value = mock_kg

        from whitemagic.tools.handlers.knowledge_graph import handle_kg_top
        result = handle_kg_top(limit=10)
        self.assertEqual(result["status"], "success")
        self.assertEqual(len(result["entities"]), 1)

    @patch('whitemagic.core.intelligence.knowledge_graph.get_knowledge_graph')
    def test_kg_status(self, mock_kg_fn):
        mock_kg = MagicMock()
        mock_kg.status.return_value = {"entities": 100, "relations": 50}
        mock_kg_fn.return_value = mock_kg

        from whitemagic.tools.handlers.knowledge_graph import handle_kg_status
        result = handle_kg_status()
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["entities"], 100)


# =========================================================================
# Grimoire handlers
# =========================================================================

class TestGrimoireHandlers(unittest.TestCase):

    def test_grimoire_suggest_missing_task(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_suggest
        result = handle_grimoire_suggest()
        self.assertEqual(result["status"], "error")
        self.assertIn("task", result["message"])

    def test_grimoire_cast_missing_spell(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_cast
        result = handle_grimoire_cast()
        self.assertEqual(result["status"], "error")
        self.assertIn("spell_name", result["message"])

    def test_grimoire_recommend_missing_task(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_recommend
        result = handle_grimoire_recommend()
        self.assertEqual(result["status"], "error")
        self.assertIn("task", result["message"])

    def test_grimoire_auto_status(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_auto_status
        result = handle_grimoire_auto_status()
        self.assertEqual(result["status"], "success")
        self.assertIn("spells_available", result)
        self.assertIn("mode", result)

    def test_grimoire_suggest_returns_results(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_suggest
        result = handle_grimoire_suggest(task="explore unknown memory patterns")
        self.assertEqual(result["status"], "success")
        self.assertIn("suggestions", result)
        self.assertIn("count", result)

    def test_grimoire_recommend_returns_results(self):
        from whitemagic.tools.handlers.grimoire import handle_grimoire_recommend
        result = handle_grimoire_recommend(task="create and organize memories")
        self.assertEqual(result["status"], "success")
        self.assertIn("recommendations", result)


# =========================================================================
# Swarm handlers
# =========================================================================

class TestSwarmHandlers(unittest.TestCase):

    def test_swarm_decompose_missing_goal(self):
        from whitemagic.tools.handlers.swarm import handle_swarm_decompose
        result = handle_swarm_decompose()
        self.assertEqual(result["status"], "error")

    def test_swarm_route_missing_plan_id(self):
        from whitemagic.tools.handlers.swarm import handle_swarm_route
        result = handle_swarm_route()
        self.assertEqual(result["status"], "error")

    def test_swarm_complete_missing_ids(self):
        from whitemagic.tools.handlers.swarm import handle_swarm_complete
        result = handle_swarm_complete()
        self.assertEqual(result["status"], "error")

    def test_swarm_vote_missing_fields(self):
        from whitemagic.tools.handlers.swarm import handle_swarm_vote
        result = handle_swarm_vote()
        self.assertEqual(result["status"], "error")

    def test_swarm_resolve_missing_topic(self):
        from whitemagic.tools.handlers.swarm import handle_swarm_resolve
        result = handle_swarm_resolve()
        self.assertEqual(result["status"], "error")

    def test_swarm_plan_missing_id(self):
        from whitemagic.tools.handlers.swarm import handle_swarm_plan
        result = handle_swarm_plan()
        self.assertEqual(result["status"], "error")

    @patch('whitemagic.agents.swarm.get_swarm')
    def test_swarm_status(self, mock_swarm_fn):
        mock_swarm = MagicMock()
        mock_swarm.status.return_value = {"plans": 0, "agents": 0, "votes": 0}
        mock_swarm_fn.return_value = mock_swarm

        from whitemagic.tools.handlers.swarm import handle_swarm_status
        result = handle_swarm_status()
        self.assertEqual(result["status"], "success")

    @patch('whitemagic.agents.swarm.get_swarm')
    def test_swarm_decompose_success(self, mock_swarm_fn):
        mock_swarm = MagicMock()
        mock_plan = MagicMock()
        mock_plan.to_dict.return_value = {"plan_id": "p1", "tasks": []}
        mock_swarm.decompose.return_value = mock_plan
        mock_swarm_fn.return_value = mock_swarm

        from whitemagic.tools.handlers.swarm import handle_swarm_decompose
        result = handle_swarm_decompose(goal="build a search engine")
        self.assertEqual(result["status"], "success")
        self.assertEqual(result["plan_id"], "p1")


if __name__ == "__main__":
    unittest.main()
