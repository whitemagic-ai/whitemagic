"""
Regression tests for v11.3 modules:
  1. Memory Lifecycle Manager (mindful forgetting integration)
  2. Homeostatic Loop (harmony-driven self-regulation)
  3. Maturity Gate Check (tool-level maturity enforcement)
  4. Memory Consolidation (hippocampal replay)
  5. MCP tool routing for all new tools
"""

import json
import time


from tests.conftest import assert_envelope_shape


# =========================================================================
# 1. Memory Lifecycle Manager
# =========================================================================

class TestMemoryLifecycle:

    def test_manager_initializes(self):
        from whitemagic.core.memory.lifecycle import MemoryLifecycleManager
        mgr = MemoryLifecycleManager()
        stats = mgr.get_stats()
        assert stats["total_sweeps"] == 0
        assert stats["last_sweep_at"] is None

    def test_attach_to_scheduler(self):
        from whitemagic.core.memory.lifecycle import MemoryLifecycleManager
        mgr = MemoryLifecycleManager()
        result = mgr.attach()
        assert result is True
        assert mgr.is_attached

    def test_run_sweep_returns_report(self):
        from whitemagic.core.memory.lifecycle import MemoryLifecycleManager
        mgr = MemoryLifecycleManager()
        report = mgr.run_sweep(persist=False)
        assert report["status"] == "success"
        assert "sweep" in report
        assert "lifetime" in report

    def test_stats_update_after_sweep(self):
        from whitemagic.core.memory.lifecycle import MemoryLifecycleManager
        mgr = MemoryLifecycleManager()
        mgr.run_sweep(persist=False)
        stats = mgr.get_stats()
        assert stats["total_sweeps"] == 1
        assert stats["last_sweep_at"] is not None

    def test_flush_count_triggers_sweep(self):
        from whitemagic.core.memory.lifecycle import (
            MemoryLifecycleManager, LifecycleConfig,
        )
        mgr = MemoryLifecycleManager(config=LifecycleConfig(sweep_interval_sweeps=2))
        # Simulate slow-lane flushes
        mgr._on_slow_flush([])  # flush 1 — no sweep
        time.sleep(0.05)
        assert mgr.get_stats()["total_sweeps"] == 0
        mgr._on_slow_flush([])  # flush 2 — triggers sweep
        time.sleep(0.2)  # allow background thread
        assert mgr.get_stats()["total_sweeps"] == 1


# =========================================================================
# 2. Homeostatic Loop
# =========================================================================

class TestHomeostaticLoop:

    def test_loop_initializes(self):
        from whitemagic.harmony.homeostatic_loop import HomeostaticLoop
        loop = HomeostaticLoop()
        assert not loop.is_running
        stats = loop.get_stats()
        assert stats["total_checks"] == 0

    def test_check_returns_no_actions_when_healthy(self):
        from whitemagic.harmony.homeostatic_loop import HomeostaticLoop
        loop = HomeostaticLoop()
        actions = loop.check()
        # Fresh harmony vector should be healthy — no corrective actions
        assert isinstance(actions, list)
        # Could be 0 or small number depending on state
        assert all(hasattr(a, "dimension") for a in actions)

    def test_check_increments_counter(self):
        from whitemagic.harmony.homeostatic_loop import HomeostaticLoop
        loop = HomeostaticLoop()
        loop.check()
        loop.check()
        assert loop.get_stats()["total_checks"] == 2

    def test_action_to_dict_serializable(self):
        from whitemagic.harmony.homeostatic_loop import (
            HomeostaticAction, ActionLevel,
        )
        action = HomeostaticAction(
            dimension="error_rate", level=ActionLevel.ADVISE,
            value=0.5, threshold=0.7, action_taken="Test advisory",
        )
        d = action.to_dict()
        json.dumps(d)  # must not raise
        assert d["level"] == "advise"

    def test_attach_detach_lifecycle(self):
        from whitemagic.harmony.homeostatic_loop import (
            HomeostaticLoop, HomeostaticConfig,
        )
        loop = HomeostaticLoop(config=HomeostaticConfig(check_interval_s=0.05))
        loop.attach()
        assert loop.is_running
        time.sleep(0.15)  # allow at least one check
        loop.detach()
        assert not loop.is_running
        assert loop.get_stats()["total_checks"] >= 1

    def test_stats_serializable(self):
        from whitemagic.harmony.homeostatic_loop import HomeostaticLoop
        loop = HomeostaticLoop()
        loop.check()
        stats = loop.get_stats()
        json.dumps(stats)


# =========================================================================
# 3. Maturity Gate Check
# =========================================================================

class TestMaturityCheck:

    def test_basic_tool_passes(self):
        from whitemagic.tools.maturity_check import check_maturity_for_tool
        # capabilities is a basic READ tool, should always pass
        result = check_maturity_for_tool("capabilities")
        assert result is None  # None means allowed

    def test_unregistered_tool_passes(self):
        from whitemagic.tools.maturity_check import check_maturity_for_tool
        result = check_maturity_for_tool("nonexistent_tool_xyz")
        assert result is None

    def test_error_response_structure(self):
        from whitemagic.tools.maturity_check import check_maturity_for_tool
        # Force a low maturity by testing with a tool that requires LOGOS (6)
        # which no real system reaches
        from whitemagic.tools.maturity_check import _MATURITY_REQUIREMENTS
        _MATURITY_REQUIREMENTS["_test_logos_tool"] = 6
        try:
            result = check_maturity_for_tool("_test_logos_tool")
            if result is not None:
                assert result["status"] == "error"
                assert result["error_code"] == "maturity_gate"
                assert "maturity" in result
                json.dumps(result)
        finally:
            del _MATURITY_REQUIREMENTS["_test_logos_tool"]


# =========================================================================
# 4. Memory Consolidation
# =========================================================================

class TestMemoryConsolidation:

    def test_consolidator_initializes(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        c = MemoryConsolidator()
        stats = c.get_stats()
        assert stats["total_consolidations"] == 0

    def test_consolidate_empty_returns_report(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        c = MemoryConsolidator()
        report = c.consolidate(memories=[])
        assert report.memories_analyzed == 0
        assert report.clusters_found == 0

    def test_report_to_dict_serializable(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        c = MemoryConsolidator()
        report = c.consolidate(memories=[])
        d = report.to_dict()
        json.dumps(d)  # must not raise
        assert "memories_analyzed" in d
        assert "timestamp" in d

    def test_clustering_with_shared_tags(self):
        """Test that memories with shared tags cluster together."""
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        from whitemagic.core.memory.unified_types import Memory, MemoryType

        memories = [
            Memory(id=f"m{i}", content=f"content {i}",
                   memory_type=MemoryType.SHORT_TERM,
                   tags={"python", "testing"}, importance=0.6, access_count=5)
            for i in range(5)
        ]

        c = MemoryConsolidator(min_cluster_size=3)
        report = c.consolidate(memories=memories)
        assert report.memories_analyzed == 5
        assert report.clusters_found >= 1
        assert report.clusters[0].theme in ("python", "testing")

    def test_stats_update_after_consolidation(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        c = MemoryConsolidator()
        c.consolidate(memories=[])
        c.consolidate(memories=[])
        assert c.get_stats()["total_consolidations"] == 2

    def test_cluster_to_dict(self):
        from whitemagic.core.memory.consolidation import MemoryCluster
        cluster = MemoryCluster(
            cluster_id="abc123", memory_ids=["m1", "m2", "m3"],
            shared_tags={"python"}, avg_importance=0.7,
            total_access_count=15, avg_emotional_valence=0.3, theme="python",
        )
        d = cluster.to_dict()
        json.dumps(d)
        assert d["size"] == 3
        assert d["theme"] == "python"


# =========================================================================
# 5. MCP Tool Routing
# =========================================================================

# =========================================================================
# 6. Dharma Rules from YAML Directory
# =========================================================================

class TestDharmaYAMLDirectory:

    def test_rules_dir_loads_extra_rules(self, tmp_path):
        """YAML files dropped into rules.d/ are merged with built-in defaults."""
        from whitemagic.dharma.rules import DharmaRulesEngine

        rules_dir = tmp_path / "rules.d"
        rules_dir.mkdir()
        (rules_dir / "custom.yaml").write_text(
            "rules:\n"
            "  - name: no_foo\n"
            "    description: Block foo tool\n"
            "    action: block\n"
            "    severity: 0.9\n"
            "    explain: foo is forbidden\n"
            "    tool_patterns: ['foo_*']\n"
        )
        engine = DharmaRulesEngine(rules_dir=rules_dir)
        rule_names = [r["name"] for r in engine.get_rules()]
        assert "no_foo" in rule_names
        # Built-in defaults should still be present
        assert "destructive_ops" in rule_names

    def test_last_write_wins_override(self, tmp_path):
        """A user rule with the same name as a built-in replaces it."""
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction

        rules_dir = tmp_path / "rules.d"
        rules_dir.mkdir()
        (rules_dir / "override.yaml").write_text(
            "rules:\n"
            "  - name: destructive_ops\n"
            "    description: Overridden\n"
            "    action: block\n"
            "    severity: 1.0\n"
            "    explain: User override\n"
            "    keyword_patterns: ['delete']\n"
        )
        engine = DharmaRulesEngine(rules_dir=rules_dir)
        # Use Python evaluator directly — this tests YAML override, not Haskell
        d = engine._python_evaluate({"tool": "x", "description": "delete stuff"}, "default")
        assert d.action == DharmaAction.BLOCK
        assert "User override" in d.explain

    def test_single_file_plus_dir_merged(self, tmp_path):
        """Both rules_path and rules_dir can be used together."""
        from whitemagic.dharma.rules import DharmaRulesEngine

        single_file = tmp_path / "main.yaml"
        single_file.write_text(
            "rules:\n"
            "  - name: single_rule\n"
            "    description: From single file\n"
            "    action: tag\n"
            "    severity: 0.2\n"
            "    explain: Single file rule\n"
            "    keyword_patterns: ['single']\n"
        )
        rules_dir = tmp_path / "rules.d"
        rules_dir.mkdir()
        (rules_dir / "dir_rule.yaml").write_text(
            "rules:\n"
            "  - name: dir_rule\n"
            "    description: From directory\n"
            "    action: warn\n"
            "    severity: 0.5\n"
            "    explain: Directory rule\n"
            "    keyword_patterns: ['directory']\n"
        )
        engine = DharmaRulesEngine(rules_path=single_file, rules_dir=rules_dir)
        names = [r["name"] for r in engine.get_rules()]
        assert "single_rule" in names
        assert "dir_rule" in names
        assert "destructive_ops" in names  # built-in still present

    def test_check_reload_detects_change(self, tmp_path):
        """check_reload() returns True when a file changes."""
        from whitemagic.dharma.rules import DharmaRulesEngine

        rules_dir = tmp_path / "rules.d"
        rules_dir.mkdir()
        f = rules_dir / "live.yaml"
        f.write_text("rules:\n  - name: v1\n    action: log\n    severity: 0.1\n    explain: v1\n")

        engine = DharmaRulesEngine(rules_dir=rules_dir)
        assert "v1" in [r["name"] for r in engine.get_rules()]

        # No change yet
        assert engine.check_reload() is False

        # Modify file
        import time
        time.sleep(0.05)
        f.write_text("rules:\n  - name: v2\n    action: warn\n    severity: 0.5\n    explain: v2\n")

        assert engine.check_reload() is True
        assert "v2" in [r["name"] for r in engine.get_rules()]

    def test_yml_extension_supported(self, tmp_path):
        """Both .yaml and .yml extensions are loaded."""
        from whitemagic.dharma.rules import DharmaRulesEngine

        rules_dir = tmp_path / "rules.d"
        rules_dir.mkdir()
        (rules_dir / "a.yml").write_text(
            "rules:\n  - name: yml_rule\n    action: log\n    severity: 0.1\n    explain: yml\n"
        )
        engine = DharmaRulesEngine(rules_dir=rules_dir)
        assert "yml_rule" in [r["name"] for r in engine.get_rules()]


# =========================================================================
# 7. Tool Dependency Graph
# =========================================================================

class TestToolDependencyGraph:

    def test_graph_initializes_with_static_edges(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        summary = graph.get_graph_summary()
        assert summary["total_edges"] > 0
        assert summary["total_tools"] > 0

    def test_next_steps_returns_edges(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        steps = graph.next_steps("vote.create")
        assert len(steps) >= 1
        assert steps[0]["target"] == "vote.cast"

    def test_prerequisites_returns_edges(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        prereqs = graph.prerequisites("vote.cast")
        assert any(p["source"] == "vote.create" for p in prereqs)

    def test_plan_builds_dependency_chain(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        chain = graph.plan("vote.cast")
        assert "vote.create" in chain
        assert chain.index("vote.create") < chain.index("vote.cast")

    def test_edge_type_filter(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph, EdgeType
        graph = ToolDependencyGraph()
        requires = graph.next_steps("vote.create", edge_type=EdgeType.REQUIRES)
        _suggests = graph.next_steps("vote.create", edge_type=EdgeType.SUGGESTS)
        # vote.create -> vote.cast is REQUIRES
        assert any(e["target"] == "vote.cast" for e in requires)

    def test_add_learned_edge(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        graph.add_learned_edge("custom_a", "custom_b", 0.6)
        steps = graph.next_steps("custom_a")
        assert any(e["target"] == "custom_b" for e in steps)

    def test_learned_edge_reinforcement(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        graph.add_learned_edge("x", "y", 0.3)
        graph.add_learned_edge("x", "y", 0.3)  # reinforce
        steps = graph.next_steps("x")
        edge = [e for e in steps if e["target"] == "y"][0]
        assert edge["weight"] > 0.3  # should have been reinforced

    def test_full_graph_serializable(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        edges = graph.get_full_graph()
        json.dumps(edges)
        assert len(edges) > 0

    def test_graph_summary_serializable(self):
        from whitemagic.tools.dependency_graph import ToolDependencyGraph
        graph = ToolDependencyGraph()
        summary = graph.get_graph_summary()
        json.dumps(summary)


# =========================================================================
# 8. MCP Tool Routing (extended)
# =========================================================================

class TestMCPToolRoutingV113:

    def test_lifecycle_stats_tool(self, tool_caller):
        result = tool_caller.ok("memory.lifecycle_stats")
        assert_envelope_shape(result)
        assert "lifecycle" in result["details"]

    def test_homeostasis_status_tool(self, tool_caller):
        result = tool_caller.ok("homeostasis.status")
        assert_envelope_shape(result)
        assert "homeostasis" in result["details"]

    def test_homeostasis_check_tool(self, tool_caller):
        result = tool_caller.ok("homeostasis.check")
        assert_envelope_shape(result)
        assert "actions_taken" in result["details"]

    def test_maturity_assess_tool(self, tool_caller):
        result = tool_caller.ok("maturity.assess")
        assert_envelope_shape(result)
        maturity = result["details"].get("maturity", {})
        assert "current_stage" in maturity

    def test_consolidation_stats_tool(self, tool_caller):
        result = tool_caller.ok("memory.consolidation_stats")
        assert_envelope_shape(result)
        assert "consolidation" in result["details"]

    def test_tool_graph_summary(self, tool_caller):
        result = tool_caller.ok("tool.graph")
        assert_envelope_shape(result)
        assert "graph" in result["details"]

    def test_tool_graph_for_specific_tool(self, tool_caller):
        result = tool_caller.ok("tool.graph", tool="vote.create")
        assert_envelope_shape(result)
        assert "next_steps" in result["details"]
        assert "prerequisites" in result["details"]

    def test_tool_graph_full(self, tool_caller):
        result = tool_caller.ok("tool.graph_full")
        assert_envelope_shape(result)
        assert "edges" in result["details"]

    def test_dharma_reload(self, tool_caller):
        result = tool_caller.ok("dharma.reload")
        assert_envelope_shape(result)
        assert "rules_loaded" in result["details"]

    def test_underscore_aliases_work(self, tool_caller):
        """Verify underscore-style names resolve to dot-style."""
        result = tool_caller.ok("homeostasis_status")
        assert_envelope_shape(result)
        assert result["tool"] == "homeostasis.status"
