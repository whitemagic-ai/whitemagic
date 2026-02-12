"""
Tests for Synthesis Gaps & Missing Subsystems (v12.3)
======================================================
Covers:
  C2: Registry domain split + auto-collection
  B1: Explain This pre-execution impact preview
  B2: Agent trust scores from Karma Ledger
  B3: Cross-node mesh awareness
  A1: Salience Arbiter ↔ Homeostatic Loop coupling
  A2: Dependency Graph ↔ Pipeline Engine validation
  A3: Consolidation ↔ Bicameral Reasoner creative clustering
  A4: Galactic Map zone distribution → Harmony Vector energy
"""

import pytest
from unittest.mock import patch, MagicMock


# =========================================================================
# C2: Registry domain split + auto-collection
# =========================================================================

class TestRegistryDomainSplit:
    def test_tool_types_importable(self):
        from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety
        assert ToolCategory.GANA.value == "gana"
        assert ToolSafety.READ.value == "read"

    def test_backward_compat_imports(self):
        from whitemagic.tools.registry import ToolCategory, ToolDefinition, ToolSafety
        assert ToolCategory.GANA.value == "gana"

    def test_registry_defs_collect(self):
        from whitemagic.tools.registry_defs import collect
        tools = collect()
        assert len(tools) >= 28  # At least GANA tools
        gana_names = [t.name for t in tools if t.category.value == "gana"]
        assert "gana_horn" in gana_names
        assert "gana_wall" in gana_names

    def test_gana_tools_in_registry(self):
        from whitemagic.tools.registry import TOOL_REGISTRY, ToolCategory
        gana = [t for t in TOOL_REGISTRY if t.category == ToolCategory.GANA]
        assert len(gana) == 28

    def test_gana_tool_schema(self):
        from whitemagic.tools.registry import get_tool
        horn = get_tool("gana_horn")
        assert horn is not None
        assert horn.safety.value == "write"
        assert "operation" in horn.input_schema["properties"]

    def test_total_tool_count_unchanged(self):
        from whitemagic.tools.registry import TOOL_REGISTRY
        # v12.5: consolidated 207→~179 tools via multi-action fusion
        assert len(TOOL_REGISTRY) >= 170


# =========================================================================
# B1: Explain This
# =========================================================================

class TestExplainThis:
    def test_explain_tool_basic(self):
        from whitemagic.tools.explain_this import explain_tool
        result = explain_tool("create_memory", title="test")
        assert result["tool"] == "create_memory"
        assert result["status"] == "preview"
        assert "recommendation" in result
        assert result["recommendation"]["verdict"] in (
            "SAFE_TO_PROCEED", "PROCEED_WITH_CAUTION", "BLOCKED"
        )

    def test_explain_tool_metadata(self):
        from whitemagic.tools.explain_this import explain_tool
        result = explain_tool("harmony_vector")
        meta = result["metadata"]
        assert meta["category"] == "metrics"
        assert meta["safety"] == "read"

    def test_explain_unknown_tool(self):
        from whitemagic.tools.explain_this import explain_tool
        result = explain_tool("nonexistent_tool_xyz")
        assert result["tool"] == "nonexistent_tool_xyz"
        # Should still return a preview, just with unknown metadata
        assert result["metadata"]["category"] == "unknown"

    def test_explain_resource_estimate(self):
        from whitemagic.tools.explain_this import explain_tool
        result = explain_tool("create_memory")
        est = result["resource_estimate"]
        assert est["writes"] is True

    def test_explain_dependency_info(self):
        from whitemagic.tools.explain_this import explain_tool
        result = explain_tool("search_memory")
        deps = result["dependencies"]
        assert "prerequisites" in deps
        assert "suggested_next" in deps

    def test_explain_karma_forecast(self):
        from whitemagic.tools.explain_this import explain_tool
        result = explain_tool("create_memory")
        karma = result["karma_forecast"]
        assert "declared_safety" in karma

    def test_explain_dispatch_handler(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        assert "explain_this" in DISPATCH_TABLE

    def test_explain_registry_entry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("explain_this")
        assert tool is not None
        assert tool.safety.value == "read"
        assert "target_tool" in tool.input_schema["properties"]


# =========================================================================
# B2: Agent Trust Scores
# =========================================================================

class TestAgentTrust:
    def test_get_trust_scores_empty(self):
        from whitemagic.tools.agent_trust import get_agent_trust_scores
        result = get_agent_trust_scores()
        assert result["status"] == "success"

    def test_trust_tier_mapping(self):
        from whitemagic.tools.agent_trust import _trust_tier
        assert _trust_tier(0.96) == "EXEMPLARY"
        assert _trust_tier(0.90) == "TRUSTED"
        assert _trust_tier(0.75) == "STANDARD"
        assert _trust_tier(0.55) == "PROBATIONARY"
        assert _trust_tier(0.30) == "RESTRICTED"

    def test_agent_trust_dispatch(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        assert "agent.trust" in DISPATCH_TABLE

    def test_agent_trust_registry(self):
        from whitemagic.tools.registry import get_tool
        tool = get_tool("agent.trust")
        assert tool is not None
        assert tool.category.value == "agent"

    def test_agent_trust_handler(self):
        from whitemagic.tools.handlers.agent_ergonomics import handle_agent_trust
        result = handle_agent_trust()
        assert result["status"] == "success"


# =========================================================================
# B3: Cross-Node Mesh Awareness
# =========================================================================

class TestMeshAwareness:
    def test_mesh_awareness_singleton(self):
        from whitemagic.mesh.awareness import get_mesh_awareness
        aw1 = get_mesh_awareness()
        aw2 = get_mesh_awareness()
        assert aw1 is aw2

    def test_register_peer(self):
        from whitemagic.mesh.awareness import MeshAwareness
        aw = MeshAwareness()
        aw.register_peer("node-1", "192.168.1.5:50051")
        peers = aw.get_peers()
        assert len(peers) == 1
        assert peers[0]["node_id"] == "node-1"

    def test_process_redis_message_peer_discovered(self):
        import json
        from whitemagic.mesh.awareness import MeshAwareness
        aw = MeshAwareness()
        aw.process_redis_message(json.dumps({
            "type": "PEER_DISCOVERED",
            "node_id": "go-mesh-1",
            "address": "10.0.0.1:50051",
        }))
        peers = aw.get_peers()
        assert len(peers) == 1
        assert peers[0]["node_id"] == "go-mesh-1"

    def test_process_redis_message_peer_left(self):
        import json
        from whitemagic.mesh.awareness import MeshAwareness
        aw = MeshAwareness()
        aw.register_peer("node-x", "addr")
        aw.process_redis_message(json.dumps({
            "type": "PEER_LEFT",
            "node_id": "node-x",
        }))
        assert len(aw.get_peers()) == 0

    def test_mesh_status(self):
        from whitemagic.mesh.awareness import MeshAwareness
        aw = MeshAwareness()
        s = aw.status()
        assert "peer_count" in s
        assert "client" in s

    def test_mesh_dispatch_entries(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        assert "mesh.status" in DISPATCH_TABLE
        assert "mesh.broadcast" in DISPATCH_TABLE

    def test_mesh_registry_entries(self):
        from whitemagic.tools.registry import get_tool
        assert get_tool("mesh.status") is not None
        assert get_tool("mesh.broadcast") is not None


# =========================================================================
# A1: Salience Arbiter ↔ Homeostatic Loop
# =========================================================================

class TestSalienceHomeostaticCoupling:
    def test_salience_urgency_boost_no_events(self):
        from whitemagic.harmony.homeostatic_loop import HomeostaticLoop
        loop = HomeostaticLoop()
        boost = loop._salience_urgency_boost()
        assert boost == 0.0

    def test_emit_to_arbiter(self):
        from whitemagic.harmony.homeostatic_loop import (
            HomeostaticLoop, HomeostaticAction, ActionLevel,
        )
        loop = HomeostaticLoop()
        action = HomeostaticAction(
            dimension="error_rate", level=ActionLevel.CORRECT,
            value=0.3, threshold=0.4, action_taken="test",
        )
        # Should not raise
        loop._emit_to_arbiter(action)

    def test_check_uses_salience_boost(self):
        from whitemagic.harmony.homeostatic_loop import HomeostaticLoop
        loop = HomeostaticLoop()
        # check() should not raise even without harmony vector
        actions = loop.check()
        assert isinstance(actions, list)


# =========================================================================
# A2: Dependency Graph ↔ Pipeline Validation
# =========================================================================

class TestDepGraphPipelineValidation:
    def test_validate_empty_pipeline(self):
        from whitemagic.tools.handlers.pipeline import _validate_pipeline_deps
        result = _validate_pipeline_deps([])
        assert result["valid"] is True

    def test_validate_single_step(self):
        from whitemagic.tools.handlers.pipeline import _validate_pipeline_deps
        result = _validate_pipeline_deps([{"tool": "search_memory"}])
        assert result["valid"] is True

    def test_validate_suggests_follow_up(self):
        from whitemagic.tools.handlers.pipeline import _validate_pipeline_deps
        result = _validate_pipeline_deps([
            {"tool": "create_memory"},
            {"tool": "search_memory"},
        ])
        # create_memory -> search_memory is a suggested chain
        assert "suggestions" in result

    def test_validate_returns_suggestions(self):
        from whitemagic.tools.handlers.pipeline import _validate_pipeline_deps
        result = _validate_pipeline_deps([
            {"tool": "memory.consolidate"},
        ])
        # memory.consolidate suggests memory.consolidation_stats
        suggestions = result.get("suggestions", [])
        assert isinstance(suggestions, list)


# =========================================================================
# A3: Consolidation ↔ Bicameral Reasoner
# =========================================================================

class TestConsolidationBicameral:
    def test_bicameral_enrich_exists(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        c = MemoryConsolidator()
        assert hasattr(c, "_bicameral_enrich")

    def test_bicameral_enrich_no_clusters(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator, MemoryCluster
        c = MemoryConsolidator()
        strategies = []
        # Should be a no-op with fewer than 2 clusters
        c._bicameral_enrich([MemoryCluster(
            cluster_id="x", memory_ids=["m1"], shared_tags={"a"},
            avg_importance=0.5, total_access_count=1,
            avg_emotional_valence=0.5, theme="test",
        )], strategies)
        assert len(strategies) == 0

    def test_bicameral_enrich_finds_logical_links(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator, MemoryCluster
        c = MemoryConsolidator()
        strategies = []
        c._bicameral_enrich([
            MemoryCluster("c1", ["m1"], {"shared_tag", "a"}, 0.5, 5, 0.3, "theme_a"),
            MemoryCluster("c2", ["m2"], {"shared_tag", "b"}, 0.8, 3, 0.4, "theme_b"),
        ], strategies)
        logical = [s for s in strategies if s.get("type") == "logical_bridge"]
        assert len(logical) >= 1

    def test_bicameral_enrich_finds_creative_links(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator, MemoryCluster
        c = MemoryConsolidator()
        strategies = []
        c._bicameral_enrich([
            MemoryCluster("c1", ["m1"], {"tag_a"}, 0.9, 5, 0.8, "theme_a"),
            MemoryCluster("c2", ["m2"], {"tag_b"}, 0.3, 3, 0.7, "theme_b"),
        ], strategies)
        # Should find creative bridges (emotional affinity or importance contrast)
        creative = [s for s in strategies if s.get("type") == "creative_bridge"]
        assert len(creative) >= 1


# =========================================================================
# A4: Galactic Map → Harmony Vector energy
# =========================================================================

class TestGalacticHarmonyCoupling:
    def test_galactic_vitality_method_exists(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        assert hasattr(hv, "_galactic_vitality")

    def test_galactic_vitality_returns_float(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        v = hv._galactic_vitality()
        assert isinstance(v, float)
        assert 0.0 <= v <= 1.0

    def test_galactic_map_get_zone_counts(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()
        assert hasattr(gmap, "get_zone_counts")
        counts = gmap.get_zone_counts()
        assert isinstance(counts, dict)

    def test_energy_blends_galactic(self):
        """Energy dimension should blend runtime and galactic signals."""
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        # Record a call so _compute runs
        snap = hv.record_call("test_tool", 0.1, True)
        # Energy should be a valid score
        assert 0.0 <= snap.energy <= 1.0


# =========================================================================
# Middleware Pipeline (C1 - regression)
# =========================================================================

class TestMiddlewarePipeline:
    def test_pipeline_importable(self):
        from whitemagic.tools.middleware import DispatchPipeline, DispatchContext
        p = DispatchPipeline()
        assert p is not None

    def test_pipeline_describe(self):
        from whitemagic.tools.dispatch_table import get_pipeline
        p = get_pipeline()
        stages = p.describe()
        assert "input_sanitizer" in stages
        assert "core_router" in stages
