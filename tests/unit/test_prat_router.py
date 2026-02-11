"""
Tests for PRAT Router — Polymorphic Resonant Adaptive Tools
============================================================
Validates the 28-Gana nesting of 175+ tools.
"""

import pytest
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../.."))

from whitemagic.tools.prat_router import (
    TOOL_TO_GANA,
    GANA_TO_TOOLS,
    get_gana_for_tool,
    get_tools_for_gana,
    build_prat_description,
    build_prat_schema,
    route_prat_call,
    validate_mapping,
)


class TestPRATMapping:
    """Test the tool-to-Gana mapping."""

    def test_all_28_ganas_have_tools(self):
        """Every Gana should have at least one nested tool."""
        gana_names = [
            "gana_horn", "gana_neck", "gana_root", "gana_room",
            "gana_heart", "gana_tail", "gana_winnowing_basket",
            "gana_ghost", "gana_willow", "gana_star", "gana_extended_net",
            "gana_wings", "gana_chariot", "gana_abundance",
            "gana_straddling_legs", "gana_mound", "gana_stomach",
            "gana_hairy_head", "gana_net", "gana_turtle_beak", "gana_three_stars",
            "gana_dipper", "gana_ox", "gana_girl", "gana_void",
            "gana_roof", "gana_encampment", "gana_wall",
        ]
        for gana in gana_names:
            tools = get_tools_for_gana(gana)
            assert len(tools) > 0, f"{gana} has no nested tools"

    def test_mapping_coverage(self):
        """All non-Gana tools should be mapped."""
        from whitemagic.tools.registry import TOOL_REGISTRY
        result = validate_mapping(TOOL_REGISTRY)
        assert result["unmapped"] == [], f"Unmapped tools: {result['unmapped']}"
        assert result["orphaned"] == [], f"Orphaned mappings: {result['orphaned']}"

    def test_no_duplicate_mappings(self):
        """Each tool maps to exactly one Gana."""
        seen = {}
        for tool, gana in TOOL_TO_GANA.items():
            assert tool not in seen, f"{tool} mapped to both {seen[tool]} and {gana}"
            seen[tool] = gana

    def test_gana_tools_not_in_mapping(self):
        """Gana tools themselves should not be in the mapping."""
        for tool in TOOL_TO_GANA:
            assert not tool.startswith("gana_"), f"Gana tool {tool} shouldn't be in TOOL_TO_GANA"

    def test_get_gana_for_tool(self):
        """Lookup should work for known tools."""
        assert get_gana_for_tool("gnosis") == "gana_ghost"
        assert get_gana_for_tool("create_memory") == "gana_neck"
        assert get_gana_for_tool("session_bootstrap") == "gana_horn"
        assert get_gana_for_tool("nonexistent_tool") is None

    def test_get_tools_for_gana(self):
        """Reverse lookup should list nested tools."""
        ghost_tools = get_tools_for_gana("gana_ghost")
        assert "gnosis" in ghost_tools
        assert "capabilities" in ghost_tools
        assert len(ghost_tools) >= 5

    def test_total_mapped_tools(self):
        """Should map all 147 non-Gana tools."""
        assert len(TOOL_TO_GANA) >= 140  # At least 140 tools mapped


class TestPRATSchema:
    """Test PRAT schema generation."""

    def test_build_prat_description(self):
        """Description should list nested tools."""
        desc = build_prat_description("gana_ghost", "Introspection & Metric Tracking")
        assert "Nested tools" in desc
        assert "gnosis" in desc
        assert "tool='<name>'" in desc

    def test_build_prat_schema_has_tool_enum(self):
        """Schema should include tool enum."""
        from whitemagic.tools.registry import TOOL_REGISTRY
        schema = build_prat_schema("gana_ghost", TOOL_REGISTRY)
        assert "tool" in schema["properties"]
        tool_prop = schema["properties"]["tool"]
        assert "enum" in tool_prop
        assert "gnosis" in tool_prop["enum"]

    def test_build_prat_schema_has_args(self):
        """Schema should include args object."""
        from whitemagic.tools.registry import TOOL_REGISTRY
        schema = build_prat_schema("gana_ghost", TOOL_REGISTRY)
        assert "args" in schema["properties"]
        assert schema["properties"]["args"]["type"] == "object"

    def test_build_prat_schema_has_operation(self):
        """Schema should include polymorphic operation."""
        from whitemagic.tools.registry import TOOL_REGISTRY
        schema = build_prat_schema("gana_ghost", TOOL_REGISTRY)
        assert "operation" in schema["properties"]
        assert "enum" in schema["properties"]["operation"]


class TestPRATRouting:
    """Test PRAT call routing."""

    def test_route_to_specific_tool(self):
        """Should route to specific tool via call_tool."""
        result = route_prat_call("gana_ghost", tool="gnosis", args={"compact": True})
        assert result.get("status") in ("success", "ok")

    def test_route_wrong_gana_gives_error(self):
        """Should error when tool doesn't belong to the called Gana."""
        result = route_prat_call("gana_horn", tool="gnosis", args={})
        assert result["status"] == "error"
        assert "gana_ghost" in result["hint"]

    def test_native_operation_without_tool(self):
        """Should return native Gana operation when no tool specified."""
        result = route_prat_call("gana_ghost", operation="search")
        assert result["status"] == "ok"
        assert result["gana"] == "gana_ghost"
        assert "available_tools" in result

    def test_route_health_report(self):
        """Should route health_report through gana_root."""
        result = route_prat_call("gana_root", tool="health_report", args={})
        assert result.get("status") in ("success", "ok")

    def test_route_create_memory(self):
        """Should route create_memory through gana_neck."""
        result = route_prat_call(
            "gana_neck",
            tool="create_memory",
            args={"title": "PRAT test", "content": "test", "tags": ["prat"]},
        )
        # May succeed or fail depending on backend state, but shouldn't crash
        assert "status" in result

    def test_route_with_empty_args(self):
        """Should handle empty args gracefully."""
        result = route_prat_call("gana_root", tool="rust_status", args={})
        assert "status" in result

    def test_native_lists_available_tools(self):
        """Native operation should list what tools are available."""
        result = route_prat_call("gana_winnowing_basket")
        assert "available_tools" in result
        assert "search_memories" in result["available_tools"]


class TestPRATMCPIntegration:
    """Test PRAT mode in the MCP server registration."""

    def test_prat_registers_28_tools(self):
        """PRAT mode should register exactly 28 tools."""
        os.environ["WM_MCP_PRAT"] = "1"
        try:
            from whitemagic.run_mcp import mcp, _register_prat_tools
            count = _register_prat_tools("")
            assert count == 28
        finally:
            os.environ.pop("WM_MCP_PRAT", None)


class TestPRATQuadrantCoverage:
    """Test that all four quadrants are represented."""

    def test_eastern_quadrant(self):
        """Eastern quadrant Ganas should all have tools."""
        eastern = ["gana_horn", "gana_neck", "gana_root", "gana_room",
                    "gana_heart", "gana_tail", "gana_winnowing_basket"]
        for g in eastern:
            assert len(get_tools_for_gana(g)) > 0

    def test_southern_quadrant(self):
        """Southern quadrant Ganas should all have tools."""
        southern = ["gana_ghost", "gana_willow", "gana_star", "gana_extended_net",
                     "gana_wings", "gana_chariot", "gana_abundance"]
        for g in southern:
            assert len(get_tools_for_gana(g)) > 0

    def test_western_quadrant(self):
        """Western quadrant Ganas should all have tools."""
        western = ["gana_straddling_legs", "gana_mound", "gana_stomach",
                    "gana_hairy_head", "gana_net", "gana_turtle_beak", "gana_three_stars"]
        for g in western:
            assert len(get_tools_for_gana(g)) > 0

    def test_northern_quadrant(self):
        """Northern quadrant Ganas should all have tools."""
        northern = ["gana_dipper", "gana_ox", "gana_girl", "gana_void",
                     "gana_roof", "gana_encampment", "gana_wall"]
        for g in northern:
            assert len(get_tools_for_gana(g)) > 0
