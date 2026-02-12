"""Tests for Bhīṣma Governance Proposals (Mahābhārata 12.108).

Tests the five systems built from Bhīṣma's gaṇa governance advice:
1. Gana Vitality — Performance reputation + silence detection
2. Gana Sabha — Cross-quadrant council protocol
3. Gana Forge — Declarative extension protocol
4. Grimoire Unification — 28-mansion chapter alignment
5. Resonance Integration — Vitality in resonance context
"""

import json
import tempfile
from pathlib import Path


# ═══════════════════════════════════════════════════════════════════
# 1. Gana Vitality Tests (12.108.20 + 12.108.29)
# ═══════════════════════════════════════════════════════════════════

class TestGanaVitality:
    """Test per-Gana health monitoring and performance reputation."""

    def test_import(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        monitor = GanaVitalityMonitor()
        assert monitor is not None

    def test_record_success(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        m.record_call("gana_horn", success=True, latency_ms=50.0)
        rep = m.get_reputation("gana_horn")
        assert rep["total_calls"] == 1
        assert rep["total_successes"] == 1
        assert rep["total_failures"] == 0
        assert rep["success_rate"] == 1.0
        assert rep["vitality"] == "healthy"

    def test_record_failure(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        m.record_call("gana_ghost", success=False, latency_ms=100.0)
        rep = m.get_reputation("gana_ghost")
        assert rep["total_failures"] == 1
        assert rep["consecutive_failures"] == 1

    def test_consecutive_failures_degrade(self):
        """12.108.29: When people stop speaking, defeat shows."""
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        for _ in range(5):
            m.record_call("gana_root", success=False)
        rep = m.get_reputation("gana_root")
        assert rep["vitality"] == "degraded"
        assert rep["is_degraded"] is True

    def test_success_resets_consecutive_failures(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        for _ in range(4):
            m.record_call("gana_heart", success=False)
        m.record_call("gana_heart", success=True)
        rep = m.get_reputation("gana_heart")
        assert rep["consecutive_failures"] == 0
        assert rep["vitality"] != "degraded"

    def test_latency_tracking(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        m.record_call("gana_tail", success=True, latency_ms=100.0)
        m.record_call("gana_tail", success=True, latency_ms=200.0)
        rep = m.get_reputation("gana_tail")
        assert rep["avg_latency_ms"] == 150.0

    def test_should_route_around_degraded(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        for _ in range(5):
            m.record_call("gana_star", success=False)
        assert m.should_route_around("gana_star") is True
        assert m.should_route_around("gana_horn") is False

    def test_get_degraded_ganas(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        for _ in range(5):
            m.record_call("gana_ox", success=False)
        degraded = m.get_degraded_ganas()
        assert "gana_ox" in degraded

    def test_vitality_summary(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        m.record_call("gana_horn", success=True)
        m.record_call("gana_neck", success=True)
        for _ in range(5):
            m.record_call("gana_root", success=False)
        summary = m.get_vitality_summary()
        assert summary["total_ganas_tracked"] == 3
        assert summary["healthy"] >= 2
        assert summary["degraded"] >= 1

    def test_reset(self):
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        m.record_call("gana_horn", success=True)
        m.reset()
        assert m.get_all_reputations() == {}

    def test_singleton(self):
        from whitemagic.tools.gana_vitality import get_vitality_monitor
        m1 = get_vitality_monitor()
        m2 = get_vitality_monitor()
        assert m1 is m2

    def test_unused_gana_not_silent(self):
        """A never-called Gana should not be considered 'silent'."""
        from whitemagic.tools.gana_vitality import GanaVitalityMonitor
        m = GanaVitalityMonitor()
        rep = m.get_reputation("gana_void")
        assert rep["is_silent"] is False
        assert rep["vitality"] == "healthy"


# ═══════════════════════════════════════════════════════════════════
# 2. Gana Sabha Tests (12.108.25)
# ═══════════════════════════════════════════════════════════════════

class TestGanaSabha:
    """Test cross-quadrant council protocol."""

    def test_import(self):
        from whitemagic.tools.gana_sabha import convene_sabha
        assert convene_sabha is not None

    def test_convene_full_council(self):
        from whitemagic.tools.gana_sabha import convene_sabha
        result = convene_sabha(task="Design a new memory architecture")
        assert result["status"] == "success"
        assert result["sabha_type"] == "full_council"
        assert len(result["quadrants_represented"]) == 4
        assert len(result["perspectives"]) >= 4
        assert "recommendation" in result

    def test_convene_partial_council(self):
        from whitemagic.tools.gana_sabha import convene_sabha
        result = convene_sabha(task="Search optimization", quadrants=["East", "South"])
        assert result["sabha_type"] == "partial_council"
        assert set(result["quadrants_represented"]) == {"East", "South"}

    def test_convene_with_specific_ganas(self):
        from whitemagic.tools.gana_sabha import convene_sabha
        result = convene_sabha(
            task="Security review",
            ganas=["gana_room", "gana_wall", "gana_three_stars"]
        )
        assert result["status"] == "success"
        gana_names = [p["gana"] for p in result["perspectives"]]
        assert "gana_three_stars" in gana_names

    def test_east_west_tension_detected(self):
        """12.108.27: quarrels ignored by elders metastasize."""
        from whitemagic.tools.gana_sabha import convene_sabha
        result = convene_sabha(
            task="Should we ship fast or review carefully?",
            quadrants=["East", "West"]
        )
        conflicts = result["conflicts"]
        pace_tensions = [c for c in conflicts if c["type"] == "pace_tension"]
        assert len(pace_tensions) >= 1

    def test_south_north_tension_detected(self):
        from whitemagic.tools.gana_sabha import convene_sabha
        result = convene_sabha(
            task="Expand features vs conserve resources",
            quadrants=["South", "North"]
        )
        conflicts = result["conflicts"]
        scope_tensions = [c for c in conflicts if c["type"] == "scope_tension"]
        assert len(scope_tensions) >= 1

    def test_should_convene_sabha(self):
        from whitemagic.tools.gana_sabha import should_convene_sabha
        assert should_convene_sabha(["gana_horn", "gana_ghost"]) is True  # East + South
        assert should_convene_sabha(["gana_horn", "gana_neck"]) is False  # both East

    def test_handler_convene(self):
        from whitemagic.tools.gana_sabha import handle_sabha_convene
        result = handle_sabha_convene(task="Test task")
        assert result["status"] == "success"

    def test_handler_convene_requires_task(self):
        from whitemagic.tools.gana_sabha import handle_sabha_convene
        result = handle_sabha_convene()
        assert result["status"] == "error"

    def test_handler_status(self):
        from whitemagic.tools.gana_sabha import handle_sabha_status
        result = handle_sabha_status()
        assert result["status"] in ("success", "no_data")

    def test_principle_in_result(self):
        from whitemagic.tools.gana_sabha import convene_sabha
        result = convene_sabha(task="Test")
        assert "saṃghātam" in result.get("principle", "")


# ═══════════════════════════════════════════════════════════════════
# 3. Gana Forge Tests (12.108.17)
# ═══════════════════════════════════════════════════════════════════

class TestGanaForge:
    """Test declarative tool extension protocol."""

    def test_import(self):
        from whitemagic.tools.gana_forge import load_extensions
        assert load_extensions is not None

    def test_discover_empty_dir(self):
        from whitemagic.tools.gana_forge import discover_extensions
        with tempfile.TemporaryDirectory() as td:
            result = discover_extensions(Path(td))
            assert result == []

    def test_discover_nonexistent_dir(self):
        from whitemagic.tools.gana_forge import discover_extensions
        result = discover_extensions(Path("/nonexistent"))
        assert result == []

    def test_validate_valid_manifest(self):
        from whitemagic.tools.gana_forge import _validate_manifest
        manifest = {
            "tool": {
                "name": "custom.test",
                "description": "A test tool",
                "gana": "gana_ghost",
                "safety": "read",
                "handler": "builtin:echo",
            }
        }
        errors = _validate_manifest(manifest, Path("test.yaml"))
        assert errors == []

    def test_validate_missing_name(self):
        from whitemagic.tools.gana_forge import _validate_manifest
        manifest = {"tool": {"description": "No name", "gana": "gana_ghost", "handler": "x:y"}}
        errors = _validate_manifest(manifest, Path("test.yaml"))
        assert any("name" in e for e in errors)

    def test_validate_invalid_gana(self):
        from whitemagic.tools.gana_forge import _validate_manifest
        manifest = {
            "tool": {
                "name": "x",
                "description": "y",
                "gana": "gana_nonexistent",
                "handler": "x:y",
            }
        }
        errors = _validate_manifest(manifest, Path("test.yaml"))
        assert any("gana" in e.lower() for e in errors)

    def test_resolve_builtin_echo(self):
        from whitemagic.tools.gana_forge import _resolve_handler
        handler = _resolve_handler("builtin:echo")
        assert handler is not None
        result = handler(test="hello")
        assert result["echo"]["test"] == "hello"

    def test_load_json_manifest(self):
        from whitemagic.tools.gana_forge import discover_extensions
        with tempfile.TemporaryDirectory() as td:
            manifest = {
                "tool": {
                    "name": "custom.test_json",
                    "description": "JSON test",
                    "gana": "gana_ghost",
                    "safety": "read",
                    "handler": "builtin:echo",
                }
            }
            (Path(td) / "test.json").write_text(json.dumps(manifest))
            results = discover_extensions(Path(td))
            assert len(results) == 1

    def test_handler_forge_status(self):
        from whitemagic.tools.gana_forge import handle_forge_status
        result = handle_forge_status()
        assert result["status"] == "success"
        assert "extensions_dir" in result

    def test_handler_forge_validate(self):
        from whitemagic.tools.gana_forge import handle_forge_validate
        result = handle_forge_validate()
        assert result["status"] == "success"

    def test_handler_forge_reload(self):
        from whitemagic.tools.gana_forge import handle_forge_reload
        result = handle_forge_reload()
        assert result["status"] == "success"


# ═══════════════════════════════════════════════════════════════════
# 4. Grimoire Unification Tests (bheda elimination)
# ═══════════════════════════════════════════════════════════════════

class TestGrimoireUnification:
    """Test that all grimoire systems now speak with one voice."""

    def test_chapters_count_28(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        assert len(idx.CHAPTERS) == 28

    def test_chapters_numbered_1_to_28(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        numbers = [ch.number for ch in idx.CHAPTERS]
        assert numbers == list(range(1, 29))

    def test_every_chapter_has_gana(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        for ch in idx.CHAPTERS:
            assert ch.gana.startswith("gana_"), f"Chapter {ch.number} missing gana"

    def test_every_chapter_has_garden(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        for ch in idx.CHAPTERS:
            assert ch.garden, f"Chapter {ch.number} missing garden"

    def test_every_chapter_has_quadrant(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        valid_quadrants = {"East", "South", "West", "North"}
        for ch in idx.CHAPTERS:
            assert ch.quadrant in valid_quadrants, f"Chapter {ch.number} bad quadrant"

    def test_eastern_quadrant_chapters_1_to_7(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        for ch in idx.CHAPTERS[:7]:
            assert ch.quadrant == "East"

    def test_southern_quadrant_chapters_8_to_14(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        for ch in idx.CHAPTERS[7:14]:
            assert ch.quadrant == "South"

    def test_western_quadrant_chapters_15_to_21(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        for ch in idx.CHAPTERS[14:21]:
            assert ch.quadrant == "West"

    def test_northern_quadrant_chapters_22_to_28(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        for ch in idx.CHAPTERS[21:28]:
            assert ch.quadrant == "North"

    def test_gana_names_match_prat_resonance(self):
        """Verify chapters use the same gana names as prat_resonance._GANA_META."""
        from whitemagic.grimoire.chapters import ChapterIndex
        from whitemagic.tools.prat_resonance import _GANA_META
        idx = ChapterIndex()
        chapter_ganas = {ch.gana for ch in idx.CHAPTERS}
        meta_ganas = set(_GANA_META.keys())
        assert chapter_ganas == meta_ganas

    def test_garden_names_match_prat_resonance(self):
        """Verify chapters use the same garden names as prat_resonance._GANA_META."""
        from whitemagic.grimoire.chapters import ChapterIndex
        from whitemagic.tools.prat_resonance import _GANA_META
        idx = ChapterIndex()
        for ch in idx.CHAPTERS:
            meta = _GANA_META.get(ch.gana)
            assert meta is not None, f"Chapter {ch.number} gana '{ch.gana}' not in _GANA_META"
            expected_garden = meta[3].lower()
            assert ch.garden.lower() == expected_garden.lower(), (
                f"Chapter {ch.number} ({ch.gana}): garden '{ch.garden}' != "
                f"expected '{expected_garden}' from _GANA_META"
            )

    def test_by_gana_lookup(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        ch = idx._by_gana.get("gana_horn")
        assert ch is not None
        assert ch.number == 1

    def test_find_by_keyword(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        results = idx.find_by_keyword("memory")
        assert len(results) >= 1
        assert any(ch.number == 2 for ch in results)

    def test_find_for_task(self):
        from whitemagic.grimoire.chapters import ChapterIndex
        idx = ChapterIndex()
        results = idx.find_for_task("search for memories about rust performance")
        assert len(results) >= 1

    def test_spells_use_28_chapter_numbers(self):
        from whitemagic.grimoire.spells import SpellBook
        sb = SpellBook()
        for spell in sb.list_all():
            assert 1 <= spell.chapter <= 28, (
                f"Spell '{spell.name}' has chapter {spell.chapter}, expected 1-28"
            )

    def test_spells_cover_all_quadrants(self):
        from whitemagic.grimoire.spells import SpellBook
        sb = SpellBook()
        chapters = {spell.chapter for spell in sb.list_all()}
        # Eastern (1-7), Southern (8-14), Western (15-21), Northern (22-28)
        assert chapters & set(range(1, 8)), "No Eastern quadrant spells"
        assert chapters & set(range(8, 15)), "No Southern quadrant spells"
        assert chapters & set(range(15, 22)), "No Western quadrant spells"
        assert chapters & set(range(22, 29)), "No Northern quadrant spells"

    def test_core_grimoire_domains_28_chapters(self):
        from whitemagic.grimoire.core import Grimoire
        g = Grimoire()
        assert len(g.CHAPTER_DOMAINS) == 28
        for ch_num in range(1, 29):
            assert ch_num in g.CHAPTER_DOMAINS, f"Chapter {ch_num} missing from Grimoire.CHAPTER_DOMAINS"


# ═══════════════════════════════════════════════════════════════════
# 5. Dispatch + PRAT Integration Tests
# ═══════════════════════════════════════════════════════════════════

class TestDispatchIntegration:
    """Test that new tools are properly wired into dispatch and PRAT."""

    def test_sabha_in_dispatch_table(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        assert "sabha.convene" in DISPATCH_TABLE
        assert "sabha.status" in DISPATCH_TABLE

    def test_forge_in_dispatch_table(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        assert "forge.status" in DISPATCH_TABLE
        assert "forge.reload" in DISPATCH_TABLE
        assert "forge.validate" in DISPATCH_TABLE

    def test_sabha_in_prat_router(self):
        from whitemagic.tools.prat_router import TOOL_TO_GANA
        assert TOOL_TO_GANA.get("sabha.convene") == "gana_three_stars"
        assert TOOL_TO_GANA.get("sabha.status") == "gana_three_stars"

    def test_forge_in_prat_router(self):
        from whitemagic.tools.prat_router import TOOL_TO_GANA
        assert TOOL_TO_GANA.get("forge.status") == "gana_star"
        assert TOOL_TO_GANA.get("forge.reload") == "gana_star"
        assert TOOL_TO_GANA.get("forge.validate") == "gana_star"

    def test_sabha_in_gana_to_tools(self):
        from whitemagic.tools.prat_router import GANA_TO_TOOLS
        assert "sabha.convene" in GANA_TO_TOOLS.get("gana_three_stars", [])
        assert "sabha.status" in GANA_TO_TOOLS.get("gana_three_stars", [])

    def test_forge_in_gana_to_tools(self):
        from whitemagic.tools.prat_router import GANA_TO_TOOLS
        assert "forge.status" in GANA_TO_TOOLS.get("gana_star", [])

    def test_registry_defs_governance(self):
        from whitemagic.tools.registry_defs.governance import TOOLS
        names = {t.name for t in TOOLS}
        assert "sabha.convene" in names
        assert "sabha.status" in names
        assert "forge.status" in names
        assert "forge.reload" in names
        assert "forge.validate" in names

    def test_dispatch_table_handlers_callable(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        for tool_name in ["sabha.convene", "sabha.status", "forge.status", "forge.reload", "forge.validate"]:
            handler = DISPATCH_TABLE[tool_name]
            assert callable(handler), f"{tool_name} handler is not callable"
