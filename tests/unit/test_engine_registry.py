"""Tests for Engine Registry (Leap 7d)."""

from whitemagic.core.engines.registry import (
    ENGINE_REGISTRY,
    EngineEntry,
    EngineStatus,
    Quadrant,
    get_engine_entry,
    get_engine_stats,
    get_engines_by_quadrant,
    get_engines_by_status,
)


class TestEngineRegistryStructure:
    def test_exactly_28_engines(self):
        assert len(ENGINE_REGISTRY) == 28

    def test_slots_are_sequential(self):
        for i, engine in enumerate(ENGINE_REGISTRY):
            assert engine.slot == i, f"Engine {engine.engine_name} has slot {engine.slot}, expected {i}"

    def test_all_entries_are_engine_entry(self):
        for engine in ENGINE_REGISTRY:
            assert isinstance(engine, EngineEntry)

    def test_all_gardens_unique(self):
        gardens = [e.garden for e in ENGINE_REGISTRY]
        assert len(gardens) == len(set(gardens)), f"Duplicate gardens: {[g for g in gardens if gardens.count(g) > 1]}"

    def test_all_engine_names_unique(self):
        names = [e.engine_name for e in ENGINE_REGISTRY]
        assert len(names) == len(set(names)), f"Duplicate names: {[n for n in names if names.count(n) > 1]}"

    def test_all_mansion_names_unique(self):
        mansions = [e.mansion_name for e in ENGINE_REGISTRY]
        assert len(mansions) == len(set(mansions))

    def test_grimoire_chapters_are_1_to_28(self):
        chapters = sorted(e.grimoire_chapter for e in ENGINE_REGISTRY)
        assert chapters == list(range(1, 29))

    def test_handler_ids_are_100_to_127(self):
        handlers = sorted(e.handler_id for e in ENGINE_REGISTRY)
        assert handlers == list(range(100, 128))


class TestQuadrantDistribution:
    def test_east_has_7(self):
        assert len(get_engines_by_quadrant(Quadrant.EAST)) == 7

    def test_south_has_7(self):
        assert len(get_engines_by_quadrant(Quadrant.SOUTH)) == 7

    def test_west_has_7(self):
        assert len(get_engines_by_quadrant(Quadrant.WEST)) == 7

    def test_north_has_7(self):
        assert len(get_engines_by_quadrant(Quadrant.NORTH)) == 7

    def test_east_is_wood(self):
        for e in get_engines_by_quadrant(Quadrant.EAST):
            assert e.wu_xing == "wood"

    def test_south_is_fire(self):
        for e in get_engines_by_quadrant(Quadrant.SOUTH):
            assert e.wu_xing == "fire"

    def test_west_is_metal(self):
        for e in get_engines_by_quadrant(Quadrant.WEST):
            assert e.wu_xing == "metal"

    def test_north_is_water(self):
        for e in get_engines_by_quadrant(Quadrant.NORTH):
            assert e.wu_xing == "water"


class TestLookups:
    def test_lookup_by_slot(self):
        e = get_engine_entry(0)
        assert e is not None
        assert e.engine_name == "SessionEngine"

    def test_lookup_by_engine_name(self):
        e = get_engine_entry("ResonanceEngine")
        assert e is not None
        assert e.slot == 13
        assert e.garden == "joy"

    def test_lookup_by_garden(self):
        e = get_engine_entry("wisdom")
        assert e is not None
        assert e.engine_name == "SerendipityEngine"

    def test_lookup_by_mansion(self):
        e = get_engine_entry("Wall")
        assert e is not None
        assert e.engine_name == "EmergenceEngine"
        assert e.slot == 27

    def test_lookup_invalid(self):
        assert get_engine_entry("nonexistent") is None
        assert get_engine_entry(999) is None


class TestEngineEntryProperties:
    def test_handler_id(self):
        e = get_engine_entry(0)
        assert e.handler_id == 100

    def test_season(self):
        east = get_engine_entry(0)
        assert east.season == "spring"
        south = get_engine_entry(7)
        assert south.season == "summer"
        west = get_engine_entry(14)
        assert west.season == "autumn"
        north = get_engine_entry(21)
        assert north.season == "winter"

    def test_celestial_animal(self):
        east = get_engine_entry(0)
        assert "Azure Dragon" in east.celestial_animal
        south = get_engine_entry(7)
        assert "Vermilion Bird" in south.celestial_animal


class TestEngineStatus:
    def test_most_engines_exist(self):
        existing = get_engines_by_status(EngineStatus.EXISTS)
        assert len(existing) >= 27  # All but one are EXISTS

    def test_distributed_engine(self):
        e = get_engine_entry("AccelerationEngine")
        assert e is not None
        assert e.status == EngineStatus.DISTRIBUTED


class TestEngineStats:
    def test_total_is_28(self):
        stats = get_engine_stats()
        assert stats["total_engines"] == 28

    def test_quadrant_distribution(self):
        stats = get_engine_stats()
        by_q = stats["by_quadrant"]
        assert by_q["east"] == 7
        assert by_q["south"] == 7
        assert by_q["west"] == 7
        assert by_q["north"] == 7

    def test_wu_xing_distribution(self):
        stats = get_engine_stats()
        by_wx = stats["by_wu_xing"]
        assert by_wx["wood"] == 7
        assert by_wx["fire"] == 7
        assert by_wx["metal"] == 7
        assert by_wx["water"] == 7


class TestKnownEngines:
    """Verify specific engines are correctly mapped."""

    def test_session_engine(self):
        e = get_engine_entry("SessionEngine")
        assert e.mansion_chinese == "角"
        assert e.garden == "courage"
        assert e.quadrant == Quadrant.EAST

    def test_kaizen_engine(self):
        e = get_engine_entry("KaizenEngine")
        assert e.mansion_chinese == "昴"
        assert e.garden == "presence"
        assert e.quadrant == Quadrant.WEST

    def test_predictive_engine(self):
        e = get_engine_entry("PredictiveEngine")
        assert e.mansion_chinese == "斗"
        assert e.garden == "awe"
        assert e.quadrant == Quadrant.NORTH

    def test_clone_army_engine(self):
        e = get_engine_entry("CloneArmyEngine")
        assert e.mansion_chinese == "女"
        assert e.garden == "wonder"
        assert e.quadrant == Quadrant.NORTH

    def test_resonance_engine(self):
        e = get_engine_entry("ResonanceEngine")
        assert e.mansion_chinese == "豐"
        assert e.garden == "joy"
        assert e.quadrant == Quadrant.SOUTH

    def test_emergence_engine(self):
        e = get_engine_entry("EmergenceEngine")
        assert e.mansion_chinese == "壁"
        assert e.garden == "air"
        assert e.quadrant == Quadrant.NORTH
        assert e.grimoire_chapter == 28

    def test_boundary_engine(self):
        e = get_engine_entry("BoundaryEngine")
        assert e.mansion_chinese == "氐"
        assert e.garden == "truth"
        assert e.quadrant == Quadrant.EAST
