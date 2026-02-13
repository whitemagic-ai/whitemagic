"""Tests for memory reconsolidation engine."""


class TestLabileMemory:
    """Test the LabileMemory dataclass."""

    def test_is_expired_fresh(self):
        from whitemagic.core.intelligence.reconsolidation import LabileMemory

        lm = LabileMemory(memory_id="1", original_content="test", original_tags=["a"])
        assert not lm.is_expired

    def test_age_seconds(self):
        from whitemagic.core.intelligence.reconsolidation import LabileMemory

        lm = LabileMemory(memory_id="1", original_content="test", original_tags=[])
        assert lm.age_seconds >= 0
        assert lm.age_seconds < 2


class TestReconsolidationEngine:
    """Test the reconsolidation engine."""

    def test_mark_labile(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        lm = engine.mark_labile("mem1", content="Hello", tags=["greeting"], query="hi")
        assert lm.memory_id == "mem1"
        assert engine.is_labile("mem1")

    def test_mark_labile_refreshes(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        engine.mark_labile("mem1", content="Hello", tags=["a"], query="first")
        lm2 = engine.mark_labile("mem1", content="Hello", tags=["a"], query="second")
        assert lm2.query_context == "second"

    def test_update_labile(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        engine.mark_labile("mem1", content="Hello", tags=["a"])
        assert engine.update_labile("mem1", new_context="Extra info", new_tags=["b"])
        assert not engine.update_labile("nonexistent", new_context="Nope")

    def test_reconsolidate_with_updates(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        engine.mark_labile("mem1", content="Original", tags=["a"])
        engine.update_labile("mem1", new_context="New info", new_tags=["updated"])
        result = engine.reconsolidate("mem1")
        assert result is not None
        assert result["action"] == "reconsolidated"
        assert result["content_additions"] == 1
        assert "updated" in result["tag_additions"]
        assert not engine.is_labile("mem1")

    def test_reconsolidate_no_updates(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        engine.mark_labile("mem1", content="Original", tags=["a"])
        result = engine.reconsolidate("mem1")
        assert result["action"] == "no_changes"

    def test_reconsolidate_all(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        engine.mark_labile("a", content="A", tags=["x"])
        engine.mark_labile("b", content="B", tags=["y"])
        engine.update_labile("a", new_context="Updated A")
        results = engine.reconsolidate_all()
        assert len(results) == 2

    def test_get_labile_ids(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        engine.mark_labile("a", content="A", tags=[])
        engine.mark_labile("b", content="B", tags=[])
        ids = engine.get_labile_ids()
        assert "a" in ids
        assert "b" in ids

    def test_max_labile_eviction(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine, MAX_LABILE

        engine = ReconsolidationEngine()
        for i in range(MAX_LABILE + 5):
            engine.mark_labile(f"mem_{i}", content=f"Content {i}", tags=[])
        assert len(engine.get_labile_ids()) <= MAX_LABILE

    def test_get_status(self):
        from whitemagic.core.intelligence.reconsolidation import ReconsolidationEngine

        engine = ReconsolidationEngine()
        engine.mark_labile("a", content="A", tags=[])
        status = engine.get_status()
        assert status["labile_count"] == 1
        assert "total_marked" in status

    def test_singleton(self):
        from whitemagic.core.intelligence.reconsolidation import get_reconsolidation_engine

        e1 = get_reconsolidation_engine()
        e2 = get_reconsolidation_engine()
        assert e1 is e2
