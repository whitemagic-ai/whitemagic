"""Tests for working memory capacity model."""


class TestWorkingChunk:
    """Test WorkingChunk dataclass behavior."""

    def test_effective_activation_fresh(self):
        from whitemagic.core.intelligence.working_memory import WorkingChunk

        chunk = WorkingChunk(memory_id="1", content="test", activation=1.0, importance=0.5)
        # Fresh chunk should have high activation
        assert chunk.effective_activation > 0.9

    def test_rehearse_refreshes(self):
        from whitemagic.core.intelligence.working_memory import WorkingChunk

        chunk = WorkingChunk(memory_id="1", content="test", activation=0.5)
        old_access = chunk.last_accessed
        chunk.rehearse()
        assert chunk.activation == 0.7  # 0.5 + 0.2
        assert chunk.access_count == 2
        assert chunk.last_accessed >= old_access

    def test_to_dict(self):
        from whitemagic.core.intelligence.working_memory import WorkingChunk

        chunk = WorkingChunk(memory_id="1", content="short", title="Title", tags=["a"])
        d = chunk.to_dict()
        assert d["memory_id"] == "1"
        assert d["title"] == "Title"
        assert "activation" in d


class TestWorkingMemory:
    """Test the bounded working memory system."""

    def test_attend_creates_chunk(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=5)
        chunk = wm.attend("mem1", content="Hello world", importance=0.8)
        assert chunk.memory_id == "mem1"
        assert len(wm.get_active_ids()) == 1

    def test_attend_rehearses_existing(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=5)
        wm.attend("mem1", content="Hello")
        chunk = wm.attend("mem1", content="Hello")
        assert chunk.access_count == 2

    def test_capacity_eviction(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=3)
        wm.attend("a", content="A", importance=0.1)
        wm.attend("b", content="B", importance=0.9)
        wm.attend("c", content="C", importance=0.5)
        # At capacity — adding d should evict the lowest activation
        wm.attend("d", content="D", importance=0.8)
        active = wm.get_active_ids()
        assert len(active) <= 3
        assert "d" in active

    def test_group_frees_slots(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=5)
        wm.attend("a", content="Alpha", tags=["x"])
        wm.attend("b", content="Beta", tags=["y"])
        wm.attend("c", content="Gamma")

        # Group a and b into one chunk
        grouped = wm.group(["a", "b"], "ab_group", "Alpha-Beta")
        assert grouped is not None
        assert len(wm.get_active_ids()) == 2  # ab_group + c
        assert "ab_group" in wm.get_active_ids()
        assert grouped.grouped_ids == ["a", "b"]

    def test_forget(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=5)
        wm.attend("mem1", content="Hello")
        assert wm.forget("mem1") is True
        assert wm.forget("nonexistent") is False
        assert len(wm.get_active_ids()) == 0

    def test_get_context_sorted(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=5)
        wm.attend("low", content="Low", importance=0.1)
        wm.attend("high", content="High", importance=0.9)
        context = wm.get_context()
        assert len(context) == 2
        # High importance should be first
        assert context[0]["memory_id"] == "high"

    def test_get_context_with_token_limit(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=5)
        wm.attend("a", content="x" * 400, importance=0.9)  # ~100 tokens
        wm.attend("b", content="x" * 400, importance=0.5)  # ~100 tokens
        context = wm.get_context(max_tokens=120)
        # Should only fit ~1 chunk
        assert len(context) <= 2

    def test_clear(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=5)
        wm.attend("a", content="A")
        wm.attend("b", content="B")
        wm.clear()
        assert len(wm.get_active_ids()) == 0

    def test_get_status(self):
        from whitemagic.core.intelligence.working_memory import WorkingMemory

        wm = WorkingMemory(capacity=7)
        wm.attend("a", content="A")
        status = wm.get_status()
        assert status["capacity"] == 7
        assert status["used"] == 1
        assert status["available"] == 6

    def test_singleton(self):
        from whitemagic.core.intelligence.working_memory import get_working_memory

        wm1 = get_working_memory()
        wm2 = get_working_memory()
        assert wm1 is wm2
