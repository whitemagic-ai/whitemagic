"""Tests for the causal edge miner."""

import pytest

from whitemagic.core.memory.causal_miner import (
    CausalEdge,
    CausalMiner,
    CausalMiningReport,
)


@pytest.fixture
def miner():
    return CausalMiner(persist=False)


class TestCausalEdge:
    def test_to_dict(self):
        edge = CausalEdge(
            source_id="a", target_id="b", relation="led_to",
            strength=0.75, semantic_sim=0.8, temporal_proximity=0.9,
            tag_overlap=0.5, time_delta_hours=2.0,
            reason="led_to: sem=0.80, time=2.0h, tags=0.50",
        )
        d = edge.to_dict()
        assert d["source_id"] == "a"
        assert d["target_id"] == "b"
        assert d["relation"] == "led_to"
        assert d["strength"] == 0.75


class TestCausalMiningReport:
    def test_to_dict(self):
        report = CausalMiningReport(
            memories_sampled=50, pairs_evaluated=100,
            edges_proposed=10, edges_created=8,
        )
        d = report.to_dict()
        assert d["memories_sampled"] == 50
        assert d["edges_proposed"] == 10


class TestTemporalProximity:
    def test_zero_delta(self, miner):
        assert miner._temporal_proximity(0.0) == 1.0

    def test_24h_half_life(self, miner):
        prox = miner._temporal_proximity(24.0)
        assert abs(prox - 0.5) < 0.05, f"24h proximity should be ~0.5, got {prox}"

    def test_48h_quarter(self, miner):
        prox = miner._temporal_proximity(48.0)
        assert abs(prox - 0.25) < 0.05

    def test_168h_very_low(self, miner):
        prox = miner._temporal_proximity(168.0)
        assert prox < 0.01


class TestTagJaccard:
    def test_identical_tags(self, miner):
        tags = {"python", "memory", "guide"}
        assert miner._tag_jaccard(tags, tags) == 1.0

    def test_disjoint_tags(self, miner):
        a = {"python", "memory"}
        b = {"rust", "zig"}
        assert miner._tag_jaccard(a, b) == 0.0

    def test_partial_overlap(self, miner):
        a = {"python", "memory", "guide"}
        b = {"memory", "guide", "tutorial"}
        j = miner._tag_jaccard(a, b)
        # shared: {memory, guide}, union: {python, memory, guide, tutorial}
        assert abs(j - 0.5) < 0.01

    def test_empty_tags(self, miner):
        assert miner._tag_jaccard(set(), set()) == 0.0
        assert miner._tag_jaccard({"a"}, set()) == 0.0


class TestClassifyRelation:
    def test_strong_causal(self, miner):
        rel = miner._classify_relation(strength=0.7, semantic_sim=0.8, temporal_prox=0.6)
        assert rel == "led_to"

    def test_moderate_causal(self, miner):
        rel = miner._classify_relation(strength=0.45, semantic_sim=0.5, temporal_prox=0.3)
        assert rel == "influenced"

    def test_close_in_time(self, miner):
        rel = miner._classify_relation(strength=0.3, semantic_sim=0.3, temporal_prox=0.8)
        assert rel == "preceded"

    def test_weak_relation(self, miner):
        rel = miner._classify_relation(strength=0.15, semantic_sim=0.2, temporal_prox=0.2)
        assert rel == "related_to"


class TestCausalStrength:
    def test_perfect_signals(self, miner):
        s = miner._compute_causal_strength(1.0, 1.0, 1.0)
        assert abs(s - 1.0) < 0.01

    def test_zero_signals(self, miner):
        s = miner._compute_causal_strength(0.0, 0.0, 0.0)
        assert s == 0.0

    def test_semantic_dominant(self, miner):
        s = miner._compute_causal_strength(0.8, 0.0, 0.0)
        assert abs(s - 0.4) < 0.01  # 0.5 * 0.8 = 0.4

    def test_blended(self, miner):
        s = miner._compute_causal_strength(0.6, 0.8, 0.4)
        # 0.5*0.6 + 0.35*0.8 + 0.15*0.4 = 0.30 + 0.28 + 0.06 = 0.64
        assert abs(s - 0.64) < 0.01


class TestGetStats:
    def test_initial_stats(self, miner):
        stats = miner.get_stats()
        assert stats["total_runs"] == 0
        assert stats["total_edges_created"] == 0


class TestSingleton:
    def test_get_causal_miner(self):
        from whitemagic.core.memory.causal_miner import get_causal_miner
        m1 = get_causal_miner()
        m2 = get_causal_miner()
        assert m1 is m2
