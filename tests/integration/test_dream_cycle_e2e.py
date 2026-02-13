"""
End-to-end Dream Cycle Integration Test
=========================================
Exercises all 8 dream phases with synthetic memories,
verifying triage, consolidation, serendipity (association mining),
governance, narrative, kaizen (harmony hints), oracle (grimoire), and decay (lifecycle sweep).

Also verifies galactic promotion of strategy memories after consolidation.
"""
import time
from dataclasses import dataclass, field
from typing import Any, List, Optional, Set
from unittest.mock import MagicMock, patch


from whitemagic.core.dreaming.dream_cycle import (
    DreamCycle,
    DreamPhase,
    DreamReport,
)


# ---------------------------------------------------------------------------
# Synthetic memory objects for consolidation / association tests
# ---------------------------------------------------------------------------

@dataclass
class FakeMemory:
    id: str
    title: str = ""
    content: str = ""
    tags: Set[str] = field(default_factory=set)
    importance: float = 0.5
    access_count: int = 0
    emotional_valence: float = 0.0
    memory_type: str = "SHORT_TERM"
    retention_score: float = 0.5
    last_accessed: Any = None
    created_at: Any = None
    galactic_distance: Optional[float] = None


def _make_memories(n: int = 20) -> List[FakeMemory]:
    """Generate a cluster-friendly set of synthetic memories."""
    mems = []
    # Cluster 1: Python/ML theme (7 mems, high importance, frequently accessed)
    for i in range(7):
        mems.append(FakeMemory(
            id=f"py-{i}",
            title=f"Python ML note {i}",
            content=f"Machine learning model training run {i}",
            tags={"python", "ml", "training"},
            importance=0.7 + i * 0.02,
            access_count=5 + i,
            emotional_valence=0.3,
            memory_type="SHORT_TERM",
        ))
    # Cluster 2: DevOps theme (6 mems)
    for i in range(6):
        mems.append(FakeMemory(
            id=f"ops-{i}",
            title=f"DevOps deploy note {i}",
            content=f"Deployment pipeline stage {i}",
            tags={"devops", "deploy", "ci"},
            importance=0.5 + i * 0.01,
            access_count=3 + i,
            emotional_valence=0.1,
            memory_type="SHORT_TERM",
        ))
    # Cluster 3: scattered (no strong cluster)
    for i in range(n - 13):
        mems.append(FakeMemory(
            id=f"misc-{i}",
            title=f"Miscellaneous note {i}",
            content=f"Random thought {i}",
            tags={f"tag-{i}"},
            importance=0.3,
            access_count=1,
            emotional_valence=0.0,
        ))
    return mems


# ---------------------------------------------------------------------------
# Phase-level tests
# ---------------------------------------------------------------------------

class TestDreamPhaseConsolidation:
    """Test the CONSOLIDATION phase with synthetic memories."""

    def test_consolidation_clusters_and_promotes(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator

        consolidator = MemoryConsolidator(min_cluster_size=3)
        mems = _make_memories()
        report = consolidator.consolidate(memories=mems)

        assert report.memories_analyzed == len(mems)
        assert report.clusters_found >= 2  # python/ml + devops
        assert report.duration_ms >= 0

        # At least the Python cluster should be strong enough for a strategy
        # (importance > 0.4, access > 3, size >= 3)
        # Strategies depend on UnifiedMemory store which may not be available
        # in test isolation, so strategies_synthesized could be 0

    def test_consolidation_with_empty_memories(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator

        consolidator = MemoryConsolidator()
        report = consolidator.consolidate(memories=[])
        assert report.memories_analyzed == 0
        assert report.clusters_found == 0

    def test_galactic_promote_called(self):
        """Verify _galactic_promote is invoked after consolidation."""
        from whitemagic.core.memory.consolidation import MemoryConsolidator

        consolidator = MemoryConsolidator(min_cluster_size=3)
        consolidator._galactic_promote = MagicMock()

        mems = _make_memories()
        consolidator.consolidate(memories=mems)
        consolidator._galactic_promote.assert_called_once()


class TestDreamPhaseSerendipity:
    """Test the SERENDIPITY phase (association mining)."""

    def test_serendipity_phase_graceful(self):
        """Serendipity phase should complete gracefully (empty DB is fine)."""
        dc = DreamCycle()
        # Advance to SERENDIPITY (phase index 2, after TRIAGE and CONSOLIDATION)
        dc._current_phase_index = 2
        dc._run_phase()

        report = dc._history[-1]
        assert report.phase == DreamPhase.SERENDIPITY
        assert isinstance(report.details, dict)

    def test_association_miner_with_rust_fallback(self):
        """Test Python fallback path for association mining keywords."""
        from whitemagic.optimization.rust_accelerators import _association_mine_python

        texts = [
            ("m1", "python machine learning deep neural network training"),
            ("m2", "python deep learning neural network inference optimization"),
            ("m3", "javascript react frontend web development deployment"),
        ]
        result = _association_mine_python(texts, max_keywords=50, min_score=0.1, max_results=100)
        assert result["memory_count"] == 3
        assert "overlaps" in result
        # m1+m2 share more keywords than m1+m3
        overlaps = result["overlaps"]
        m1_m2 = [o for o in overlaps if {o["source_id"], o["target_id"]} == {"m1", "m2"}]
        m1_m3 = [o for o in overlaps if {o["source_id"], o["target_id"]} == {"m1", "m3"}]
        if m1_m2 and m1_m3:
            assert m1_m2[0]["overlap_score"] > m1_m3[0]["overlap_score"]


class TestDreamPhaseKaizen:
    """Test the KAIZEN phase (harmony vector analysis)."""

    def test_harmony_snapshot_returns_hints(self):
        from whitemagic.harmony.vector import get_harmony_vector

        hv = get_harmony_vector()
        snap = hv.snapshot()

        # All 7 dimensions should be numeric
        assert isinstance(snap.balance, float)
        assert isinstance(snap.energy, float)
        assert isinstance(snap.error_rate, float)

    def test_kaizen_phase_produces_details(self):
        dc = DreamCycle()
        details = dc._dream_kaizen()

        # Should either return hints or be skipped gracefully
        assert isinstance(details, dict)
        if not details.get("skipped"):
            assert "hints" in details
            assert "harmony_score" in details


class TestDreamPhaseOracle:
    """Test the ORACLE phase (grimoire consultation)."""

    def test_oracle_phase_graceful(self):
        dc = DreamCycle()
        details = dc._dream_oracle()

        # Should either return suggestions or skip gracefully
        assert isinstance(details, dict)
        if not details.get("skipped"):
            assert "suggestions" in details
            assert "suggestion_count" in details


class TestDreamPhaseDecay:
    """Test the DECAY phase (lifecycle sweep)."""

    def test_decay_phase_graceful(self):
        dc = DreamCycle()
        details = dc._dream_decay()

        assert isinstance(details, dict)


# ---------------------------------------------------------------------------
# Full E2E cycle test
# ---------------------------------------------------------------------------

class TestDreamCycleE2E:
    """End-to-end: run all 5 phases sequentially and verify rotation."""

    def test_full_rotation_all_phases(self):
        """Run all 5 phases and verify each produces a valid DreamReport."""
        dc = DreamCycle(idle_threshold_seconds=0.01, cycle_interval_seconds=0.01)

        reports: List[DreamReport] = []
        for i in range(8):
            dc._run_phase()
            reports.append(dc._history[-1])

        assert dc._total_cycles == 8
        assert len(dc._history) == 8

        # Verify each phase was visited exactly once (TRIAGE first, added v15.3)
        phases_seen = [r.phase for r in reports]
        assert phases_seen == [
            DreamPhase.TRIAGE,
            DreamPhase.CONSOLIDATION,
            DreamPhase.SERENDIPITY,
            DreamPhase.GOVERNANCE,
            DreamPhase.NARRATIVE,
            DreamPhase.KAIZEN,
            DreamPhase.ORACLE,
            DreamPhase.DECAY,
        ]

        # Every report should have a valid structure
        for r in reports:
            assert isinstance(r.started_at, str)
            assert r.duration_ms >= 0
            assert isinstance(r.details, dict)
            d = r.to_dict()
            assert d["phase"] in ("triage", "consolidation", "serendipity", "governance", "narrative", "kaizen", "oracle", "decay")

    def test_phase_rotation_wraps_around(self):
        """After 8 phases, phase 9 should be TRIAGE again."""
        dc = DreamCycle()
        for _ in range(9):
            dc._run_phase()

        assert dc._total_cycles == 9
        # Phase 9 (index 8) should wrap to TRIAGE (index 0)
        assert dc._history[-1].phase == DreamPhase.TRIAGE

    def test_status_reflects_history(self):
        dc = DreamCycle()
        dc._run_phase()
        dc._run_phase()

        status = dc.status()
        assert status["total_cycles"] == 2
        assert len(status["recent_dreams"]) == 2
        assert status["current_phase"] == "serendipity"  # next phase after 2 runs (v15.3: triage=0, consolidation=1, serendipity=2)

    def test_consolidation_with_mock_memories_and_galactic_promotion(self):
        """
        Full E2E: inject mock memories into consolidation,
        verify clustering and galactic promotion pathway.
        """
        dc = DreamCycle()
        mems = _make_memories(20)

        # Skip TRIAGE (phase 0) to reach CONSOLIDATION (phase 1)
        dc._current_phase_index = 1

        # Patch consolidation to use our synthetic memories
        with patch(
            "whitemagic.core.memory.consolidation.MemoryConsolidator._load_recent",
            return_value=mems,
        ):
            dc._run_phase()  # CONSOLIDATION

        report = dc._history[-1]
        assert report.phase == DreamPhase.CONSOLIDATION
        assert report.success is True
        details = report.details

        # Should have analyzed our memories
        assert details.get("memories_analyzed", 0) == len(mems)
        # Should have found clusters
        assert details.get("clusters_found", 0) >= 2

    def test_dreaming_lifecycle_start_stop(self):
        """Start/stop the dream loop; verify it respects idle threshold."""
        dc = DreamCycle(idle_threshold_seconds=9999, cycle_interval_seconds=0.05)
        dc.start()
        assert dc._running

        # Not idle enough, so no dreaming should happen
        time.sleep(0.15)
        assert dc._total_cycles == 0

        dc.stop()
        assert not dc._running

    def test_dreaming_triggers_on_idle(self):
        """With very low idle threshold, dreaming should start quickly."""
        dc = DreamCycle(idle_threshold_seconds=0.01, cycle_interval_seconds=0.05)
        dc._last_activity = time.time() - 10  # pretend idle for 10s
        dc.start()

        # Give the thread time to run at least one phase
        time.sleep(0.25)
        dc.stop()

        assert dc._total_cycles >= 1
        assert len(dc._history) >= 1

    def test_touch_interrupts_dreaming(self):
        """Calling touch() should reset idle and stop dreaming."""
        dc = DreamCycle(idle_threshold_seconds=0.01, cycle_interval_seconds=0.05)
        dc._last_activity = time.time() - 10
        dc.start()
        time.sleep(0.15)  # let it enter dream state

        dc.touch()  # interrupt
        assert not dc._dreaming

        dc.stop()

    def test_event_emission(self):
        """Verify dream phases emit Gan Ying events."""
        dc = DreamCycle()
        emitted = []
        dc._emit_event = lambda name, data: emitted.append((name, data))

        dc._run_phase()

        # Should have emitted DREAM_PHASE_TRIAGE (first phase since v15.3)
        assert len(emitted) == 1
        assert emitted[0][0] == "DREAM_PHASE_TRIAGE"
        assert "phase" in emitted[0][1]

    def test_all_reports_json_serializable(self):
        """All dream reports must be JSON-serializable."""
        import json

        dc = DreamCycle()
        for _ in range(5):
            dc._run_phase()

        for report in dc._history:
            d = report.to_dict()
            json.dumps(d)  # should not raise
