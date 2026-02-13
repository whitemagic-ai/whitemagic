"""
Tests for CyberBrains-inspired modules:
  - TemporalScheduler (time-bucketed event scheduling)
  - SalienceArbiter (global workspace attention routing)
  - RetentionEngine (mindful forgetting / multi-signal memory retention)
  - MaturityEngine (gated developmental milestones)
  - BicameralReasoner (dual-hemisphere reasoning)
"""

import pytest
from datetime import datetime
from unittest.mock import MagicMock


# ---------------------------------------------------------------------------
# Temporal Scheduler Tests
# ---------------------------------------------------------------------------

class TestTemporalScheduler:
    """Tests for the time-bucketed event scheduling system."""

    def _make_event(self, event_type_name: str, source: str = "test"):
        from whitemagic.core.resonance.gan_ying_enhanced import (
            EventType, ResonanceEvent,
        )
        et = EventType[event_type_name]
        return ResonanceEvent(
            source=source,
            event_type=et,
            data={"test": True},
            timestamp=datetime.now(),
            confidence=0.9,
        )

    def test_classify_fast_events(self):
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        from whitemagic.core.resonance.gan_ying_enhanced import EventType

        assert classify_event(EventType.THREAT_DETECTED) == TemporalLane.FAST
        assert classify_event(EventType.SYSTEM_STARTED) == TemporalLane.FAST
        assert classify_event(EventType.ANOMALY_DETECTED) == TemporalLane.FAST
        assert classify_event(EventType.BOUNDARY_VIOLATED) == TemporalLane.FAST

    def test_classify_slow_events(self):
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        from whitemagic.core.resonance.gan_ying_enhanced import EventType

        assert classify_event(EventType.MEMORY_CONSOLIDATED) == TemporalLane.SLOW
        assert classify_event(EventType.PATTERN_DETECTED) == TemporalLane.SLOW
        assert classify_event(EventType.WISDOM_INTEGRATED) == TemporalLane.SLOW
        assert classify_event(EventType.LEARNING_COMPLETED) == TemporalLane.SLOW

    def test_classify_medium_default(self):
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        from whitemagic.core.resonance.gan_ying_enhanced import EventType

        assert classify_event(EventType.MEMORY_CREATED) == TemporalLane.MEDIUM
        assert classify_event(EventType.JOY_TRIGGERED) == TemporalLane.MEDIUM
        assert classify_event(EventType.ORACLE_CAST) == TemporalLane.MEDIUM

    def test_fast_events_dispatch_immediately(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler, TemporalLane

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)

        event = self._make_event("THREAT_DETECTED")
        lane = scheduler.schedule(event)

        assert lane == TemporalLane.FAST
        mock_bus.emit.assert_called_once_with(event)

    def test_medium_events_queue(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler, TemporalLane

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)

        event = self._make_event("MEMORY_CREATED")
        lane = scheduler.schedule(event)

        assert lane == TemporalLane.MEDIUM
        # Should NOT have emitted immediately (it's queued)
        mock_bus.emit.assert_not_called()
        assert scheduler.get_queue_depth(TemporalLane.MEDIUM) == 1

    def test_force_lane_override(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler, TemporalLane

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)

        # Memory events normally go MEDIUM, but force to FAST
        event = self._make_event("MEMORY_CREATED")
        lane = scheduler.schedule(event, lane=TemporalLane.FAST)

        assert lane == TemporalLane.FAST
        mock_bus.emit.assert_called_once()

    def test_flush_all(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler, TemporalLane

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)

        # Queue some events
        for _ in range(5):
            scheduler.schedule(self._make_event("MEMORY_CREATED"))
        for _ in range(3):
            scheduler.schedule(self._make_event("PATTERN_DETECTED"))

        assert scheduler.get_queue_depth(TemporalLane.MEDIUM) == 5
        assert scheduler.get_queue_depth(TemporalLane.SLOW) == 3

        counts = scheduler.flush_all()
        assert counts["medium"] == 5
        assert counts["slow"] == 3
        assert mock_bus.emit.call_count == 8

    def test_stats(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)

        scheduler.schedule(self._make_event("THREAT_DETECTED"))
        scheduler.schedule(self._make_event("MEMORY_CREATED"))
        scheduler.schedule(self._make_event("PATTERN_DETECTED"))

        stats = scheduler.get_stats()
        assert stats["fast"]["events_queued"] == 1
        assert stats["medium"]["events_queued"] == 1
        assert stats["slow"]["events_queued"] == 1

    def test_start_stop(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)

        scheduler.start()
        assert scheduler.is_running

        scheduler.stop()
        assert not scheduler.is_running

    def test_pre_flush_hook(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler, TemporalLane

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)

        hook_called = []
        def my_hook(events):
            hook_called.append(len(events))
            return events  # pass-through

        scheduler.add_pre_flush_hook(TemporalLane.MEDIUM, my_hook)
        scheduler.schedule(self._make_event("MEMORY_CREATED"))
        scheduler.schedule(self._make_event("MEMORY_UPDATED"))
        scheduler.flush_all()

        assert hook_called == [2]


# ---------------------------------------------------------------------------
# Salience Arbiter Tests
# ---------------------------------------------------------------------------

class TestSalienceArbiter:
    """Tests for the salience scoring and arbitration system."""

    def _make_event(self, event_type_name: str, confidence: float = 0.8):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType, ResonanceEvent
        et = EventType[event_type_name]
        return ResonanceEvent(
            source="test",
            event_type=et,
            data={"test": True},
            timestamp=datetime.now(),
            confidence=confidence,
        )

    def test_score_threat_high_urgency(self):
        from whitemagic.core.resonance.salience_arbiter import SalienceScorer

        scorer = SalienceScorer()
        event = self._make_event("THREAT_DETECTED")
        score = scorer.score(event)

        assert score.urgency >= 0.9
        assert score.composite > 0.5

    def test_score_pattern_low_urgency(self):
        from whitemagic.core.resonance.salience_arbiter import SalienceScorer

        scorer = SalienceScorer()
        event = self._make_event("PATTERN_DETECTED")
        score = scorer.score(event)

        assert score.urgency < 0.5

    def test_novelty_decreases_with_repetition(self):
        from whitemagic.core.resonance.salience_arbiter import SalienceScorer

        scorer = SalienceScorer(novelty_window=50)

        # First event should have high novelty
        event1 = self._make_event("MEMORY_CREATED")
        score1 = scorer.score(event1)

        # Emit same event type many times
        for _ in range(20):
            scorer.score(self._make_event("MEMORY_CREATED"))

        # Next one should have lower novelty
        event_late = self._make_event("MEMORY_CREATED")
        score_late = scorer.score(event_late)

        assert score_late.novelty < score1.novelty

    def test_arbiter_admit_and_spotlight(self):
        from whitemagic.core.resonance.salience_arbiter import SalienceArbiter

        arbiter = SalienceArbiter(spotlight_size=5, min_salience=0.1)

        # Admit several events
        arbiter.admit(self._make_event("THREAT_DETECTED", confidence=0.95))
        arbiter.admit(self._make_event("MEMORY_CREATED", confidence=0.7))
        arbiter.admit(self._make_event("PATTERN_DETECTED", confidence=0.5))

        spotlight = arbiter.get_spotlight()
        assert len(spotlight) == 3
        # Threat should be #1 (highest salience)
        assert spotlight[0].event.event_type.value == "threat_detected"

    def test_arbiter_drops_low_salience(self):
        from whitemagic.core.resonance.salience_arbiter import SalienceArbiter

        arbiter = SalienceArbiter(min_salience=0.99)  # Very high threshold

        entry = arbiter.admit(self._make_event("PATTERN_DETECTED", confidence=0.3))
        assert entry is None  # Should be dropped

    def test_pre_flush_filter(self):
        from whitemagic.core.resonance.salience_arbiter import SalienceArbiter

        arbiter = SalienceArbiter(min_salience=0.1)
        events = [
            self._make_event("THREAT_DETECTED", confidence=0.95),
            self._make_event("MEMORY_CREATED", confidence=0.7),
            self._make_event("PATTERN_DETECTED", confidence=0.5),
        ]

        filtered = arbiter.pre_flush_filter(events)
        # All should pass min_salience=0.1
        assert len(filtered) >= 2
        # Should be ordered by salience descending
        # (threat should come first)

    def test_arbiter_stats(self):
        from whitemagic.core.resonance.salience_arbiter import SalienceArbiter

        arbiter = SalienceArbiter(spotlight_size=3, min_salience=0.1)
        arbiter.admit(self._make_event("THREAT_DETECTED"))
        arbiter.admit(self._make_event("MEMORY_CREATED"))

        stats = arbiter.get_stats()
        assert stats["total_scored"] == 2
        assert stats["total_admitted"] == 2


# ---------------------------------------------------------------------------
# Mindful Forgetting Tests
# ---------------------------------------------------------------------------

class TestMindfulForgetting:
    """Tests for the multi-signal memory retention system."""

    def _make_memory(self, **overrides):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        defaults = dict(
            id="test-mem-001",
            content="Test memory content",
            memory_type=MemoryType.SHORT_TERM,
            title="Test Memory",
            importance=0.5,
            emotional_valence=0.0,
            neuro_score=0.8,
            novelty_score=0.7,
            recall_count=3,
            half_life_days=30.0,
            is_protected=False,
            is_core_identity=False,
            is_sacred=False,
            is_pinned=False,
            emotional_weight=0.5,
        )
        defaults.update(overrides)
        return Memory(**defaults)

    def test_protected_memory_always_retained(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine()
        mem = self._make_memory(is_protected=True, neuro_score=0.01, importance=0.01)
        verdict = engine.evaluate(mem)

        assert verdict.retain is True
        assert verdict.recommended_action == "protect"

    def test_pinned_memory_always_retained(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine()
        mem = self._make_memory(is_pinned=True, neuro_score=0.05)
        verdict = engine.evaluate(mem)

        assert verdict.retain is True
        assert verdict.recommended_action == "protect"

    def test_strong_memory_retained(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine()
        mem = self._make_memory(importance=0.9, neuro_score=0.95, recall_count=20)
        verdict = engine.evaluate(mem)

        assert verdict.retain is True
        assert verdict.recommended_action == "keep"

    def test_weak_memory_archived(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine(retain_threshold=0.4, archive_threshold=0.2)
        mem = self._make_memory(
            importance=0.05,
            neuro_score=0.1,
            recall_count=0,
            emotional_valence=0.0,
            emotional_weight=0.05,
            half_life_days=1.0,
        )
        verdict = engine.evaluate(mem)

        # Very weak memory should be either decayed or archived
        assert verdict.score < 0.4

    def test_emotional_memory_boosted(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine()
        neutral = self._make_memory(emotional_valence=0.0, emotional_weight=0.1)
        emotional = self._make_memory(emotional_valence=0.9, emotional_weight=0.9)

        v_neutral = engine.evaluate(neutral)
        v_emotional = engine.evaluate(emotional)

        assert v_emotional.score > v_neutral.score

    def test_signal_breakdown(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine()
        mem = self._make_memory()
        verdict = engine.evaluate(mem)

        signal_names = {s.name for s in verdict.signals}
        assert "semantic" in signal_names
        assert "emotional" in signal_names
        assert "recency" in signal_names
        assert "connection" in signal_names
        assert "protection" in signal_names

    def test_sweep_with_list(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine()
        memories = [
            self._make_memory(id="strong", importance=0.9, neuro_score=0.95),
            self._make_memory(id="weak", importance=0.05, neuro_score=0.1, emotional_weight=0.05),
            self._make_memory(id="protected", is_protected=True, neuro_score=0.01),
        ]
        report = engine.sweep(memories)

        assert report.total_evaluated == 3
        assert report.protected >= 1

    def test_engine_stats(self):
        from whitemagic.core.memory.mindful_forgetting import RetentionEngine

        engine = RetentionEngine()
        mem = self._make_memory()
        engine.evaluate(mem)
        engine.evaluate(mem)

        stats = engine.get_stats()
        assert stats["total_evaluations"] == 2


# ---------------------------------------------------------------------------
# Maturity Gates Tests
# ---------------------------------------------------------------------------

class TestMaturityGates:
    """Tests for the gated developmental milestone system."""

    def test_maturity_stage_ordering(self):
        from whitemagic.core.governance.maturity_gates import MaturityStage

        assert MaturityStage.SEED < MaturityStage.BICAMERAL
        assert MaturityStage.BICAMERAL < MaturityStage.REFLECTIVE
        assert MaturityStage.REFLECTIVE < MaturityStage.RADIANT
        assert MaturityStage.RADIANT < MaturityStage.COLLECTIVE
        assert MaturityStage.COLLECTIVE < MaturityStage.LOGOS

    def test_stage_capabilities_defined(self):
        from whitemagic.core.governance.maturity_gates import _STAGE_CAPABILITIES, MaturityStage

        for stage in MaturityStage:
            assert stage in _STAGE_CAPABILITIES
            assert len(_STAGE_CAPABILITIES[stage]) > 0

    def test_assess_returns_report(self):
        from whitemagic.core.governance.maturity_gates import MaturityEngine

        engine = MaturityEngine()
        report = engine.assess()

        assert report.current_stage is not None
        assert isinstance(report.unlocked_capabilities, set)
        assert len(report.unlocked_capabilities) > 0
        assert report.timestamp is not None

    def test_seed_capabilities_always_present(self):
        from whitemagic.core.governance.maturity_gates import MaturityEngine

        engine = MaturityEngine()
        report = engine.assess()

        # At minimum, SEED capabilities should be present
        assert "memory.create" in report.unlocked_capabilities
        assert "tools.registry" in report.unlocked_capabilities

    def test_report_to_dict(self):
        from whitemagic.core.governance.maturity_gates import MaturityEngine

        engine = MaturityEngine()
        report = engine.assess()
        d = report.to_dict()

        assert "current_stage" in d
        assert "unlocked_capabilities" in d
        assert "capability_count" in d
        assert "next_gate" in d
        assert "timestamp" in d

    def test_is_capable(self):
        from whitemagic.core.governance.maturity_gates import MaturityEngine

        engine = MaturityEngine()
        # Should always be capable of basic memory ops
        assert engine.is_capable("memory.create") is True

    def test_require_stage(self):
        from whitemagic.core.governance.maturity_gates import MaturityEngine, MaturityStage

        engine = MaturityEngine()
        # Should always reach at least SEED
        assert engine.require_stage(MaturityStage.SEED) is True

    def test_logos_not_yet_reached(self):
        from whitemagic.core.governance.maturity_gates import MaturityEngine, MaturityStage

        engine = MaturityEngine()
        report = engine.assess()
        # Logos is aspirational and should not pass yet
        assert report.current_stage < MaturityStage.LOGOS


# ---------------------------------------------------------------------------
# Bicameral Reasoning Tests
# ---------------------------------------------------------------------------

class TestBicameralReasoning:
    """Tests for the dual-hemisphere reasoning system."""

    @pytest.mark.asyncio
    async def test_basic_reasoning(self):
        from whitemagic.core.intelligence.bicameral import BicameralReasoner

        reasoner = BicameralReasoner(left_clones=5, right_clones=5)
        result = await reasoner.reason("How should we optimize memory access?")

        assert result.query == "How should we optimize memory access?"
        assert result.left_analysis is not None
        assert result.right_analysis is not None
        assert result.left_analysis.hemisphere == "left"
        assert result.right_analysis.hemisphere == "right"
        assert result.synthesis != ""
        assert 0.0 <= result.final_confidence <= 1.0
        assert 0.0 <= result.tension_score <= 1.0

    @pytest.mark.asyncio
    async def test_cross_critique_generated(self):
        from whitemagic.core.intelligence.bicameral import BicameralReasoner

        reasoner = BicameralReasoner(left_clones=5, right_clones=5, cross_critique_enabled=True)
        result = await reasoner.reason("What is the best approach?")

        assert len(result.cross_critique) == 2
        critics = {c.critic for c in result.cross_critique}
        assert "left" in critics
        assert "right" in critics

    @pytest.mark.asyncio
    async def test_cross_critique_disabled(self):
        from whitemagic.core.intelligence.bicameral import BicameralReasoner

        reasoner = BicameralReasoner(left_clones=5, right_clones=5, cross_critique_enabled=False)
        result = await reasoner.reason("Quick test")

        assert len(result.cross_critique) == 0

    @pytest.mark.asyncio
    async def test_dominant_hemisphere(self):
        from whitemagic.core.intelligence.bicameral import BicameralReasoner

        reasoner = BicameralReasoner(left_clones=5, right_clones=5)
        result = await reasoner.reason("Analyze this systematically")

        assert result.dominant_hemisphere in ("left", "right", "balanced")

    @pytest.mark.asyncio
    async def test_result_to_dict(self):
        from whitemagic.core.intelligence.bicameral import BicameralReasoner

        reasoner = BicameralReasoner(left_clones=5, right_clones=5)
        result = await reasoner.reason("Test query")
        d = result.to_dict()

        assert "left" in d
        assert "right" in d
        assert "synthesis" in d
        assert "tension_score" in d
        assert "dominant_hemisphere" in d

    @pytest.mark.asyncio
    async def test_stats_tracking(self):
        from whitemagic.core.intelligence.bicameral import BicameralReasoner

        reasoner = BicameralReasoner(left_clones=5, right_clones=5)
        await reasoner.reason("First question")
        await reasoner.reason("Second question")

        stats = reasoner.get_stats()
        assert stats["total_reasonings"] == 2
        assert stats["left_wins"] + stats["right_wins"] + stats["ties"] == 2


# ---------------------------------------------------------------------------
# Integration: Temporal + Salience wired together
# ---------------------------------------------------------------------------

class TestTemporalSalienceIntegration:
    """Test that the temporal scheduler and salience arbiter wire together."""

    def _make_event(self, event_type_name: str, confidence: float = 0.8):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType, ResonanceEvent
        et = EventType[event_type_name]
        return ResonanceEvent(
            source="integration_test",
            event_type=et,
            data={"test": True},
            timestamp=datetime.now(),
            confidence=confidence,
        )

    def test_salience_as_pre_flush_hook(self):
        from whitemagic.core.resonance.temporal_scheduler import TemporalScheduler, TemporalLane
        from whitemagic.core.resonance.salience_arbiter import SalienceArbiter

        mock_bus = MagicMock()
        scheduler = TemporalScheduler(bus=mock_bus)
        arbiter = SalienceArbiter(min_salience=0.1)

        # Wire arbiter as pre-flush hook on MEDIUM lane
        scheduler.add_pre_flush_hook(TemporalLane.MEDIUM, arbiter.pre_flush_filter)

        # Queue events
        scheduler.schedule(self._make_event("MEMORY_CREATED", confidence=0.9))
        scheduler.schedule(self._make_event("MEMORY_UPDATED", confidence=0.7))
        scheduler.schedule(self._make_event("JOY_TRIGGERED", confidence=0.6))

        # Flush — arbiter should score and reorder
        counts = scheduler.flush_all()
        assert counts["medium"] >= 2  # Most should pass min_salience=0.1
