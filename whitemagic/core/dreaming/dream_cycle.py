"""Dream Cycle — Background Processing During Idle Time.
=====================================================
Inspired by biological sleep: the brain consolidates memories, prunes
weak connections, surfaces serendipitous associations, and integrates
new knowledge during rest periods.

The Dream Cycle runs on the Temporal Scheduler's SLOW lane and activates
when the system has been idle (no tool calls) for a configurable period.

Dream phases (one per cycle):
  1. CONSOLIDATION  — Run hippocampal replay (cluster + promote memories)
  2. SERENDIPITY    — Surface unexpected memory connections
  3. KAIZEN         — Analyze tool usage patterns for improvement hints
  4. ORACLE         — Consult Grimoire for contextual recommendations
  5. DECAY          — Run mindful forgetting / galactic rotation

Each phase emits DREAM_PHASE_* events to the Gan Ying bus so other
systems can react. The cycle is gentle — never destructive, always
opt-in, and respects the no-delete policy.

Usage:
    from whitemagic.core.dreaming.dream_cycle import get_dream_cycle
    dc = get_dream_cycle()
    dc.start()   # begin watching for idle periods
    dc.stop()    # halt dreaming
    dc.status()  # introspection
"""

from __future__ import annotations

import logging
import threading
import time
from collections import deque
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class DreamPhase(Enum):
    """Phases of the dream cycle."""

    CONSOLIDATION = "consolidation"
    SERENDIPITY = "serendipity"
    GOVERNANCE = "governance"  # v14.0: Echo chamber detection
    NARRATIVE = "narrative"    # v14.2: Narrative compression
    KAIZEN = "kaizen"
    ORACLE = "oracle"
    DECAY = "decay"


@dataclass
class DreamReport:
    """Result of a single dream cycle."""

    phase: DreamPhase
    started_at: str
    duration_ms: float = 0.0
    details: dict[str, Any] = field(default_factory=dict)
    success: bool = True
    error: str | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "phase": self.phase.value,
            "started_at": self.started_at,
            "duration_ms": round(self.duration_ms, 1),
            "details": self.details,
            "success": self.success,
            "error": self.error,
        }


class DreamCycle:
    """Background dreaming engine.

    Monitors system idle time and cycles through dream phases,
    each performing a different background maintenance task.
    """

    def __init__(
        self,
        idle_threshold_seconds: float = 120.0,
        cycle_interval_seconds: float = 60.0,
        max_history: int = 100,
    ) -> None:
        self._idle_threshold = idle_threshold_seconds
        self._cycle_interval = cycle_interval_seconds
        self._max_history = max_history

        self._running = False
        self._thread: threading.Thread | None = None
        self._lock = threading.Lock()

        self._last_activity = time.time()
        self._dreaming = False
        self._current_phase_index = 0
        self._phases = list(DreamPhase)
        self._total_cycles = 0
        self._history: deque = deque(maxlen=max_history)

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def start(self) -> None:
        """Start the dream cycle watcher."""
        with self._lock:
            if self._running:
                return
            self._running = True
            self._thread = threading.Thread(
                target=self._run_loop, daemon=True, name="dream-cycle",
            )
            self._thread.start()
            logger.info("🌙 Dream Cycle started (idle threshold: %.0fs)", self._idle_threshold)

    def stop(self) -> None:
        """Stop the dream cycle."""
        with self._lock:
            self._running = False
            self._dreaming = False
        if self._thread:
            self._thread.join(timeout=5)
        logger.info("☀️ Dream Cycle stopped")

    def touch(self) -> None:
        """Record activity — resets idle timer. Call on every tool dispatch."""
        self._last_activity = time.time()
        if self._dreaming:
            self._dreaming = False
            logger.debug("Dream interrupted by activity")

    # ------------------------------------------------------------------
    # Main loop
    # ------------------------------------------------------------------

    def _run_loop(self) -> None:
        """Background thread: watch for idle → dream."""
        while self._running:
            try:
                idle_seconds = time.time() - self._last_activity

                if idle_seconds >= self._idle_threshold and not self._dreaming:
                    self._dreaming = True
                    logger.info("🌙 System idle for %.0fs — entering dream state", idle_seconds)
                    self._emit_event("DREAM_STARTED", {"idle_seconds": idle_seconds})

                if self._dreaming and self._running:
                    self._run_phase()

            except Exception as e:
                logger.debug(f"Dream cycle error: {e}")

            time.sleep(self._cycle_interval)

    def _run_phase(self) -> None:
        """Execute the next dream phase in rotation."""
        phase = self._phases[self._current_phase_index % len(self._phases)]
        self._current_phase_index += 1

        start = time.perf_counter()
        report = DreamReport(
            phase=phase,
            started_at=datetime.now().isoformat(),
        )

        try:
            if phase == DreamPhase.CONSOLIDATION:
                report.details = self._dream_consolidation()
            elif phase == DreamPhase.SERENDIPITY:
                report.details = self._dream_serendipity()
            elif phase == DreamPhase.GOVERNANCE:
                report.details = self._dream_governance()
            elif phase == DreamPhase.NARRATIVE:
                report.details = self._dream_narrative()
            elif phase == DreamPhase.KAIZEN:
                report.details = self._dream_kaizen()
            elif phase == DreamPhase.ORACLE:
                report.details = self._dream_oracle()
            elif phase == DreamPhase.DECAY:
                report.details = self._dream_decay()
        except Exception as e:
            report.success = False
            report.error = str(e)
            logger.debug(f"Dream phase {phase.value} error: {e}")

        report.duration_ms = (time.perf_counter() - start) * 1000

        with self._lock:
            self._total_cycles += 1
            self._history.append(report)

        self._emit_event(f"DREAM_PHASE_{phase.value.upper()}", report.to_dict())
        logger.info(
            "💤 Dream phase %s: %s (%.0fms)",
            phase.value,
            "ok" if report.success else f"error: {report.error}",
            report.duration_ms,
        )

    # ------------------------------------------------------------------
    # Dream phases
    # ------------------------------------------------------------------

    def _get_core_access(self) -> Any:
        """Lazy-load the CoreAccessLayer."""
        try:
            from whitemagic.core.intelligence.core_access import get_core_access
            return get_core_access()
        except Exception:
            return None

    def _dream_consolidation(self) -> dict[str, Any]:
        """Phase 1: Run hippocampal memory consolidation + constellation detection."""
        result: dict[str, Any] = {}

        # Standard consolidation
        try:
            from whitemagic.core.memory.consolidation import get_consolidator
            consolidator = get_consolidator()
            report = consolidator.consolidate()
            result.update({
                "memories_analyzed": report.memories_analyzed,
                "clusters_found": report.clusters_found,
                "strategies_synthesized": report.strategies_synthesized,
                "promotions": report.promotions,
            })
        except Exception as e:
            result["consolidation_error"] = str(e)

        # Constellation refresh via CoreAccessLayer
        try:
            from whitemagic.core.memory.constellations import get_constellation_detector
            detector = get_constellation_detector()
            detection = detector.detect(sample_limit=10000)
            result["constellations_detected"] = detection.constellations_found
            result["largest_constellation"] = detection.largest_constellation
        except Exception:
            pass

        return result

    def _dream_serendipity(self) -> dict[str, Any]:
        """Phase 2: Surface unexpected connections via Graph Engine + Bridge Synthesizer.

        v14.0 upgrade: Uses the graph topology engine to find bridge nodes
        (memories connecting disconnected communities) and synthesizes
        insights from those bridges.
        """
        result: dict[str, Any] = {}

        # v14.0: Graph-based bridge discovery
        try:
            from whitemagic.core.memory.graph_engine import get_graph_engine
            engine = get_graph_engine()
            engine.rebuild(sample_limit=20000)

            bridges = engine.find_bridge_nodes(top_n=5)
            result["bridge_nodes_found"] = len(bridges)
            result["top_bridges"] = bridges[:3]

            # Synthesize insights from bridges
            if bridges:
                try:
                    from whitemagic.core.memory.bridge_synthesizer import get_bridge_synthesizer
                    synth = get_bridge_synthesizer()
                    insights = synth.synthesize_from_bridges(bridges, top_n=3)
                    result["bridge_insights"] = [i.to_dict() for i in insights]
                    result["insights_generated"] = len(insights)
                except Exception as e:
                    result["synthesis_error"] = str(e)
        except Exception as e:
            result["graph_error"] = str(e)

        # Standard association mining (fallback + complement)
        try:
            from whitemagic.core.memory.association_miner import get_association_miner
            miner = get_association_miner()
            report = miner.mine(sample_size=100)

            connections = []
            for p in report.top_proposals[:10]:
                connections.append({
                    "from": p.source_id[:8],
                    "to": p.target_id[:8],
                    "score": round(p.overlap_score, 3),
                    "shared": sorted(p.shared_keywords)[:5],
                })

            result.update({
                "connections_surfaced": report.links_proposed,
                "connections_created": report.links_created,
                "pairs_evaluated": report.pairs_evaluated,
                "connections": connections,
            })
        except Exception as e:
            result["mining_error"] = str(e)

        # Cross-constellation bridge discovery via CoreAccessLayer
        cal = self._get_core_access()
        if cal:
            try:
                bridges_cal = cal.find_constellation_bridges(limit=5)
                result["constellation_bridges"] = len(bridges_cal)
                if bridges_cal:
                    result["bridge_pairs"] = [
                        f"{b['constellation_1']} <-> {b['constellation_2']}"
                        for b in bridges_cal[:3]
                    ]
            except Exception:
                pass

        return result

    def _dream_governance(self) -> dict[str, Any]:
        """Phase 3 (v14.0): Echo chamber detection and inhibition.

        1. Compute eigenvector centrality snapshot
        2. Compare with previous snapshot
        3. Flag nodes with centrality spike > 2σ WITHOUT new data
        4. Inhibit reinforcing edges (reduce weight 50%)
        5. Log to Karma Ledger
        """
        result: dict[str, Any] = {}

        try:
            from whitemagic.core.memory.graph_engine import get_graph_engine
            engine = get_graph_engine()

            # Take centrality snapshot (stores as T_now, shifts previous to T_prev)
            snapshot = engine.centrality_snapshot()
            result["snapshot_nodes"] = snapshot.node_count
            result["snapshot_edges"] = snapshot.edge_count

            # Detect echo chambers
            echo_chambers = engine.detect_echo_chambers(sigma_threshold=2.0)
            result["echo_chambers_detected"] = len(echo_chambers)
            result["echo_chambers"] = [ec.to_dict() for ec in echo_chambers[:5]]

            # Inhibit reinforcing edges for echo chamber nodes
            inhibited = 0
            if echo_chambers:
                try:
                    from whitemagic.core.memory.unified import get_unified_memory
                    um = get_unified_memory()
                    with um.backend.pool.connection() as conn:
                        with conn:
                            for ec in echo_chambers[:10]:
                                # Reduce edge weights by 50% for echo chamber node
                                conn.execute(
                                    """UPDATE associations
                                       SET strength = strength * 0.5
                                       WHERE (source_id = ? OR target_id = ?)
                                       AND strength > 0.1""",
                                    (ec.node_id, ec.node_id),
                                )
                                inhibited += 1
                    result["edges_inhibited_for_nodes"] = inhibited
                except Exception as e:
                    result["inhibition_error"] = str(e)

            # Log to Karma Ledger
            if echo_chambers:
                try:
                    from whitemagic.dharma.karma_ledger import get_karma_ledger
                    ledger = get_karma_ledger()
                    ledger.record(
                        tool="dream_governance",
                        declared_safety="WRITE",
                        actual_writes=inhibited,
                        success=True,
                    )
                except Exception:
                    pass

            # Detect communities for context
            try:
                communities = engine.detect_communities()
                result["communities_detected"] = len(communities)
                result["largest_community"] = communities[0].size if communities else 0
            except Exception:
                pass

        except Exception as e:
            result["governance_error"] = str(e)

        return result

    def _dream_narrative(self) -> dict[str, Any]:
        """Phase 4 (v14.2): Narrative compression of episodic memory clusters.

        Compresses clusters of temporally-adjacent, tag-similar memories
        into coherent narrative summaries. This reduces memory fragmentation
        and creates high-quality recall anchors.
        """
        try:
            from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor
            nc = get_narrative_compressor()
            result = nc.compress(max_clusters=3, sample_limit=200)
            return result.to_dict()
        except Exception as e:
            return {"skipped": True, "reason": str(e)}

    def _dream_kaizen(self) -> dict[str, Any]:
        """Phase 5: Analyze patterns for improvement hints + run Emergence scan.

        New in v14: Emergence insights are persisted as memories, creating
        a self-reinforcing intelligence loop where dream discoveries feed
        back into the Data Sea for future recall.
        """
        # Run Emergence Engine proactive scan during Kaizen phase
        emergence_insights = []
        persisted_count = 0
        try:
            from whitemagic.core.intelligence.agentic.emergence_engine import get_emergence_engine
            ee = get_emergence_engine()
            insights = ee.scan_for_emergence()
            emergence_insights = [i.to_dict() for i in insights[:5]]

            # Persist emergence insights as dream memories (self-reinforcing loop)
            persisted_count = self._persist_dream_insights(insights[:3])
        except Exception:
            pass

        try:
            from whitemagic.harmony.vector import get_harmony_vector
            hv = get_harmony_vector()
            snap = hv.snapshot()

            hints = []
            if snap.error_rate > 0.1:
                hints.append(f"High error rate ({snap.error_rate:.2f}) — check circuit breakers")
            if snap.energy < 0.3:
                hints.append(f"Low energy ({snap.energy:.2f}) — consider memory lifecycle sweep")
            if snap.karma_debt > 0.2:
                hints.append(f"Karma debt ({snap.karma_debt:.2f}) — review side-effect declarations")
            if snap.balance < 0.3:
                hints.append(f"Yin-Yang imbalance ({snap.balance:.2f}) — alternate action/reflection")

            # Check guna distribution
            guna = {
                "sattvic": snap.guna_sattvic_pct,
                "rajasic": snap.guna_rajasic_pct,
                "tamasic": snap.guna_tamasic_pct,
            }
            if guna.get("tamasic", 0) > 0.4:
                hints.append("High tamasic ratio — too many failed/stalled operations")

            # Neuro-score sample memories for decay hints
            neuro_hints = []
            try:
                from whitemagic.core.memory.unified import get_unified_memory
                from whitemagic.core.scoring import NeuroScoreInput, neuro_score
                um = get_unified_memory()
                sample = um.list_recent(limit=5)
                for mem in sample:
                    days = max(0.0, (time.time() - (mem.last_accessed or mem.created_at).timestamp()) / 86400) if hasattr(mem, "last_accessed") and mem.last_accessed else 30.0
                    result = neuro_score(NeuroScoreInput(
                        current_score=getattr(mem, "retention_score", 0.5) or 0.5,
                        access_count=getattr(mem, "access_count", 0) or 0,
                        total_memories=100,
                        days_since_access=days,
                        importance=getattr(mem, "importance", 0.5) or 0.5,
                    ))
                    if result.final_score < 0.3:
                        neuro_hints.append(f"Memory '{(mem.title or mem.id[:8])}' score={result.final_score:.3f} — candidate for galactic drift")
            except Exception:
                pass

            return {
                "harmony_score": round(snap.balance, 3),
                "hints": hints + neuro_hints,
                "hint_count": len(hints) + len(neuro_hints),
                "neuro_score_used": len(neuro_hints) > 0,
                "emergence_insights": emergence_insights,
                "emergence_count": len(emergence_insights),
                "dream_insights_persisted": persisted_count,
            }
        except Exception as e:
            return {"skipped": True, "reason": str(e), "emergence_insights": emergence_insights,
                    "dream_insights_persisted": persisted_count}

    def _dream_oracle(self) -> dict[str, Any]:
        """Phase 4: Consult Grimoire for contextual recommendations."""
        try:
            from whitemagic.grimoire.auto_cast import AutoCaster, CastContext, CastMode

            caster = AutoCaster(mode=CastMode.SUGGEST_ONLY)
            caster.activate()

            # Dream about what tasks might be needed
            dream_prompts = [
                "system maintenance and optimization",
                "memory organization and consolidation",
                "pattern recognition and insight",
            ]

            suggestions = []
            for prompt in dream_prompts:
                ctx = CastContext(
                    task=prompt,
                    emotional_state="contemplative",
                    wu_xing="water",
                    yin_yang="yin",
                )
                results = caster.process_context(ctx)
                for r in results:
                    if r.spell and r.confidence > 0.3:
                        suggestions.append({
                            "spell": r.spell.name,
                            "confidence": round(r.confidence, 3),
                            "for": prompt,
                        })

            caster.deactivate()
            return {
                "suggestions": suggestions[:5],
                "suggestion_count": len(suggestions),
            }
        except Exception as e:
            return {"skipped": True, "reason": str(e)}

    def _dream_decay(self) -> dict[str, Any]:
        """Phase 5: Run mindful forgetting / galactic rotation."""
        try:
            from whitemagic.core.memory.lifecycle import get_lifecycle_manager
            mgr = get_lifecycle_manager()
            result = mgr.run_sweep()
            return {
                "swept": True,
                "details": result if isinstance(result, dict) else str(result),
            }
        except Exception as e:
            return {"skipped": True, "reason": str(e)}

    # ------------------------------------------------------------------
    # Events
    # ------------------------------------------------------------------

    def _persist_dream_insights(self, insights: list) -> int:
        """Persist emergence insights as dream memories.

        This creates the self-reinforcing intelligence loop: discoveries
        from dream scanning are stored as memories, which are then
        discoverable by future scans, briefings, and searches.
        """
        persisted = 0
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            from whitemagic.core.memory.unified_types import MemoryType
            um = get_unified_memory()

            for insight in insights:
                title = f"Dream Insight: {insight.title}"
                content = (
                    f"Emergence insight discovered during dream cycle.\n"
                    f"Source: {insight.source}\n"
                    f"Confidence: {insight.confidence:.2f}\n"
                    f"Description: {insight.description}\n"
                )
                try:
                    um.store(
                        content=content,
                        title=title,
                        memory_type=MemoryType.LONG_TERM,
                        importance=0.5 + (insight.confidence * 0.3),
                        tags={
                            "dream_insight",
                            "emergence",
                            "auto_generated",
                            f"source_{insight.source}",
                        },
                        metadata={
                            "dream_cycle": True,
                            "insight_id": insight.id,
                            "confidence": insight.confidence,
                            "source": insight.source,
                        },
                    )
                    persisted += 1
                except Exception:
                    pass
        except Exception as e:
            logger.debug(f"Failed to persist dream insights: {e}")
        return persisted

    def _emit_event(self, event_name: str, data: dict[str, Any]) -> None:
        """Emit a dream event to the Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            bus = get_bus()
            # Use REFLECTION_RECORDED as closest existing event type
            bus.emit(ResonanceEvent(
                event_type=EventType.REFLECTION_RECORDED,
                source="dream_cycle",
                data={"dream_event": event_name, **data},
            ))
        except Exception:
            pass

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def status(self) -> dict[str, Any]:
        """Get dream cycle status."""
        with self._lock:
            recent = [r.to_dict() for r in list(self._history)[-5:]]
            idle_seconds = time.time() - self._last_activity
            return {
                "running": self._running,
                "dreaming": self._dreaming,
                "idle_seconds": round(idle_seconds, 1),
                "idle_threshold": self._idle_threshold,
                "cycle_interval": self._cycle_interval,
                "total_cycles": self._total_cycles,
                "current_phase": self._phases[
                    self._current_phase_index % len(self._phases)
                ].value,
                "recent_dreams": recent,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_dream_cycle: DreamCycle | None = None
_dc_lock = threading.Lock()


def get_dream_cycle() -> DreamCycle:
    """Get the global Dream Cycle instance."""
    global _dream_cycle
    if _dream_cycle is None:
        with _dc_lock:
            if _dream_cycle is None:
                _dream_cycle = DreamCycle()
    return _dream_cycle
