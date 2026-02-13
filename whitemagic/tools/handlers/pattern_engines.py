"""Handlers for the 12 previously-unwired pattern analysis engines.

v15.8 — Exposes CausalMiner, EmergenceEngine, AssociationMiner,
ConstellationDetector, SatkonaFusion, MultiSpectralReasoner,
NoveltyDetector, BridgeSynthesizer, GalacticMap, GuidelineEvolution,
ElementalOptimization, and PatternConsciousness to MCP.
"""

from __future__ import annotations

import logging
from typing import Any

logger = logging.getLogger(__name__)


# ── CausalMiner ─────────────────────────────────────────────────

def handle_causal_mine(**kwargs: Any) -> dict[str, Any]:
    """Run causal edge mining — discovers directed temporal-semantic edges."""
    try:
        from whitemagic.core.memory.causal_miner import get_causal_miner
        miner = get_causal_miner()
        sample_size = kwargs.get("sample_size", 200)
        report = miner.mine(sample_size=sample_size)
        return {"status": "success", "tool": "causal.mine", "details": report.to_dict()}
    except Exception as e:
        return {"status": "error", "tool": "causal.mine", "message": str(e)}


def handle_causal_stats(**kwargs: Any) -> dict[str, Any]:
    """Get causal miner statistics."""
    try:
        from whitemagic.core.memory.causal_miner import get_causal_miner
        miner = get_causal_miner()
        return {
            "status": "success",
            "tool": "causal.stats",
            "details": {
                "total_runs": miner._total_runs,
                "total_edges_created": miner._total_edges_created,
            },
        }
    except Exception as e:
        return {"status": "error", "tool": "causal.stats", "message": str(e)}


# ── EmergenceEngine ─────────────────────────────────────────────

def handle_emergence_scan(**kwargs: Any) -> dict[str, Any]:
    """Proactively scan for emergent patterns (constellation convergence, hotspots, bursts)."""
    try:
        from whitemagic.core.intelligence.agentic.emergence_engine import get_emergence_engine
        engine = get_emergence_engine()
        engine.start()
        insights = engine.scan_for_emergence()
        return {
            "status": "success",
            "tool": "emergence.scan",
            "details": {
                "insights_found": len(insights),
                "insights": [i.to_dict() for i in insights[:20]],
            },
        }
    except Exception as e:
        return {"status": "error", "tool": "emergence.scan", "message": str(e)}


def handle_emergence_status(**kwargs: Any) -> dict[str, Any]:
    """Get emergence engine status and recent insights."""
    try:
        from whitemagic.core.intelligence.agentic.emergence_engine import get_emergence_engine
        engine = get_emergence_engine()
        status = engine.get_status()
        status["recent_insights"] = engine.get_insights(limit=kwargs.get("limit", 10))
        return {"status": "success", "tool": "emergence.status", "details": status}
    except Exception as e:
        return {"status": "error", "tool": "emergence.status", "message": str(e)}


# ── AssociationMiner ────────────────────────────────────────────

def handle_association_mine(**kwargs: Any) -> dict[str, Any]:
    """Run keyword-Jaccard association mining."""
    try:
        from whitemagic.core.memory.association_miner import get_association_miner
        miner = get_association_miner()
        sample_size = kwargs.get("sample_size", 200)
        report = miner.mine(sample_size=sample_size)
        return {"status": "success", "tool": "association.mine", "details": report.to_dict()}
    except Exception as e:
        return {"status": "error", "tool": "association.mine", "message": str(e)}


def handle_association_mine_semantic(**kwargs: Any) -> dict[str, Any]:
    """Run embedding-based semantic association mining."""
    try:
        from whitemagic.core.memory.association_miner import get_association_miner
        miner = get_association_miner()
        report = miner.mine_semantic(
            min_similarity=kwargs.get("min_similarity", 0.50),
            strong_threshold=kwargs.get("strong_threshold", 0.70),
        )
        return {"status": "success", "tool": "association.mine_semantic", "details": report.to_dict()}
    except Exception as e:
        return {"status": "error", "tool": "association.mine_semantic", "message": str(e)}


# ── ConstellationDetector ───────────────────────────────────────

def handle_constellation_detect(**kwargs: Any) -> dict[str, Any]:
    """Run HDBSCAN constellation detection in 5D holographic space."""
    try:
        from whitemagic.core.memory.constellations import get_constellation_detector
        detector = get_constellation_detector()
        sample_limit = kwargs.get("sample_limit", 50000)
        report = detector.detect(sample_limit=sample_limit)
        return {"status": "success", "tool": "constellation.detect", "details": report.to_dict()}
    except Exception as e:
        return {"status": "error", "tool": "constellation.detect", "message": str(e)}


def handle_constellation_stats(**kwargs: Any) -> dict[str, Any]:
    """Get constellation centroids and statistics."""
    try:
        from whitemagic.core.memory.constellations import get_constellation_detector
        detector = get_constellation_detector()
        centroids = detector.get_constellation_centroids()
        return {
            "status": "success",
            "tool": "constellation.stats",
            "details": {
                "constellations": len(centroids),
                "centroids": centroids[:30],
            },
        }
    except Exception as e:
        return {"status": "error", "tool": "constellation.stats", "message": str(e)}


# ── SatkonaFusion ───────────────────────────────────────────────

def handle_satkona_fuse(**kwargs: Any) -> dict[str, Any]:
    """Run Satkona multi-signal fusion ranking (Wu Xing + Constellation + Dream + Polyglot)."""
    try:
        from whitemagic.core.fusion.satkona_fusion import fuse_signals_with_fusion
        result = fuse_signals_with_fusion(
            signals=kwargs.get("signals", {}),
            clusters=kwargs.get("clusters", {}),
            memories=kwargs.get("memories", {}),
        )
        return {"status": "success", "tool": "satkona.fuse", "details": result}
    except Exception as e:
        return {"status": "error", "tool": "satkona.fuse", "message": str(e)}


# ── MultiSpectralReasoner ───────────────────────────────────────

def handle_reasoning_multispectral(**kwargs: Any) -> dict[str, Any]:
    """Run multi-spectral reasoning (I Ching + Wu Xing + Art of War + Zodiac)."""
    try:
        from whitemagic.core.intelligence.multi_spectral_reasoning import get_reasoner
        reasoner = get_reasoner()
        question = kwargs.get("question", "")
        if not question:
            return {"status": "error", "tool": "reasoning.multispectral", "message": "question is required"}
        result = reasoner.reason(question=question)
        return {
            "status": "success",
            "tool": "reasoning.multispectral",
            "details": {
                "question": result.question,
                "synthesis": result.synthesis,
                "recommendation": result.recommendation,
                "confidence": result.confidence,
                "lenses_used": [p.lens.value for p in result.perspectives],
                "patterns_matched": len(result.patterns_matched),
            },
        }
    except Exception as e:
        return {"status": "error", "tool": "reasoning.multispectral", "message": str(e)}


# ── NoveltyDetector ─────────────────────────────────────────────

def handle_novelty_detect(**kwargs: Any) -> dict[str, Any]:
    """Detect novelty in content — scores how new/surprising a piece of information is."""
    try:
        from whitemagic.core.patterns.emergence.novelty_detector import get_novelty_detector
        detector = get_novelty_detector()
        content = kwargs.get("content", "")
        if not content:
            return {"status": "error", "tool": "novelty.detect", "message": "content is required"}
        event = detector.detect(content, context=kwargs.get("context"))
        return {
            "status": "success",
            "tool": "novelty.detect",
            "details": {
                "content_hash": event.hash_id,
                "novelty_score": event.novelty_score,
                "is_novel": event.novelty_score > 0.5,
            },
        }
    except Exception as e:
        return {"status": "error", "tool": "novelty.detect", "message": str(e)}


def handle_novelty_stats(**kwargs: Any) -> dict[str, Any]:
    """Get novelty detection statistics."""
    try:
        from whitemagic.core.patterns.emergence.novelty_detector import get_novelty_detector
        detector = get_novelty_detector()
        return {"status": "success", "tool": "novelty.stats", "details": detector.get_novelty_stats()}
    except Exception as e:
        return {"status": "error", "tool": "novelty.stats", "message": str(e)}


# ── BridgeSynthesizer ──────────────────────────────────────────

def handle_bridge_synthesize(**kwargs: Any) -> dict[str, Any]:
    """Synthesize insights from graph bridge nodes."""
    try:
        from whitemagic.core.memory.bridge_synthesizer import get_bridge_synthesizer
        synth = get_bridge_synthesizer()
        bridge_nodes = kwargs.get("bridge_nodes", [])
        if not bridge_nodes:
            try:
                from whitemagic.core.memory.graph_engine import get_graph_engine
                ge = get_graph_engine()
                topology = ge.compute_topology()
                bridge_nodes = topology.get("bridge_nodes", [])[:10]
            except Exception:
                bridge_nodes = []
        if not bridge_nodes:
            return {"status": "success", "tool": "bridge.synthesize", "details": {"insights": [], "message": "No bridge nodes available"}}
        insights = synth.synthesize_from_bridges(bridge_nodes, top_n=kwargs.get("top_n", 5))
        return {
            "status": "success",
            "tool": "bridge.synthesize",
            "details": {
                "insights": [i.to_dict() for i in insights],
                "stats": synth.get_stats(),
            },
        }
    except Exception as e:
        return {"status": "error", "tool": "bridge.synthesize", "message": str(e)}


# ── GalacticMap (direct access) ─────────────────────────────────

def handle_galactic_sweep(**kwargs: Any) -> dict[str, Any]:
    """Run a full galactic sweep — map memories to galactic zones."""
    try:
        from whitemagic.core.memory.galactic_map import get_galactic_map
        gmap = get_galactic_map()
        limit = kwargs.get("limit", 50000)
        report = gmap.full_sweep(limit=limit)
        return {"status": "success", "tool": "galactic.sweep", "details": report}
    except Exception as e:
        return {"status": "error", "tool": "galactic.sweep", "message": str(e)}


def handle_galactic_stats(**kwargs: Any) -> dict[str, Any]:
    """Get galactic zone distribution statistics."""
    try:
        from whitemagic.core.memory.galactic_map import get_galactic_map
        gmap = get_galactic_map()
        zones = gmap.get_zone_counts()
        return {"status": "success", "tool": "galactic.stats", "details": zones}
    except Exception as e:
        return {"status": "error", "tool": "galactic.stats", "message": str(e)}


# ── GuidelineEvolution ──────────────────────────────────────────

def handle_guideline_evolve(**kwargs: Any) -> dict[str, Any]:
    """Evolve system guidelines based on pattern analysis."""
    try:
        from whitemagic.core.patterns.emergence.guideline_evolution import get_guideline_evolver
        evolver = get_guideline_evolver()
        result = evolver.evolve()
        return {"status": "success", "tool": "guideline.evolve", "details": result}
    except Exception as e:
        return {"status": "error", "tool": "guideline.evolve", "message": str(e)}


# ── ElementalOptimization ──────────────────────────────────────

def handle_elemental_optimize(**kwargs: Any) -> dict[str, Any]:
    """Run Wu Xing-based elemental optimization."""
    try:
        from whitemagic.core.intelligence.elemental_optimization import get_optimizer
        optimizer = get_optimizer()
        target = kwargs.get("target", "balance")
        result = optimizer.optimize(target=target)
        return {"status": "success", "tool": "elemental.optimize", "details": result}
    except Exception as e:
        return {"status": "error", "tool": "elemental.optimize", "message": str(e)}


# ── PatternConsciousness ────────────────────────────────────────

def handle_pattern_consciousness_status(**kwargs: Any) -> dict[str, Any]:
    """Get the status of the autonomous pattern consciousness system."""
    try:
        from whitemagic.core.patterns.pattern_consciousness import get_pattern_consciousness
        pc = get_pattern_consciousness()
        return {"status": "success", "tool": "pattern_consciousness.status", "details": pc.get_status()}
    except Exception as e:
        return {"status": "error", "tool": "pattern_consciousness.status", "message": str(e)}
