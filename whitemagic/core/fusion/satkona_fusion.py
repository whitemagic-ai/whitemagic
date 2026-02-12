#!/usr/bin/env python3
"""Satkona Fusion Module — Phase 2 Integration.
============================================
Wires three external systems into Satkona's multi-signal ranking:

1. Wu Xing → Satkona: Elemental phase-based signal weight modulation
2. Constellation → Satkona: Spatial novelty prior from under-explored regions
3. Dream Daemon → Satkona: Feedback loop from dream-generated insights

Usage:
    from scripts.satkona_fusion import get_elemental_weights, get_constellation_prior, get_dream_feedback

    # In satkona.py fuse_signals():
    weights = get_elemental_weights()  # Modulated by current Wu Xing phase

Created: 2026-02-05
Phase: 2 (Fusion Opportunities)
"""

import json
import os
import sqlite3
import subprocess
from datetime import datetime
from importlib.util import find_spec
from pathlib import Path
from typing import Any

from whitemagic.config.paths import DB_PATH as MEM_DB

ROOT = Path(__file__).resolve().parents[2]


# ---------------------------------------------------------------------------
# Fusion 1: Wu Xing → Satkona (Elemental Phase-Based Signal Weighting)
# ---------------------------------------------------------------------------
# The 5 elements modulate which signals are emphasized based on system phase:
#   Wood (木)  → Growth phase: boost novelty (exploration)
#   Fire (火)  → Expansion: boost alignment (validation)
#   Earth (土) → Stability: boost frequency (reliability)
#   Metal (金) → Refinement: boost yin_idf (specificity)
#   Water (水) → Reflection: boost pagerank (structure)

ELEMENT_SIGNAL_AFFINITIES = {
    "wood":  {"novelty": 0.45, "yin_idf": 0.20, "pagerank": 0.15, "alignment": 0.10, "freq": 0.10},
    "fire":  {"novelty": 0.25, "yin_idf": 0.20, "pagerank": 0.15, "alignment": 0.30, "freq": 0.10},
    "earth": {"novelty": 0.20, "yin_idf": 0.20, "pagerank": 0.20, "alignment": 0.10, "freq": 0.30},
    "metal": {"novelty": 0.25, "yin_idf": 0.40, "pagerank": 0.15, "alignment": 0.10, "freq": 0.10},
    "water": {"novelty": 0.30, "yin_idf": 0.15, "pagerank": 0.35, "alignment": 0.10, "freq": 0.10},
}

# Default weights (balanced)
DEFAULT_SIGNAL_WEIGHTS = {
    "novelty":   0.35,
    "yin_idf":   0.25,
    "pagerank":  0.20,
    "alignment": 0.10,
    "freq":      0.10,
}


def get_current_element() -> str:
    """Determine the current dominant element based on Wu Xing state.
    Falls back to hour-based approximation if WuXingEngine unavailable.
    """
    try:
        from whitemagic.wu_xing import Element, get_wuxing_engine
        engine = get_wuxing_engine()

        # Find element with highest energy
        max_energy: float = -1
        dominant = "wood"
        for elem in Element:
            state = engine.get_element(elem)
            if state.energy > max_energy:
                max_energy = state.energy
                dominant = elem.value
        return dominant
    except ImportError:
        pass

    # Fallback: hour-based element approximation (traditional Chinese hour mapping)
    hour = datetime.now().hour
    if 3 <= hour < 7:      # Yin Wood/Yang Wood
        return "wood"
    elif 7 <= hour < 11:   # Yang Fire
        return "fire"
    elif 11 <= hour < 15:  # Yin Fire / Yang Earth
        return "earth"
    elif 15 <= hour < 19:  # Yang Metal
        return "metal"
    else:                  # Water hours (19-03)
        return "water"


def get_elemental_weights() -> dict[str, float]:
    """Get Satkona signal weights modulated by current Wu Xing element.

    Returns:
        Dict mapping signal names to their weights (sum = 1.0)

    """
    element = get_current_element()
    weights = ELEMENT_SIGNAL_AFFINITIES.get(element, DEFAULT_SIGNAL_WEIGHTS)

    # Normalize to ensure sum = 1.0
    total = sum(weights.values())
    return {k: v / total for k, v in weights.items()}


def get_element_blend(intensity: float = 1.0) -> dict[str, float]:
    """Blend default weights with elemental weights.

    Args:
        intensity: 0.0 = pure default, 1.0 = pure elemental

    Returns:
        Blended weight dictionary

    """
    elemental = get_elemental_weights()
    blended = {}
    for signal in DEFAULT_SIGNAL_WEIGHTS:
        base = DEFAULT_SIGNAL_WEIGHTS[signal]
        elem = elemental.get(signal, base)
        blended[signal] = base * (1 - intensity) + elem * intensity

    # Normalize
    total = sum(blended.values())
    return {k: v / total for k, v in blended.items()}


# ---------------------------------------------------------------------------
# Fusion 2: Constellation → Satkona (Spatial Novelty Prior)
# ---------------------------------------------------------------------------
# Use ConstellationSearch to identify under-explored regions of holographic
# space and boost strategies that emerge from those regions.

def get_constellation_density(conn: sqlite3.Connection | None = None) -> dict[str, float]:
    """Compute density map of holographic space.
    Returns dict of quadrant -> memory count.
    """
    if conn is None:
        conn = sqlite3.connect(str(MEM_DB))
        close_conn = True
    else:
        close_conn = False

    try:
        # Count memories per quadrant using SQL
        rows = conn.execute("""
            SELECT
                CASE
                    WHEN x >= 0 AND y >= 0 THEN 'logic-macro'
                    WHEN x >= 0 AND y < 0 THEN 'logic-micro'
                    WHEN x < 0 AND y >= 0 THEN 'emotion-macro'
                    ELSE 'emotion-micro'
                END as quadrant,
                COUNT(*) as cnt
            FROM holographic_coords
            GROUP BY quadrant
        """).fetchall()

        density: dict[str, float] = {q: 0.0 for q in ["logic-macro", "logic-micro", "emotion-macro", "emotion-micro"]}
        for quadrant, cnt in rows:
            density[quadrant] = cnt

        return density
    finally:
        if close_conn:
            conn.close()


def get_constellation_novelty_prior(
    clusters: dict[Any, list[str]],
    memories: dict[str, dict[str, Any]],
    conn: sqlite3.Connection | None = None,
) -> dict[Any, float]:
    """Compute a novelty prior for each cluster based on how sparse its
    holographic region is. Clusters in under-explored regions get boosted.

    Args:
        clusters: {(cx, cy): [mem_ids]}
        memories: {mid: {..., x, y, z, w}}

    Returns:
        {(cx, cy): novelty_prior} where higher = more novel region

    """
    density = get_constellation_density(conn)
    total = sum(density.values()) or 1

    # Invert density → sparsity (novelty)
    sparsity = {q: 1.0 - (density[q] / total) for q in density}

    cluster_priors = {}
    for key, mem_ids in clusters.items():
        # Determine dominant quadrant for this cluster
        quadrant_votes = {"logic-macro": 0, "logic-micro": 0,
                         "emotion-macro": 0, "emotion-micro": 0}
        for mid in mem_ids:
            m = memories.get(mid)
            if m:
                x, y = m.get("x", 0), m.get("y", 0)
                if x >= 0 and y >= 0:
                    quadrant_votes["logic-macro"] += 1
                elif x >= 0:
                    quadrant_votes["logic-micro"] += 1
                elif y >= 0:
                    quadrant_votes["emotion-macro"] += 1
                else:
                    quadrant_votes["emotion-micro"] += 1

        # Weighted average of sparsity by quadrant membership
        total_votes = sum(quadrant_votes.values()) or 1
        prior = sum(sparsity[q] * (quadrant_votes[q] / total_votes)
                   for q in quadrant_votes)
        cluster_priors[key] = prior

    return cluster_priors


# ---------------------------------------------------------------------------
# Fusion 3: Dream Daemon → Satkona (Feedback Loop)
# ---------------------------------------------------------------------------
# Read dream-generated insights and use them to boost strategies that
# align with recent dream themes.

def get_dream_insights(limit: int = 10) -> list:
    """Fetch recent dream-generated insights from memory.

    Returns:
        List of insight content strings

    """
    try:
        conn = sqlite3.connect(str(MEM_DB))
        rows = conn.execute("""
            SELECT content FROM memories
            WHERE memory_type = 'dream_insight'
            OR (metadata LIKE '%dream%' AND memory_type = 'insight')
            ORDER BY created_at DESC
            LIMIT ?
        """, (limit,)).fetchall()
        conn.close()
        return [r[0] for r in rows if r[0]]
    except Exception:
        return []


def get_dream_feedback_signal(clusters: dict, agg: dict) -> dict:
    """Compute feedback signal from dream insights.
    Strategies that contain words from recent dreams get boosted.

    Args:
        clusters: {(cx, cy): [mem_ids]}
        agg: {(cx, cy): {solutions: Counter, ...}}

    Returns:
        {(cx, cy): dream_feedback_score}

    """
    insights = get_dream_insights()
    if not insights:
        # No dream data → neutral signal
        return {k: 0.5 for k in clusters}

    # Extract dream vocabulary
    dream_words = set()
    for insight in insights:
        words = insight.lower().split()
        for w in words:
            clean = "".join(c for c in w if c.isalnum())
            if len(clean) > 3:  # Ignore short words
                dream_words.add(clean)

    if not dream_words:
        return {k: 0.5 for k in clusters}

    # Score each cluster by overlap with dream vocabulary
    scores = {}
    for key in clusters:
        data = agg.get(key)
        if not data:
            scores[key] = 0.0
            continue

        cluster_words = set()
        for cat in ("solutions", "anti_patterns", "heuristics", "optimizations"):
            for pattern in data.get(cat, {}).keys():
                for w in pattern.lower().split():
                    clean = "".join(c for c in w if c.isalnum())
                    if len(clean) > 3:
                        cluster_words.add(clean)

        if not cluster_words:
            scores[key] = 0.0
            continue

        # Jaccard-like overlap
        overlap = len(dream_words & cluster_words)
        scores[key] = overlap / (len(dream_words) + len(cluster_words) - overlap + 1)

    return scores


# ---------------------------------------------------------------------------
# Fusion 4: Polyglot Resonance (Rust/Haskell/Julia)
# ---------------------------------------------------------------------------


def get_haskell_balance() -> float:
    """Consult the I Ching (Haskell) for a global balance coefficient (0.0-1.0)."""
    try:
        # Use existing shim
        env = os.environ.copy()
        shim_path = str(ROOT.parent / "whitemagic" / "lib_shim")
        env["LD_LIBRARY_PATH"] = f"{shim_path}:{env.get('LD_LIBRARY_PATH', '')}"

        # Determine path to Haskell app
        haskell_src = str(ROOT.parent / "haskell" / "app" / "Main.hs")
        if not os.path.exists(haskell_src):
            return 0.5

        result = subprocess.run(
            ["runghc", "-ihaskell/src", haskell_src],
            cwd=str(ROOT.parent),
            capture_output=True, text=True, timeout=5, env=env,
        )
        # Parse output for Hexagram info (simplified for now)
        if "Hexagram" in result.stdout:
            return 0.8  # Auspicious
        return 0.5
    except Exception:
        return 0.5

def get_julia_resonance(impulse: float = 0.5) -> float:
    """Calculate persistent resonance (Julia) for a memory impulse."""
    try:
        julia_script = str(ROOT.parent / "whitemagic-julia" / "src" / "gan_ying.jl")
        if not os.path.exists(julia_script):
            return 0.0

        input_data = json.dumps({"magnitude": impulse, "damping": 0.1})
        result = subprocess.run(
            ["julia", julia_script, input_data],
            capture_output=True, text=True, timeout=10,
        )
        if result.returncode == 0:
            data = json.loads(result.stdout)
            return float(min(data.get("peak_amplitude", 0.0), 1.0))
        return 0.0
    except Exception:
        return 0.0

def get_rust_acceleration(query_vec: list) -> list:
    """Use Rust SIMD for rapid candidate selection (stub for high-volume fusion)."""
    if find_spec("whitemagic_rs") is None:
        return []
    # In a real scenario, this would call whitemagic_rs.search(query_vec)
    # For fusion, we just check presence
    return []

# ---------------------------------------------------------------------------
# Combined Fusion: Integrate ALL systems into Satkona
# ---------------------------------------------------------------------------

def fuse_signals_with_fusion(
    signals: dict[str, dict[Any, float]],
    clusters: dict[Any, list[str]],
    memories: dict[str, dict[str, Any]],
    agg: dict[Any, dict[str, Any]],
    elemental_intensity: float = 0.5,
    constellation_weight: float = 0.1,
    dream_weight: float = 0.05,
    conn: sqlite3.Connection | None = None,
) -> dict[Any, float]:
    """Enhanced signal fusion incorporating all three external systems.

    Args:
        signals: Original Satkona signals {name: {cluster_key: score}}
        clusters: Cluster membership
        memories: Memory data
        agg: Aggregated patterns per cluster
        elemental_intensity: How much to use Wu Xing modulation (0-1)
        constellation_weight: Weight for constellation novelty prior
        dream_weight: Weight for dream feedback

    Returns:
        {cluster_key: final_relevance}

    """
    # Get elemental weights
    base_weights = get_element_blend(elemental_intensity)

    # Normalize each signal independently
    def normalise(d: dict[Any, float]) -> dict[Any, float]:
        if not d:
            return d
        mn, mx = min(d.values()), max(d.values())
        rng = mx - mn or 1.0
        return {k: (v - mn) / rng for k, v in d.items()}

    normed = {name: normalise(vals) for name, vals in signals.items()}

    # Union of all cluster keys
    all_keys: set[Any] = set()
    for vals in normed.values():
        all_keys.update(vals.keys())

    # Base relevance with elemental weights
    relevance = {}
    for k in all_keys:
        score = 0.0
        for name, w in base_weights.items():
            score += w * normed.get(name, {}).get(k, 0.0)
        relevance[k] = score

    # Add constellation novelty prior
    if constellation_weight > 0:
        constellation_prior = get_constellation_novelty_prior(clusters, memories, conn)
        constellation_norm = normalise(constellation_prior)
        for k in all_keys:
            relevance[k] += constellation_weight * constellation_norm.get(k, 0.0)

    # Add dream feedback
    if dream_weight > 0:
        dream_feedback = get_dream_feedback_signal(clusters, agg)
        dream_norm = normalise(dream_feedback)
        for k in all_keys:
            relevance[k] += dream_weight * dream_norm.get(k, 0.0)

    # Add Polyglot Signals (Haskell Balance + Julia Resonance)
    # These act as global multipliers or bias terms
    balance = get_haskell_balance() # 0.5-1.0
    resonance = get_julia_resonance(0.8) # 0.0-1.0

    # Apply global modulation
    # If balance is high, we favor existing structure (PageRank/Frequency)
    # If balance is low, we favor Novelty
    if balance > 0.6:
        # Boost stability signals
        for k in all_keys: # straightforward bias
            relevance[k] *= (1.0 + resonance * 0.1)

    # Re-normalize final scores
    return dict(normalise(relevance))


# ---------------------------------------------------------------------------
# CLI Test
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    print("=" * 60)
    print("  SATKONA FUSION MODULE — Phase 2 Integration")
    print("=" * 60)

    # Test Wu Xing integration
    element = get_current_element()
    weights = get_elemental_weights()
    print("\n[1] Wu Xing Integration")
    print(f"    Current element: {element}")
    print(f"    Signal weights: {weights}")

    # Test Constellation integration
    print("\n[2] Constellation Integration")
    density = get_constellation_density()
    print(f"    Quadrant density: {density}")
    total = sum(density.values())
    if total > 0:
        sparsity = {q: 1.0 - (density[q] / total) for q in density}
        print(f"    Sparsity (novelty): {sparsity}")

    # Test Dream integration
    print("\n[3] Dream Daemon Integration")
    insights = get_dream_insights(limit=3)
    print(f"    Recent dream insights: {len(insights)}")
    if insights:
        for i, insight in enumerate(insights[:3]):
            print(f"      [{i+1}] {insight[:80]}...")

    print("\n" + "=" * 60)
    print("  Fusion module ready for integration into satkona.py")
    print("=" * 60)
