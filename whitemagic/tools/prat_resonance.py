"""PRAT Resonance — Session-Level Gana Resonance State.
=====================================================
Manages per-session resonance context so that sequential PRAT calls
benefit from predecessor/successor awareness, lunar amplification,
and Harmony Vector adaptation.

When an AI client calls gana_ghost(tool="gnosis"), the resonance layer:
1. Looks up the predecessor Gana's output (from the last PRAT call)
2. Gets current lunar phase and Harmony Vector snapshot
3. Builds ResonanceHints and attaches them to the response
4. Stores this call's output as predecessor context for the next call

This creates implicit resonance across sequential tool invocations —
the system "remembers" what Gana was last active and feeds that
context forward, even without explicit GanaChain orchestration.
"""

import logging
import threading
import time
from collections import deque
from dataclasses import asdict, dataclass
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Gana metadata (avoids importing heavy quadrant modules at module level)
# ---------------------------------------------------------------------------

# gana_name → (mansion_number, quadrant, meaning, garden, chinese, pinyin)
_GANA_META: dict[str, tuple] = {
    "gana_horn":             (1,  "East",  "Sharp initiation",   "Courage",        "角", "Jiao"),
    "gana_neck":             (2,  "East",  "Stability",          "Practice",       "亢", "Kang"),
    "gana_root":             (3,  "East",  "Foundation",         "Truth",          "氐", "Di"),
    "gana_room":             (4,  "East",  "Enclosure",          "Sanctuary",      "房", "Fang"),
    "gana_heart":            (5,  "East",  "Vital pulse",        "Love",           "心", "Xin"),
    "gana_tail":             (6,  "East",  "Passionate drive",   "Metal",          "尾", "Wei"),
    "gana_winnowing_basket": (7,  "East",  "Separation",         "Wisdom",         "箕", "Ji"),
    "gana_ghost":            (8,  "South", "Introspection",      "Grief",          "鬼", "Gui"),
    "gana_willow":           (9,  "South", "Flexibility",        "Play",           "柳", "Liu"),
    "gana_star":             (10, "South", "Illumination",       "Beauty",         "星", "Xing"),
    "gana_extended_net":     (11, "South", "Connectivity",       "Connection",     "张", "Zhang"),
    "gana_wings":            (12, "South", "Expansion",          "Adventure",      "翼", "Yi"),
    "gana_chariot":          (13, "South", "Movement",           "Transformation", "轸", "Zhen"),
    "gana_abundance":        (14, "South", "Surplus",            "Joy",            "豐", "Feng"),
    "gana_straddling_legs":  (15, "West",  "Balance",            "Patience",       "奎", "Kui"),
    "gana_mound":            (16, "West",  "Accumulation",       "Gratitude",      "娄", "Lou"),
    "gana_stomach":          (17, "West",  "Nourishment",        "Healing",        "胃", "Wei"),
    "gana_hairy_head":       (18, "West",  "Detail",             "Presence",       "昴", "Mao"),
    "gana_net":              (19, "West",  "Capture",            "Mystery",        "毕", "Bi"),
    "gana_turtle_beak":      (20, "West",  "Precision",          "Voice",          "觜", "Zui"),
    "gana_three_stars":      (21, "West",  "Judgment",           "Dharma",         "参", "Shen"),
    "gana_dipper":           (22, "North", "Governance",         "Awe",            "斗", "Dou"),
    "gana_ox":               (23, "North", "Endurance",          "Reverence",      "牛", "Niu"),
    "gana_girl":             (24, "North", "Nurture",            "Wonder",         "女", "Nu"),
    "gana_void":             (25, "North", "Emptiness",          "Stillness",      "虚", "Xu"),
    "gana_roof":             (26, "North", "Shelter",            "Protection",     "危", "Wei"),
    "gana_encampment":       (27, "North", "Structure",          "Sangha",         "室", "Shi"),
    "gana_wall":             (28, "North", "Boundaries",         "Air",            "壁", "Bi"),
}

# Ordered list for predecessor/successor lookup
_GANA_ORDER = [
    "gana_horn", "gana_neck", "gana_root", "gana_room", "gana_heart",
    "gana_tail", "gana_winnowing_basket", "gana_ghost", "gana_willow",
    "gana_star", "gana_extended_net", "gana_wings", "gana_chariot",
    "gana_abundance", "gana_straddling_legs", "gana_mound", "gana_stomach",
    "gana_hairy_head", "gana_net", "gana_turtle_beak", "gana_three_stars",
    "gana_dipper", "gana_ox", "gana_girl", "gana_void", "gana_roof",
    "gana_encampment", "gana_wall",
]

_GANA_INDEX = {name: i for i, name in enumerate(_GANA_ORDER)}


def _get_predecessor_gana(gana_name: str) -> str:
    """Get the predecessor Gana in the circular 28-mansion sequence."""
    idx = _GANA_INDEX.get(gana_name, 0)
    return _GANA_ORDER[(idx - 1) % 28]


def _get_successor_gana(gana_name: str) -> str:
    """Get the successor Gana in the circular 28-mansion sequence."""
    idx = _GANA_INDEX.get(gana_name, 0)
    return _GANA_ORDER[(idx + 1) % 28]


def _get_meta(gana_name: str) -> dict[str, Any]:
    """Get metadata dict for a Gana."""
    meta = _GANA_META.get(gana_name)
    if not meta:
        return {"mansion_num": 0, "quadrant": "Unknown", "meaning": gana_name,
                "garden": None, "chinese": "?", "pinyin": "?"}
    return {
        "mansion_num": meta[0],
        "quadrant": meta[1],
        "meaning": meta[2],
        "garden": meta[3],
        "chinese": meta[4],
        "pinyin": meta[5],
    }


# ---------------------------------------------------------------------------
# Resonance Snapshot (one per PRAT invocation)
# ---------------------------------------------------------------------------

@dataclass
class ResonanceSnapshot:
    """Captured resonance context from a single PRAT invocation."""

    gana_name: str
    tool_name: str | None
    operation: str | None
    output_preview: str
    timestamp: float = 0.0
    lunar_phase: float = 0.0
    lunar_mansion_num: int = 0
    harmony_score: float = 1.0
    guna_tag: str = "rajasic"
    quadrant: str = "East"
    successor_hint: str = ""

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


# ---------------------------------------------------------------------------
# Session Resonance State (singleton)
# ---------------------------------------------------------------------------

class PratResonanceState:
    """Thread-safe per-session resonance state for PRAT calls.

    Tracks:
    - Last Gana invoked + output (predecessor context)
    - Per-Gana invocation counts (for future learning)
    - Session call sequence number
    """

    def __init__(self, max_history: int = 50):
        self._lock = threading.Lock()
        self._last_snapshot: ResonanceSnapshot | None = None
        self._history: deque = deque(maxlen=max_history)
        self._gana_counts: dict[str, int] = {}
        self._session_calls: int = 0

    def get_predecessor(self) -> ResonanceSnapshot | None:
        """Get the last invocation's resonance snapshot."""
        with self._lock:
            return self._last_snapshot

    def record(self, snapshot: ResonanceSnapshot) -> None:
        """Record a completed PRAT invocation."""
        with self._lock:
            self._last_snapshot = snapshot
            self._history.append(snapshot)
            self._gana_counts[snapshot.gana_name] = (
                self._gana_counts.get(snapshot.gana_name, 0) + 1
            )
            self._session_calls += 1

    @property
    def call_count(self) -> int:
        with self._lock:
            return self._session_calls

    def get_gana_counts(self) -> dict[str, int]:
        """Per-Gana invocation counts."""
        with self._lock:
            return dict(self._gana_counts)

    def get_recent_history(self, limit: int = 10) -> list[dict[str, Any]]:
        """Recent resonance snapshots."""
        with self._lock:
            return [s.to_dict() for s in list(self._history)[-limit:]]

    def reset(self) -> None:
        """Reset session state (e.g., on new session)."""
        with self._lock:
            self._last_snapshot = None
            self._history.clear()
            self._gana_counts.clear()
            self._session_calls = 0


# Singleton
_state: PratResonanceState | None = None
_state_lock = threading.Lock()


def get_resonance_state() -> PratResonanceState:
    """Get or create the global resonance state singleton."""
    global _state
    if _state is None:
        with _state_lock:
            if _state is None:
                _state = PratResonanceState()
    return _state


# ---------------------------------------------------------------------------
# Harmony Vector + Lunar Phase helpers (lazy imports)
# ---------------------------------------------------------------------------

def _get_harmony_snapshot() -> dict[str, Any]:
    """Get current Harmony Vector snapshot (safe fallback if unavailable)."""
    try:
        from whitemagic.harmony.vector import get_harmony_vector
        hv = get_harmony_vector()
        snap = hv.snapshot()
        return {
            "harmony_score": snap.harmony_score,
            "guna_dominant": _dominant_guna(snap),
            "energy": snap.energy,
            "error_rate": snap.error_rate,
            "dharma": snap.dharma,
        }
    except Exception:
        return {
            "harmony_score": 1.0,
            "guna_dominant": "rajasic",
            "energy": 1.0,
            "error_rate": 1.0,
            "dharma": 1.0,
        }


def _dominant_guna(snap: Any) -> str:
    """Determine dominant Guna from snapshot percentages."""
    gunas = {
        "sattvic": getattr(snap, "guna_sattvic_pct", 0),
        "rajasic": getattr(snap, "guna_rajasic_pct", 0),
        "tamasic": getattr(snap, "guna_tamasic_pct", 0),
    }
    return max(gunas, key=gunas.get)  # type: ignore[arg-type]


def _get_lunar_phase() -> tuple:
    """Get (phase_float, mansion_number). Safe fallback."""
    try:
        from whitemagic.core.ganas.lunar import (
            _mock_lunar_mansion,
            get_current_lunar_phase,
        )
        phase = get_current_lunar_phase()
        mansion = _mock_lunar_mansion()
        return (phase, mansion.number)
    except Exception:
        import time as _t
        days = _t.time() / 86400
        phase = (days % 29.53059) / 29.53059
        mansion_num = int(((days % 27.321661) / 27.321661) * 28) + 1
        return (phase, mansion_num)


# ---------------------------------------------------------------------------
# Core API: build resonance context + record after call
# ---------------------------------------------------------------------------

def build_resonance_context(gana_name: str) -> dict[str, Any]:
    """Build resonance context BEFORE a PRAT call executes.

    Returns a dict with:
    - predecessor info (last Gana's output preview, name, tool)
    - lunar phase + alignment check
    - harmony vector snapshot
    - successor hint (which Gana follows in the 28-mansion circle)
    - guna adaptation hint (minimal/optimal/normal mode)
    """
    state = get_resonance_state()
    meta = _get_meta(gana_name)
    predecessor = state.get_predecessor()
    harmony = _get_harmony_snapshot()
    lunar_phase, lunar_mansion = _get_lunar_phase()

    # Check lunar alignment (Moon is in this Gana's mansion)
    lunar_aligned = (lunar_mansion == meta["mansion_num"])

    # Guna adaptation hint
    guna = harmony["guna_dominant"]
    if guna == "tamasic":
        mode_hint = "minimal"
    elif guna == "sattvic":
        mode_hint = "optimal"
    else:
        mode_hint = "normal"

    # Successor in the 28-mansion circle
    successor_name = _get_successor_gana(gana_name)
    successor_meta = _get_meta(successor_name)

    ctx: dict[str, Any] = {
        "gana": gana_name,
        "mansion_num": meta["mansion_num"],
        "quadrant": meta["quadrant"],
        "meaning": meta["meaning"],
        "garden": meta["garden"],
        "chinese": meta["chinese"],
        "lunar_phase": round(lunar_phase, 4),
        "lunar_aligned": lunar_aligned,
        "harmony_score": round(harmony["harmony_score"], 4),
        "guna": guna,
        "mode_hint": mode_hint,
        "chain_position": state.call_count,
        "successor": {
            "gana": successor_name,
            "meaning": successor_meta["meaning"],
        },
    }

    # Predecessor context (from last PRAT call)
    if predecessor:
        ctx["predecessor"] = {
            "gana": predecessor.gana_name,
            "tool": predecessor.tool_name,
            "output_preview": predecessor.output_preview,
            "timestamp": predecessor.timestamp,
        }

    # Lunar amplification note
    if lunar_aligned:
        ctx["lunar_amplification"] = (
            f"Moon is in {meta['chinese']} ({meta['pinyin']}) — "
            f"this Gana is amplified"
        )

    # Gana vitality / reputation (12.108.20 — honor competence)
    try:
        from whitemagic.tools.gana_vitality import get_vitality_monitor
        monitor = get_vitality_monitor()
        rep = monitor.get_reputation(gana_name)
        ctx["gana_reputation"] = {
            "success_rate": rep["success_rate"],
            "avg_latency_ms": rep["avg_latency_ms"],
            "vitality": rep["vitality"],
            "total_calls": rep["total_calls"],
            "consecutive_failures": rep["consecutive_failures"],
        }
        # If degraded, add warning (12.108.29 — silence = defeat)
        if rep["vitality"] in ("degraded", "struggling"):
            ctx["vitality_warning"] = (
                f"{gana_name} vitality is {rep['vitality']} "
                f"(success_rate={rep['success_rate']:.0%}, "
                f"consecutive_failures={rep['consecutive_failures']}). "
                f"Consider routing to a peer Gana."
            )
    except Exception:
        pass

    return ctx


def _compute_prat_economics(
    gana_name: str,
    tool_name: str | None,
    state: PratResonanceState,
) -> dict[str, Any]:
    """Compute PRAT economics metadata for Leap 8 Swarm protocol.

    Tracks per-Gana compute cost estimates, session totals, and
    cost-per-tool hints that agents can use for budget planning.
    """
    # Base compute cost units per category (unitless, relative)
    _CATEGORY_COST: dict[str, float] = {
        "East": 1.0,    # Core operations (session, memory, health)
        "South": 1.5,   # Analytical (introspection, patterns, export)
        "West": 2.0,    # Ethical + reasoning (dharma, synthesis, inference)
        "North": 2.5,   # Governance + coordination (swarm, agents, voting)
    }
    meta = _GANA_META.get(gana_name)
    quadrant = meta[1] if meta else "East"
    base_cost = _CATEGORY_COST.get(quadrant, 1.0)

    # Scale by safety level
    safety_multiplier = 1.0
    if tool_name:
        # Write/delete tools cost more (they do more work)
        try:
            from whitemagic.tools.registry import TOOL_REGISTRY
            td = next((t for t in TOOL_REGISTRY if t.name == tool_name), None)
            if td and td.safety.value == "write":
                safety_multiplier = 1.5
            elif td and td.safety.value == "delete":
                safety_multiplier = 2.0
        except Exception:
            pass

    call_cost = round(base_cost * safety_multiplier, 2)

    # Session totals
    gana_counts = state.get_gana_counts()
    session_total_cost = sum(
        _CATEGORY_COST.get(_GANA_META.get(g, (0, "East"))[1], 1.0) * count
        for g, count in gana_counts.items()
    )

    return {
        "call_cost_units": call_cost,
        "quadrant_base": base_cost,
        "safety_multiplier": safety_multiplier,
        "session_total_cost": round(session_total_cost, 2),
        "session_total_calls": state.call_count,
        "gana_call_count": gana_counts.get(gana_name, 0),
    }


def record_resonance(
    gana_name: str,
    tool_name: str | None,
    operation: str | None,
    result: Any,
) -> dict[str, Any]:
    """Record resonance state AFTER a PRAT call completes.

    Returns the resonance metadata that should be injected into the response.
    """
    state = get_resonance_state()
    meta = _get_meta(gana_name)
    harmony = _get_harmony_snapshot()
    lunar_phase, lunar_mansion = _get_lunar_phase()

    # Build output preview (truncated for context passing)
    if isinstance(result, dict):
        preview_parts = []
        for key in ("status", "action", "note", "error"):
            if key in result:
                preview_parts.append(f"{key}={result[key]}")
        if preview_parts:
            preview = "; ".join(preview_parts)
        else:
            preview = str(result)[:200]
    else:
        preview = str(result)[:200]

    # Successor hint
    successor_name = _get_successor_gana(gana_name)
    successor_meta = _get_meta(successor_name)
    successor_hint = (
        f"Prepared for {successor_meta['pinyin']} ({successor_meta['meaning']}): "
        f"{preview[:100]}"
    )

    snapshot = ResonanceSnapshot(
        gana_name=gana_name,
        tool_name=tool_name,
        operation=operation,
        output_preview=preview[:200],
        timestamp=time.time(),
        lunar_phase=lunar_phase,
        lunar_mansion_num=lunar_mansion,
        harmony_score=harmony["harmony_score"],
        guna_tag=harmony["guna_dominant"],
        quadrant=meta["quadrant"],
        successor_hint=successor_hint,
    )

    state.record(snapshot)

    # Leap 8: PRAT economics metadata
    economics = _compute_prat_economics(gana_name, tool_name, state)

    return {
        "gana": gana_name,
        "garden": meta["garden"],
        "quadrant": meta["quadrant"],
        "lunar_phase": round(lunar_phase, 4),
        "lunar_aligned": (lunar_mansion == meta["mansion_num"]),
        "harmony_score": round(harmony["harmony_score"], 4),
        "guna": harmony["guna_dominant"],
        "chain_position": state.call_count,
        "successor_hint": successor_hint,
        "_prat_economics": economics,
    }


def get_resonance_summary() -> dict[str, Any]:
    """Get a summary of the current resonance state (for Gnosis/introspection)."""
    state = get_resonance_state()
    predecessor = state.get_predecessor()
    harmony = _get_harmony_snapshot()
    lunar_phase, lunar_mansion = _get_lunar_phase()

    # Find which Gana the moon is currently in
    current_lunar_gana = None
    for name, meta in _GANA_META.items():
        if meta[0] == lunar_mansion:
            current_lunar_gana = name
            break

    return {
        "session_calls": state.call_count,
        "gana_counts": state.get_gana_counts(),
        "last_gana": predecessor.gana_name if predecessor else None,
        "last_tool": predecessor.tool_name if predecessor else None,
        "lunar_phase": round(lunar_phase, 4),
        "lunar_mansion": lunar_mansion,
        "lunar_gana": current_lunar_gana,
        "harmony_score": round(harmony["harmony_score"], 4),
        "guna_dominant": harmony["guna_dominant"],
        "recent_history": state.get_recent_history(5),
    }
