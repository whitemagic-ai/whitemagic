"""Gana Sabhā — Cross-Quadrant Council Protocol.
================================================
Implements Bhīṣma's principle from Mahābhārata 12.108.25:

  "The king, acting in concert with the leaders, should do what
   is for the good of the whole order."

When a task touches multiple quadrants, a Sabhā (assembly) is convened.
Each relevant Gana's garden provides its perspective on the task.
The Three Stars (dharma/judgment) Gana arbitrates conflicts.
Results are recorded in the Karma Ledger.

Also addresses 12.108.27 — "quarrels ignored by elders metastasize"
by providing inter-Gana conflict detection.
"""

import logging
import time
from typing import Any

logger = logging.getLogger(__name__)


# Quadrant → representative Gana (the "elder" of each quadrant)
QUADRANT_ELDERS: dict[str, str] = {
    "East": "gana_winnowing_basket",  # Wisdom — separation of signal/noise
    "South": "gana_ghost",            # Introspection — deep analysis
    "West": "gana_three_stars",       # Judgment — dharmic deliberation
    "North": "gana_dipper",           # Governance — strategic oversight
}

# Gana → quadrant mapping (from prat_resonance._GANA_META)
_GANA_QUADRANT: dict[str, str] = {
    "gana_horn": "East", "gana_neck": "East", "gana_root": "East",
    "gana_room": "East", "gana_heart": "East", "gana_tail": "East",
    "gana_winnowing_basket": "East",
    "gana_ghost": "South", "gana_willow": "South", "gana_star": "South",
    "gana_extended_net": "South", "gana_wings": "South",
    "gana_chariot": "South", "gana_abundance": "South",
    "gana_straddling_legs": "West", "gana_mound": "West",
    "gana_stomach": "West", "gana_hairy_head": "West", "gana_net": "West",
    "gana_turtle_beak": "West", "gana_three_stars": "West",
    "gana_dipper": "North", "gana_ox": "North", "gana_girl": "North",
    "gana_void": "North", "gana_roof": "North",
    "gana_encampment": "North", "gana_wall": "North",
}


def _get_quadrant(gana_name: str) -> str:
    """Get the quadrant for a Gana."""
    return _GANA_QUADRANT.get(gana_name, "East")


def _get_garden_perspective(gana_name: str, task: str) -> dict[str, Any]:
    """Get a Garden's perspective on a task (lightweight, no heavy imports)."""
    try:
        from whitemagic.tools.prat_resonance import _get_meta
        meta = _get_meta(gana_name)
        return {
            "gana": gana_name,
            "quadrant": meta["quadrant"],
            "meaning": meta["meaning"],
            "garden": meta["garden"],
            "perspective": f"{meta['meaning']} lens on: {task[:100]}",
        }
    except Exception:
        return {
            "gana": gana_name,
            "perspective": f"(metadata unavailable for {gana_name})",
        }


def identify_relevant_quadrants(ganas: list[str]) -> set[str]:
    """Identify which quadrants a set of Ganas spans."""
    return {_get_quadrant(g) for g in ganas}


def should_convene_sabha(ganas: list[str]) -> bool:
    """Determine if a Sabhā should be convened.

    A Sabhā is needed when a task touches 2+ quadrants,
    indicating cross-domain coordination is required.
    """
    quadrants = identify_relevant_quadrants(ganas)
    return len(quadrants) >= 2


def convene_sabha(
    task: str,
    ganas: list[str] | None = None,
    quadrants: list[str] | None = None,
) -> dict[str, Any]:
    """Convene a Gana Sabhā (council) for cross-quadrant decisions.

    Args:
        task: Description of the task requiring council
        ganas: Specific Ganas to include (optional)
        quadrants: Specific quadrants to include (optional, defaults to all)

    Returns:
        Sabha result with perspectives, conflicts, and recommendation
    """
    start_ts = time.time()

    # Determine which quadrants are involved
    if quadrants:
        involved_quadrants = set(quadrants)
    elif ganas:
        involved_quadrants = identify_relevant_quadrants(ganas)
    else:
        involved_quadrants = {"East", "South", "West", "North"}

    # Gather elder perspectives
    perspectives: list[dict[str, Any]] = []
    for quadrant in sorted(involved_quadrants):
        elder = QUADRANT_ELDERS.get(quadrant)
        if elder:
            perspective = _get_garden_perspective(elder, task)
            perspective["role"] = "quadrant_elder"
            perspective["quadrant_represented"] = quadrant
            perspectives.append(perspective)

    # Include specific Ganas if requested
    if ganas:
        for gana_name in ganas:
            if gana_name not in [p["gana"] for p in perspectives]:
                perspective = _get_garden_perspective(gana_name, task)
                perspective["role"] = "participant"
                perspectives.append(perspective)

    # Detect potential conflicts between quadrants
    conflicts = _detect_conflicts(perspectives, task)

    # Arbiter recommendation (Three Stars / dharma)
    arbiter_recommendation = _arbiter_ruling(task, perspectives, conflicts)

    elapsed_ms = (time.time() - start_ts) * 1000

    result: dict[str, Any] = {
        "status": "success",
        "sabha_type": "full_council" if len(involved_quadrants) == 4 else "partial_council",
        "quadrants_represented": sorted(involved_quadrants),
        "perspectives": perspectives,
        "conflicts_detected": len(conflicts),
        "conflicts": conflicts,
        "arbiter": "gana_three_stars",
        "recommendation": arbiter_recommendation,
        "elapsed_ms": round(elapsed_ms, 2),
        "principle": "saṃghātam evāhur gaṇānāṃ śaraṇaṃ mahat — Unity is the great refuge of the gaṇas",
    }

    # Record in karma ledger if available
    _record_sabha_karma(result)

    return result


def _detect_conflicts(
    perspectives: list[dict[str, Any]],
    task: str,
) -> list[dict[str, Any]]:
    """Detect potential inter-Gana conflicts.

    Implements 12.108.27: "quarrels ignored by elders metastasize."
    Checks for known tension patterns between quadrants.
    """
    conflicts: list[dict[str, Any]] = []

    quadrants_present = {p.get("quadrant_represented") or p.get("quadrant", "") for p in perspectives}

    # Known tension patterns
    if "East" in quadrants_present and "West" in quadrants_present:
        # East (action/initiation) vs West (refinement/judgment)
        conflicts.append({
            "type": "pace_tension",
            "between": ["East (action)", "West (judgment)"],
            "description": "East quadrant favors speed and initiation; West favors deliberation and refinement.",
            "resolution": "Let East propose, West validate — sequential, not parallel.",
        })

    if "South" in quadrants_present and "North" in quadrants_present:
        # South (expansion/fire) vs North (conservation/water)
        conflicts.append({
            "type": "scope_tension",
            "between": ["South (expansion)", "North (conservation)"],
            "description": "South quadrant favors expansion and creation; North favors conservation and depth.",
            "resolution": "Scope to what is sustainable — expand only what can be maintained.",
        })

    # Check vitality-based conflicts
    try:
        from whitemagic.tools.gana_vitality import get_vitality_monitor
        monitor = get_vitality_monitor()
        degraded = monitor.get_degraded_ganas()
        for p in perspectives:
            if p.get("gana") in degraded:
                conflicts.append({
                    "type": "vitality_concern",
                    "gana": p["gana"],
                    "description": f"{p['gana']} is degraded — its perspective may be unreliable.",
                    "resolution": "Weight this Gana's input lower; prefer healthy peers.",
                })
    except Exception:
        pass

    return conflicts


def _arbiter_ruling(
    task: str,
    perspectives: list[dict[str, Any]],
    conflicts: list[dict[str, Any]],
) -> dict[str, Any]:
    """The Three Stars (dharma/judgment) Gana's ruling.

    Synthesizes perspectives and resolves conflicts.
    """
    if not conflicts:
        return {
            "ruling": "proceed",
            "confidence": 0.9,
            "rationale": "No inter-quadrant conflicts detected. Proceed with unified action.",
        }

    # Grade severity
    vitality_issues = [c for c in conflicts if c["type"] == "vitality_concern"]
    tension_issues = [c for c in conflicts if c["type"] != "vitality_concern"]

    if vitality_issues and not tension_issues:
        return {
            "ruling": "proceed_with_caution",
            "confidence": 0.7,
            "rationale": f"{len(vitality_issues)} Gana(s) degraded. Route around them if possible.",
            "mitigations": [c["resolution"] for c in vitality_issues],
        }

    if tension_issues:
        return {
            "ruling": "deliberate",
            "confidence": 0.6,
            "rationale": f"{len(tension_issues)} quadrant tension(s) detected. Apply resolutions sequentially.",
            "mitigations": [c["resolution"] for c in conflicts],
        }

    return {
        "ruling": "proceed",
        "confidence": 0.8,
        "rationale": "Conflicts are manageable. Proceed with noted mitigations.",
    }


def _record_sabha_karma(result: dict[str, Any]) -> None:
    """Record the Sabhā outcome in the Karma Ledger."""
    try:
        from whitemagic.dharma.karma_ledger import get_karma_ledger
        ledger = get_karma_ledger()
        ledger.record(
            tool="sabha.convene",
            declared_safety="READ",
            actual_writes=0,
            success=bool(result.get("recommendation")),
        )
    except Exception:
        pass  # Karma ledger optional


def handle_sabha_convene(**kwargs: Any) -> dict[str, Any]:
    """MCP handler for convening a Gana Sabhā."""
    task = kwargs.get("task", "")
    ganas = kwargs.get("ganas")
    quadrants = kwargs.get("quadrants")

    if not task:
        return {"status": "error", "error": "task is required — describe what needs council."}

    # Parse ganas/quadrants from string if needed
    if isinstance(ganas, str):
        ganas = [g.strip() for g in ganas.split(",")]
    if isinstance(quadrants, str):
        quadrants = [q.strip() for q in quadrants.split(",")]

    return convene_sabha(task=task, ganas=ganas, quadrants=quadrants)


def handle_sabha_status(**kwargs: Any) -> dict[str, Any]:
    """MCP handler for Sabhā collective vitality status."""
    try:
        from whitemagic.tools.gana_vitality import get_vitality_monitor
        monitor = get_vitality_monitor()
        summary = monitor.get_vitality_summary()
        return {"status": "success", **summary}
    except Exception as e:
        return {"status": "error", "error": str(e)}
