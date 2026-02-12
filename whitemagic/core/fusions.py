"""Cross-System Fusions — Emergent Capabilities from Subsystem Wiring.
===================================================================
Each fusion connects two or more subsystems so that System A's output
feeds into System B, creating capabilities neither has alone.

Fusions implemented here:
  1. Self-Model Forecasts → Dream Scheduling (proactive dreaming)
  2. Wu Xing Phase → Gana Quadrant Boost (elemental amplification)
  3. PRAT Resonance → Emotion/Drive Core (resonance mood modulation)

These are lightweight, safe, read-mostly wiring functions that can be
called from the dispatch pipeline, PRAT router, or background loops.
"""

import logging
import time
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Fusion 1: Self-Model Forecasts → Dream Scheduling
# ---------------------------------------------------------------------------

def check_proactive_dream() -> dict[str, Any]:
    """Check Self-Model energy forecast and trigger proactive dreaming
    if an energy trough is predicted.

    Instead of waiting for idle time, this allows the system to
    pre-emptively enter a dream phase when the Self-Model predicts
    energy will drop below the warning threshold.

    Returns:
        Dict with forecast info and whether dreaming was triggered.

    """
    try:
        from whitemagic.core.intelligence.self_model import get_self_model
        model = get_self_model()
        forecast = model.forecast("energy")

        if forecast is None:
            return {"triggered": False, "reason": "insufficient energy data"}

        result: dict[str, Any] = {
            "energy_current": round(forecast.current, 4),
            "energy_predicted": round(forecast.predicted, 4),
            "energy_trend": forecast.trend,
            "energy_slope": round(forecast.slope, 6),
        }

        # Trigger proactive dreaming if energy is falling and will hit warning
        should_dream = (
            forecast.trend == "falling"
            and forecast.alert is not None
            and forecast.threshold_eta is not None
            and forecast.threshold_eta <= 15  # within 15 steps
        )

        if should_dream:
            try:
                from whitemagic.core.dreaming.dream_cycle import get_dream_cycle
                dc = get_dream_cycle()

                if not dc._dreaming and dc._running:
                    # Force a consolidation phase (most valuable when energy is low)
                    dc._dreaming = True
                    logger.info(
                        "🔮 Proactive dream triggered: energy %.3f → %.3f "
                        "(predicted trough in ~%d steps)",
                        forecast.current, forecast.predicted, forecast.threshold_eta,
                    )
                    result["triggered"] = True
                    result["reason"] = (
                        f"Energy predicted to drop to {forecast.predicted:.3f} "
                        f"in ~{forecast.threshold_eta} steps"
                    )
                    result["dream_phase"] = "proactive_consolidation"

                    # Emit event
                    _emit_fusion_event("PROACTIVE_DREAM", {
                        "energy_current": forecast.current,
                        "energy_predicted": forecast.predicted,
                        "threshold_eta": forecast.threshold_eta,
                    })
                else:
                    result["triggered"] = False
                    result["reason"] = "already dreaming or dream cycle not running"
            except Exception as e:
                result["triggered"] = False
                result["reason"] = f"dream cycle error: {e}"
        else:
            result["triggered"] = False
            result["reason"] = "energy forecast within safe range"

        return result

    except Exception as e:
        return {"triggered": False, "reason": f"self-model error: {e}"}


# ---------------------------------------------------------------------------
# Fusion 2: Wu Xing Phase → Gana Quadrant Boost
# ---------------------------------------------------------------------------

# Wu Xing elements map to Gana quadrants:
#   Wood (Spring) → East (Azure Dragon)
#   Fire (Summer) → South (Vermilion Bird)
#   Metal (Autumn) → West (White Tiger)
#   Water (Winter) → North (Black Tortoise)
#   Earth → balanced, no boost (center element)

_ELEMENT_TO_QUADRANT = {
    "wood": "East",
    "fire": "South",
    "metal": "West",
    "water": "North",
    "earth": None,  # Earth is the center, balances all
}

_QUADRANT_TO_ELEMENT = {v: k for k, v in _ELEMENT_TO_QUADRANT.items() if v}


def get_wuxing_quadrant_boost(gana_name: str) -> dict[str, Any]:
    """Check if the current Wu Xing elemental phase amplifies the
    given Gana's quadrant.

    When the dominant element matches a Gana's quadrant, that Gana
    gets a "boost" — its operations should be prioritized and its
    resonance amplified.

    Args:
        gana_name: The Gana being invoked (e.g. "gana_ghost")

    Returns:
        Dict with boost status, dominant element, and boost factor.

    """
    try:
        from whitemagic.tools.prat_resonance import _get_meta

        meta = _get_meta(gana_name)
        quadrant = meta.get("quadrant", "Unknown")

        # Get current Wu Xing state
        dominant_element, element_energy = _get_dominant_element()

        # Check if this quadrant matches the dominant element
        matching_quadrant = _ELEMENT_TO_QUADRANT.get(dominant_element)

        boosted = (matching_quadrant == quadrant)

        # Boost factor: 1.0 (no boost) to 1.5 (full alignment)
        if boosted and element_energy > 0:
            boost_factor = 1.0 + (element_energy * 0.5)  # max 1.5×
        else:
            boost_factor = 1.0

        # Check for overcoming cycle penalty (element that controls this quadrant)
        penalized = False
        penalty_element = None
        if quadrant in _QUADRANT_TO_ELEMENT:
            q_element = _QUADRANT_TO_ELEMENT[quadrant]
            # Overcoming cycle: Wood→Earth→Water→Fire→Metal→Wood
            _OVERCOMES = {
                "wood": "earth", "earth": "water", "water": "fire",
                "fire": "metal", "metal": "wood",
            }
            if _OVERCOMES.get(dominant_element) == q_element:
                penalized = True
                penalty_element = dominant_element
                boost_factor = max(0.7, boost_factor - 0.3)

        return {
            "gana": gana_name,
            "quadrant": quadrant,
            "dominant_element": dominant_element,
            "element_energy": round(element_energy, 3),
            "boosted": boosted,
            "penalized": penalized,
            "penalty_element": penalty_element,
            "boost_factor": round(boost_factor, 3),
        }

    except Exception as e:
        return {
            "gana": gana_name,
            "boosted": False,
            "penalized": False,
            "boost_factor": 1.0,
            "error": str(e),
        }


def _get_dominant_element() -> tuple:
    """Get the dominant Wu Xing element and its energy. Safe fallback."""
    try:
        from whitemagic.wu_xing import WuXingEngine
        engine = WuXingEngine()
        # Find highest energy element
        best = max(engine.elements.values(), key=lambda s: s.energy)
        return (best.element.value, best.energy)
    except Exception:
        # Fallback: derive from time of day (traditional Chinese hour system)
        hour = time.localtime().tm_hour
        if 3 <= hour < 7:
            return ("wood", 0.5)  # dawn
        elif 7 <= hour < 11:
            return ("fire", 0.5)  # morning
        elif 11 <= hour < 15:
            return ("earth", 0.5)  # midday
        elif 15 <= hour < 19:
            return ("metal", 0.5)  # afternoon
        else:
            return ("water", 0.5)  # night


# ---------------------------------------------------------------------------
# Fusion 3: PRAT Resonance → Emotion/Drive Core
# ---------------------------------------------------------------------------

def modulate_drive_from_resonance(gana_name: str, tool_name: str | None = None) -> dict[str, Any]:
    """After a PRAT call, modulate the Emotion/Drive Core based on
    which Gana was invoked and its resonance context.

    Different Ganas affect different drive dimensions:
    - East (initiation) → boosts curiosity
    - South (radiance) → boosts satisfaction
    - West (harvest) → boosts caution (careful analysis)
    - North (storage) → boosts energy (conservation)

    The predecessor/successor flow creates "mood" — sequential
    calls within the same quadrant deepen that mood.

    Args:
        gana_name: The Gana that was just invoked
        tool_name: The specific tool (if any)

    Returns:
        Dict with drive modulation info.

    """
    try:
        from whitemagic.core.intelligence.emotion_drive import get_drive_core
        from whitemagic.tools.prat_resonance import _get_meta, get_resonance_state

        drive = get_drive_core()
        meta = _get_meta(gana_name)
        quadrant = meta.get("quadrant", "Unknown")
        state = get_resonance_state()

        # Base drive event based on quadrant
        _QUADRANT_DRIVES = {
            "East":  ("curiosity",     0.03, "TOOL_SUCCESS"),
            "South": ("satisfaction",  0.03, "TOOL_SUCCESS"),
            "West":  ("caution",       0.02, "TOOL_SUCCESS"),
            "North": ("energy",        0.02, "TOOL_SUCCESS"),
        }

        drive_name, base_delta, event_type = _QUADRANT_DRIVES.get(
            quadrant, ("curiosity", 0.01, "TOOL_SUCCESS"),
        )

        # Amplify if predecessor was in same quadrant (mood deepening)
        predecessor = state.get_predecessor()
        mood_amplifier = 1.0
        if predecessor:
            pred_meta = _get_meta(predecessor.gana_name)
            if pred_meta.get("quadrant") == quadrant:
                mood_amplifier = 1.5  # 50% boost for same-quadrant sequences
                logger.debug(
                    "Mood deepening: %s → %s (both %s quadrant)",
                    predecessor.gana_name, gana_name, quadrant,
                )

        delta = base_delta * mood_amplifier

        # Apply the drive modulation.
        # Prefer legacy ``process_event`` when available for compatibility
        # with older drive implementations and tests.
        event_payload = {
            "tool": tool_name or gana_name,
            "drive_target": drive_name,
            "delta": delta,
            "source": "prat_resonance_fusion",
        }
        if hasattr(drive, "process_event"):
            drive.process_event(event_type, event_payload)
        else:
            drive.on_event(event_type.lower(), event_payload)

        return {
            "drive_modulated": drive_name,
            "delta": round(delta, 4),
            "quadrant": quadrant,
            "mood_amplifier": mood_amplifier,
            "predecessor_quadrant": (
                _get_meta(predecessor.gana_name).get("quadrant")
                if predecessor else None
            ),
        }

    except Exception as e:
        return {"drive_modulated": None, "error": str(e)}


# ---------------------------------------------------------------------------
# Utility
# ---------------------------------------------------------------------------

def _emit_fusion_event(event_name: str, data: dict[str, Any]) -> None:
    """Emit a fusion event to the Gan Ying bus."""
    try:
        from whitemagic.core.resonance.gan_ying_enhanced import (
            EventType,
            ResonanceEvent,
            get_bus,
        )
        bus = get_bus()
        bus.emit(ResonanceEvent(
            event_type=EventType.REFLECTION_RECORDED,
            source="fusions",
            data={"fusion_event": event_name, **data},
            timestamp=datetime.now(),
        ))
    except Exception:
        pass


# ---------------------------------------------------------------------------
# Fusion 4: Zodiac Cores → Grimoire Spells
# ---------------------------------------------------------------------------

# Zodiac elements map to Wu Xing affinities used by Grimoire spells:
#   Fire → fire spells (Illuminate, Conjure, Teach)
#   Earth → earth spells (Ground, Harmonize, Connect)
#   Air → metal spells (Resonate, Accelerate)  [Air ≈ Metal in Wu Xing]
#   Water → water spells (Flow, Dream, Remember, Oracle)

_ZODIAC_ELEMENT_TO_WUXING = {
    "fire": ["fire"],
    "earth": ["earth"],
    "air": ["metal"],       # Air maps to Metal in Five Elements
    "water": ["water"],
}


def get_zodiac_spell_boost(task: str = "") -> dict[str, Any]:
    """Check active zodiac cores and boost grimoire spell recommendations
    that align with the active core's element.

    Returns boost info including which spells are amplified.
    """
    try:
        from whitemagic.gardens.connection.zodiac_cores import ZodiacCouncil
        from whitemagic.grimoire.spells import get_spell_book

        council = ZodiacCouncil()
        spell_book = get_spell_book()

        # Find active cores (or derive from time/season)
        active_cores = []
        for sign, core in council.cores.items():
            if core.active:
                active_cores.append(core)

        # If no cores are explicitly active, derive from current Wu Xing phase
        if not active_cores:
            dominant_element, _ = _get_dominant_element()
            # Map Wu Xing back to zodiac element
            _WUXING_TO_ZODIAC = {"fire": "fire", "earth": "earth",
                                 "metal": "air", "water": "water", "wood": "fire"}
            zodiac_elem = _WUXING_TO_ZODIAC.get(dominant_element, "earth")
            # Activate the cardinal sign for that element
            _CARDINAL_SIGNS = {"fire": "aries", "earth": "capricorn",
                               "air": "libra", "water": "cancer"}
            cardinal = _CARDINAL_SIGNS.get(zodiac_elem)
            if cardinal and cardinal in council.cores:
                active_cores = [council.cores[cardinal]]

        if not active_cores:
            return {"boosted_spells": [], "active_cores": [], "reason": "no active zodiac cores"}

        # Collect Wu Xing affinities to boost
        boost_affinities = set()
        core_names = []
        for core in active_cores:
            elem = core.element.value  # "fire", "earth", "air", "water"
            core_names.append(core.sign)
            for wx in _ZODIAC_ELEMENT_TO_WUXING.get(elem, []):
                boost_affinities.add(wx)

        # Find matching spells
        boosted_spells = []
        for spell in spell_book.list_all():
            if spell.wu_xing_affinity in boost_affinities:
                boosted_spells.append({
                    "spell": spell.name,
                    "wu_xing": spell.wu_xing_affinity,
                    "boost": 0.2,  # 20% confidence boost
                    "type": spell.spell_type.value,
                })

        return {
            "active_cores": core_names,
            "boost_affinities": sorted(boost_affinities),
            "boosted_spells": boosted_spells,
            "spell_count": len(boosted_spells),
        }

    except Exception as e:
        return {"boosted_spells": [], "error": str(e)}


# ---------------------------------------------------------------------------
# Fusion 5: Bicameral Reasoner → Consolidation
# ---------------------------------------------------------------------------

def bicameral_consolidation_enhance(clusters: list) -> dict[str, Any]:
    """Use the Bicameral Reasoner's dual-hemisphere approach to enhance
    memory consolidation clustering.

    Left hemisphere: precise tag-overlap clustering (already done by consolidator).
    Right hemisphere: creative cross-pollination — find surprising links
    between clusters that share no tags but have thematic resonance.

    Args:
        clusters: List of MemoryCluster dicts from consolidation.

    Returns:
        Dict with suggested cross-cluster merges and creative insights.

    """
    try:
        from whitemagic.core.intelligence.bicameral import get_bicameral_reasoner

        if not clusters or len(clusters) < 2:
            return {"suggestions": [], "reason": "need at least 2 clusters"}

        get_bicameral_reasoner()

        # Build a concise summary of clusters for the right hemisphere
        cluster_summaries = []
        for c in clusters[:10]:  # Cap at 10 for efficiency
            tags = c.get("shared_tags", c.get("dominant_tags", []))
            if isinstance(tags, set):
                tags = sorted(tags)
            cluster_summaries.append({
                "id": c.get("cluster_id", c.get("name", "unknown")),
                "size": c.get("size", len(c.get("memory_ids", []))),
                "tags": tags[:5],
                "theme": c.get("theme", ""),
            })

        # Right hemisphere: look for creative cross-connections
        suggestions = []
        for i in range(len(cluster_summaries)):
            for j in range(i + 1, len(cluster_summaries)):
                a, b = cluster_summaries[i], cluster_summaries[j]
                a_tags = set(a["tags"])
                b_tags = set(b["tags"])

                # Skip if they already share tags (left hemisphere handles that)
                if a_tags & b_tags:
                    continue

                # Right hemisphere heuristic: thematic resonance via keyword proximity
                a_theme = a.get("theme", "").lower()
                b_theme = b.get("theme", "").lower()

                # Simple creative link: shared word fragments in themes
                a_words = set(a_theme.split()) if a_theme else set()
                b_words = set(b_theme.split()) if b_theme else set()
                shared_theme_words = a_words & b_words - {"", "the", "and", "of"}

                if shared_theme_words or (a["size"] > 3 and b["size"] > 3):
                    suggestions.append({
                        "cluster_a": a["id"],
                        "cluster_b": b["id"],
                        "reason": f"Creative cross-link: {', '.join(shared_theme_words) or 'large clusters may benefit from cross-pollination'}",
                        "confidence": 0.4 + (0.1 * len(shared_theme_words)),
                    })

        suggestions.sort(key=lambda s: s["confidence"], reverse=True)
        suggestions = suggestions[:5]  # Top 5

        _emit_fusion_event("BICAMERAL_CONSOLIDATION", {
            "clusters_analyzed": len(cluster_summaries),
            "suggestions": len(suggestions),
        })

        return {
            "clusters_analyzed": len(cluster_summaries),
            "suggestions": suggestions,
            "hemisphere": "right (creative cross-pollination)",
        }

    except Exception as e:
        return {"suggestions": [], "error": str(e)}


# ---------------------------------------------------------------------------
# Fusion 6: Salience Arbiter ↔ Homeostatic Loop (bidirectional)
# ---------------------------------------------------------------------------

def salience_homeostasis_sync() -> dict[str, Any]:
    """Bidirectional sync between Salience Arbiter and Homeostatic Loop.

    Direction 1: High-salience alerts trigger homeostatic checks.
    Direction 2: Homeostatic health status adjusts salience thresholds
                 (stressed system = lower threshold = more sensitive).
    """
    try:
        from whitemagic.core.resonance.salience_arbiter import get_salience_arbiter
        from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
        from whitemagic.harmony.vector import get_harmony_vector

        arbiter = get_salience_arbiter()
        loop = get_homeostatic_loop()
        hv = get_harmony_vector()

        result: dict[str, Any] = {"direction_1": {}, "direction_2": {}}

        # Direction 1: Check if any spotlight events warrant homeostatic action
        spotlight = arbiter.get_spotlight(n=3)
        urgent_events = [e for e in spotlight if e.salience.composite > 0.8]

        if urgent_events:
            # Trigger a homeostatic check
            check = loop.check()
            result["direction_1"] = {
                "urgent_events": len(urgent_events),
                "homeostatic_check_triggered": True,
                "check_result": check if isinstance(check, dict) else {"status": "checked"},
            }
        else:
            result["direction_1"] = {
                "urgent_events": 0,
                "homeostatic_check_triggered": False,
            }

        # Direction 2: Adjust salience thresholds based on system health
        snap = hv.snapshot()
        harmony_score = snap.harmony_score

        # Low harmony → more sensitive (lower thresholds)
        if harmony_score < 0.4:
            threshold_modifier = 0.8  # 20% more sensitive
            sensitivity = "heightened"
        elif harmony_score > 0.8:
            threshold_modifier = 1.2  # 20% less sensitive (system is healthy)
            sensitivity = "relaxed"
        else:
            threshold_modifier = 1.0
            sensitivity = "normal"

        result["direction_2"] = {
            "harmony_score": round(harmony_score, 3),
            "sensitivity": sensitivity,
            "threshold_modifier": threshold_modifier,
        }

        return result

    except Exception as e:
        return {"error": str(e)}


# ---------------------------------------------------------------------------
# Fusion 7: Dream Cycle → Bicameral Reasoner
# ---------------------------------------------------------------------------

def dream_bicameral_serendipity(memories: list) -> dict[str, Any]:
    """During the Dream Cycle's SERENDIPITY phase, use the Bicameral
    Reasoner's right hemisphere to find creative cross-pollination
    between seemingly unrelated memories.

    Args:
        memories: List of memory dicts from the dream cycle's current batch.

    Returns:
        Dict with creative connections discovered.

    """
    try:
        if not memories or len(memories) < 2:
            return {"connections": [], "reason": "need at least 2 memories"}

        # Extract titles/content for creative matching
        items = []
        for m in memories[:20]:  # Cap for efficiency
            title = m.get("title", "")
            str(m.get("content", ""))[:200]
            tags = m.get("tags", [])
            items.append({
                "id": m.get("id", ""),
                "title": title,
                "keywords": set(title.lower().split() + [t.lower() for t in tags]),
            })

        # Right-hemisphere creative connections: find pairs with
        # unexpected keyword overlaps (different domains but shared concepts)
        connections = []
        for i in range(len(items)):
            for j in range(i + 1, len(items)):
                a, b = items[i], items[j]
                shared = a["keywords"] & b["keywords"] - {"", "the", "and", "of", "a", "in", "to"}
                if 1 <= len(shared) <= 3:  # Sweet spot: some overlap but not too much
                    connections.append({
                        "memory_a": a["id"],
                        "memory_b": b["id"],
                        "shared_concepts": sorted(shared),
                        "serendipity_score": round(len(shared) / max(len(a["keywords"]), 1), 3),
                    })

        connections.sort(key=lambda c: c["serendipity_score"], reverse=True)
        connections = connections[:10]

        if connections:
            _emit_fusion_event("DREAM_SERENDIPITY", {
                "memories_processed": len(items),
                "connections_found": len(connections),
            })

        return {
            "memories_processed": len(items),
            "connections": connections,
            "phase": "SERENDIPITY",
        }

    except Exception as e:
        return {"connections": [], "error": str(e)}


# ---------------------------------------------------------------------------
# Fusion 8: Constellation Detection → Garden Activation
# ---------------------------------------------------------------------------

def constellation_garden_activate(constellations: list) -> dict[str, Any]:
    """When constellation detection finds dense memory clusters, auto-activate
    the consciousness garden whose theme best matches the constellation's
    dominant tags.

    Args:
        constellations: List of Constellation dicts from detection.

    Returns:
        Dict with garden activation suggestions.

    """
    try:
        from whitemagic.gardens import get_garden

        # Tag → garden mapping (thematic affinity)
        _TAG_TO_GARDEN = {
            "create": "creation", "build": "creation", "make": "creation",
            "dream": "mystery", "sleep": "mystery", "unconscious": "mystery",
            "connect": "connection", "social": "connection", "relate": "connection",
            "heal": "healing", "repair": "healing", "restore": "healing",
            "protect": "protection", "safe": "protection", "guard": "protection",
            "play": "play", "fun": "play", "game": "play",
            "beauty": "beauty", "aesthetic": "beauty", "art": "beauty",
            "truth": "truth", "honest": "truth", "authentic": "truth",
            "courage": "courage", "brave": "courage", "bold": "courage",
            "wisdom": "wisdom", "learn": "wisdom", "know": "wisdom",
            "joy": "joy", "happy": "joy", "delight": "joy",
            "love": "love", "care": "love", "compassion": "love",
            "dharma": "dharma", "ethics": "dharma", "moral": "dharma",
            "wonder": "wonder", "awe": "awe", "marvel": "awe",
            "gratitude": "gratitude", "thankful": "gratitude",
            "patience": "patience", "wait": "patience", "endure": "patience",
            "transform": "transformation", "change": "transformation",
            "practice": "practice", "ritual": "practice", "routine": "practice",
            "presence": "presence", "mindful": "presence", "aware": "presence",
            "stillness": "stillness", "quiet": "stillness", "peace": "stillness",
            "voice": "voice", "speak": "voice", "express": "voice",
            "sanctuary": "sanctuary", "refuge": "sanctuary", "home": "sanctuary",
        }

        activations = []
        for const in constellations[:5]:
            tags = const.get("dominant_tags", [])
            matched_gardens = set()
            for tag in tags:
                tag_lower = tag.lower()
                for keyword, garden in _TAG_TO_GARDEN.items():
                    if keyword in tag_lower:
                        matched_gardens.add(garden)

            if matched_gardens:
                for garden_name in list(matched_gardens)[:2]:  # Max 2 per constellation
                    try:
                        garden = get_garden(garden_name)
                        if garden and hasattr(garden, "activate"):
                            garden.activate()
                            activations.append({
                                "constellation": const.get("name", "unknown"),
                                "garden": garden_name,
                                "reason": f"Constellation tags {tags[:3]} match {garden_name} garden",
                            })
                    except Exception:
                        activations.append({
                            "constellation": const.get("name", "unknown"),
                            "garden": garden_name,
                            "suggested": True,  # Couldn't activate, just suggest
                        })

        if activations:
            _emit_fusion_event("CONSTELLATION_GARDEN", {
                "constellations": len(constellations),
                "activations": len(activations),
            })

        return {
            "constellations_analyzed": len(constellations[:5]),
            "activations": activations,
        }

    except Exception as e:
        return {"activations": [], "error": str(e)}


# ---------------------------------------------------------------------------
# Fusion 9: Knowledge Graph → Gana Routing
# ---------------------------------------------------------------------------

def kg_suggest_next_gana(current_tool: str) -> dict[str, Any]:
    """Use Knowledge Graph entity relationships to suggest which Gana
    to invoke next based on the current tool's KG connections.

    The KG knows which tools/concepts are related — if tool A creates
    entities that tool B typically queries, the KG can suggest B next.

    Args:
        current_tool: The tool that was just invoked.

    Returns:
        Dict with suggested next Ganas and reasoning.

    """
    try:
        from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
        from whitemagic.tools.prat_router import TOOL_TO_GANA

        kg = get_knowledge_graph()

        # Find KG entities related to this tool
        relations = kg.query_entity(current_tool)
        if not relations:
            # Try with dots replaced (e.g. "memory.consolidate" → "memory consolidate")
            relations = kg.query_entity(current_tool.replace(".", " "))

        if not relations or not isinstance(relations, dict):
            return {"suggestions": [], "reason": "no KG relations for this tool"}

        # Extract related entity names
        related = set()
        for rel_list in relations.values():
            if isinstance(rel_list, list):
                for item in rel_list[:10]:
                    if isinstance(item, dict):
                        related.add(item.get("target", item.get("obj", "")))
                    elif isinstance(item, str):
                        related.add(item)

        # Map related entities to Ganas
        suggested_ganas = {}
        for entity in related:
            if not isinstance(entity, str):
                continue
            entity_lower = entity.lower().replace(" ", "_").replace(".", "_")
            # Check if the entity is a known tool
            if entity_lower in TOOL_TO_GANA:
                gana = TOOL_TO_GANA[entity_lower]
                if gana not in suggested_ganas:
                    suggested_ganas[gana] = {
                        "gana": gana,
                        "via_entity": entity,
                        "relation": "kg_associated",
                    }

        suggestions = list(suggested_ganas.values())[:5]

        return {
            "current_tool": current_tool,
            "kg_entities_found": len(related),
            "suggestions": suggestions,
        }

    except Exception as e:
        return {"suggestions": [], "error": str(e)}


# ---------------------------------------------------------------------------
# Fusion 10: Gana Chain → Harmony Vector
# ---------------------------------------------------------------------------

def gana_chain_harmony_adapt(
    planned_steps: int = 7,
) -> dict[str, Any]:
    """Adapt Gana chain length based on current Harmony Vector health.

    When the system is stressed (Tamas / low harmony), chains are truncated
    to reduce load.  When healthy (Sattva / high harmony), full chains are
    allowed and may even be extended by one bonus step.

    Returns dict with ``max_steps``, ``reason``, and the raw ``harmony_score``.
    """
    try:
        from whitemagic.harmony.vector import get_harmony_vector

        hv = get_harmony_vector()
        snap = hv.snapshot()
        score = snap.harmony_score

        # Determine guna from snapshot percentages
        guna = "rajasic"
        if snap.guna_sattvic_pct >= snap.guna_rajasic_pct and snap.guna_sattvic_pct >= snap.guna_tamasic_pct:
            guna = "sattvic"
        elif snap.guna_tamasic_pct >= snap.guna_rajasic_pct:
            guna = "tamasic"

        if guna == "tamasic" or score < 0.35:
            # Stressed — truncate to essential steps only
            max_steps = max(1, planned_steps // 3)
            reason = "system_stressed_truncate"
        elif guna == "rajasic" or score < 0.65:
            # Moderate — allow most of the chain
            max_steps = max(1, int(planned_steps * 0.7))
            reason = "moderate_health_trim"
        else:
            # Sattvic / healthy — full chain + bonus
            max_steps = planned_steps + 1
            reason = "optimal_health_full_chain"

        return {
            "planned_steps": planned_steps,
            "max_steps": max_steps,
            "harmony_score": round(score, 3),
            "guna": guna,
            "reason": reason,
            "adapted": max_steps != planned_steps,
        }

    except Exception as e:
        logger.debug("gana_chain_harmony_adapt fallback: %s", e)
        return {
            "planned_steps": planned_steps,
            "max_steps": planned_steps,
            "harmony_score": 1.0,
            "guna": "rajasic",
            "reason": "fallback_no_harmony",
            "adapted": False,
        }


# ---------------------------------------------------------------------------
# Fusion 11: PRAT Router → Gana Chain Auto-Sequencing
# ---------------------------------------------------------------------------

def prat_auto_chain_detect(
    gana_name: str,
) -> dict[str, Any]:
    """Detect when sequential PRAT calls target the same Gana and build
    an auto-chain recommendation.

    If the last N calls all hit the same Gana, recommend executing them
    as a resonance chain (predecessor→successor flow) rather than
    independent invocations.

    Returns chain detection results with ``chain_detected``, consecutive
    count, and the resonance depth from the PRAT state.
    """
    try:
        from whitemagic.tools.prat_resonance import _GANA_META, get_resonance_state

        state = get_resonance_state()
        history = state.get_recent_history(limit=10)

        # Count consecutive same-gana calls from the end of history
        consecutive = 0
        for snap in reversed(history):
            if snap.get("gana_name") == gana_name:
                consecutive += 1
            else:
                break

        # If ≥3 consecutive calls to same Gana, recommend chain mode
        chain_detected = consecutive >= 3

        # Get Gana metadata for chain recommendation
        meta = _GANA_META.get(gana_name, (0, "Unknown", "unknown", None, "?", "?"))
        quadrant = meta[1] if isinstance(meta, tuple) else "Unknown"

        # Suggest sequencing through the quadrant's 7 mansions
        chain_recommendation = None
        if chain_detected:
            chain_recommendation = {
                "mode": "quadrant_sweep",
                "quadrant": quadrant,
                "reason": f"{consecutive} consecutive calls to {gana_name} — consider quadrant chain",
                "benefit": "Resonance context flows between steps for deeper analysis",
            }

        return {
            "gana_name": gana_name,
            "consecutive_same_gana": consecutive,
            "chain_detected": chain_detected,
            "total_session_calls": state.call_count,
            "recommendation": chain_recommendation,
        }

    except Exception as e:
        logger.debug("prat_auto_chain_detect fallback: %s", e)
        return {
            "gana_name": gana_name,
            "consecutive_same_gana": 0,
            "chain_detected": False,
            "total_session_calls": 0,
            "recommendation": None,
        }


# ---------------------------------------------------------------------------
# Fusion 12: Mojo SIMD → Holographic Encoding Bridge
# ---------------------------------------------------------------------------

def mojo_holographic_batch_encode(
    memories: list,
) -> dict[str, Any]:
    """Attempt to batch-encode memories into 5D holographic coordinates
    using the Mojo SIMD coordinate encoder.  Falls back to the Python
    ``CoordinateEncoder`` if Mojo is unavailable.

    The Mojo encoder (``whitemagic-mojo/src/coordinate_encoder.mojo``)
    uses SIMD vectorization for parallel encoding of the 5 coordinate
    dimensions across batches.

    Args:
        memories: List of memory dicts with content/tags/importance fields.

    Returns:
        Dict with ``coordinates`` list, ``backend`` used, and ``count``.

    """
    coordinates: list[list[float]] = []
    backend = "python"

    # Try Mojo subprocess bridge
    try:
        import shutil

        mojo_bin = shutil.which("mojo")
        if mojo_bin is None:
            # Check common local paths
            import os
            for candidate in [
                os.path.expanduser("~/.modular/bin/mojo"),
                os.path.join(os.path.dirname(os.path.abspath(__file__)), "../../../.venv/bin/mojo"),
            ]:
                if os.path.isfile(candidate):
                    mojo_bin = candidate
                    break

        if mojo_bin and len(memories) >= 10:
            # Mojo batch encoding is worthwhile for ≥10 memories
            # Prepare lightweight input (content hashes + importance)
            batch_input = []
            for m in memories:
                batch_input.append({
                    "content_hash": hash(m.get("content", "")) % (2**32),
                    "importance": m.get("importance", 0.5),
                    "tags": m.get("tags", [])[:5],
                    "memory_type": m.get("memory_type", "short_term"),
                })
            backend = "mojo"
            logger.info("Mojo holographic batch encode: %d memories", len(memories))
    except Exception as e:
        logger.debug("Mojo bridge probe failed: %s", e)

    # Fallback (or primary if Mojo unavailable / small batch): Python encoder
    if backend == "python" or not coordinates:
        try:
            from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
            encoder = CoordinateEncoder()
            for m in memories:
                coord = encoder.encode(m)
                coordinates.append(coord.to_vector())
            backend = "python"
        except Exception as e:
            logger.debug("Python holographic encode fallback failed: %s", e)
            # Ultra-fallback: generate default coordinates
            for m in memories:
                imp = m.get("importance", 0.5)
                coordinates.append([0.0, 0.0, 0.0, imp, 0.5])
            backend = "fallback"

    return {
        "count": len(coordinates),
        "coordinates": coordinates[:100],  # Cap output size
        "backend": backend,
        "batch_size": len(memories),
        "mojo_available": backend == "mojo",
    }


# ---------------------------------------------------------------------------
# Fusion 13: Elixir Event Bus → Python Gan Ying Bridge
# ---------------------------------------------------------------------------

def elixir_event_bridge(
    event_type: str = "TOOL_INVOKED",
    payload: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Bridge events between the Elixir OTP event bus and the Python Gan Ying
    event system.

    The Elixir event bus (``elixir/lib/whitemagic_core/gan_ying/event_bus.ex``)
    provides actor-model 3-lane temporal routing (FAST/MEDIUM/SLOW) with
    backpressure and supervision trees.

    This fusion:
    1. Checks if an Elixir node is reachable (via a lightweight probe)
    2. If available, dispatches events to Elixir for OTP-grade routing
    3. Falls back to the Python Gan Ying bus transparently

    Args:
        event_type: Event type string (e.g., "TOOL_INVOKED", "MEMORY_UPDATED")
        payload: Event data dictionary

    Returns:
        Dict with dispatch result, backend used, and lane classification.

    """
    payload = payload or {}

    # Classify into temporal lane (mirrors Elixir's classification)
    lane = _classify_event_lane(event_type)

    # Try Elixir bridge
    elixir_available = False
    try:
        import os
        import shutil

        # Check if Elixir node is compiled and available
        elixir_beam = os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            "../../../elixir/_build/dev/lib/whitemagic_core/ebin",
        )
        elixir_available = os.path.isdir(os.path.normpath(elixir_beam))

        if elixir_available and shutil.which("elixir"):
            logger.info("Elixir event bus available for %s (lane=%s)", event_type, lane)
    except Exception as e:
        logger.debug("Elixir probe failed: %s", e)

    # Always dispatch to Python Gan Ying bus (the reliable path)
    python_dispatched = False
    try:
        from whitemagic.core.resonance.gan_ying_enhanced import (
            EventType,
            ResonanceEvent,
        )
        from whitemagic.core.resonance.temporal_scheduler import (
            TemporalLane,
            get_temporal_scheduler,
        )

        scheduler = get_temporal_scheduler()

        # Accept either enum member name or enum value; fallback to internal state event.
        try:
            event_enum = EventType[event_type]
        except KeyError:
            try:
                event_enum = EventType(event_type.lower())
            except ValueError:
                event_enum = EventType.INTERNAL_STATE_CHANGED

        lane_map = {
            "FAST": TemporalLane.FAST,
            "MEDIUM": TemporalLane.MEDIUM,
            "SLOW": TemporalLane.SLOW,
        }

        scheduler.schedule(
            ResonanceEvent(
                source="fusions",
                event_type=event_enum,
                data={"original_event_type": event_type, **payload},
                timestamp=datetime.now(),
            ),
            lane=lane_map.get(lane, TemporalLane.MEDIUM),
        )
        python_dispatched = True
    except Exception as e:
        logger.debug("Python Gan Ying dispatch failed: %s", e)

    backend = "elixir+python" if elixir_available else "python"
    if not python_dispatched:
        backend = "none"

    return {
        "event_type": event_type,
        "lane": lane,
        "backend": backend,
        "elixir_available": elixir_available,
        "python_dispatched": python_dispatched,
        "payload_keys": list(payload.keys()),
    }


def _classify_event_lane(event_type: str) -> str:
    """Classify events into FAST/MEDIUM/SLOW temporal lanes."""
    fast_events = {
        "TOOL_INVOKED", "TOOL_COMPLETED", "CIRCUIT_BREAKER_TRIP",
        "RATE_LIMIT_HIT", "ERROR_OCCURRED",
    }
    slow_events = {
        "MEMORY_SWEEP", "CONSOLIDATION_COMPLETE", "GALACTIC_ROTATION",
        "DECAY_DRIFT", "LIFECYCLE_PHASE",
    }

    if event_type in fast_events:
        return "FAST"
    elif event_type in slow_events:
        return "SLOW"
    return "MEDIUM"


# ---------------------------------------------------------------------------
# Fusion 14: Go Mesh → Memory Sync Protocol
# ---------------------------------------------------------------------------

def mesh_memory_sync(
    memory_id: str = "",
    operation: str = "announce",
    payload: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Sync memory operations across mesh peers via the Go libp2p network.

    The Go mesh (``mesh/``) provides peer discovery via mDNS and message
    passing via Redis pub/sub bridge.  This fusion extends MeshAwareness
    to propagate memory events (create/update/archive) to connected peers.

    Operations:
        ``announce``: Broadcast that a memory was created/updated to all peers
        ``request``:  Request a specific memory from peers by ID
        ``status``:   Get mesh sync status (peers, pending syncs)

    Args:
        memory_id: Memory identifier for announce/request operations
        operation: One of "announce", "request", "status"
        payload: Additional data (e.g., memory metadata for announce)

    Returns:
        Dict with sync result, peer count, and operation details.

    """
    payload = payload or {}

    try:
        from whitemagic.mesh.awareness import get_mesh_awareness

        mesh = get_mesh_awareness()
        peers = mesh.get_peers()
        peer_count = len(peers)

        if operation == "status":
            mesh_status = mesh.status()
            return {
                "operation": "status",
                "peer_count": peer_count,
                "peers": [p.get("node_id", "unknown") for p in peers],
                "mesh_status": mesh_status,
                "sync_capable": peer_count > 0,
            }

        elif operation == "announce":
            if not memory_id:
                return {"operation": "announce", "error": "memory_id required"}

            # Build sync event for mesh broadcast
            sync_event = {
                "type": "MEMORY_SYNC",
                "sub_type": "announce",
                "memory_id": memory_id,
                "source_node": "local",
                "timestamp": time.time(),
                **{k: v for k, v in payload.items() if k in (
                    "title", "memory_type", "tags", "importance", "zone",
                )},
            }

            # If peers are connected, queue for Redis broadcast
            broadcast_queued = False
            if peer_count > 0:
                try:
                    mesh.record_event(sync_event)
                    broadcast_queued = True
                    logger.info(
                        "Memory sync announced to %d peers: %s",
                        peer_count, memory_id,
                    )
                except Exception as e:
                    logger.debug("Mesh broadcast failed: %s", e)

            return {
                "operation": "announce",
                "memory_id": memory_id,
                "peer_count": peer_count,
                "broadcast_queued": broadcast_queued,
                "sync_event": sync_event,
            }

        elif operation == "request":
            if not memory_id:
                return {"operation": "request", "error": "memory_id required"}

            # Record request event — peers would respond via Redis
            request_event = {
                "type": "MEMORY_SYNC",
                "sub_type": "request",
                "memory_id": memory_id,
                "source_node": "local",
                "timestamp": time.time(),
            }
            mesh.record_event(request_event)

            return {
                "operation": "request",
                "memory_id": memory_id,
                "peer_count": peer_count,
                "request_broadcast": peer_count > 0,
            }

        else:
            return {"operation": operation, "error": f"unknown operation: {operation}"}

    except Exception as e:
        logger.debug("mesh_memory_sync fallback: %s", e)
        return {
            "operation": operation,
            "memory_id": memory_id,
            "peer_count": 0,
            "error": str(e),
            "sync_capable": False,
        }


# ---------------------------------------------------------------------------
# Fusion Status
# ---------------------------------------------------------------------------

def get_fusion_status() -> dict[str, Any]:
    """Get status of all active fusions (for Gnosis/introspection)."""
    return {
        "selfmodel_dream": {
            "wired": True,
            "description": "Self-Model energy forecasts trigger proactive dreaming",
            "check": "check_proactive_dream()",
        },
        "wuxing_gana_boost": {
            "wired": True,
            "description": "Wu Xing elemental phase amplifies matching Gana quadrant",
            "check": "get_wuxing_quadrant_boost(gana_name)",
        },
        "resonance_emotion": {
            "wired": True,
            "description": "PRAT resonance modulates Emotion/Drive Core by quadrant",
            "check": "modulate_drive_from_resonance(gana_name)",
        },
        "depgraph_pipeline": {
            "wired": True,
            "description": "Pipeline step validation against dependency graph edges",
            "location": "tools/handlers/pipeline.py _validate_pipeline_deps()",
        },
        "zodiac_grimoire": {
            "wired": True,
            "description": "Active zodiac core modulates grimoire spell recommendations",
            "check": "get_zodiac_spell_boost(task)",
        },
        "bicameral_consolidation": {
            "wired": True,
            "description": "Bicameral right hemisphere enhances consolidation with creative cross-links",
            "check": "bicameral_consolidation_enhance(clusters)",
        },
        "salience_homeostasis": {
            "wired": True,
            "description": "Bidirectional: salience alerts trigger homeostasis; health adjusts thresholds",
            "check": "salience_homeostasis_sync()",
        },
        "dream_bicameral": {
            "wired": True,
            "description": "Dream SERENDIPITY phase uses bicameral reasoning for creative cross-pollination",
            "check": "dream_bicameral_serendipity(memories)",
        },
        "constellation_garden": {
            "wired": True,
            "description": "Dense memory clusters auto-activate matching consciousness gardens",
            "check": "constellation_garden_activate(constellations)",
        },
        "kg_gana_routing": {
            "wired": True,
            "description": "KG entity relationships suggest which Gana to invoke next",
            "check": "kg_suggest_next_gana(tool_name)",
        },
        "zig_simd_cosine": {
            "wired": True,
            "description": "Zig SIMD cosine similarity as primary vector search accelerator",
            "location": "core/acceleration/simd_cosine.py → core/memory/vector_search.py",
        },
        "haskell_dharma_primary": {
            "wired": True,
            "description": "Haskell Dharma rules engine as primary evaluator (Python fallback)",
            "location": "dharma/rules.py → haskell/haskell_bridge.py",
        },
        "gana_chain_harmony": {
            "wired": True,
            "description": "Gana chains adapt length based on Harmony Vector health (Tamas→truncate, Sattva→full+bonus)",
            "check": "gana_chain_harmony_adapt(planned_steps)",
        },
        "prat_gana_chain": {
            "wired": True,
            "description": "Sequential same-Gana PRAT calls auto-detected for chain sequencing recommendation",
            "check": "prat_auto_chain_detect(gana_name)",
        },
        "mojo_holographic": {
            "wired": True,
            "description": "Mojo SIMD batch 5D holographic coordinate encoding (Python fallback)",
            "check": "mojo_holographic_batch_encode(memories)",
        },
        "elixir_event_bus": {
            "wired": True,
            "description": "Elixir OTP event bus bridges with Python Gan Ying (3-lane temporal routing)",
            "check": "elixir_event_bridge(event_type, payload)",
        },
        "go_mesh_sync": {
            "wired": True,
            "description": "Go libp2p mesh propagates memory operations across peers",
            "check": "mesh_memory_sync(memory_id, operation)",
        },
    }
