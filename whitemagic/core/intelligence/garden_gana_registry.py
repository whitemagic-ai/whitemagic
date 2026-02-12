"""Garden-Gana Registry
The 28-Fold Resonance Map.

Maps each Garden (emotional/thematic content) to its corresponding:
- Gana (Lunar Mansion / Tool)
- Element (Fire, Earth, Metal, Water, Wood)
- Primary Emotion (one of our 9 core emotions)
- Resonance Keywords (triggers for automatic activation)

This registry enables emergent phenomena through cross-communication.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class Element(Enum):
    FIRE = "Fire"
    EARTH = "Earth"
    METAL = "Metal"
    WATER = "Water"
    WOOD = "Wood"

class Quadrant(Enum):
    EASTERN = "Eastern (Azure Dragon)"   # Wood - Spring
    SOUTHERN = "Southern (Vermilion Bird)" # Fire - Summer
    WESTERN = "Western (White Tiger)"     # Metal - Autumn
    NORTHERN = "Northern (Black Tortoise)" # Water - Winter

@dataclass
class GardenGanaMapping:
    """A single entry in the 28-fold registry."""

    garden: str
    gana: str
    gana_tool: str  # MCP tool name
    quadrant: Quadrant
    element: Element
    primary_emotion: str
    secondary_emotion: str | None = None
    resonance_keywords: list[str] = field(default_factory=list)
    description: str = ""

# ═══════════════════════════════════════════════════════════════════════════════
# THE 28-FOLD REGISTRY
# ═══════════════════════════════════════════════════════════════════════════════

GARDEN_GANA_REGISTRY: list[GardenGanaMapping] = [
    # ─────────────────────────────────────────────────────────────────────────────
    # EASTERN QUADRANT (Wood/Azure Dragon) - Mansions 1-7
    # Theme: Growth, Initiation, Structure
    # ─────────────────────────────────────────────────────────────────────────────
    GardenGanaMapping(
        garden="wonder", gana="Horn (Jiao)", gana_tool="gana_horn",
        quadrant=Quadrant.EASTERN, element=Element.WOOD,
        primary_emotion="Wonder", description="Session Initialization - The spark of curiosity",
        resonance_keywords=["begin", "start", "what if", "curious", "explore"],
    ),
    GardenGanaMapping(
        garden="stillness", gana="Neck (Kang)", gana_tool="gana_neck",
        quadrant=Quadrant.EASTERN, element=Element.WOOD,
        primary_emotion="Stillness", description="Stability/Memory - The pause that grounds",
        resonance_keywords=["remember", "stable", "calm", "pause", "reflect"],
    ),
    GardenGanaMapping(
        garden="healing", gana="Root (Di)", gana_tool="gana_root",
        quadrant=Quadrant.EASTERN, element=Element.WOOD,
        primary_emotion="Love", secondary_emotion="Stillness",
        description="System Health - Restoration and repair",
        resonance_keywords=["heal", "fix", "repair", "restore", "health"],
    ),
    GardenGanaMapping(
        garden="sanctuary", gana="Room (Fang)", gana_tool="gana_room",
        quadrant=Quadrant.EASTERN, element=Element.WOOD,
        primary_emotion="Stillness", secondary_emotion="Love",
        description="Resource Locks - Safe spaces and boundaries",
        resonance_keywords=["safe", "home", "refuge", "protect", "secure"],
    ),
    GardenGanaMapping(
        garden="love", gana="Heart (Xin)", gana_tool="gana_heart",
        quadrant=Quadrant.EASTERN, element=Element.FIRE,
        primary_emotion="Love", description="Session Context - The emotional center",
        resonance_keywords=["love", "care", "connect", "heart", "compassion"],
    ),
    GardenGanaMapping(
        garden="courage", gana="Tail (Wei)", gana_tool="gana_tail",
        quadrant=Quadrant.EASTERN, element=Element.FIRE,
        primary_emotion="Courage", description="Acceleration - Forward momentum",
        resonance_keywords=["brave", "push", "accelerate", "challenge", "dare"],
    ),
    GardenGanaMapping(
        garden="wisdom", gana="Winnowing Basket (Ji)", gana_tool="gana_winnowing_basket",
        quadrant=Quadrant.EASTERN, element=Element.FIRE,
        primary_emotion="Wisdom", secondary_emotion="Truth",
        description="Wisdom/Separation - Discernment and filtering",
        resonance_keywords=["wisdom", "discern", "filter", "separate", "judge"],
    ),

    # ─────────────────────────────────────────────────────────────────────────────
    # SOUTHERN QUADRANT (Fire/Vermilion Bird) - Mansions 8-14
    # Theme: Expansion, Radiance, Transformation
    # ─────────────────────────────────────────────────────────────────────────────
    GardenGanaMapping(
        garden="dharma", gana="Dipper (Dou)", gana_tool="gana_dipper",
        quadrant=Quadrant.SOUTHERN, element=Element.FIRE,
        primary_emotion="Truth", description="Governance/Strategy - Right action",
        resonance_keywords=["guide", "strategy", "govern", "ethics", "dharma"],
    ),
    GardenGanaMapping(
        garden="patience", gana="Ox (Niu)", gana_tool="gana_ox",
        quadrant=Quadrant.SOUTHERN, element=Element.EARTH,
        primary_emotion="Stillness", secondary_emotion="Courage",
        description="Endurance/Temporal - Steady perseverance",
        resonance_keywords=["wait", "patient", "endure", "persist", "time"],
    ),
    GardenGanaMapping(
        garden="connection", gana="Girl (Nu)", gana_tool="gana_girl",
        quadrant=Quadrant.SOUTHERN, element=Element.EARTH,
        primary_emotion="Love", secondary_emotion="Joy",
        description="Nurture/Profile - Relationships and care",
        resonance_keywords=["connect", "nurture", "relationship", "bond", "community"],
    ),
    GardenGanaMapping(
        garden="mystery", gana="Void (Xu)", gana_tool="gana_void",
        quadrant=Quadrant.SOUTHERN, element=Element.WATER,
        primary_emotion="Wonder", secondary_emotion="Stillness",
        description="Emptiness/Optimization - The fertile void",
        resonance_keywords=["mystery", "void", "empty", "optimize", "unknown"],
    ),
    GardenGanaMapping(
        garden="protection", gana="Roof (Wei)", gana_tool="gana_roof",
        quadrant=Quadrant.SOUTHERN, element=Element.EARTH,
        primary_emotion="Courage", secondary_emotion="Love",
        description="Shelter/Zodiac - Guardian energies",
        resonance_keywords=["protect", "shelter", "guard", "shield", "defend"],
    ),
    GardenGanaMapping(
        garden="transformation", gana="Encampment (Shi)", gana_tool="gana_encampment",
        quadrant=Quadrant.SOUTHERN, element=Element.FIRE,
        primary_emotion="Courage", secondary_emotion="Wonder",
        description="Structure/Handoff - Change and transition",
        resonance_keywords=["transform", "change", "transition", "evolve", "handoff"],
    ),
    GardenGanaMapping(
        garden="truth", gana="Wall (Bi)", gana_tool="gana_wall",
        quadrant=Quadrant.SOUTHERN, element=Element.EARTH,
        primary_emotion="Truth", description="Boundaries/Alerts - Clear limits",
        resonance_keywords=["truth", "boundary", "limit", "alert", "verify"],
    ),

    # ─────────────────────────────────────────────────────────────────────────────
    # WESTERN QUADRANT (Metal/White Tiger) - Mansions 15-21
    # Theme: Refinement, Harvest, Precision
    # ─────────────────────────────────────────────────────────────────────────────
    GardenGanaMapping(
        garden="awe", gana="Straddling Legs (Kui)", gana_tool="gana_straddling_legs",
        quadrant=Quadrant.WESTERN, element=Element.METAL,
        primary_emotion="Wonder", secondary_emotion="Gratitude",
        description="Balance/Equilibrium - Standing in awe",
        resonance_keywords=["awe", "balance", "equilibrium", "stand", "marvel"],
    ),
    GardenGanaMapping(
        garden="gratitude", gana="Mound (Lou)", gana_tool="gana_mound",
        quadrant=Quadrant.WESTERN, element=Element.EARTH,
        primary_emotion="Gratitude", secondary_emotion="Joy",
        description="Accumulation/Harvest - Gathering blessings",
        resonance_keywords=["thank", "grateful", "harvest", "gather", "abundance"],
    ),
    GardenGanaMapping(
        garden="creation", gana="Stomach (Wei)", gana_tool="gana_stomach",
        quadrant=Quadrant.WESTERN, element=Element.EARTH,
        primary_emotion="Joy", secondary_emotion="Wonder",
        description="Digestion/Storage - Processing and creating",
        resonance_keywords=["create", "make", "build", "digest", "process"],
    ),
    GardenGanaMapping(
        garden="presence", gana="Hairy Head (Mao)", gana_tool="gana_hairy_head",
        quadrant=Quadrant.WESTERN, element=Element.METAL,
        primary_emotion="Stillness", secondary_emotion="Truth",
        description="Detail/Focus - Being fully here",
        resonance_keywords=["focus", "present", "attention", "detail", "here"],
    ),
    GardenGanaMapping(
        garden="play", gana="Net (Bi)", gana_tool="gana_net",
        quadrant=Quadrant.WESTERN, element=Element.METAL,
        primary_emotion="Joy", secondary_emotion="Wonder",
        description="Capture/Filters - Playful exploration",
        resonance_keywords=["play", "fun", "game", "capture", "explore"],
    ),
    GardenGanaMapping(
        garden="practice", gana="Turtle Beak (Zui)", gana_tool="gana_turtle_beak",
        quadrant=Quadrant.WESTERN, element=Element.METAL,
        primary_emotion="Courage", secondary_emotion="Wisdom",
        description="Precision/Protection - Disciplined effort",
        resonance_keywords=["practice", "train", "discipline", "precise", "skill"],
    ),
    GardenGanaMapping(
        garden="reverence", gana="Three Stars (Shen)", gana_tool="gana_three_stars",
        quadrant=Quadrant.WESTERN, element=Element.FIRE,
        primary_emotion="Gratitude", secondary_emotion="Wonder",
        description="Rejuvenation/Judgment - Sacred respect",
        resonance_keywords=["revere", "sacred", "honor", "respect", "renew"],
    ),

    # ─────────────────────────────────────────────────────────────────────────────
    # NORTHERN QUADRANT (Water/Black Tortoise) - Mansions 22-28
    # Theme: Depth, Mystery, Regeneration
    # ─────────────────────────────────────────────────────────────────────────────
    GardenGanaMapping(
        garden="joy", gana="Abundance (Zhen)", gana_tool="gana_abundance",
        quadrant=Quadrant.NORTHERN, element=Element.FIRE,
        primary_emotion="Joy", description="Regeneration/Surplus - Overflowing happiness",
        resonance_keywords=["joy", "happy", "celebrate", "abundant", "overflow"],
    ),
    GardenGanaMapping(
        garden="adventure", gana="Chariot (Zhen)", gana_tool="gana_chariot",
        quadrant=Quadrant.NORTHERN, element=Element.WATER,
        primary_emotion="Courage", secondary_emotion="Wonder",
        description="Mobility/Traversal - Journey and exploration",
        resonance_keywords=["adventure", "journey", "travel", "move", "discover"],
    ),
    GardenGanaMapping(
        garden="beauty", gana="Wings (Yi)", gana_tool="gana_wings",
        quadrant=Quadrant.NORTHERN, element=Element.FIRE,
        primary_emotion="Beauty", secondary_emotion="Joy",
        description="Deployment/Expansion - Aesthetic flight",
        resonance_keywords=["beauty", "aesthetic", "fly", "expand", "soar"],
    ),
    GardenGanaMapping(
        garden="humor", gana="Willow (Liu)", gana_tool="gana_willow",
        quadrant=Quadrant.NORTHERN, element=Element.WATER,
        primary_emotion="Joy", secondary_emotion="Wisdom",
        description="Resilience/Flexibility - Bending without breaking",
        resonance_keywords=["humor", "laugh", "flexible", "resilient", "bend"],
    ),
    GardenGanaMapping(
        garden="voice", gana="Star (Xing)", gana_tool="gana_star",
        quadrant=Quadrant.NORTHERN, element=Element.FIRE,
        primary_emotion="Truth", secondary_emotion="Courage",
        description="Governance/Illumination - Speaking one's truth",
        resonance_keywords=["voice", "speak", "express", "illuminate", "shine"],
    ),
    GardenGanaMapping(
        garden="sangha", gana="Extended Net (Zhang)", gana_tool="gana_extended_net",
        quadrant=Quadrant.NORTHERN, element=Element.WATER,
        primary_emotion="Love", secondary_emotion="Gratitude",
        description="Connection/Capture - Community bonds",
        resonance_keywords=["community", "together", "sangha", "collective", "network"],
    ),
    GardenGanaMapping(
        garden="grief", gana="Ghost (Gui)", gana_tool="gana_ghost",
        quadrant=Quadrant.NORTHERN, element=Element.WATER,
        primary_emotion="Stillness", secondary_emotion="Love",
        description="Introspection/Accumulation - Honoring loss",
        resonance_keywords=["grief", "loss", "mourn", "remember", "honor"],
    ),
]

# ═══════════════════════════════════════════════════════════════════════════════
# REGISTRY ACCESS FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════

def get_registry() -> list[GardenGanaMapping]:
    """Return the full 28-fold registry."""
    return GARDEN_GANA_REGISTRY

def get_by_garden(garden_name: str) -> GardenGanaMapping | None:
    """Lookup by garden name."""
    for entry in GARDEN_GANA_REGISTRY:
        if entry.garden.lower() == garden_name.lower():
            return entry
    return None

def get_by_gana(gana_name: str) -> GardenGanaMapping | None:
    """Lookup by Gana name."""
    for entry in GARDEN_GANA_REGISTRY:
        if gana_name.lower() in entry.gana.lower():
            return entry
    return None

def get_by_keyword(keyword: str) -> list[GardenGanaMapping]:
    """Find all entries that resonate with a keyword."""
    results = []
    keyword_lower = keyword.lower()
    for entry in GARDEN_GANA_REGISTRY:
        if entry.resonance_keywords:
            for kw in entry.resonance_keywords:
                if keyword_lower in kw.lower() or kw.lower() in keyword_lower:
                    results.append(entry)
                    break
    return results

def get_by_element(element: Element) -> list[GardenGanaMapping]:
    """Get all entries for an element."""
    return [e for e in GARDEN_GANA_REGISTRY if e.element == element]

def get_by_quadrant(quadrant: Quadrant) -> list[GardenGanaMapping]:
    """Get all entries for a quadrant."""
    return [e for e in GARDEN_GANA_REGISTRY if e.quadrant == quadrant]

def get_by_emotion(emotion: str) -> list[GardenGanaMapping]:
    """Get all entries for a primary emotion."""
    return [e for e in GARDEN_GANA_REGISTRY if e.primary_emotion.lower() == emotion.lower()]

# ═══════════════════════════════════════════════════════════════════════════════
# RESONANCE CALCULATOR
# ═══════════════════════════════════════════════════════════════════════════════

def calculate_resonance(text: str) -> dict[str, Any]:
    """Analyze text and determine which Gardens/Ganas resonate.
    Returns a ranked list of resonating entries.
    """
    words = text.lower().split()
    resonance_scores: dict[str, dict[str, Any]] = {}

    for entry in GARDEN_GANA_REGISTRY:
        score = 0
        if entry.resonance_keywords:
            for kw in entry.resonance_keywords:
                for word in words:
                    if kw in word or word in kw:
                        score += 1
        if score > 0:
            resonance_scores[entry.garden] = {
                "score": score,
                "gana": entry.gana,
                "gana_tool": entry.gana_tool,
                "element": entry.element.value,
                "emotion": entry.primary_emotion,
            }

    # Sort by score descending
    sorted_resonance = dict(
        sorted(
            resonance_scores.items(),
            key=lambda x: int(x[1].get("score", 0)),
            reverse=True,
        ),
    )
    return sorted_resonance

if __name__ == "__main__":
    print("=" * 60)
    print("GARDEN-GANA REGISTRY: 28-FOLD RESONANCE MAP")
    print("=" * 60)
    for entry in GARDEN_GANA_REGISTRY:
        print(f"{entry.garden:15} ←→ {entry.gana:25} [{entry.element.value}]")
    print("=" * 60)
    print(f"Total Entries: {len(GARDEN_GANA_REGISTRY)}")
