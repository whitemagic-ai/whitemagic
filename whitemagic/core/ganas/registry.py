"""Gana Registry.

Maps MCP tools to their corresponding Ganas.
"""

from typing import Any, cast

from .base import BaseGana
from .eastern_quadrant import (
    HeartGana,
    HornGana,
    NeckGana,
    RoomGana,
    RootGana,
    TailGana,
    WinnowingBasketGana,
)
from .northern_quadrant import (
    DipperGana,
    EncampmentGana,
    GirlGana,
    OxGana,
    RoofGana,
    VoidGana,
    WallGana,
)
from .southern_quadrant import (
    AbundanceGana,
    ChariotGana,
    ExtendedNetGana,
    GhostGana,
    StarGana,
    WillowGana,
    WingsGana,
)
from .western_quadrant import (
    HairyHeadGana,
    MoundGana,
    NetGana,
    StomachGana,
    StraddlingLegsGana,
    ThreeStarsGana,
    TurtleBeakGana,
)

# Map consolidated tool names to Gana classes
TOOL_TO_GANA: dict[str, type[BaseGana]] = {
    # --- EASTERN QUADRANT (Spring) ---
    "gana_horn": HornGana,      # Session Initialization & Setup
    "gana_neck": NeckGana,      # Core Memory Creation & Stability
    "gana_root": RootGana,      # System Health & Foundations
    "gana_room": RoomGana,      # Resource Locks & Privacy
    "gana_heart": HeartGana,    # Session Context & Pulse
    "gana_tail": TailGana,      # Performance & Acceleration
    "gana_winnowing_basket": WinnowingBasketGana, # Wisdom & Tag Filtering

    # --- SOUTHERN QUADRANT (Summer) ---
    "gana_ghost": GhostGana,    # Introspection & Metric Tracking
    "gana_willow": WillowGana,  # Resilience & UI Flexibility
    "gana_star": StarGana,      # Governance & PRAT Invocation
    "gana_extended_net": ExtendedNetGana, # Pattern Connectivity
    "gana_wings": WingsGana,    # Deployment & Scalability
    "gana_chariot": ChariotGana, # Mobility & Archaeology
    "gana_abundance": AbundanceGana, # Regeneration & Surplus

    # --- WESTERN QUADRANT (Autumn) ---
    "gana_straddling_legs": StraddlingLegsGana, # Balance & Equilibrium
    "gana_mound": MoundGana,    # Accumulation & Caching
    "gana_stomach": StomachGana, # Digestion & Resource Management
    "gana_hairy_head": HairyHeadGana, # Detail & Focus (Debug)
    "gana_net": NetGana,        # Capture & Filtering
    "gana_turtle_beak": TurtleBeakGana, # Precision & Protection
    "gana_three_stars": ThreeStarsGana, # Rejuvenation & Judgment (Wisdom)

    # --- NORTHERN QUADRANT (Winter) ---
    "gana_dipper": DipperGana,  # Governance & Strategy
    "gana_ox": OxGana,          # Endurance & Watchdog
    "gana_girl": GirlGana,      # Nurture & User Profile
    "gana_void": VoidGana,      # Emptiness & Defrag
    "gana_roof": RoofGana,      # Shelter & Zodiac Cores
    "gana_encampment": EncampmentGana, # Transition & Handoff
    "gana_wall": WallGana,      # Boundaries & Notifications
}

def get_gana_for_tool(tool_name: str) -> BaseGana | None:
    """Get instantiated Gana for a given tool."""
    gana_class = TOOL_TO_GANA.get(tool_name)
    if gana_class:
        # Cast to Any to allow zero-arg constructor call for subclasses, then back to BaseGana
        inst = cast("Any", gana_class)()
        return cast("BaseGana", inst)
    return None

def get_all_ganas() -> list[BaseGana]:
    """Get instances of all 28 Ganas in order."""
    return [
        cast("BaseGana", HornGana()), cast("BaseGana", NeckGana()), cast("BaseGana", RootGana()),
        cast("BaseGana", RoomGana()), cast("BaseGana", HeartGana()), cast("BaseGana", TailGana()),
        cast("BaseGana", WinnowingBasketGana()),
        cast("BaseGana", GhostGana()), cast("BaseGana", WillowGana()), cast("BaseGana", StarGana()),
        cast("BaseGana", ExtendedNetGana()), cast("BaseGana", WingsGana()), cast("BaseGana", ChariotGana()),
        cast("BaseGana", AbundanceGana()),
        cast("BaseGana", StraddlingLegsGana()), cast("BaseGana", MoundGana()), cast("BaseGana", StomachGana()),
        cast("BaseGana", HairyHeadGana()), cast("BaseGana", NetGana()), cast("BaseGana", TurtleBeakGana()),
        cast("BaseGana", ThreeStarsGana()),
        cast("BaseGana", DipperGana()), cast("BaseGana", OxGana()), cast("BaseGana", GirlGana()),
        cast("BaseGana", VoidGana()), cast("BaseGana", RoofGana()), cast("BaseGana", EncampmentGana()),
        cast("BaseGana", WallGana()),
    ]
