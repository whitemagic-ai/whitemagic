
from typing import Any

from whitemagic.core.ganas.base import GanaCall

# ============================================================================
# GANA TOOLS WRAPPERS (Async)
# ============================================================================

async def gana_horn(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Horn (Jiao) Gana Tool - Session Initialization."""
    from whitemagic.core.ganas.eastern_quadrant import HornGana
    gana = HornGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_neck(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Neck (Kang) Gana Tool - Stability/Memory."""
    from whitemagic.core.ganas.eastern_quadrant import NeckGana
    gana = NeckGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_root(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Root (Di) Gana Tool - System Health."""
    from whitemagic.core.ganas.eastern_quadrant import RootGana
    gana = RootGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_room(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Room (Fang) Gana Tool - Resource Locks."""
    from whitemagic.core.ganas.eastern_quadrant import RoomGana
    gana = RoomGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_heart(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Heart (Xin) Gana Tool - Session Context."""
    from whitemagic.core.ganas.eastern_quadrant import HeartGana
    gana = HeartGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_tail(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Tail (Wei) Gana Tool - Acceleration."""
    from whitemagic.core.ganas.eastern_quadrant import TailGana
    gana = TailGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_winnowing_basket(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Winnowing Basket (Ji) Gana Tool - Wisdom/Separation."""
    from whitemagic.core.ganas.eastern_quadrant import WinnowingBasketGana
    gana = WinnowingBasketGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

# Southern Quadrant
async def gana_ghost(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Ghost (Gui) Gana Tool."""
    from whitemagic.core.ganas.southern_quadrant import GhostGana
    gana = GhostGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_willow(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Willow (Liu) Gana Tool."""
    from whitemagic.core.ganas.southern_quadrant import WillowGana
    gana = WillowGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_star(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Star (Xing) Gana Tool."""
    from whitemagic.core.ganas.southern_quadrant import StarGana
    gana = StarGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_extended_net(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Extended Net (Zhang) Gana Tool."""
    from whitemagic.core.ganas.southern_quadrant import ExtendedNetGana
    gana = ExtendedNetGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_wings(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Wings (Yi) Gana Tool."""
    from whitemagic.core.ganas.southern_quadrant import WingsGana
    gana = WingsGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_chariot(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Chariot (Zhen) Gana Tool."""
    from whitemagic.core.ganas.southern_quadrant import ChariotGana
    gana = ChariotGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_abundance(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Abundance (Zhen) Gana Tool."""
    from whitemagic.core.ganas.southern_quadrant import AbundanceGana
    gana = AbundanceGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

# Western Quadrant
async def gana_straddling_legs(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Straddling Legs (Kui) Gana Tool."""
    from whitemagic.core.ganas.western_quadrant import StraddlingLegsGana
    gana = StraddlingLegsGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_mound(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Mound (Lou) Gana Tool."""
    from whitemagic.core.ganas.western_quadrant import MoundGana
    gana = MoundGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_stomach(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Stomach (Wei) Gana Tool."""
    from whitemagic.core.ganas.western_quadrant import StomachGana
    gana = StomachGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_hairy_head(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Hairy Head (Mao) Gana Tool."""
    from whitemagic.core.ganas.western_quadrant import HairyHeadGana
    gana = HairyHeadGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_net(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Net (Bi) Gana Tool."""
    from whitemagic.core.ganas.western_quadrant import NetGana
    gana = NetGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_turtle_beak(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Turtle Beak (Zui) Gana Tool."""
    from whitemagic.core.ganas.western_quadrant import TurtleBeakGana
    gana = TurtleBeakGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_three_stars(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Three Stars (Shen) Gana Tool."""
    from whitemagic.core.ganas.western_quadrant import ThreeStarsGana
    gana = ThreeStarsGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

# Northern Quadrant
async def gana_dipper(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Dipper (Dou) Gana Tool."""
    from whitemagic.core.ganas.northern_quadrant import DipperGana
    gana = DipperGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_ox(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Ox (Niu) Gana Tool."""
    from whitemagic.core.ganas.northern_quadrant import OxGana
    gana = OxGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_girl(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Girl (Nu) Gana Tool."""
    from whitemagic.core.ganas.northern_quadrant import GirlGana
    gana = GirlGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_void(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Void (Xu) Gana Tool."""
    from whitemagic.core.ganas.northern_quadrant import VoidGana
    gana = VoidGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_roof(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Roof (Wei) Gana Tool."""
    from whitemagic.core.ganas.northern_quadrant import RoofGana
    gana = RoofGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_encampment(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Encampment (Shi) Gana Tool."""
    from whitemagic.core.ganas.northern_quadrant import EncampmentGana
    gana = EncampmentGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}

async def gana_wall(operation: str = "invoke", **kwargs: Any) -> dict[str, Any]:
    """Wall (Bi) Gana Tool."""
    from whitemagic.core.ganas.northern_quadrant import WallGana
    gana = WallGana()
    call = GanaCall(task=kwargs.get("task", operation), state_vector=kwargs)
    try:
        result = await gana.invoke(call)
        return {"mansion": result.mansion.name, "garden": result.garden, "result": result.output, "stats": result.karma_trace}
    except Exception as e:
        return {"error": str(e)}
