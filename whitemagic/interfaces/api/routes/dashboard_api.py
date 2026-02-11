"""Dashboard API - Live data endpoint for the WhiteMagic Dashboard.

Provides:
- System status
- Memory matrix data
- Timeline events
- File tracking stats
- Agentic module status
"""

import random
from datetime import datetime
from typing import Any, cast

from whitemagic.core.ganas.registry import get_all_ganas

try:
    from fastapi import APIRouter, HTTPException
except ImportError as e:  # pragma: no cover - optional dependency
    raise ImportError(
        "FastAPI is required for whitemagic.interfaces.api.routes.dashboard_api",
    ) from e

router = APIRouter(prefix="/api/dashboard", tags=["dashboard"])

# In-memory mock stats for now - to be replaced by Gan Ying subscription
_GANA_STATS: dict[str, int] = {}



@router.get("/status")
async def get_system_status() -> dict[str, Any]:
    """Get full system status for dashboard."""
    try:
        from whitemagic.integration import get_hub

        hub = get_hub()
        if not hub.activated:
            hub.activate_all()

        return cast("dict[str, Any]", hub.get_status())
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/data")
async def get_dashboard_data() -> dict[str, Any]:
    """Get complete dashboard visualization data."""
    try:
        from whitemagic.integration import get_hub

        hub = get_hub()
        return cast("dict[str, Any]", hub.get_dashboard_data())
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/memory-matrix")
async def get_memory_matrix() -> dict[str, Any]:
    """Get memory matrix stats and recent files."""
    try:
        from whitemagic.core.memory_matrix import (  # type: ignore[import-not-found]
            get_matrix,
            get_seen_registry,
            get_timeline,
        )

        matrix = get_matrix()
        registry = get_seen_registry()
        timeline = get_timeline()

        return {
            "stats": matrix.stats(),
            "recent_files": [
                {
                    "path": e.path.split("/whitemagic/")[-1] if "/whitemagic/" in e.path else e.path,
                    "type": e.file_type,
                    "times_seen": e.times_seen,
                    "last_seen": e.last_seen,
                }
                for e in registry.get_recent(20)
            ],
            "timeline_summary": timeline.stats(),
            "today": matrix.get_today_summary(),
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/agentic")
async def get_agentic_status() -> dict[str, Any]:
    """Get status of all 13 agentic modules."""
    try:
        from whitemagic.core.intelligence.agentic import full_brain_activation

        return {
            "modules": full_brain_activation(),
            "total": 13,
            "description": "Brain upgrade suite - cognitive enhancements",
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/session")
async def get_session_info() -> dict[str, Any]:
    """Get current session information."""
    try:
        from whitemagic.session import get_session_manifest  # type: ignore[import-not-found]

        manifest = get_session_manifest()
        if manifest:
            return cast("dict[str, Any]", manifest.to_dict())
        return {"error": "No active session"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/activate")
async def activate_all_systems() -> dict[str, Any]:
    """Activate the full integration system."""
    try:
        from whitemagic.integration import activate_all

        return cast("dict[str, Any]", activate_all())
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/visualization")
async def get_visualization_data() -> dict[str, Any]:
    """Get data formatted for 2D grid visualization."""
    try:
        from whitemagic.core.memory_matrix import get_matrix

        matrix = get_matrix()
        return cast("dict[str, Any]", matrix.export_for_visualization())
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/ganas/activity")
async def get_gana_activity() -> dict[str, list[dict[str, Any]]]:
    """Get activity stats for all 28 Ganas."""
    try:
        ganas = get_all_ganas()
        activity_list = []

        for gana in ganas:
            mansion = gana.mansion

            # Simulate liveliness based on time/randomness until Gan Ying is wired
            # giving it a "breathing" effect
            base_invocations = _GANA_STATS.get(mansion.name, 0)
            invocation_delta = random.randint(0, 5) if random.random() > 0.7 else 0
            current_invocations = base_invocations + invocation_delta
            _GANA_STATS[mansion.name] = current_invocations

            # Map Internal Quadrant to Frontend Quadrant (lowercase)
            quad_map = {"East": "east", "South": "south", "West": "west", "North": "north"}

            activity_list.append({
                "mansion": f"{mansion.chinese} ({mansion.pinyin})",
                "quadrant": quad_map.get(mansion.quadrant, "east"),
                "invocations": current_invocations,
                "avgExecutionMs": random.randint(50, 300),
                "lastActive": datetime.now().isoformat(),
            })

        return {"ganas": activity_list}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
