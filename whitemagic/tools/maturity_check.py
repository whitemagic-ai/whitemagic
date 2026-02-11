"""Maturity Check — Tool-Level Maturity Gate Enforcement.
======================================================
Maps tool names and categories to minimum maturity stages.
Called by dispatch_table.dispatch() as step 0.5.

Tools that are READ-safe or belong to basic categories pass at any
maturity level. Dangerous or advanced tools require higher stages.

Returns None if the tool is allowed, or an error dict if blocked.
"""

from __future__ import annotations

import logging
from typing import Any

logger = logging.getLogger(__name__)


# Tools/prefixes that require higher maturity stages
_MATURITY_REQUIREMENTS: dict[str, int] = {
    # Stage 3 (REFLECTIVE) — tools that affect memory retention or patterns
    "retention_sweep": 3,
    "memory_consolidate": 3,
    "memory.lifecycle_sweep": 3,
    # Stage 4 (RADIANT) — multi-agent coordination
    "agent.register": 4,
    "agent.heartbeat": 4,
    "agent.deregister": 4,
    "task.distribute": 4,
    "pipeline.create": 4,
    "vote.create": 4,
    "vote.cast": 4,
    # Stage 5 (COLLECTIVE) — mesh/guild
    "mesh.connect": 5,
    "mesh.guild_contract": 5,
}

# Category-level defaults (applied if no specific tool match)
_CATEGORY_MATURITY: dict[str, int] = {
    "MESH": 5,
}


def check_maturity_for_tool(tool_name: str) -> dict[str, Any] | None:
    """Check if the system's maturity is sufficient for this tool.

    Returns:
        None if the tool is allowed.
        Error dict if blocked by maturity gate.

    """
    # Find the required maturity level
    required = _MATURITY_REQUIREMENTS.get(tool_name)

    if required is None:
        # Check category-level requirement
        try:
            from whitemagic.tools.registry import get_tool
            tool_def = get_tool(tool_name)
            if tool_def is not None:
                cat = tool_def.category.name
                required = _CATEGORY_MATURITY.get(cat)
        except Exception:
            pass

    if required is None:
        return None  # No maturity requirement — allow

    # Try Haskell algebraic maturity gate first (exhaustive case coverage)
    try:
        from whitemagic.core.acceleration.haskell_bridge import haskell_maturity_assess
        from whitemagic.core.governance.maturity_gates import get_maturity_engine
        engine = get_maturity_engine()
        if engine._last_report:
            hs_result = haskell_maturity_assess(
                stage=engine._last_report.current_stage.value,
                tools_executed=getattr(engine, "_tools_executed", 0),
                session_count=getattr(engine, "_session_count", 0),
                dharma_score=getattr(engine, "_dharma_score", 0.5),
                harmony_score=getattr(engine, "_harmony_score", 0.5),
            )
            if hs_result and "allowed_categories" in hs_result:
                logger.debug("Haskell maturity gate consulted for %s", tool_name)
    except Exception:
        pass  # Haskell unavailable, fall through to Python

    # Check current maturity
    try:
        from whitemagic.core.governance.maturity_gates import (
            MaturityStage,
            get_maturity_engine,
        )
        engine = get_maturity_engine()
        if engine._last_report is None:
            engine.assess()
        current = engine._last_report.current_stage if engine._last_report else MaturityStage.SEED

        if current.value >= required:
            return None  # Mature enough

        # Map int to stage name for the message
        stage_names = {s.value: s.name for s in MaturityStage}
        required_name = stage_names.get(required, str(required))

        logger.info(
            f"Maturity gate blocked {tool_name}: "
            f"requires {required_name} (stage {required}), "
            f"current is {current.name} (stage {current.value})",
        )

        return {
            "status": "error",
            "error_code": "maturity_gate",
            "message": (
                f"Tool '{tool_name}' requires maturity stage "
                f"{required_name} (level {required}), but the system is at "
                f"{current.name} (level {current.value}). "
                f"Advance the system's maturity to unlock this capability."
            ),
            "maturity": {
                "current_stage": current.name,
                "current_level": current.value,
                "required_stage": required_name,
                "required_level": required,
            },
        }
    except Exception:
        return None  # If maturity system is unavailable, allow through
