# ruff: noqa: F403

# ============================================================================
# WHITEMAGIC MCP API BRIDGE (FACADE)
# ============================================================================
# This file is now a facade for the modular package `whitemagic.core.bridge`.
# It maintains backward compatibility for existing tools/servers invoking this script.
# ============================================================================

import sys
import json
from whitemagic.core.bridge.utils import logger

# === IMPORT ALL TOOLS FROM MODULAR BRIDGE ===
from whitemagic.core.bridge.zodiac import *
from whitemagic.core.bridge.kaizen import *
from whitemagic.core.bridge.garden import *
from whitemagic.core.bridge.archaeology import *
from whitemagic.core.bridge.memory import *
from whitemagic.core.bridge.session import *
from whitemagic.core.bridge.wisdom import *
from whitemagic.core.bridge.reasoning import *
from whitemagic.core.bridge.pattern import *
from whitemagic.core.bridge.dharma import *
from whitemagic.core.bridge.inference import *
from whitemagic.core.bridge.agent import *
from whitemagic.core.bridge.voice import *
from whitemagic.core.bridge.autonomous import *
from whitemagic.core.bridge.benchmark import *
from whitemagic.core.bridge.optimization import *
from whitemagic.core.bridge.system import *
from whitemagic.core.bridge.rust import *
from whitemagic.core.bridge.adaptive import *
from whitemagic.core.bridge.gana import *
from whitemagic.core.bridge.metrics import *
from whitemagic.core.bridge.collaboration import *
from whitemagic.core.bridge.meditation import *
from whitemagic.core.bridge.gana_wrappers import *

# Import the Main Dispatcher
from whitemagic.core.bridge.tools import (
    execute_mcp_tool
)

if __name__ == '__main__':
    if len(sys.argv) > 1:
        tool_name = sys.argv[1]
        kwargs = json.loads(sys.argv[2]) if len(sys.argv) > 2 else {}
        result = execute_mcp_tool(tool_name, **kwargs)
        logger.info(json.dumps(result, default=str))
