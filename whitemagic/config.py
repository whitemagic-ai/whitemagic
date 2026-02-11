"""WhiteMagic Configuration"""

import os
from pathlib import Path
from typing import Any, Dict
from whitemagic.config.paths import (
    WM_ROOT,
    DB_PATH
)

# Project root directory (Code location)
PROJECT_ROOT = Path(__file__).parent.parent

# Directories (Data location)
# Imported from paths.py for canonical usage

# Default settings
DEFAULT_CONFIG: Dict[str, Any] = {
    "max_workers": os.cpu_count() or 4,
    "cache_enabled": True,
    "debug": False,
    "log_level": "INFO",
    "root_dir": str(WM_ROOT),
    "database": {"url": f"sqlite:///{DB_PATH}"}
}
