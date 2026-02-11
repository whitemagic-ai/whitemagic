import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT))

from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.config.paths import DB_PATH

print(f"Checking DB at: {DB_PATH}")
um = get_unified_memory()
stats = um.get_stats()
print(f"Total Memories: {stats.get('total_memories')}")
print(f"Breakdown: {stats}")
