
from pathlib import Path

ROOT_DIR = Path("staging/core_system/whitemagic").resolve()
PROJECT_MEM_DIR = ROOT_DIR.parent.parent.parent / "staging" / "project_memory"

print(f"Project Mem Dir: {PROJECT_MEM_DIR}")

target_dir = PROJECT_MEM_DIR / "recovery_abc"
print(f"Target Dir: {target_dir}")
print(f"Exists: {target_dir.exists()}")

found = 0
for p in target_dir.rglob("*.md"):
    if "CROSSING_THE_GREAT_WATER" in p.name:
        print(f"FOUND: {p}")
    found += 1

print(f"Total .md files found: {found}")
