# Chapter 27: Encampment & Housing

**Gana**: EncampmentGana (Chinese: 室, Pinyin: Shì)
**Garden**: grief
**Quadrant**: Northern (Black Tortoise)
**Element**: Water
**Phase**: Yin Peak
**I Ching Hexagram**: 37. 家人 Jiā Rén (The Family) - Housing and structure

---

## 🎯 Purpose

Chapter 27 provides **encampment and housing**—creating the structure for session artifacts, maintaining archives, and ensuring the "house" is ready for rest. The Encampment (室) is the stable structure that allows for recovery and long-term storage.

Use this chapter when you need to:
- **Build archival structures** for session data
- **House session artifacts** securely
- **Maintain the camp** (stable project structure)
- **Prepare the environment** for the session's end
- **Organize deep archives** of conversation and code

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `windsurf_backup` | Archive Windsurf conversations | Structure maintenance |
| `manage_archaeology` | Explore and organize archives | Archeological retrieval |
| `session_checkpoint` | House the current state | Final structural save |
| `Gana::Encampment` | Execute archival logic | Automatic housing |

---

## 📋 Workflow

### 1. Archiving Session Artifacts

Create a secure, structured archive of all session activities and outcomes.

```python
from whitemagic.tools import windsurf_backup

# Securely archive the current work state
windsurf_backup(
    destination="~/.whitemagic/archives/session_2026_02_02",
    compress=True
)
```

### 2. Structural Integrity Check

Ensure all files are in their correct "rooms" (directories) and that the camp is organized.

```python
import os
from pathlib import Path

def check_structural_integrity(project_root: str):
    """Verify that the project encampment is structurally sound."""
    root = Path(project_root)
    required_rooms = ["core", "systems", "grimoire", "memory"]
    
    for room in required_rooms:
        if not (root / room).is_dir():
            print(f"⚠️ Missing room: {room}. Rebuilding structure...")
            (root / room).mkdir(parents=True, exist_ok=True)
            
    print("🏠 Encampment structure is secure.")
```

---

## 🧭 Navigation

**Next**: [Chapter 28: Boundaries & Alerts](28_WALL_BOUNDARIES.md) 
**Previous**: [Chapter 26: Shelter & Synthesis](26_ROOF_SHELTER.md)
**Quadrant**: Northern (Winter/Water) - Position 6/7
