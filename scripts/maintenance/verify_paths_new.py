import sys
import os
from pathlib import Path

# Add staging to path so we can import whitemagic
import sys; import os; sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))) # Auto-fixed path

try:
    from whitemagic.config.paths import ensure_paths, WM_ROOT, SESSIONS_DIR, USER_PROFILE_PATH
    from whitemagic.config import PROJECT_ROOT, DATA_DIR
    from whitemagic.sessions.manager import SessionManager
    from whitemagic.systems.user import UserManager
    
    print(f"WM_ROOT: {WM_ROOT}")
    print(f"SESSIONS_DIR: {SESSIONS_DIR}")
    print(f"USER_PROFILE_PATH: {USER_PROFILE_PATH}")
    print(f"PROJECT_ROOT: {PROJECT_ROOT}")
    print(f"DATA_DIR: {DATA_DIR}")
    
    print("\nEnsuring paths...")
    ensure_paths()
    
    print("\nChecking SessionManager defaults...")
    sm = SessionManager()
    print(f"SessionManager base_dir: {sm.base_dir}")
    assert sm.base_dir == SESSIONS_DIR
    
    print("\nChecking UserManager defaults...")
    um = UserManager()
    print(f"UserManager profile_path: {um.profile_path}")
    assert um.profile_path == USER_PROFILE_PATH
    
    print("\n✅ Path verification SUCCESS!")
    
except Exception as e:
    print(f"\n❌ Path verification FAILED: {e}")
    sys.exit(1)
