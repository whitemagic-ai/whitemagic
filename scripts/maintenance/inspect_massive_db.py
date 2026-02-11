import sqlite3
import os
from pathlib import Path
import sys

repo_root = Path(__file__).resolve().parents[2]
if str(repo_root) not in sys.path:
    sys.path.insert(0, str(repo_root))

try:
    from whitemagic.config.paths import DB_PATH as DEFAULT_DB_PATH  # type: ignore
except Exception:
    DEFAULT_DB_PATH = Path.home() / ".whitemagic" / "memory" / "whitemagic.db"

def inspect_db():
    db_path = Path(os.getenv("WM_DB_PATH", str(DEFAULT_DB_PATH))).expanduser().resolve()
    
    if not db_path.exists():
        print(f"Database not found at {db_path}")
        return

    print(f"Inspecting MASSIVE database at: {db_path}")
    
    try:
        conn = sqlite3.connect(str(db_path))
        cursor = conn.cursor()
        
        # Check holographic_coords count
        try:
            cursor.execute("SELECT count(*) FROM holographic_coords")
            count = cursor.fetchone()[0]
            print(f"\nTotal Holographic Coordinates: {count}")
        except Exception as e:
            print(f"\nCould not read holographic_coords: {e}")
        
        # Check memories count
        cursor.execute("SELECT count(*) FROM memories")
        mem_count = cursor.fetchone()[0]
        print(f"\nTotal Memories: {mem_count}")
        
        # Check percentage
        if mem_count > 0:
            print(f"Holographic Coverage: {(count/mem_count)*100:.1f}%")

    except Exception as e:
        print(f"Error inspecting DB: {e}")
    finally:
        if conn:
            conn.close()

if __name__ == "__main__":
    inspect_db()
