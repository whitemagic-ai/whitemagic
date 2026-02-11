import os
import sys
import sqlite3
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from whitemagic.config.paths import DB_PATH as DEFAULT_DB_PATH

DB_PATH = Path(os.getenv("WM_DB_PATH", str(DEFAULT_DB_PATH))).expanduser().resolve()

def deduplicate():
    print(f"Connecting to {DB_PATH}...")
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    
    # Check duplicate titles
    print("Scanning for duplicate titles...")
    cursor.execute("""
        SELECT title, COUNT(*) as c 
        FROM memories 
        GROUP BY title 
        HAVING c > 1 
        ORDER BY c DESC
    """)
    duplicates = cursor.fetchall()
    print(f"Found {len(duplicates)} titles with duplicates.")
    
    total_removed = 0
    for title, count in duplicates:
        # Keep the most recent one (highest ID assuming auto-increment, or created_at)
        # We need to delete count-1 entries
        cursor.execute("""
            DELETE FROM memories 
            WHERE id NOT IN (
                SELECT id FROM memories WHERE title = ? ORDER BY created_at DESC LIMIT 1
            ) AND title = ?
        """, (title, title))
        total_removed += (count - 1)
        
        if total_removed % 1000 == 0:
            print(f"Removed {total_removed} duplicates so far...", end='\r')
            conn.commit()
            
    conn.commit()
    print(f"\nTotal duplicates removed: {total_removed}")
    
    # Run VACUUM to reclaim space
    print("Vacuuming database...")
    cursor.execute("VACUUM")
    conn.close()

if __name__ == "__main__":
    if not DB_PATH.exists():
        print(f"Error: {DB_PATH} not found!")
        sys.exit(1)
    deduplicate()
