import os
import sys
import sqlite3
from pathlib import Path

def get_history_content():
    repo_root = Path(__file__).resolve().parents[2]
    if str(repo_root) not in sys.path:
        sys.path.insert(0, str(repo_root))

    try:
        from whitemagic.config.paths import DB_PATH as DEFAULT_DB_PATH  # type: ignore
    except Exception:
        DEFAULT_DB_PATH = Path.home() / ".whitemagic" / "memory" / "whitemagic.db"

    db_path = Path(os.getenv("WM_DB_PATH", str(DEFAULT_DB_PATH))).expanduser().resolve()
    conn = sqlite3.connect(str(db_path))
    cursor = conn.cursor()
    
    query = """
    SELECT title, content, created_at FROM memories 
    WHERE title = 'transcript_2025-11-18'
    LIMIT 1
    """
    
    cursor.execute(query)
    rows = cursor.fetchall()
    
    print(f"Found {len(rows)} matches:")
    for row in rows:
        title, content, created_at = row
        print(f"\n--- {title} ({created_at}) ---")
        print(content[:2000]) # Print first 2000 chars
        if len(content) > 2000:
            print("...(truncated)...")
    
    conn.close()

if __name__ == "__main__":
    get_history_content()
