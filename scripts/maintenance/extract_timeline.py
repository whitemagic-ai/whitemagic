import os
import sys
import sqlite3
from pathlib import Path
from datetime import datetime
import json

def extract_timeline():
    repo_root = Path(__file__).resolve().parents[2]
    if str(repo_root) not in sys.path:
        sys.path.insert(0, str(repo_root))

    try:
        from whitemagic.config.paths import DB_PATH as DEFAULT_DB_PATH  # type: ignore
    except Exception:
        DEFAULT_DB_PATH = Path.home() / ".whitemagic" / "memory" / "whitemagic.db"

    db_path = Path(os.getenv("WM_DB_PATH", str(DEFAULT_DB_PATH))).expanduser().resolve()
    if not db_path.exists():
        print(f"DB not found at {db_path}")
        return

    conn = sqlite3.connect(str(db_path))
    cursor = conn.cursor()

    # Try to find date column
    # Usually created_at or timestamp
    try:
        cursor.execute("PRAGMA table_info(memories)")
        columns = [c[1] for c in cursor.fetchall()]
        print(f"Columns: {columns}")
        
        date_col = "created_at" if "created_at" in columns else "timestamp"
        
        query = f"""
            SELECT {date_col}, content, memory_type 
            FROM memories 
            WHERE content LIKE '%2025%' OR content LIKE '%November%' OR content LIKE '%October%'
            ORDER BY {date_col} ASC
        """
        cursor.execute(query)
        rows = cursor.fetchall()
        
        print(f"Found {len(rows)} memories")
        
        # Group by month
        timeline = {}
        for row in rows:
            ts, content, mtype = row
            try:
                dt = datetime.fromisoformat(ts)
                month_key = dt.strftime("%Y-%m")
                if month_key not in timeline:
                    timeline[month_key] = []
                
                # key events only (simple heuristic)
                if "create" in content.lower() or "version" in content.lower() or "system" in content.lower():
                    timeline[month_key].append(f"{ts[:10]}: {content[:50]}...")
            except Exception:
                pass

        print("\n=== TIMELINE ===")
        for month in sorted(timeline.keys()):
            print(f"\n[{month}]")
            for event in timeline[month][:5]: # Show top 5 per month
                print(f" - {event}")

    except Exception as e:
        print(f"Error: {e}")
    finally:
        conn.close()

if __name__ == "__main__":
    extract_timeline()
