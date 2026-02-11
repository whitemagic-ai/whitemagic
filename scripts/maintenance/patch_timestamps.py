
import sqlite3
import json
import os
import datetime
from pathlib import Path

DB_PATH = Path('staging/project_memory/whitemagic.db')
PROJECT_MEM_DIR = Path('staging/project_memory')

def patch():
    print("🩹 Patching Timestamps...")
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    
    # Get all memories
    rows = cursor.execute("SELECT id, metadata, title FROM memories").fetchall()
    
    updated = 0
    errors = 0
    
    for row in rows:
        try:
            meta = json.loads(row['metadata'] or '{}')
            src = meta.get('source_path')
            
            if src:
                # Find file
                fpath = PROJECT_MEM_DIR / src
                if fpath.exists():
                    mtime = fpath.stat().st_mtime
                    iso_date = datetime.datetime.fromtimestamp(mtime).isoformat()
                    
                    cursor.execute(
                        "UPDATE memories SET created_at = ? WHERE id = ?",
                        (iso_date, row['id'])
                    )
                    updated += 1
        except Exception as e:
            errors += 1
            
    conn.commit()
    conn.close()
    print(f"✅ Patched {updated} timestamps. ({errors} errors)")

if __name__ == "__main__":
    patch()
