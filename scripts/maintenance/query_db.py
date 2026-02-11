
import sqlite3
from pathlib import Path

DB_PATH = Path("staging/project_memory/whitemagic.db")

def query():
    if not DB_PATH.exists():
        print(f"❌ Error: Database not found at {DB_PATH}")
        return

    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    
    print("--- Count ---")
    rows = cursor.execute("SELECT count(*) as c FROM memories").fetchone()
    print(f"Total: {rows['c']}")
    
    print("\n--- Non-Untitled ---")
    rows = cursor.execute("SELECT title, memory_type, created_at FROM memories WHERE title NOT LIKE 'Untitled%' LIMIT 10").fetchall()
    for row in rows:
        print(f"Title: {row['title']} | Type: {row['memory_type']} | Date: {row['created_at']}")
        
    print("\n--- Recent 5 ---")
    rows = cursor.execute("SELECT title, memory_type, created_at FROM memories ORDER BY created_at DESC LIMIT 5").fetchall()
    for row in rows:
        print(f"Title: {row['title']} | Type: {row['memory_type']} | Date: {row['created_at']}")

    conn.close()

if __name__ == "__main__":
    query()
