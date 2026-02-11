
import os
import sys
import sqlite3
import hashlib
from datetime import datetime
from pathlib import Path

# Config
SOURCE_DIR = Path("archive/legacy_roots/newmagic")
DB_PATH = Path("memory/whitemagic.db")
EXTENSIONS = {'.md', '.txt'}

def get_content_hash(content):
    return hashlib.md5(content.encode('utf-8')).hexdigest()

def import_memories():
    if not DB_PATH.exists():
        print("DB not found!")
        return

    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    
    # Ensure tables exist (brief check)
    cursor.execute("CREATE TABLE IF NOT EXISTS memories (id TEXT PRIMARY KEY, title TEXT, content TEXT, created_at TEXT, metadata TEXT)")

    count = 0
    skipped = 0
    errors = 0

    print(f"Scanning {SOURCE_DIR}...")
    
    # Pre-cache existing hashes to avoid dupes? Too slow for 188k maybe.
    # Just rely on title/content duplication check?
    
    for root, dirs, files in os.walk(SOURCE_DIR):
        for file in files:
            ext = os.path.splitext(file)[1].lower()
            if ext in EXTENSIONS:
                path = Path(root) / file
                try:
                    with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    if not content.strip():
                        continue

                    # Basic dedupe by title (filename) + content hash
                    title = path.stem
                    
                    # Check existence
                    cursor.execute("SELECT id FROM memories WHERE title = ?", (title,))
                    existing = cursor.fetchone()
                    
                    if existing:
                        skipped += 1
                        continue

                    # Insert
                    mem_id = f"legacy_{hashlib.sha256(str(path).encode()).hexdigest()[:16]}"
                    created_at = datetime.fromtimestamp(path.stat().st_mtime).isoformat()
                    
                    cursor.execute(
                        "INSERT INTO memories (id, title, content, created_at, metadata) VALUES (?, ?, ?, ?, ?)",
                        (mem_id, title, content, created_at, f'{{"source": "{path}"}}')
                    )
                    count += 1
                    
                    if count % 1000 == 0:
                        print(f"Imported {count} memories...", end='\r')
                        conn.commit()
                        
                except Exception as e:
                    errors += 1
    
    conn.commit()
    conn.close()
    print(f"\nImport Complete: {count} imported, {skipped} skipped, {errors} errors.")

if __name__ == "__main__":
    import_memories()
