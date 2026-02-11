
import sqlite3
import json
import logging
from pathlib import Path

# Config
DB_PATH = Path("memory/whitemagic.db")
LOG_PATH = Path("logs/tagging.log")

# Setup Logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(LOG_PATH),
        logging.StreamHandler()
    ]
)

# Heuristics
KEYWORDS = {
    "divination": ["hexagram", "oracle", "i ching", "gua", "trigram", "yarrow", "consultation"],
    "error_log": ["traceback", "exception", "stack trace", "error:", "fail:", "panic:"],
    "test_artifact": ["test_", "monkeypatch", "assertion", "pytest", "fixture"],
    "source_code": ["function", "def ", "class ", "import ", "package ", "module ", "public static", "fn "],
    "config": ["api_key", "secret", "config", "settings", "env", "json"]
}

def tag_memories():
    if not DB_PATH.exists():
        logging.error("Database not found!")
        return

    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    # Ensure tags table exists (it should, but safety first)
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS tags (
            memory_id TEXT,
            tag TEXT,
            FOREIGN KEY (memory_id) REFERENCES memories(id) ON DELETE CASCADE,
            PRIMARY KEY (memory_id, tag)
        )
    """)
    conn.commit()

    # Get untagged memories (or just all for a sweep)
    # We'll fetch id, content, title
    logging.info("Fetching memories...")
    cursor.execute("SELECT id, title, content FROM memories")
    rows = cursor.fetchall()
    
    total_tagged = 0
    tags_applied = 0
    
    for row in rows:
        mem_id, title, content = row
        title = (title or "").lower()
        content = (content or "").lower()
        
        applied_tags = set()
        
        # 1. Keyword check
        for tag, words in KEYWORDS.items():
            for word in words:
                if word in title or word in content:
                    applied_tags.add(tag)
                    break
        
        # 2. Structural check (JSON)
        if content.strip().startswith("{") or content.strip().startswith("["):
            try:
                json.loads(row[2]) # use original case content
                applied_tags.add("structured_data")
            except:
                pass

        # 3. Apply tags
        if applied_tags:
            total_tagged += 1
            for tag in applied_tags:
                try:
                    cursor.execute("INSERT OR IGNORE INTO tags (memory_id, tag) VALUES (?, ?)", (mem_id, tag))
                    tags_applied += 1
                except Exception as e:
                    logging.error(f"Failed to tag {mem_id}: {e}")
            
    conn.commit()
    conn.close()
    
    logging.info(f"Tagging Complete.")
    logging.info(f"Memories Processed: {len(rows)}")
    logging.info(f"Memories Tagged: {total_tagged}")
    logging.info(f"Total Tags Applied: {tags_applied}")

if __name__ == "__main__":
    tag_memories()
