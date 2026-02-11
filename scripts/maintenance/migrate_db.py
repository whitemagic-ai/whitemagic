import os
import sqlite3
import json
import uuid
import sys
from datetime import datetime
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

try:
    from whitemagic.config.paths import DB_PATH as DEFAULT_DB_PATH  # type: ignore
except Exception:
    DEFAULT_DB_PATH = Path.home() / ".whitemagic" / "memory" / "whitemagic.db"

LEGACY_DB_PATH = Path(os.getenv("WM_LEGACY_DB_PATH", "")).expanduser()
TARGET_DB_PATH = Path(os.getenv("WM_DB_PATH", str(DEFAULT_DB_PATH))).expanduser()

def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d

def migrate():
    if not LEGACY_DB_PATH.exists():
        print("Error: legacy DB not found. Set WM_LEGACY_DB_PATH to a valid SQLite file.")
        return
    TARGET_DB_PATH.parent.mkdir(parents=True, exist_ok=True)

    print(f"Opening legacy DB: {LEGACY_DB_PATH}")
    try:
        conn_legacy = sqlite3.connect(str(LEGACY_DB_PATH))
        conn_legacy.row_factory = dict_factory
        cur_legacy = conn_legacy.cursor()
    except Exception as e:
        print(f"Error opening legacy DB: {e}")
        return

    print(f"Opening target DB: {TARGET_DB_PATH}")
    try:
        conn_target = sqlite3.connect(str(TARGET_DB_PATH))
        cur_target = conn_target.cursor()
    except Exception as e:
        print(f"Error opening target DB: {e}")
        return

    # Check target columns to know what we can insert directly
    cur_target.execute("PRAGMA table_info(memories)")
    target_columns = {r[1] for r in cur_target.fetchall()}
    print(f"Target columns: {target_columns}")

    # Fetch all legacy memories
    print("Fetching legacy memories...")
    cur_legacy.execute("SELECT * FROM memories")
    legacy_rows = cur_legacy.fetchall()
    print(f"Found {len(legacy_rows)} legacy memories.")

    migrated_count = 0
    skipped_count = 0

    cur_target.execute("BEGIN TRANSACTION")

    try:
        for row in legacy_rows:
            # Map fields
            memory_id = row.get('id')
            if not memory_id:
                memory_id = str(uuid.uuid4())
            
            # Check if exists
            cur_target.execute("SELECT 1 FROM memories WHERE id = ?", (memory_id,))
            if cur_target.fetchone():
                skipped_count += 1
                continue

            # Prepare data for insertion
            
            # Base fields usually present in both
            title = row.get('title')
            content = row.get('content')
            memory_type = row.get('memory_type', 'legacy')
            created_at = row.get('created_at')
            
            # Handle timestamps (legacy might be different format?)
            # Both seem to be strings/TEXT based on check.
            
            # Map specific fields
            emotional_valence = row.get('emotional_valence')
            if emotional_valence is None: emotional_valence = 0.0
            
            importance = row.get('importance')
            if importance is None: importance = 0.5
            
            # Metadata merge
            meta_str = row.get('metadata', '{}')
            try:
                meta = json.loads(meta_str)
            except:
                meta = {}
            
            # Stash extra legacy fields into metadata if target doesn't have them
            extra_fields = ['path', 'embedding', 'format', 'resonance', 'checksum']
            for field in extra_fields:
                if field in row and row[field] is not None:
                    if field not in target_columns:
                        meta[f"legacy_{field}"] = row[field]
            
            meta['migrated_from'] = 'legacy_archive_2950'
            meta['migration_date'] = datetime.now().isoformat()
            
            new_meta_str = json.dumps(meta)
            
            # Construct INSERT statement dynamically based on available target columns
            insert_data = {
                'id': memory_id,
                'title': title,
                'content': content,
                'memory_type': memory_type,
                'created_at': created_at,
                'emotional_valence': emotional_valence,
                'importance': importance,
                'metadata': new_meta_str
            }
            
            # Add other matching columns if they exist in source and target
            common_cols = ['neuro_score', 'novelty_score', 'recall_count', 'half_life_days', 'is_protected', 'accessed_at', 'access_count', 'tags']
            for col in common_cols:
                if col in row and col in target_columns:
                    insert_data[col] = row[col]
                
            cols = list(insert_data.keys())
            placeholders = ', '.join(['?'] * len(cols))
            col_names = ', '.join(cols)
            values = [insert_data[c] for c in cols]
            
            sql = f"INSERT INTO memories ({col_names}) VALUES ({placeholders})"
            cur_target.execute(sql, values)
            migrated_count += 1
            
            if migrated_count % 500 == 0:
                print(f"Migrated {migrated_count}...")

        conn_target.commit()
        print(f"Migration complete. Migrated: {migrated_count}, Skipped (duplicate ID): {skipped_count}")

    except Exception as e:
        conn_target.rollback()
        print(f"Error during migration: {e}")
        import traceback
        traceback.print_exc()
    finally:
        conn_legacy.close()
        conn_target.close()

if __name__ == "__main__":
    migrate()
