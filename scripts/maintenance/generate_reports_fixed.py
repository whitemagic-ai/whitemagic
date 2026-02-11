
import sqlite3
import json
from pathlib import Path
from collections import defaultdict
import datetime

# Setup paths
ROOT_DIR = Path(".").resolve()
PROJECT_MEM_DIR = ROOT_DIR / "staging" / "project_memory"
DB_PATH = PROJECT_MEM_DIR / "whitemagic.db"
REPORTS_DIR = ROOT_DIR / "reports"

def generate_reports():
    print("📊 Generating Intelligence Reports (Direct SQL)...")
    
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    
    # Fetch all memories
    rows = cursor.execute("SELECT title, memory_type, created_at, metadata FROM memories ORDER BY created_at DESC").fetchall()
    
    print(f"   Retrieved {len(rows)} artifacts.")
    
    if not rows:
        print("⚠️ No memories found.")
        conn.close()
        return

    # --- 1. Artifact Categorization ---
    print("\n🗂️  Building Categorization Report...")
    by_tag = defaultdict(list)
    by_type = defaultdict(list)
    
    for row in rows:
        title = row['title'] or 'Untitled'
        m_type = row['memory_type'] or 'unknown'
        metadata_json = row['metadata']
        tags = []
        if metadata_json:
            try:
                meta = json.loads(metadata_json)
                tags = meta.get('tags', [])
            except:
                pass
        
        by_type[m_type].append(title)
        
        if not tags:
            by_tag['untagged'].append(title)
        else:
            for t in tags:
                by_tag[t].append(title)
                
    cat_report = "# Artifact Categorization Report\n\n"
    cat_report += f"**Total Artifacts**: {len(rows)}\n\n"
    
    cat_report += "## By Memory Type\n"
    for m_type, items in sorted(by_type.items(), key=lambda x: len(x[1]), reverse=True):
        cat_report += f"- **{m_type}**: {len(items)} artifacts\n"
        
    cat_report += "\n## TOP 20 Tag Categories\n"
    sorted_tags = sorted(by_tag.items(), key=lambda x: len(x[1]), reverse=True)
    for tag, items in sorted_tags[:20]:
        cat_report += f"### #{tag} ({len(items)})\n"
        for item in items[:5]:
            cat_report += f"- {item}\n"
        if len(items) > 5:
            cat_report += f"- ... and {len(items)-5} more\n"
        cat_report += "\n"
        
    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    (REPORTS_DIR / "categorization.md").write_text(cat_report)
    print("   ✅ Created reports/categorization.md")

    # --- 2. Chronological Timeline ---
    print("\n⏳ Building Timeline Report...")
    
    timeline_report = "# Chronological Timeline\n\n"
    timeline_report += "Reconstructed from artifact timestamps.\n\n"
    timeline_report += "## Order of Recovery (Ingestion Sequence)\n"
    
    # Rows are already ordered by DESC created_at
    for i, row in enumerate(rows[:100]):
        title = row['title'] or 'Untitled'
        metadata_json = row['metadata']
        try:
             meta = json.loads(metadata_json)
             tags_list = meta.get('tags', [])
             tags_str = ", ".join(tags_list[:3])
        except:
             tags_str = ""
             
        timeline_report += f"{i+1}. **{title}** ({tags_str})\n"
        
    (REPORTS_DIR / "timeline.md").write_text(timeline_report)
    print("   ✅ Created reports/timeline.md")
    
    conn.close()

if __name__ == "__main__":
    generate_reports()
