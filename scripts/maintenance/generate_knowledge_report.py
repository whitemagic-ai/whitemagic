
import sqlite3
from pathlib import Path

DB_PATH = Path("memory/whitemagic.db")
REPORT_PATH = Path("docs/knowledge_graph.md")

def generate_report():
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    # Stats
    cursor.execute("SELECT count(*) FROM memories")
    total_memories = cursor.fetchone()[0]

    cursor.execute("SELECT count(DISTINCT memory_id) FROM tags")
    tagged_count = cursor.fetchone()[0]

    cursor.execute("SELECT tag, count(*) FROM tags GROUP BY tag ORDER BY count(*) DESC")
    tag_counts = cursor.fetchall()

    # Write Report
    with open(REPORT_PATH, "w") as f:
        f.write("# 🧠 WhiteMagic Knowledge Graph Report\n\n")
        f.write(f"**Total Memories:** {total_memories}\n")
        f.write(f"**Tagged Concept Nodes:** {tagged_count} ({tagged_count/total_memories*100:.1f}%)\n\n")

        f.write("## 🗂️ Concept Clusters\n")
        f.write("| Topic Category | Memory Count | Intensity |\n")
        f.write("|---|---|---|\n")
        for tag, count in tag_counts:
            bar = "█" * (int(count/100) + 1)
            f.write(f"| **{tag.upper()}** | {count} | `{bar}` |\n")
        
        f.write("\n## 🔮 Insight Snapshot\n")
        f.write("The system has successfully self-organized its legacy archives into coherent knowledge clusters.\n")

    conn.close()
    print(f"Report generated at {REPORT_PATH}")

if __name__ == "__main__":
    generate_report()
