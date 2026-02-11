import sqlite3
import sys

def inspect(db_path):
    print(f"--- Inspecting {db_path} ---")
    try:
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        cursor.execute("SELECT type, name, sql FROM sqlite_master WHERE type='table';")
        tables = cursor.fetchall()
        for type, name, sql in tables:
            print(f"Table: {name}")
            print(f"SQL: {sql}\n")
        
        # Count rows
        for _, name, _ in tables:
            try:
                cursor.execute(f"SELECT COUNT(*) FROM {name}")
                count = cursor.fetchone()[0]
                print(f"Count for {name}: {count}")
            except:
                pass
        conn.close()
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    for path in sys.argv[1:]:
        inspect(path)
