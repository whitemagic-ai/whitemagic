
import unittest
import sys
import os
import sqlite3
import json
from datetime import datetime

# Path setup
sys.path.append(os.path.join(os.getcwd(), "staging/core_system"))

from whitemagic.gardens.air.agentic.terminal_scratchpad import TerminalScratchpad

class TestTerminalScratchpad(unittest.TestCase):
    
    def setUp(self):
        # Use in-memory DB for testing
        self.test_db = ":memory:"
        self._init_db()

    def _init_db(self):
        conn = sqlite3.connect(self.test_db)
        cursor = conn.cursor()
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS memories (
                id TEXT PRIMARY KEY,
                content TEXT,
                memory_type TEXT,
                created_at TEXT,
                metadata TEXT,
                title TEXT,
                neuro_score REAL,
                novelty_score REAL,
                importance REAL
            )
        """)
        conn.commit()
        conn.close()

    def test_recording_thoughts(self):
        """Test capturing thoughts of different types"""
        # Pass the special :memory: path handling logic would need injection in the real class
        # For this test, we accept that the real class might try to open a file path.
        # So we better mock the db connection or pass a real file path.
        # Let's use a temp file for safety.
        
        temp_db = "test_scratchpad.db"
        if os.path.exists(temp_db):
            os.remove(temp_db)
            
        # Init temp db locally
        conn = sqlite3.connect(temp_db)
        cursor = conn.cursor()
        cursor.execute("""
            CREATE TABLE memories (
                id TEXT PRIMARY KEY,
                content TEXT,
                memory_type TEXT,
                created_at TEXT,
                metadata TEXT,
                title TEXT,
                neuro_score REAL,
                novelty_score REAL,
                importance REAL
            )
        """)
        conn.commit()
        conn.close()
        
        try:
            with TerminalScratchpad("Test Task", db_path=temp_db) as pad:
                pad.think("Thinking...")
                pad.decide("Deciding...")
                pad.question("Questioning?")
                
                self.assertEqual(len(pad.thoughts), 3)
                self.assertEqual(pad.thoughts[0].type, "think")
                self.assertEqual(pad.thoughts[1].type, "decide")
                
            # Verify Persistence
            conn = sqlite3.connect(temp_db)
            cursor = conn.cursor()
            cursor.execute("SELECT content, memory_type FROM memories")
            result = cursor.fetchone()
            
            self.assertIsNotNone(result)
            self.assertEqual(result[1], "scratchpad_session")
            self.assertIn("Thinking...", result[0])
            self.assertIn("Questioning?", result[0])
            
        finally:
            if os.path.exists(temp_db):
                os.remove(temp_db)

    def test_error_handling(self):
        """Test that memory is saved even if crash occurs"""
        temp_db = "test_scratchpad_crash.db"
        # Init DB... (omitted for brevity, relying on auto-create if handled, 
        # but the class relies on existing table usually. Let's create it.)
        conn = sqlite3.connect(temp_db)
        cursor = conn.cursor()
        cursor.execute("CREATE TABLE memories (id TEXT, content TEXT, memory_type TEXT, created_at TEXT, metadata TEXT, title TEXT, neuro_score REAL, novelty_score REAL, importance REAL)")
        conn.commit()
        conn.close()

        try:
            with self.assertRaises(ValueError):
                with TerminalScratchpad("Crash Task", db_path=temp_db) as pad:
                    pad.think("About to crash")
                    raise ValueError("Intentional Crash")
            
            # Check if saved despite crash
            conn = sqlite3.connect(temp_db)
            cursor = conn.cursor()
            cursor.execute("SELECT content FROM memories")
            result = cursor.fetchone()
            self.assertIsNotNone(result)
            self.assertIn("About to crash", result[0])
            
        finally:
            if os.path.exists(temp_db):
                os.remove(temp_db)

if __name__ == '__main__':
    unittest.main()
