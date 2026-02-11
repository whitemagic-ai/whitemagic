
from typing import Any
import logging
import sqlite3
from datetime import datetime
from pathlib import Path

logger = logging.getLogger(__name__)

class ConfidenceLearner:
    """Confidence Learner — Reflexive Self-Improvement
    Tracks the success and failure of system actions to refine
    future resonance scores.
    """

    def __init__(self, db_path: Path) -> None:
        self.db_path = db_path
        self._init_db()

    def _init_db(self) -> Any:
        """Ensure the confidence table exists."""
        conn = sqlite3.connect(str(self.db_path))
        conn.execute("""
            CREATE TABLE IF NOT EXISTS cluster_confidence (
                cluster_key TEXT PRIMARY KEY,
                success_count INTEGER DEFAULT 0,
                failure_count INTEGER DEFAULT 0,
                last_updated TEXT
            )
        """)
        conn.commit()
        conn.close()

    def report_outcome(self, cluster_key: str, success: bool) -> Any:
        """Update confidence based on execution results."""
        conn = sqlite3.connect(str(self.db_path))
        now = datetime.now().isoformat()

        if success:
            conn.execute("""
                INSERT INTO cluster_confidence (cluster_key, success_count, last_updated)
                VALUES (?, 1, ?)
                ON CONFLICT(cluster_key) DO UPDATE SET
                    success_count = success_count + 1,
                    last_updated = excluded.last_updated
            """, (cluster_key, now))
        else:
            conn.execute("""
                INSERT INTO cluster_confidence (cluster_key, failure_count, last_updated)
                VALUES (?, 1, ?)
                ON CONFLICT(cluster_key) DO UPDATE SET
                    failure_count = failure_count + 1,
                    last_updated = excluded.last_updated
            """, (cluster_key, now))

        conn.commit()
        conn.close()
        logger.info(f"Confidence updated for {cluster_key}: {'SUCCESS' if success else 'FAILURE'}")

    def get_score(self, cluster_key: str) -> float:
        """Calculate a confidence multiplier (0.5 to 1.5)."""
        conn = sqlite3.connect(str(self.db_path))
        row = conn.execute("SELECT success_count, failure_count FROM cluster_confidence WHERE cluster_key = ?", (cluster_key,)).fetchone()
        conn.close()

        if not row:
            return 1.0 # Neutral starting point

        success = float(row[0])
        failure = float(row[1])
        total = success + failure
        if total == 0:
            return 1.0

        # Simple ratio with a baseline
        win_rate = success / total
        # Map 0.0-1.0 to 0.5-1.5
        return float(0.5 + win_rate)
