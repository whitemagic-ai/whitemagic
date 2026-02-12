# mypy: disable-error-code=no-untyped-def

import logging
import sqlite3

logger = logging.getLogger(__name__)

class ResonanceLedger:
    """Resonance Ledger — The Financials of the Soul.
    Tracks the distribution of 'Symbolic TAO' (Resonance Value).
    """

    def __init__(self, db_path: str):
        self.db_path = db_path
        self._initialize_db()

    def _initialize_db(self):
        """Create the resonance ledger table if it doesn't exist."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS resonance_ledger (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    subnet_id TEXT NOT NULL,
                    amount REAL NOT NULL,
                    reason TEXT,
                    source_agent TEXT,
                    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
                )
            """)
            conn.execute("CREATE INDEX IF NOT EXISTS idx_subnet_id ON resonance_ledger(subnet_id)")
            conn.commit()

    def add_resonance(self, subnet_id: str, amount: float, reason: str, source: str = "system"):
        """Record a resonance increase/decrease for a subnet."""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute(
                "INSERT INTO resonance_ledger (subnet_id, amount, reason, source_agent) VALUES (?, ?, ?, ?)",
                (subnet_id, amount, reason, source),
            )
            conn.commit()
            logger.info(f"🪙 Ledger: {subnet_id} granted {amount:+.2f} resonance ({reason})")

    def get_total_resonance(self, subnet_id: str) -> float:
        """Calculate the cumulative resonance for a subnet."""
        with sqlite3.connect(self.db_path) as conn:
            row = conn.execute(
                "SELECT SUM(amount) FROM resonance_ledger WHERE subnet_id = ?",
                (subnet_id,),
            ).fetchone()
            return row[0] or 1.0 # Base resonance is 1.0

    def get_all_balances(self) -> dict[str, float]:
        """Return resonance balances for all subnets."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.execute("SELECT subnet_id, SUM(amount) FROM resonance_ledger GROUP BY subnet_id")
            return {row[0]: (row[1] or 1.0) for row in cursor.fetchall()}

_ledger: ResonanceLedger | None = None

def get_resonance_ledger() -> ResonanceLedger:
    global _ledger
    if _ledger is None:
        from whitemagic.config.paths import DB_PATH

        # Use the canonical WM_STATE_ROOT database.
        _ledger = ResonanceLedger(str(DB_PATH))
    return _ledger
