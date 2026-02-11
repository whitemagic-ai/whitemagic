"""Schema version tracking and utilities.
"""

import logging
import sqlite3
from pathlib import Path

from whitemagic.config.paths import DB_PATH as DEFAULT_DB_PATH

logger = logging.getLogger(__name__)


def get_db_connection(db_path: Path | None = None) -> sqlite3.Connection:
    """Get a database connection."""
    path = db_path or DEFAULT_DB_PATH
    path.parent.mkdir(parents=True, exist_ok=True)
    return sqlite3.connect(path)


def ensure_version_table(conn: sqlite3.Connection) -> None:
    """Create version table if it doesn't exist."""
    conn.execute("""
        CREATE TABLE IF NOT EXISTS schema_version (
            version INTEGER PRIMARY KEY,
            applied_at TEXT NOT NULL,
            description TEXT,
            checksum TEXT
        )
    """)
    conn.commit()


def get_schema_version(conn: sqlite3.Connection) -> int:
    """Get current schema version from database."""
    ensure_version_table(conn)
    cursor = conn.execute("SELECT MAX(version) FROM schema_version")
    row = cursor.fetchone()
    if row is None or row[0] is None:
        return 0
    return int(row[0])


def get_current_version(db_path: Path | None = None) -> int:
    """Get current schema version (convenience function)."""
    with get_db_connection(db_path) as conn:
        return get_schema_version(conn)


def record_migration(
    conn: sqlite3.Connection,
    version: int,
    description: str,
    checksum: str | None = None,
    commit: bool = True,
) -> None:
    """Record a migration as applied."""
    from datetime import datetime
    conn.execute(
        """
        INSERT INTO schema_version (version, applied_at, description, checksum)
        VALUES (?, ?, ?, ?)
        """,
        (version, datetime.now().isoformat(), description, checksum),
    )
    if commit:
        conn.commit()
    logger.info(f"Recorded migration v{version}: {description}")


def remove_migration(conn: sqlite3.Connection, version: int, commit: bool = True) -> None:
    """Remove a migration record (for rollback)."""
    conn.execute("DELETE FROM schema_version WHERE version = ?", (version,))
    if commit:
        conn.commit()
    logger.info(f"Removed migration record v{version}")


def get_migration_history(db_path: Path | None = None) -> list:
    """Get full migration history."""
    with get_db_connection(db_path) as conn:
        ensure_version_table(conn)
        cursor = conn.execute(
            "SELECT version, applied_at, description FROM schema_version ORDER BY version",
        )
        return [
            {"version": row[0], "applied_at": row[1], "description": row[2]}
            for row in cursor.fetchall()
        ]
