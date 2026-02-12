"""Migration Runner - Executes database migrations in order.
"""

import hashlib
import importlib
import logging
import sqlite3
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path

from .schema import (
    DEFAULT_DB_PATH,
    ensure_version_table,
    get_db_connection,
    get_schema_version,
    record_migration,
    remove_migration,
)

logger = logging.getLogger(__name__)


@dataclass
class Migration(ABC):
    """Base class for migrations."""

    version: int
    description: str

    @abstractmethod
    def upgrade(self, conn: sqlite3.Connection) -> None:
        """Apply the migration."""
        pass

    @abstractmethod
    def downgrade(self, conn: sqlite3.Connection) -> None:
        """Rollback the migration."""
        pass

    def get_checksum(self) -> str:
        """Generate checksum for migration validation."""
        import inspect
        source = inspect.getsource(self.upgrade)
        return hashlib.md5(source.encode()).hexdigest()[:8]


class MigrationRunner:
    """Runs migrations in order."""

    def __init__(self, db_path: Path | None = None):
        self.db_path = db_path or DEFAULT_DB_PATH
        self._migrations: list[Migration] = []
        self._load_migrations()

    def _load_migrations(self) -> None:
        """Load all migration modules from versions directory."""
        versions_dir = Path(__file__).parent / "versions"
        if not versions_dir.exists():
            versions_dir.mkdir(parents=True, exist_ok=True)
            return

        for file in sorted(versions_dir.glob("*.py")):
            if file.name.startswith("_"):
                continue

            # Import the module
            module_name = f"whitemagic.core.memory.migrations.versions.{file.stem}"
            try:
                module = importlib.import_module(module_name)
                if hasattr(module, "migration"):
                    self._migrations.append(module.migration)
                    logger.debug(f"Loaded migration: {file.stem}")
            except Exception as e:
                logger.error(f"Failed to load migration {file.stem}: {e}")

        # Sort by version
        self._migrations.sort(key=lambda m: m.version)

    def get_pending(self) -> list[Migration]:
        """Get list of migrations that haven't been applied."""
        with get_db_connection(self.db_path) as conn:
            current = get_schema_version(conn)
        return [m for m in self._migrations if m.version > current]

    def status(self) -> dict:
        """Get migration status."""
        with get_db_connection(self.db_path) as conn:
            current = get_schema_version(conn)

        pending = self.get_pending()
        return {
            "current_version": current,
            "latest_version": self._migrations[-1].version if self._migrations else 0,
            "pending_count": len(pending),
            "pending": [{"version": m.version, "description": m.description} for m in pending],
        }

    def upgrade(self, target_version: int | None = None) -> dict:
        """Apply pending migrations up to target version.

        Args:
            target_version: Stop at this version (default: apply all)

        Returns:
            Summary of applied migrations

        """
        applied = []
        errors = []

        conn = get_db_connection(self.db_path)
        try:
            ensure_version_table(conn)
            current = get_schema_version(conn)

            # Start explicit transaction
            conn.execute("BEGIN")

            for migration in self._migrations:
                if migration.version <= current:
                    continue

                if target_version and migration.version > target_version:
                    break

                logger.debug(f"Applying migration v{migration.version}: {migration.description}")

                # Apply migration
                migration.upgrade(conn)

                # Record it (commit=False because we handle it here)
                record_migration(
                    conn,
                    migration.version,
                    migration.description,
                    migration.get_checksum(),
                    commit=False,
                )

                applied.append({
                    "version": migration.version,
                    "description": migration.description,
                })

            conn.commit()

        except Exception as e:
            conn.rollback()
            logger.error(f"Migration batch failed: {e}")
            errors.append({
                "version": migration.version if "migration" in locals() else None,
                "error": str(e),
            })
        finally:
            conn.close()

        return {
            "applied": applied,
            "errors": errors,
            "new_version": get_schema_version(get_db_connection(self.db_path)),
        }

    def rollback(self, steps: int = 1) -> dict:
        """Rollback migrations.

        Args:
            steps: Number of migrations to rollback

        Returns:
            Summary of rolled back migrations

        """
        rolled_back = []
        errors = []

        conn = get_db_connection(self.db_path)
        try:
            current = get_schema_version(conn)

            # Start explicit transaction
            conn.execute("BEGIN")

            # Find migrations to rollback (in reverse order)
            to_rollback = [m for m in reversed(self._migrations) if m.version <= current][:steps]

            for migration in to_rollback:
                logger.debug(f"Rolling back v{migration.version}: {migration.description}")

                # Rollback migration
                migration.downgrade(conn)

                # Remove record (commit=False because we handle it here)
                remove_migration(conn, migration.version, commit=False)

                rolled_back.append({
                    "version": migration.version,
                    "description": migration.description,
                })

            conn.commit()

        except Exception as e:
            conn.rollback()
            logger.error(f"Rollback batch failed: {e}")
            errors.append({
                "version": migration.version if "migration" in locals() else None,
                "error": str(e),
            })
        finally:
            conn.close()

        return {
            "rolled_back": rolled_back,
            "errors": errors,
            "new_version": get_schema_version(get_db_connection(self.db_path)),
        }

    def history(self) -> list[dict]:
        """Get migration history from database."""
        from .schema import get_migration_history
        return get_migration_history(self.db_path)
