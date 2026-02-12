"""Database Migration System for WhiteMagic.

Provides Alembic-style schema versioning for safe database upgrades.

Usage:
    from whitemagic.core.memory.migrations import MigrationRunner

    runner = MigrationRunner()
    runner.upgrade()  # Apply pending migrations
    runner.rollback()  # Rollback last migration
    runner.status()  # Show current version
"""

from .runner import Migration, MigrationRunner
from .schema import get_current_version, get_schema_version

__all__ = ["MigrationRunner", "Migration", "get_current_version", "get_schema_version"]
