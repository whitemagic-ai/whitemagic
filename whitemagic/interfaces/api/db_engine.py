"""Database engine and session management."""

from __future__ import annotations

import os
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from typing import Any, cast

try:
    from sqlalchemy.ext.asyncio import (
        AsyncEngine,
        AsyncSession,
        async_sessionmaker,
        create_async_engine,
    )

    from .models import Base
    HAS_SQLALCHEMY = True
except ImportError:
    HAS_SQLALCHEMY = False
    AsyncEngine = cast("Any", None)  # type: ignore[misc]
    AsyncSession = cast("Any", None)  # type: ignore[misc]
    async_sessionmaker = cast("Any", None)  # type: ignore[misc]
    create_async_engine = cast("Any", None)
    Base = cast("Any", None)  # type: ignore[misc]

# Global engine instance
_engine: Any = None
_session_maker: Any = None

def get_database_url() -> str:
    """Get database URL from environment or use default."""
    from whitemagic.config.paths import DB_PATH

    default_url = f"sqlite+aiosqlite:///{DB_PATH}"
    return os.getenv("DATABASE_URL", default_url)

def init_engine(url: str | None = None) -> AsyncEngine | None:
    """Initialize database engine."""
    global _engine, _session_maker

    if not HAS_SQLALCHEMY:
        return None

    if _engine is None:
        db_url = url or get_database_url()
        _engine = create_async_engine(
            db_url,
            echo=False,
            pool_pre_ping=True,
            pool_recycle=3600,
            pool_size=20,
            max_overflow=10,
        )
        _session_maker = async_sessionmaker(
            _engine,
            class_=AsyncSession,
            expire_on_commit=False,
        )

    if _engine is None:
        return None
    return cast("AsyncEngine", _engine)

async def create_tables() -> None:
    """Create all tables."""
    if not HAS_SQLALCHEMY or _engine is None:
        return

    async with _engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)

async def drop_tables() -> None:
    """Drop all tables - use with caution!"""
    if not HAS_SQLALCHEMY or _engine is None:
        return

    async with _engine.begin() as conn:
        await conn.run_sync(Base.metadata.drop_all)

@asynccontextmanager
async def get_async_session() -> AsyncIterator[Any]:
    """Get async database session."""
    if not HAS_SQLALCHEMY or _session_maker is None:
        # Fallback to stub
        from .database import Database
        db = Database()
        async with db.get_session() as session:
            yield session
        return

    async with _session_maker() as session:
        try:
            yield session
            await session.commit()
        except Exception:
            await session.rollback()
            raise
        finally:
            await session.close()

async def dispose_engine() -> None:
    """Dispose of engine connections."""
    global _engine
    if _engine is not None:
        await _engine.dispose()
        _engine = None
