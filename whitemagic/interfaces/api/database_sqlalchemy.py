"""SQLAlchemy Database Layer for WhiteMagic API.

Production-ready async database implementation using SQLAlchemy 2.0.
Falls back to in-memory stub if database URL not configured.

Usage:
    from whitemagic.interfaces.api.database_sqlalchemy import get_db_session, User

    async with get_db_session() as session:
        result = await session.execute(select(User).where(User.email == email))
        user = result.scalar_one_or_none()
"""

from __future__ import annotations

import os
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from datetime import datetime
from typing import Any

try:
    from sqlalchemy import Boolean, DateTime, Integer, String
    from sqlalchemy.ext.asyncio import (
        AsyncSession,
        async_sessionmaker,
        create_async_engine,
    )
    from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column
    SQLALCHEMY_AVAILABLE = True
except ImportError:
    SQLALCHEMY_AVAILABLE = False


# SQLAlchemy Models (if available)
if SQLALCHEMY_AVAILABLE:
    class Base(DeclarativeBase):
        """Base model for all database tables."""

        pass

    class User(Base):
        __tablename__ = "users"

        id: Mapped[int] = mapped_column(Integer, primary_key=True)
        username: Mapped[str] = mapped_column(String(255), unique=True, nullable=False)
        email: Mapped[str] = mapped_column(String(255), unique=True, nullable=False)
        plan_tier: Mapped[str] = mapped_column(String(50), default="free")
        is_active: Mapped[bool] = mapped_column(Boolean, default=True)
        created_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)

    class APIKey(Base):
        __tablename__ = "api_keys"

        id: Mapped[int] = mapped_column(Integer, primary_key=True)
        key_hash: Mapped[str] = mapped_column(String(255), unique=True, nullable=False)
        user_id: Mapped[int] = mapped_column(Integer, nullable=False)
        name: Mapped[str] = mapped_column(String(255))
        is_active: Mapped[bool] = mapped_column(Boolean, default=True)
        created_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)
        last_used: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)

    class Quota(Base):
        __tablename__ = "quotas"

        id: Mapped[int] = mapped_column(Integer, primary_key=True)
        user_id: Mapped[int] = mapped_column(Integer, unique=True, nullable=False)
        limit: Mapped[int] = mapped_column(Integer, default=1000)
        used: Mapped[int] = mapped_column(Integer, default=0)
        reset_at: Mapped[datetime] = mapped_column(DateTime)

    class UsageRecord(Base):
        __tablename__ = "usage_records"

        id: Mapped[int] = mapped_column(Integer, primary_key=True)
        user_id: Mapped[int] = mapped_column(Integer, nullable=False)
        api_key_id: Mapped[int] = mapped_column(Integer, nullable=False)
        endpoint: Mapped[str] = mapped_column(String(255))
        tokens: Mapped[int] = mapped_column(Integer, default=0)
        timestamp: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)


# Database Engine Setup
_engine: Any = None
_async_session_maker: Any = None

def get_database_url() -> str | None:
    """Get database URL from environment or config."""
    # Check environment variable
    url = os.getenv("WHITEMAGIC_DATABASE_URL")
    if url:
        return url

    # Check config file
    try:
        from whitemagic.config import get_config
        config = get_config()
        if isinstance(config, dict):
            database_cfg = config.get("database", {})
            if isinstance(database_cfg, dict):
                url = database_cfg.get("url")
                if url is None:
                    return None
                return str(url)
    except Exception:
        pass

    return None


def initialize_engine() -> bool:
    """Initialize SQLAlchemy async engine."""
    global _engine, _async_session_maker

    if not SQLALCHEMY_AVAILABLE:
        return False

    db_url = get_database_url()
    if not db_url:
        return False

    _engine = create_async_engine(
        db_url,
        echo=os.getenv("SQLALCHEMY_ECHO", "false").lower() == "true",
        pool_pre_ping=True,
    )

    _async_session_maker = async_sessionmaker(
        _engine,
        class_=AsyncSession,
        expire_on_commit=False,
    )

    return True


async def create_tables() -> None:
    """Create all tables (for development/testing)."""
    if not SQLALCHEMY_AVAILABLE or not _engine:
        return

    async with _engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)


@asynccontextmanager
async def get_db_session() -> AsyncIterator[Any]:
    """Get async database session.

    Falls back to in-memory stub if SQLAlchemy not configured.

    Usage:
        async with get_db_session() as session:
            result = await session.execute(select(User).where(...))
            user = result.scalar_one_or_none()
    """
    # Try to initialize engine if not already done
    if _async_session_maker is None:
        initialize_engine()

    # Use real SQLAlchemy session if available
    if _async_session_maker is not None:
        async with _async_session_maker() as session:
            try:
                yield session
                await session.commit()
            except Exception:
                await session.rollback()
                raise
    else:
        # Fallback to in-memory stub
        from whitemagic.interfaces.api.database import Database
        db = Database()
        async with db.get_session() as session:
            yield session


# Convenience exports
__all__ = [
    "get_db_session",
    "create_tables",
    "initialize_engine",
    "User",
    "APIKey",
    "Quota",
    "UsageRecord",
    "SQLALCHEMY_AVAILABLE",
]
