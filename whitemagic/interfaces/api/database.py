"""Database models for API."""

from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from typing import Any


class Base:
    """Base model with common functionality."""

    def to_dict(self) -> dict:
        """Convert model to dictionary."""
        return {k: v for k, v in self.__dict__.items() if not k.startswith("_")}

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.to_dict()})"

class User(Base):
    """User model."""

    def __init__(self, username: str = "", email: str = "") -> None:
        self.username = username
        self.email = email
        from datetime import datetime
        self.created_at = datetime.now()

class APIKey(Base):
    """API key model."""

    def __init__(self, key: str = "", user_id: int = 0) -> None:
        self.key = key
        self.user_id = user_id
        from datetime import datetime
        self.created_at = datetime.now()

class Quota(Base):
    """Quota model."""

    def __init__(self, user_id: int = 0, limit: int = 1000) -> None:
        self.user_id = user_id
        self.limit = limit
        self.used = 0

class UsageRecord(Base):
    """Usage record."""

    def __init__(self, user_id: int = 0, tokens: int = 0) -> None:
        self.user_id = user_id
        self.tokens = tokens
        from datetime import datetime
        self.timestamp = datetime.now()

class Database:
    """Database connection (in-memory stub for development)."""

    def __init__(self) -> None:
        self.users: list[User] = []
        self.api_keys: list[APIKey] = []
        self.quotas: list[Quota] = []
        self.usage_records: list[UsageRecord] = []

    @asynccontextmanager
    async def get_session(self) -> AsyncIterator["Database"]:
        """Async context manager for database session (in-memory stub).

        For production SQLAlchemy implementation, use:
            from whitemagic.interfaces.api.database_sqlalchemy import get_db_session
            async with get_db_session() as session:
                # Real async PostgreSQL/SQLite session
                pass
        """
        yield self

    def add_user(self, user: User) -> None:
        self.users.append(user)

    def get_user(self, username: str) -> User | None:
        for user in self.users:
            if user.username == username:
                return user
        return None

    async def execute(self, query: Any) -> Any:
        """Stub for SQLAlchemy execute - returns empty result."""
        class StubResult:
            def scalars(self) -> "StubResult":
                return self
            def all(self) -> list:
                return []
            def first(self) -> None:
                return None
            def scalar_one_or_none(self) -> None:
                return None
            def one_or_none(self) -> None:
                return None
        return StubResult()

    def add(self, obj: Any) -> None:
        """Stub for SQLAlchemy session.add() - stores in memory."""
        if isinstance(obj, User):
            self.users.append(obj)
        elif isinstance(obj, APIKey):
            self.api_keys.append(obj)
        elif isinstance(obj, Quota):
            self.quotas.append(obj)
        elif isinstance(obj, UsageRecord):
            self.usage_records.append(obj)

    async def commit(self) -> None:
        """Stub for SQLAlchemy session.commit() - no-op for in-memory."""
        pass

    async def refresh(self, obj: Any) -> None:
        """Stub for SQLAlchemy session.refresh() - no-op."""
        pass

    async def flush(self) -> None:
        """Stub for SQLAlchemy session.flush() - no-op."""
        pass

    async def scalar_one_or_none(self) -> None:
        """Stub for direct session.scalar_one_or_none() calls."""
        return None

    async def delete(self, obj: Any) -> None:
        """Stub for SQLAlchemy session.delete()."""
        pass
