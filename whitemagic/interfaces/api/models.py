"""SQLAlchemy models for API database."""
# mypy: disable-error-code="no-redef,assignment,misc"

import uuid
from datetime import datetime

try:
    from sqlalchemy import (
        Boolean,
        Column,
        DateTime,
        Float,
        ForeignKey,
        Integer,
        String,
        Text,
    )
    from sqlalchemy.ext.asyncio import AsyncAttrs
    from sqlalchemy.orm import DeclarativeBase, relationship
    HAS_SQLALCHEMY = True

    class Base(DeclarativeBase):
        """Base class for all models."""

        pass

except ImportError:
    HAS_SQLALCHEMY = False
    # Stub classes for when SQLAlchemy not available
    class DeclarativeBase:
        pass
    class AsyncAttrs:
        pass
    class Base:
        pass
    Column = Integer = String = DateTime = Boolean = ForeignKey = Float = Text = relationship = None

if HAS_SQLALCHEMY:
    class User(Base):
        __tablename__ = "users"

        id = Column(Integer, primary_key=True)
        username = Column(String(255), unique=True, nullable=False, index=True)
        email = Column(String(255), unique=True, nullable=False, index=True)
        created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
        updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
        is_active = Column(Boolean, default=True, nullable=False)

        # Relationships
        api_keys = relationship("APIKey", back_populates="user", cascade="all, delete-orphan")
        quotas = relationship("Quota", back_populates="user", cascade="all, delete-orphan")
        usage_records = relationship("UsageRecord", back_populates="user")

    class APIKey(Base):
        __tablename__ = "api_keys"

        id = Column(Integer, primary_key=True)
        user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)
        key_hash = Column(String(64), unique=True, nullable=False, index=True)
        name = Column(String(255), nullable=True)
        created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
        last_used = Column(DateTime, nullable=True)
        revoked = Column(Boolean, default=False, nullable=False, index=True)
        revoked_at = Column(DateTime, nullable=True)

        # Relationships
        user = relationship("User", back_populates="api_keys")

    class Quota(Base):
        __tablename__ = "quotas"

        id = Column(Integer, primary_key=True)
        user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)
        plan = Column(String(50), default="free", nullable=False)
        requests_limit = Column(Integer, default=1000, nullable=False)
        requests_used = Column(Integer, default=0, nullable=False)
        tokens_limit = Column(Integer, default=100000, nullable=False)
        tokens_used = Column(Integer, default=0, nullable=False)
        reset_at = Column(DateTime, nullable=False)

        # Relationships
        user = relationship("User", back_populates="quotas")

    class UsageRecord(Base):
        __tablename__ = "usage_records"

        id = Column(Integer, primary_key=True)
        user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)
        endpoint = Column(String(255), nullable=False)
        method = Column(String(10), nullable=False)
        status_code = Column(Integer, nullable=False)
        response_time_ms = Column(Float, nullable=False)
        tokens_used = Column(Integer, default=0)
        created_at = Column(DateTime, default=datetime.utcnow, nullable=False, index=True)

        # Relationships
        user = relationship("User", back_populates="usage_records")

    class Memory(Base):
        __tablename__ = "memories"

        id = Column(String(36), primary_key=True, default=lambda: str(uuid.uuid4()))
        title = Column(String(255), nullable=True)
        content = Column(Text, nullable=True)
        format = Column(String(50), default="markdown")
        path = Column(String(512), nullable=True, index=True, unique=True)
        checksum = Column(String(64), nullable=True)

        # Metadata
        importance = Column(Float, default=0.5)
        resonance = Column(Float, default=0.0)
        tags = Column(Text, default="[]") # JSON string
        embedding = Column(Text, nullable=True) # JSON string or Blob

        created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
        updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow, nullable=False)

    class MemoryLink(Base):
        __tablename__ = "memory_links"

        id = Column(String(36), primary_key=True, default=lambda: str(uuid.uuid4()))
        type = Column(String(50), nullable=False)
        weight = Column(Float, default=1.0)

        source_id = Column(String(36), ForeignKey("memories.id"), nullable=False)
        target_id = Column(String(36), ForeignKey("memories.id"), nullable=False)

        created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
        updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow, nullable=False)

        source = relationship("Memory", foreign_keys=[source_id], back_populates="links_outgoing")
        target = relationship("Memory", foreign_keys=[target_id], back_populates="links_incoming")

    # Add back_populates to Memory after defining MemoryLink
    Memory.links_outgoing = relationship("MemoryLink", foreign_keys=[MemoryLink.source_id], back_populates="source")
    Memory.links_incoming = relationship("MemoryLink", foreign_keys=[MemoryLink.target_id], back_populates="target")

else:
    # Fallback to simple dataclasses when SQLAlchemy not available
    from dataclasses import dataclass

    @dataclass
    class User:
        id: int = 0
        username: str = ""
        email: str = ""
        is_active: bool = True

    @dataclass
    class APIKey:
        id: int = 0
        user_id: int = 0
        key_hash: str = ""
        name: str | None = None
        revoked: bool = False

    @dataclass
    class Quota:
        id: int = 0
        user_id: int = 0
        plan: str = "free"
        requests_limit: int = 1000
        requests_used: int = 0

    @dataclass
    class UsageRecord:
        id: int = 0
        user_id: int = 0
        endpoint: str = ""
        method: str = ""
        status_code: int = 200

    @dataclass
    class Memory:
        id: str = ""
        title: str = ""
        content: str = ""

    @dataclass
    class MemoryLink:
        id: str = ""
        source_id: str = ""
        target_id: str = ""
