"""Extended models for exec endpoint - audit logging and user allowlists."""
# mypy: ignore-errors

from datetime import datetime

try:
    from sqlalchemy import Column, DateTime, Float, ForeignKey, Integer, String, Text
    from sqlalchemy.orm import relationship

    from .models import Base
    HAS_SQLALCHEMY = True
except ImportError:
    HAS_SQLALCHEMY = False
    Base = None
    Column = Integer = String = DateTime = ForeignKey = Text = Float = relationship = None

if HAS_SQLALCHEMY:
    class TerminalAuditLog(Base):
        """Audit log for all terminal executions."""

        __tablename__ = "terminal_audit_logs"

        id = Column(Integer, primary_key=True)
        user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)
        run_id = Column(String(36), unique=True, nullable=False, index=True)
        command = Column(String(255), nullable=False)
        args = Column(Text, nullable=True)  # JSON array
        mode = Column(String(10), nullable=False)  # read/write
        cwd = Column(String(500), nullable=True)
        exit_code = Column(Integer, nullable=True)
        duration_ms = Column(Float, nullable=True)
        approved = Column(Integer, default=0)  # 0=rejected, 1=approved
        approved_by = Column(String(100), nullable=True)  # user/auto/admin
        created_at = Column(DateTime, default=datetime.utcnow, nullable=False, index=True)

        user = relationship("User")

    class UserAllowlist(Base):
        """Per-user command allowlists."""

        __tablename__ = "user_allowlists"

        id = Column(Integer, primary_key=True)
        user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)
        command_pattern = Column(String(255), nullable=False)
        mode = Column(String(10), nullable=False)  # read/write
        enabled = Column(Integer, default=1)
        created_at = Column(DateTime, default=datetime.utcnow)
        created_by = Column(String(100), nullable=True)  # admin username

        user = relationship("User")
else:
    from dataclasses import dataclass

    @dataclass
    class TerminalAuditLog:
        id: int = 0
        user_id: int = 0
        run_id: str = ""
        command: str = ""
        mode: str = "read"
        exit_code: int | None = None

    @dataclass
    class UserAllowlist:
        id: int = 0
        user_id: int = 0
        command_pattern: str = ""
        mode: str = "read"
        enabled: int = 1
