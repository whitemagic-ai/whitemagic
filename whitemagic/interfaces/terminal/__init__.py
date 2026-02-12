"""WhiteMagic Terminal Tool - Structured execution for agents."""

from .allowlist import Allowlist, Profile
from .audit import AuditLog, AuditLogger
from .executor import ExecutionResult, Executor
from .mcp_tools import TOOLS, TerminalMCPTools
from .models import ExecutionMode, ExecutionRequest, ExecutionResponse

__all__ = [
    "Executor",
    "ExecutionResult",
    "Allowlist",
    "Profile",
    "AuditLogger",
    "AuditLog",
    "TerminalMCPTools",
    "TOOLS",
    "ExecutionMode",
    "ExecutionRequest",
    "ExecutionResponse",
]

__version__ = "2.6.5"
