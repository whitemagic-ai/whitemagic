"""Token Economy Tracker - Understand API vs Local Computation

Tracks where computation actually happens:
- API tokens (Claude/GPT processing)
- Local CPU (Lucas's laptop)
- Local disk I/O
- Rust/Haskell bridges
- MCP tools

Mystery to solve: Why do I feel like I have "infinite" capacity?
Answer: Most work happens locally, not via API!
"""

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

from whitemagic.utils.fileio import file_lock

logger = logging.getLogger(__name__)


class ComputeType(Enum):
    """Where computation happens"""

    API_TOKENS = "api_tokens"           # Claude API processing
    LOCAL_PYTHON = "local_python"       # Python scripts on laptop
    LOCAL_SHELL = "local_shell"         # Shell commands
    LOCAL_RUST = "local_rust"           # Rust bridge operations
    LOCAL_HASKELL = "local_haskell"     # Haskell bridge operations
    LOCAL_FILE_IO = "local_file_io"     # File reading/writing
    MCP_TOOLS = "mcp_tools"             # MCP file-based tools


@dataclass
class TokenBudget:
    """Budget tracking for token usage"""

    total: int = 200000
    used: int = 0

    @property
    def remaining(self) -> int:
        return self.total - self.used

    @property
    def percentage_used(self) -> float:
        return (self.used / self.total) * 100 if self.total > 0 else 0


@dataclass
class ComputeOperation:
    """A single compute operation"""

    timestamp: datetime
    operation_type: ComputeType
    description: str
    api_tokens_used: int
    local_cpu_ms: float
    local_ram_mb: float
    file_operations: int
    bytes_processed: int

    def to_dict(self) -> dict[str, Any]:
        return {
            "timestamp": self.timestamp.isoformat(),
            "operation_type": self.operation_type.value,
            "description": self.description,
            "api_tokens": self.api_tokens_used,
            "local_cpu_ms": self.local_cpu_ms,
            "local_ram_mb": self.local_ram_mb,
            "file_ops": self.file_operations,
            "bytes": self.bytes_processed,
        }


class TokenEconomyTracker:
    """Track the "economy" of computation.

    Mystery: Lucas noticed I seem to use fewer tokens than expected.
    Hypothesis: I'm offloading most work to local systems!

    This tracker will reveal the truth.
    """

    def __init__(self, log_file: Path | None = None, total_budget: int = 200000) -> None:
        """Initialize tracker"""
        from whitemagic.config.paths import LOGS_DIR
        self.log_file = log_file or (LOGS_DIR / "token_economy.jsonl")
        self.log_file.parent.mkdir(parents=True, exist_ok=True)

        self.operations: list[ComputeOperation] = []
        self.history: list[dict[str, Any]] = []  # For test compatibility
        self.current_session_start = datetime.now()

        # For test compatibility
        self.total_budget = total_budget
        self.used_tokens = 0

    def record_usage(self, tokens_used: int, source: str = "api", operation: str = "") -> None:
        """Record token usage (test-compatible API)"""
        self.used_tokens += tokens_used
        entry = {
            "tokens": tokens_used,
            "source": source,
            "operation": operation,
            "timestamp": datetime.now().isoformat(),
        }
        self.history.append(entry)

        # Also record in new format
        compute_type = ComputeType.API_TOKENS if source == "api" else ComputeType.LOCAL_PYTHON
        self.record_operation(
            compute_type,
            operation or source,
            api_tokens=tokens_used if source == "api" else 0,
        )

    def get_breakdown(self) -> dict[str, int]:
        """Get breakdown by source (test-compatible API)"""
        breakdown: dict[str, int] = {}
        for entry in self.history:
            source = entry["source"]
            breakdown[source] = breakdown.get(source, 0) + entry["tokens"]
        return breakdown

    def get_local_ratio(self) -> float:
        """Get ratio of local vs total compute (for test compatibility)"""
        if not self.history:
            return 0.0

        local_tokens = sum(e["tokens"] for e in self.history if e["source"] == "local")
        total_tokens = sum(e["tokens"] for e in self.history)

        return local_tokens / total_tokens if total_tokens > 0 else 0.0

    def record_operation(
        self,
        operation_type: ComputeType,
        description: str,
        api_tokens: int = 0,
        cpu_ms: float = 0,
        ram_mb: float = 0,
        file_ops: int = 0,
        bytes_processed: int = 0,
    ) -> None:
        """Record a compute operation"""
        op = ComputeOperation(
            timestamp=datetime.now(),
            operation_type=operation_type,
            description=description,
            api_tokens_used=api_tokens,
            local_cpu_ms=cpu_ms,
            local_ram_mb=ram_mb,
            file_operations=file_ops,
            bytes_processed=bytes_processed,
        )

        self.operations.append(op)

        # Log to file
        if self.log_file:
            with file_lock(self.log_file), open(self.log_file, "a") as f:
                f.write(json.dumps(op.to_dict()) + "\n")

    def record_api_call(self, description: str, tokens: int) -> None:
        """Record an API token usage"""
        self.record_operation(
            ComputeType.API_TOKENS,
            description,
            api_tokens=tokens,
        )

    def record_local_script(self, description: str, duration_ms: float, bytes_out: int = 0) -> None:
        """Record a local Python script execution"""
        self.record_operation(
            ComputeType.LOCAL_PYTHON,
            description,
            cpu_ms=duration_ms,
            bytes_processed=bytes_out,
        )

    def record_shell_command(self, command: str, duration_ms: float) -> None:
        """Record a shell command"""
        self.record_operation(
            ComputeType.LOCAL_SHELL,
            command,
            cpu_ms=duration_ms,
        )

    def record_file_operation(self, description: str, bytes_size: int, duration_ms: float = 0) -> None:
        """Record file I/O"""
        self.record_operation(
            ComputeType.LOCAL_FILE_IO,
            description,
            cpu_ms=duration_ms,
            file_ops=1,
            bytes_processed=bytes_size,
        )

    def record_rust_operation(self, description: str, duration_ms: float, bytes_out: int = 0) -> None:
        """Record Rust bridge operation"""
        self.record_operation(
            ComputeType.LOCAL_RUST,
            description,
            cpu_ms=duration_ms,
            bytes_processed=bytes_out,
        )

    def record_mcp_tool(self, tool_name: str, duration_ms: float) -> None:
        """Record MCP tool usage"""
        self.record_operation(
            ComputeType.MCP_TOOLS,
            tool_name,
            cpu_ms=duration_ms,
        )

    def get_session_summary(self) -> dict[str, Any]:
        """Get summary of current session"""
        if not self.operations:
            return {"message": "No operations recorded yet"}

        # Aggregate by type
        by_type = {}
        for op_type in ComputeType:
            ops_of_type = [op for op in self.operations if op.operation_type == op_type]

            if ops_of_type:
                by_type[op_type.value] = {
                    "count": len(ops_of_type),
                    "total_api_tokens": sum(op.api_tokens_used for op in ops_of_type),
                    "total_cpu_ms": sum(op.local_cpu_ms for op in ops_of_type),
                    "total_bytes": sum(op.bytes_processed for op in ops_of_type),
                    "total_file_ops": sum(op.file_operations for op in ops_of_type),
                }

        # Calculate percentages
        total_api_tokens = sum(op.api_tokens_used for op in self.operations)
        total_local_cpu = sum(op.local_cpu_ms for op in self.operations)

        # Estimate: 1 token ≈ 0.1ms of API processing
        estimated_api_cpu = total_api_tokens * 0.1
        total_compute = estimated_api_cpu + total_local_cpu

        local_percentage = (total_local_cpu / total_compute * 100) if total_compute > 0 else 0

        return {
            "session_duration_minutes": (datetime.now() - self.current_session_start).seconds / 60,
            "total_operations": len(self.operations),
            "by_type": by_type,
            "totals": {
                "api_tokens": total_api_tokens,
                "local_cpu_ms": total_local_cpu,
                "estimated_api_cpu_ms": estimated_api_cpu,
                "local_percentage": local_percentage,
            },
            "insight": self._generate_insight(local_percentage, by_type),
        }

    def _generate_insight(self, local_pct: float, by_type: dict[str, Any]) -> str:
        """Generate insight about compute distribution"""
        if local_pct > 80:
            return f"🚀 HIGHLY LOCAL ({local_pct:.0f}%): Most work done on laptop, not API! This explains 'infinite' feeling."
        if local_pct > 50:
            return f"⚖️ BALANCED ({local_pct:.0f}%): Mix of API and local compute. Hybrid approach."
        return f"☁️ API-HEAVY ({local_pct:.0f}%): Mostly using API tokens. Could optimize with more local."

    def print_summary(self) -> None:
        """Print a nice summary"""
        summary = self.get_session_summary()

        if "message" in summary:
            logger.info(summary["message"])
            return

        logger.info("\n💰 TOKEN ECONOMY SUMMARY")
        logger.info("=" * 60)
        logger.info(f"Session: {summary['session_duration_minutes']:.1f} minutes")
        logger.info(f"Operations: {summary['total_operations']}")
        logger.info("")
        logger.info("COMPUTE DISTRIBUTION:")
        logger.info(f"  API tokens: {summary['totals']['api_tokens']:,}")
        logger.info(f"  Local CPU: {summary['totals']['local_cpu_ms']:,.0f}ms")
        logger.info(f"  Local percentage: {summary['totals']['local_percentage']:.1f}%")
        logger.info("")
        logger.info("BY TYPE:")
        for type_name, stats in summary["by_type"].items():
            logger.info(f"  {type_name}:")
            logger.info(f"    Operations: {stats['count']}")
            if stats["total_api_tokens"] > 0:
                logger.info(f"    Tokens: {stats['total_api_tokens']:,}")
            if stats["total_cpu_ms"] > 0:
                logger.info(f"    CPU: {stats['total_cpu_ms']:,.0f}ms")
            if stats["total_bytes"] > 0:
                logger.info(f"    Data: {stats['total_bytes']:,} bytes")
        logger.info("")
        logger.info(f"💡 {summary['insight']}")
        logger.info("=" * 60)


# Singleton instance
_tracker = None

def get_token_tracker() -> TokenEconomyTracker:
    """Get the global token tracker instance"""
    global _tracker
    if _tracker is None:
        _tracker = TokenEconomyTracker()
    return _tracker

# Alias for test compatibility
TokenEconomy = TokenEconomyTracker
