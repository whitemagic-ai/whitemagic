"""Tool Types — Base classes for the WhiteMagic Tool Registry.
============================================================
Extracted to avoid circular imports between registry.py and
domain definition files in registry_defs/.
"""

from dataclasses import dataclass
from enum import Enum
from typing import Any


class ToolCategory(str, Enum):
    MEMORY = "memory"
    SESSION = "session"
    GARDEN = "garden"
    METRICS = "metrics"
    EDGE = "edge"
    INTROSPECTION = "introspection"
    SYSTEM = "system"
    ARCHAEOLOGY = "archaeology"
    GOVERNOR = "governor"
    WATCHER = "watcher"
    BROWSER = "browser"
    INFERENCE = "inference"
    SYNTHESIS = "synthesis"
    DHARMA = "dharma"
    GANA = "gana"
    BROKER = "broker"
    TASK = "task"
    VOTING = "voting"
    AGENT = "agent"


class ToolSafety(str, Enum):
    READ = "read"
    WRITE = "write"
    DELETE = "delete"


@dataclass
class ToolDefinition:
    """Definition of a WhiteMagic tool."""

    name: str
    description: str
    category: ToolCategory
    safety: ToolSafety
    input_schema: dict[str, Any]
    gana: str | None = None
    garden: str | None = None
    quadrant: str | None = None
    element: str | None = None
    permissions: tuple[str, ...] = ()  # Leap 9: declared capability scopes

    @property
    def risk_level(self) -> str:
        """Map tool metadata to Governor risk levels."""
        from whitemagic.core.governor import RiskLevel

        # 1. FORBIDDEN categories/tools
        if self.category == ToolCategory.GOVERNOR and self.safety != ToolSafety.READ:
            return str(RiskLevel.FORBIDDEN.name)

        # 2. DANGEROUS actions (Delete/Execute)
        if self.safety == ToolSafety.DELETE:
            return str(RiskLevel.DANGEROUS.name)

        if self.category == ToolCategory.SYSTEM and self.safety != ToolSafety.READ:
            # Most system operations are risky
            return str(RiskLevel.DANGEROUS.name)

        # 3. CAUTION (Writes)
        if self.safety == ToolSafety.WRITE:
            return str(RiskLevel.CAUTION.name)

        # 4. SAFE defaults (Read)
        return str(RiskLevel.SAFE.name)

    def to_openai_function(self) -> dict[str, Any]:
        """Convert to OpenAI function calling format."""
        return {
            "type": "function",
            "function": {
                "name": self.name,
                "description": f"[{self.category.value.upper()}] {self.description}",
                "parameters": self.input_schema,
            },
        }

    def to_mcp_tool(self) -> dict[str, Any]:
        """Convert to MCP tool format."""
        safety_suffix = "" if self.safety == ToolSafety.READ else f" | {self.safety.value.upper()}"
        return {
            "name": self.name,
            "description": f"[{self.category.value.upper()}{safety_suffix}] {self.description}",
            "inputSchema": self.input_schema,
        }

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return {
            "name": self.name,
            "description": self.description,
            "category": self.category.value,
            "safety": self.safety.value,
            "input_schema": self.input_schema,
        }
