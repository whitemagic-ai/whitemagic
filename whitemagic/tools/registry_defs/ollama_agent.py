"""Ollama Agent Loop — Local AI with WhiteMagic tool access.
==========================================================
Enables local LLMs (via Ollama) to autonomously call WhiteMagic
tools in an agentic loop.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
    ToolDefinition(
        name="ollama.agent",
        description=(
            "Run an agentic loop with a local Ollama model that can autonomously "
            "call WhiteMagic tools (search, create memories, analyze patterns) to "
            "complete a given task. Injects relevant memories as context and supports "
            "up to 10 tool-call iterations."
        ),
        category=ToolCategory.INFERENCE,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "model": {"type": "string", "description": "Ollama model name (e.g., 'llama3.2', 'phi4', 'qwen2.5')"},
                "task": {"type": "string", "description": "The task or question for the agent"},
                "max_iterations": {"type": "integer", "default": 10, "description": "Maximum tool-call rounds"},
                "context": {"type": "boolean", "default": True, "description": "Whether to inject relevant memories"},
            },
            "required": ["model", "task"],
        },
        gana="Roof", garden="protection", quadrant="northern", element="water",
    ),
]
