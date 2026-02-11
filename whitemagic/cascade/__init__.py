"""MCP Cascade Intelligence - Meta-orchestration for tool chains

Vision:
-------
Enable local models to coordinate complex multi-step workflows by intelligently
chaining MCP tools in parallel batches, offloading reasoning from tokens to tools.

Key Innovations:
---------------
1. Tools calling tools autonomously (no LLM roundtrips)
2. Yin-Yang feedback loops for sustainable pacing
3. Scratchpad multiplexing for massive effective context
4. Multi-spectral synthesis (I Ching, Wu Xing, Tarot)

Impact: 10-100x effective context for local models
"""

from .engine import CascadeEngine
from .executor import ParallelBatchExecutor
from .graph import Batch, DependencyGraph, ToolCall
from .patterns import CascadePattern, PatternLibrary
from .synthesizer import ResultSynthesizer

__all__ = [
    "Batch",
    "CascadeEngine",
    "CascadePattern",
    "DependencyGraph",
    "ParallelBatchExecutor",
    "PatternLibrary",
    "ResultSynthesizer",
    "ToolCall",
]

__version__ = "0.1.0"
