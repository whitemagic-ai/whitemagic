"""Cascade Patterns - Predefined tool chain templates"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class ActivityType(Enum):
    """Activity types for Yin-Yang balance"""

    # Yang activities (action, output)
    CREATE = "yang"
    WRITE = "yang"
    IMPLEMENT = "yang"
    EXECUTE = "yang"
    DEPLOY = "yang"
    CODE = "yang"
    BUILD = "yang"
    UPDATE = "yang"

    # Yin activities (reflection, input)
    READ = "yin"
    SEARCH = "yin"
    ANALYZE = "yin"
    REFLECT = "yin"
    CONSOLIDATE = "yin"
    REVIEW = "yin"
    LEARN = "yin"
    LIST = "yin"


@dataclass
class PatternStep:
    """Single step in a cascade pattern"""

    tool_name: str
    description: str
    params: dict[str, Any] = field(default_factory=dict)
    activity_type: ActivityType = ActivityType.READ
    depends_on: list[str] = field(default_factory=list)
    optional: bool = False


@dataclass
class CascadePattern:
    """Template for a tool cascade workflow"""

    name: str
    description: str
    steps: list[PatternStep]
    expected_inputs: list[str] = field(default_factory=list)
    tags: list[str] = field(default_factory=list)


class PatternLibrary:
    """Library of predefined cascade patterns"""

    def __init__(self) -> None:
        self.patterns: dict[str, CascadePattern] = {}
        self._register_default_patterns()

    def _register_default_patterns(self) -> Any:
        """Register built-in cascade patterns"""
        # Pattern: Analyze codebase
        self.register(CascadePattern(
            name="analyze_codebase",
            description="Comprehensive codebase analysis with context loading",
            expected_inputs=["query", "module"],
            tags=["analysis", "codebase"],
            steps=[
                PatternStep(
                    tool_name="search_memories",
                    description="Find prior knowledge about the module",
                    params={"query": "$query"},
                    activity_type=ActivityType.SEARCH,
                ),
                PatternStep(
                    tool_name="parallel_search",
                    description="Multi-query scan for related concepts",
                    params={
                        "queries": ["$module architecture", "$module patterns", "$module dependencies"],
                    },
                    activity_type=ActivityType.SEARCH,
                ),
                PatternStep(
                    tool_name="batch_read_memories",
                    description="Load full content of relevant memories",
                    params={"filenames": "$search_results"},
                    activity_type=ActivityType.READ,
                    depends_on=["search_memories", "parallel_search"],
                ),
                PatternStep(
                    tool_name="create_scratchpad",
                    description="Create workspace for analysis",
                    params={"name": "$module_analysis"},
                    activity_type=ActivityType.CREATE,
                    depends_on=["batch_read_memories"],
                ),
                PatternStep(
                    tool_name="analyze_scratchpad",
                    description="Synthesize findings from loaded context",
                    params={"scratchpad_id": "$scratchpad"},
                    activity_type=ActivityType.ANALYZE,
                    depends_on=["create_scratchpad"],
                ),
                PatternStep(
                    tool_name="create_memory",
                    description="Store analysis results",
                    params={
                        "title": "Analysis: $module",
                        "content": "$analysis_results",
                        "type": "long_term",
                    },
                    activity_type=ActivityType.CREATE,
                    depends_on=["analyze_scratchpad"],
                ),
            ],
        ))

        # Pattern 2: Refactor Module
        self.register(CascadePattern(
            name="refactor_module",
            description="Plan and execute module refactoring",
            expected_inputs=["module", "reason"],
            tags=["refactor", "code"],
            steps=[
                PatternStep(
                    tool_name="read_memory",
                    description="Load current module code",
                    params={"filename": "$module_file"},
                    activity_type=ActivityType.READ,
                ),
                PatternStep(
                    tool_name="search_memories",
                    description="Find similar patterns and best practices",
                    params={"query": "$module refactoring patterns"},
                    activity_type=ActivityType.SEARCH,
                ),
                PatternStep(
                    tool_name="create_scratchpad",
                    description="Create refactoring plan workspace",
                    params={"name": "$module_refactor_plan"},
                    activity_type=ActivityType.CREATE,
                    depends_on=["read_memory", "search_memories"],
                ),
                PatternStep(
                    tool_name="garden_activate",
                    description="Activate dharma garden for ethical considerations",
                    params={"garden": "dharma"},
                    activity_type=ActivityType.READ,
                    depends_on=["create_scratchpad"],
                ),
                PatternStep(
                    tool_name="update_scratchpad",
                    description="Document refactoring decisions",
                    params={
                        "scratchpad_id": "$scratchpad",
                        "section": "decisions",
                        "content": "$refactor_plan",
                    },
                    activity_type=ActivityType.WRITE,
                    depends_on=["garden_activate"],
                ),
                PatternStep(
                    tool_name="batch_create_memories",
                    description="Store refactoring changes",
                    params={"memories": "$refactored_modules"},
                    activity_type=ActivityType.CREATE,
                    depends_on=["update_scratchpad"],
                ),
            ],
        ))

        # Pattern 3: Debug Issue
        self.register(CascadePattern(
            name="debug_issue",
            description="Systematic debugging with context gathering",
            expected_inputs=["issue", "stacktrace"],
            tags=["debug", "troubleshooting"],
            steps=[
                PatternStep(
                    tool_name="search_memories",
                    description="Find similar issues from history",
                    params={"query": "$issue"},
                    activity_type=ActivityType.SEARCH,
                ),
                PatternStep(
                    tool_name="parallel_search",
                    description="Scan logs and error patterns",
                    params={
                        "queries": ["$stacktrace", "error handling $module", "known issues $module"],
                    },
                    activity_type=ActivityType.SEARCH,
                ),
                PatternStep(
                    tool_name="create_scratchpad",
                    description="Create investigation workspace",
                    params={"name": "$issue_investigation"},
                    activity_type=ActivityType.CREATE,
                    depends_on=["search_memories", "parallel_search"],
                ),
                PatternStep(
                    tool_name="garden_activate",
                    description="Activate mystery garden for deep investigation",
                    params={"garden": "mystery"},
                    activity_type=ActivityType.READ,
                    depends_on=["create_scratchpad"],
                ),
                PatternStep(
                    tool_name="cast_oracle",
                    description="Get guidance on investigation approach",
                    params={"question": "Where is the root cause of $issue?"},
                    activity_type=ActivityType.REFLECT,
                    depends_on=["garden_activate"],
                    optional=True,
                ),
                PatternStep(
                    tool_name="update_scratchpad",
                    description="Track investigation findings",
                    params={
                        "scratchpad_id": "$scratchpad",
                        "section": "questions",
                        "content": "$investigation_notes",
                    },
                    activity_type=ActivityType.WRITE,
                    depends_on=["cast_oracle"],
                ),
                PatternStep(
                    tool_name="update_memory",
                    description="Document solution for future reference",
                    params={
                        "filename": "$issue_memory",
                        "content": "$solution",
                        "add_tags": ["debug", "solved", "$module"],
                    },
                    activity_type=ActivityType.UPDATE,
                    depends_on=["update_scratchpad"],
                ),
            ],
        ))

    def register(self, pattern: CascadePattern) -> Any:
        """Register a new pattern

        Args:
            pattern: Pattern to register

        """
        self.patterns[pattern.name] = pattern

    def get_pattern(self, name: str) -> CascadePattern | None:
        """Get pattern by name

        Args:
            name: Pattern name

        Returns:
            Pattern or None if not found

        """
        return self.patterns.get(name)

    def list_patterns(self) -> list[str]:
        """List all available pattern names

        Returns:
            List of pattern names

        """
        return list(self.patterns.keys())

    def search_patterns(self, tag: str) -> list[CascadePattern]:
        """Search patterns by tag

        Args:
            tag: Tag to search for

        Returns:
            List of matching patterns

        """
        return [
            pattern for pattern in self.patterns.values()
            if tag in pattern.tags
        ]
