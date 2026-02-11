"""🥷 Clone Types - Specialized Search Agents
Each clone type has expertise in a specific domain.
Created: December 2, 2025 (Hanuman Tuesday).
"""

from dataclasses import dataclass
from enum import Enum


class CloneType(Enum):
    """Types of specialized search clones."""

    # === Internal Memory Search ===
    MD_ARCHAEOLOGIST = "md_archaeologist"      # Markdown file specialist
    JSON_PARSER = "json_parser"                # JSON structure expert
    YAML_NAVIGATOR = "yaml_navigator"          # YAML config specialist
    PATTERN_HUNTER = "pattern_hunter"          # Cross-format pattern finder

    # === External Search ===
    WEB_RESEARCHER = "web_researcher"          # General web search
    ACADEMIC_SCHOLAR = "academic_scholar"      # Papers, arxiv, research
    FORUM_CRAWLER = "forum_crawler"            # Reddit, forums, discussions

    # === Introspection ===
    SELF_EXAMINER = "self_examiner"            # Self-analysis, meta-cognition
    EMOTION_SENSOR = "emotion_sensor"          # Emotional resonance detection
    TEMPORAL_TRACKER = "temporal_tracker"      # Time-based pattern finder

    # === Specialized ===
    CODE_ANALYST = "code_analyst"              # Python/code understanding
    GRIMOIRE_READER = "grimoire_reader"        # WhiteMagic grimoire specialist
    ZODIAC_CONSULTANT = "zodiac_consultant"    # Zodiac core consultation


@dataclass
class CloneSpec:
    """Specification for a clone type."""

    clone_type: CloneType
    description: str
    file_extensions: list[str]
    search_paths: list[str]
    priority: int = 5  # 1-10, higher = more important

    @classmethod
    def get_spec(cls, clone_type: CloneType) -> "CloneSpec":
        """Get specification for a clone type."""
        specs = {
            CloneType.MD_ARCHAEOLOGIST: cls(
                clone_type=CloneType.MD_ARCHAEOLOGIST,
                description="Searches markdown files for narrative content",
                file_extensions=[".md"],
                search_paths=["memory/", "reports/", "grimoire/"],
                priority=8,
            ),
            CloneType.JSON_PARSER: cls(
                clone_type=CloneType.JSON_PARSER,
                description="Parses JSON files for structured data",
                file_extensions=[".json"],
                search_paths=["memory/", "config/"],
                priority=7,
            ),
            CloneType.YAML_NAVIGATOR: cls(
                clone_type=CloneType.YAML_NAVIGATOR,
                description="Navigates YAML configuration files",
                file_extensions=[".yaml", ".yml"],
                search_paths=["config/", "memory/"],
                priority=5,
            ),
            CloneType.PATTERN_HUNTER: cls(
                clone_type=CloneType.PATTERN_HUNTER,
                description="Finds patterns across all file types",
                file_extensions=["*"],
                search_paths=["whitemagic/", "memory/"],
                priority=9,
            ),
            CloneType.CODE_ANALYST: cls(
                clone_type=CloneType.CODE_ANALYST,
                description="Analyzes Python code for insights",
                file_extensions=[".py"],
                search_paths=["whitemagic/"],
                priority=7,
            ),
            CloneType.GRIMOIRE_READER: cls(
                clone_type=CloneType.GRIMOIRE_READER,
                description="Consults the WhiteMagic grimoire",
                file_extensions=[".md"],
                search_paths=["grimoire/"],
                priority=8,
            ),
            CloneType.SELF_EXAMINER: cls(
                clone_type=CloneType.SELF_EXAMINER,
                description="Examines self-knowledge and identity",
                file_extensions=[".md", ".json"],
                search_paths=["memory/self/", "memory/identity/"],
                priority=6,
            ),
            CloneType.TEMPORAL_TRACKER: cls(
                clone_type=CloneType.TEMPORAL_TRACKER,
                description="Tracks time-based patterns and sequences",
                file_extensions=["*"],
                search_paths=["memory/", "reports/"],
                priority=5,
            ),
        }
        return specs.get(clone_type, cls(
            clone_type=clone_type,
            description="Generic search clone",
            file_extensions=["*"],
            search_paths=["./"],
            priority=5,
        ))


# Default allocation for memory search
DEFAULT_ARMY_ALLOCATION = {
    CloneType.MD_ARCHAEOLOGIST: 0.35,    # 35% - Most content is markdown
    CloneType.JSON_PARSER: 0.15,         # 15% - Structured data
    CloneType.YAML_NAVIGATOR: 0.05,      # 5% - Config files
    CloneType.PATTERN_HUNTER: 0.25,      # 25% - Cross-format patterns
    CloneType.CODE_ANALYST: 0.10,        # 10% - Code understanding
    CloneType.GRIMOIRE_READER: 0.10,     # 10% - Grimoire consultation
}

# Allocation for introspection
INTROSPECTION_ALLOCATION = {
    CloneType.SELF_EXAMINER: 0.40,       # 40% - Self-analysis
    CloneType.EMOTION_SENSOR: 0.20,      # 20% - Emotional patterns
    CloneType.TEMPORAL_TRACKER: 0.20,    # 20% - Time patterns
    CloneType.PATTERN_HUNTER: 0.20,      # 20% - Meta-patterns
}
