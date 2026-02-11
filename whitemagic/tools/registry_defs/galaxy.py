"""Galaxy Management Tools — Multi-Galaxy Memory System.
=====================================================
Project-scoped memory databases for organizing knowledge
across different projects, archives, and domains.
"""

from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

TOOLS: list[ToolDefinition] = [
    ToolDefinition(
        name="galaxy.create",
        description=(
            "Create a new galaxy (project-scoped memory database). Each galaxy gets "
            "its own SQLite database and holographic index for isolated memory storage."
        ),
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Galaxy name (alphanumeric, hyphens, underscores)"},
                "path": {"type": "string", "description": "Optional project directory this galaxy is associated with"},
                "description": {"type": "string", "description": "Human-readable description"},
                "tags": {"type": "array", "items": {"type": "string"}, "description": "Tags for categorization"},
            },
            "required": ["name"],
        },
        gana="Void", garden="stillness", quadrant="northern", element="water",
    ),
    ToolDefinition(
        name="galaxy.switch",
        description=(
            "Switch the active galaxy. All subsequent memory operations (search, create, "
            "recall) will target the new galaxy's database."
        ),
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Galaxy name to switch to"},
            },
            "required": ["name"],
        },
        gana="Void", garden="stillness", quadrant="northern", element="water",
    ),
    ToolDefinition(
        name="galaxy.list",
        description="List all known galaxies with metadata, memory counts, and active status.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.READ,
        input_schema={"type": "object", "properties": {}},
        gana="Void", garden="stillness", quadrant="northern", element="water",
    ),
    ToolDefinition(
        name="galaxy.status",
        description="Get overall galaxy manager status — active galaxy, total count, registry path.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.READ,
        input_schema={"type": "object", "properties": {}},
        gana="Void", garden="stillness", quadrant="northern", element="water",
    ),
    ToolDefinition(
        name="galaxy.ingest",
        description=(
            "Ingest files from a directory into a galaxy's memory store. Reads text files "
            "matching a glob pattern and stores each as a memory."
        ),
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Galaxy name to ingest into"},
                "source_path": {"type": "string", "description": "Directory path to ingest from"},
                "pattern": {"type": "string", "default": "**/*.md", "description": "Glob pattern for files"},
                "max_files": {"type": "integer", "default": 500, "description": "Max files to ingest"},
                "tags": {"type": "array", "items": {"type": "string"}, "description": "Tags to apply to all ingested memories"},
            },
            "required": ["name", "source_path"],
        },
        gana="Void", garden="stillness", quadrant="northern", element="water",
    ),
    ToolDefinition(
        name="galaxy.delete",
        description="Remove a galaxy from the registry. The database file is preserved on disk.",
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Galaxy name to remove"},
            },
            "required": ["name"],
        },
        gana="Void", garden="stillness", quadrant="northern", element="water",
    ),
]
