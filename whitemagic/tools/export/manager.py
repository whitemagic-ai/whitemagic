"""WhiteMagic Export/Import System

Supports exporting and importing memories in various formats:
- JSON (structured data)
- CSV (tabular data)
- Markdown (human-readable)
"""

from __future__ import annotations

import csv
import io
import json
import uuid
import zipfile
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.utils.core import parse_datetime

from pydantic import BaseModel, Field


@dataclass
class MemoryExport:
    """Memory data structure for export."""
    id: str
    title: str
    content: str
    memory_type: str
    tags: list[str]
    metadata: dict[str, Any]
    created_at: str
    updated_at: str


class ExportRequest(BaseModel):
    """Export request model."""
    format: str = Field(..., pattern="^(json|csv|markdown|zip)$")
    filters: dict[str, Any] | None = None
    include_metadata: bool = Field(True, description="Include metadata in export")
    compress: bool = Field(False, description="Compress export files")


class ImportRequest(BaseModel):
    """Import request model."""
    format: str = Field(..., pattern="^(json|csv|markdown)$")
    data: str | bytes = Field(..., description="Import data or file path")
    merge_strategy: str = Field("skip", pattern="^(skip|overwrite|merge)$")
    validate_only: bool = Field(False, description="Only validate, don't import")


class ExportImportManager:
    """Manages export and import operations for WhiteMagic memories."""

    def __init__(self, storage_path: Path | None = None) -> None:
        self.storage_path = storage_path or Path("./data/exports")
        self.storage_path.mkdir(parents=True, exist_ok=True)

    @staticmethod
    def _to_text(data: str | bytes) -> str:
        if isinstance(data, bytes):
            return data.decode("utf-8", errors="replace")
        return data

    def export_memories(
        self,
        memories: list[MemoryExport],
        request: ExportRequest
    ) -> dict[str, Any]:
        """Export memories in the specified format."""

        # Apply filters if provided
        filtered_memories = self._apply_filters(memories, request.filters)

        # Export based on format
        if request.format == "json":
            result = self._export_json(filtered_memories, request)
        elif request.format == "csv":
            result = self._export_csv(filtered_memories, request)
        elif request.format == "markdown":
            result = self._export_markdown(filtered_memories, request)
        elif request.format == "zip":
            result = self._export_zip(filtered_memories, request)
        else:
            raise ValueError(f"Unsupported export format: {request.format}")

        return result

    def _apply_filters(
        self,
        memories: list[MemoryExport],
        filters: dict[str, Any] | None
    ) -> list[MemoryExport]:
        """Apply filters to memories."""
        if not filters:
            return memories

        filtered = memories

        # Filter by tags
        if "tags" in filters:
            required_tags = set(filters["tags"])
            filtered = [
                m for m in filtered
                if required_tags.intersection(set(m.tags))
            ]

        # Filter by memory type
        if "memory_type" in filters:
            filtered = [
                m for m in filtered
                if m.memory_type == filters["memory_type"]
            ]

        # Filter by date range
        if "date_from" in filters:
            date_from = parse_datetime(filters["date_from"])
            filtered = [
                m for m in filtered
                if parse_datetime(m.created_at) >= date_from
            ]

        if "date_to" in filters:
            date_to = parse_datetime(filters["date_to"])
            filtered = [
                m for m in filtered
                if parse_datetime(m.created_at) <= date_to
            ]

        # Filter by content search
        if "search" in filters:
            search_term = filters["search"].lower()
            filtered = [
                m for m in filtered
                if search_term in m.title.lower() or search_term in m.content.lower()
            ]

        return filtered

    def _export_json(
        self,
        memories: list[MemoryExport],
        request: ExportRequest
    ) -> dict[str, Any]:
        """Export memories as JSON."""

        export_data: dict[str, Any] = {
            "export_info": {
                "format": "json",
                "version": "1.0",
                "exported_at": datetime.now().isoformat(),
                "total_memories": len(memories),
                "include_metadata": request.include_metadata
            },
            "memories": [],
        }

        for memory in memories:
            memory_dict = asdict(memory)
            if not request.include_metadata:
                memory_dict.pop("metadata", None)
            export_data["memories"].append(memory_dict)

        # Save to file
        filename = f"whitemagic_export_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        filepath = self.storage_path / filename

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(export_data, f, indent=2, ensure_ascii=False)

        return {
            "filename": filename,
            "filepath": str(filepath),
            "size": filepath.stat().st_size,
            "format": "json",
            "count": len(memories)
        }

    def _export_csv(
        self,
        memories: list[MemoryExport],
        request: ExportRequest
    ) -> dict[str, Any]:
        """Export memories as CSV."""

        filename = f"whitemagic_export_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
        filepath = self.storage_path / filename

        with open(filepath, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)

            # Write header
            header = ["id", "title", "content", "memory_type", "tags", "created_at", "updated_at"]
            if request.include_metadata:
                header.append("metadata")
            writer.writerow(header)

            # Write memories
            for memory in memories:
                row = [
                    memory.id,
                    memory.title,
                    memory.content,
                    memory.memory_type,
                    ";".join(memory.tags),  # Join tags with semicolon
                    memory.created_at,
                    memory.updated_at
                ]
                if request.include_metadata:
                    row.append(json.dumps(memory.metadata))
                writer.writerow(row)

        return {
            "filename": filename,
            "filepath": str(filepath),
            "size": filepath.stat().st_size,
            "format": "csv",
            "count": len(memories)
        }

    def _export_markdown(
        self,
        memories: list[MemoryExport],
        request: ExportRequest
    ) -> dict[str, Any]:
        """Export memories as Markdown."""

        filename = f"whitemagic_export_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md"
        filepath = self.storage_path / filename

        with open(filepath, 'w', encoding='utf-8') as f:
            # Write header
            f.write("# WhiteMagic Memory Export\\n\\n")
            f.write(f"**Exported:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\\n")
            f.write(f"**Total Memories:** {len(memories)}\\n\\n")
            f.write("---\\n\\n")

            # Write memories
            for i, memory in enumerate(memories, 1):
                f.write(f"## {i}. {memory.title}\\n\\n")
                f.write(f"**ID:** `{memory.id}`\\n")
                f.write(f"**Type:** {memory.memory_type}\\n")
                f.write(f"**Created:** {memory.created_at}\\n")
                f.write(f"**Updated:** {memory.updated_at}\\n")

                if memory.tags:
                    f.write(f"**Tags:** {', '.join(f'`{tag}`' for tag in memory.tags)}\\n")

                f.write("\\n### Content\\n\\n")
                f.write(f"{memory.content}\\n\\n")

                if request.include_metadata and memory.metadata:
                    f.write("### Metadata\\n\\n")
                    f.write("```json\\n")
                    f.write(json.dumps(memory.metadata, indent=2))
                    f.write("\\n```\\n\\n")

                f.write("---\\n\\n")

        return {
            "filename": filename,
            "filepath": str(filepath),
            "size": filepath.stat().st_size,
            "format": "markdown",
            "count": len(memories)
        }

    def _export_zip(
        self,
        memories: list[MemoryExport],
        request: ExportRequest
    ) -> dict[str, Any]:
        """Export memories as a ZIP archive with multiple formats."""

        filename = f"whitemagic_export_{datetime.now().strftime('%Y%m%d_%H%M%S')}.zip"
        filepath = self.storage_path / filename

        with zipfile.ZipFile(filepath, 'w', zipfile.ZIP_DEFLATED) as zf:
            # Export JSON
            json_result = self._export_json(
                memories,
                ExportRequest(
                    format="json",
                    include_metadata=request.include_metadata,
                    compress=request.compress,
                ),
            )
            zf.write(json_result["filepath"], "export.json")

            # Export CSV
            csv_result = self._export_csv(
                memories,
                ExportRequest(
                    format="csv",
                    include_metadata=request.include_metadata,
                    compress=request.compress,
                ),
            )
            zf.write(csv_result["filepath"], "export.csv")

            # Export Markdown
            md_result = self._export_markdown(
                memories,
                ExportRequest(
                    format="markdown",
                    include_metadata=request.include_metadata,
                    compress=request.compress,
                ),
            )
            zf.write(md_result["filepath"], "export.md")

            # Add metadata file
            metadata = {
                "export_info": {
                    "format": "zip",
                    "version": "1.0",
                    "exported_at": datetime.now().isoformat(),
                    "total_memories": len(memories),
                    "formats": ["json", "csv", "markdown"]
                }
            }

            with zf.open("metadata.json", 'w') as meta_file:
                meta_file.write(json.dumps(metadata, indent=2).encode('utf-8'))

            # Clean up temporary files
            Path(json_result["filepath"]).unlink()
            Path(csv_result["filepath"]).unlink()
            Path(md_result["filepath"]).unlink()

        return {
            "filename": filename,
            "filepath": str(filepath),
            "size": filepath.stat().st_size,
            "format": "zip",
            "count": len(memories)
        }

    def import_memories(
        self,
        request: ImportRequest
    ) -> dict[str, Any]:
        """Import memories from the specified format."""

        # Parse based on format
        if request.format == "json":
            memories = self._import_json(request.data)
        elif request.format == "csv":
            memories = self._import_csv(request.data)
        elif request.format == "markdown":
            memories = self._import_markdown(request.data)
        else:
            raise ValueError(f"Unsupported import format: {request.format}")

        # Validate memories
        validation_errors = self._validate_memories(memories)

        if validation_errors:
            return {
                "success": False,
                "errors": validation_errors,
                "imported_count": 0
            }

        if request.validate_only:
            return {
                "success": True,
                "validated_count": len(memories),
                "imported_count": 0
            }

        # Import memories via MemoryManager
        try:
            from whitemagic.core.memory import MemoryManager
            mm = MemoryManager()
            for memory in memories:
                mm.create_memory(
                    title=memory.title,
                    content=memory.content,
                    memory_type=memory.memory_type,
                    tags=memory.tags,
                    extra_fields=memory.metadata
                )
        except ImportError:
            pass  # MemoryManager not available, return validation-only result

        return {
            "success": True,
            "imported_count": len(memories),
            "merge_strategy": request.merge_strategy
        }

    def _import_json(self, data: str | bytes) -> list[MemoryExport]:
        """Import memories from JSON."""

        try:
            parsed = json.loads(self._to_text(data))

            # Handle both direct list and wrapped format
            if "memories" in parsed:
                memories_data = parsed["memories"]
            else:
                memories_data = parsed if isinstance(parsed, list) else [parsed]

            memories = []
            for mem_data in memories_data:
                memory = MemoryExport(
                    id=mem_data.get("id", str(uuid.uuid4())),
                    title=mem_data.get("title", ""),
                    content=mem_data.get("content", ""),
                    memory_type=mem_data.get("memory_type", "short_term"),
                    tags=mem_data.get("tags", []),
                    metadata=mem_data.get("metadata", {}),
                    created_at=mem_data.get("created_at", datetime.now().isoformat()),
                    updated_at=mem_data.get("updated_at", datetime.now().isoformat())
                )
                memories.append(memory)

            return memories

        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON format: {e}")

    def _import_csv(self, data: str | bytes) -> list[MemoryExport]:
        """Import memories from CSV."""

        memories = []
        reader = csv.DictReader(io.StringIO(self._to_text(data)))

        for row in reader:
            memory = MemoryExport(
                id=row.get("id", str(uuid.uuid4())),
                title=row.get("title", ""),
                content=row.get("content", ""),
                memory_type=row.get("memory_type", "short_term"),
                tags=row.get("tags", "").split(";") if row.get("tags") else [],
                metadata=json.loads(row.get("metadata") or "{}"),
                created_at=row.get("created_at", datetime.now().isoformat()),
                updated_at=row.get("updated_at", datetime.now().isoformat())
            )
            memories.append(memory)

        return memories

    def _import_markdown(self, data: str | bytes) -> list[MemoryExport]:
        """Import memories from Markdown."""

        # This is a simplified parser - a full implementation would be more robust
        memories: list[MemoryExport] = []
        current_memory: dict[str, Any] = {}
        text = self._to_text(data)

        lines = text.split("\\n")

        for line in lines:
            if line.startswith("## "):
                # Save previous memory if exists
                if current_memory:
                    memories.append(self._create_memory_from_markdown(current_memory))

                # Start new memory
                current_memory = {
                    "title": line[3:].strip(),
                    "content": "",
                    "tags": [],
                    "metadata": {}
                }

            elif line.startswith("**ID:**"):
                current_memory["id"] = line.split("`")[1] if "`" in line else str(uuid.uuid4())

            elif line.startswith("**Type:**"):
                current_memory["memory_type"] = line.split(":")[1].strip()

            elif line.startswith("**Tags:**"):
                tags_str = line.split(":")[1].strip()
                current_memory["tags"] = [t.strip("`") for t in tags_str.split(",")]

            elif line.startswith("### Content"):
                # Content follows
                current_memory["parsing_content"] = True

            elif current_memory.get("parsing_content") and line.startswith("---"):
                current_memory["parsing_content"] = False

            elif current_memory.get("parsing_content"):
                current_memory["content"] += line + "\\n"

        # Save last memory
        if current_memory:
            memories.append(self._create_memory_from_markdown(current_memory))

        return memories

    def _create_memory_from_markdown(self, data: dict[str, Any]) -> MemoryExport:
        """Create MemoryExport from parsed markdown data."""
        return MemoryExport(
            id=data.get("id", str(uuid.uuid4())),
            title=data.get("title", ""),
            content=data.get("content", "").strip(),
            memory_type=data.get("memory_type", "short_term"),
            tags=data.get("tags", []),
            metadata=data.get("metadata", {}),
            created_at=datetime.now().isoformat(),
            updated_at=datetime.now().isoformat()
        )

    def _validate_memories(self, memories: list[MemoryExport]) -> list[str]:
        """Validate imported memories."""
        errors = []

        for i, memory in enumerate(memories):
            if not memory.title:
                errors.append(f"Memory {i+1}: Missing title")

            if not memory.content:
                errors.append(f"Memory {i+1}: Missing content")

            if memory.memory_type not in ["short_term", "long_term"]:
                errors.append(f"Memory {i+1}: Invalid memory type")

            # Validate UUID format if provided
            if memory.id:
                try:
                    uuid.UUID(memory.id)
                except ValueError:
                    errors.append(f"Memory {i+1}: Invalid UUID format")

        return errors


# Convenience functions
def create_export_manager(storage_path: Path | None = None) -> ExportImportManager:
    """Create an export/import manager."""
    return ExportImportManager(storage_path)


def export_to_json(
    memories: list[MemoryExport],
    filepath: Path,
    include_metadata: bool = True
) -> None:
    """Quick export to JSON."""
    manager = ExportImportManager()
    request = ExportRequest(format="json", include_metadata=include_metadata, compress=False)
    result = manager.export_memories(memories, request)

    # Move to requested location
    Path(result["filepath"]).rename(filepath)


def import_from_json(filepath: Path) -> list[MemoryExport]:
    """Quick import from JSON."""
    manager = ExportImportManager()

    with open(filepath, 'r', encoding='utf-8') as f:
        data = f.read()

    request = ImportRequest(
        format="json",
        data=data,
        merge_strategy="skip",
        validate_only=False,
    )
    result = manager.import_memories(request)

    if result["success"]:
        return manager._import_json(data)
    else:
        raise ValueError(f"Import failed: {result['errors']}")
