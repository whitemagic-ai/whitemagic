from __future__ import annotations

import logging
from dataclasses import dataclass
from pathlib import Path
from types import TracebackType
from typing import Any, cast

from whitemagic.parallel.pools import PoolConfig, ThreadingManager

logger = logging.getLogger(__name__)


@dataclass
class FileReadResult:
    """Result of a file read operation."""

    path: Path
    content: str | None = None
    error: str | None = None
    size: int = 0
    success: bool = False

    def __post_init__(self) -> None:
        """Set success flag based on content."""
        if self.content is not None and self.error is None:
            self.success = True


class ParallelFileReader:
    """High-performance parallel file reader.

    Optimized for reading many files simultaneously using I/O-bound
    thread pool. Provides 40x+ speedup over sequential reading.
    """

    def __init__(self, max_workers: int = 64, encoding: str = "utf-8"):
        """Initialize parallel file reader.

        Args:
            max_workers: Maximum concurrent file reads (default: 64)
            encoding: File encoding (default: utf-8)

        """
        self.max_workers = max_workers
        self.encoding = encoding

        # Create custom pool config for file I/O
        config = PoolConfig(io_workers=max_workers)
        self.manager = ThreadingManager(config)

    def _read_single_file(self, path: str | Path) -> FileReadResult:
        """Read a single file synchronously.

        Args:
            path: File path

        Returns:
            FileReadResult with content or error

        """
        path = Path(path)

        try:
            if not path.exists():
                return FileReadResult(path=path, error=f"File not found: {path}", success=False)

            if not path.is_file():
                return FileReadResult(path=path, error=f"Not a file: {path}", success=False)

            content = path.read_text(encoding=self.encoding)
            size = path.stat().st_size

            return FileReadResult(path=path, content=content, size=size, success=True)

        except Exception as e:
            return FileReadResult(path=path, error=str(e), success=False)

    async def read_batch(
        self, paths: list[str | Path], fail_fast: bool = False,
    ) -> list[FileReadResult]:
        """Read multiple files in parallel.

        Args:
            paths: List of file paths to read
            fail_fast: Stop on first error (default: False)

        Returns:
            List of FileReadResult objects in same order as paths

        """
        if not paths:
            return []

        # Prepare tasks
        tasks: list[tuple[Any, tuple[Any, ...], dict[str, Any]]] = [(self._read_single_file, (path,), {}) for path in paths]

        # Execute in parallel
        results = await self.manager.run_batch(tasks, pool_type="io")

        # Check for errors if fail_fast
        if fail_fast:
            for result in results:
                if isinstance(result, Exception):
                    raise result
                if not result.success:
                    raise RuntimeError(f"Failed to read {result.path}: {result.error}")

        return cast(list[FileReadResult], results)

    async def read_batch_dict(self, paths: list[str | Path]) -> dict[str, str]:
        """Read multiple files and return as dictionary.

        Args:
            paths: List of file paths

        Returns:
            Dictionary mapping path -> content (only successful reads)

        """
        results = await self.read_batch(paths)

        return {
            str(result.path): result.content
            for result in results
            if result.success and result.content is not None
        }

    async def read_batch_filtered(
        self,
        paths: list[str | Path],
        max_size: int | None = None,
        extensions: list[str] | None = None,
    ) -> list[FileReadResult]:
        """Read files with filtering.

        Args:
            paths: List of file paths
            max_size: Maximum file size in bytes (None = no limit)
            extensions: Allowed file extensions (None = all)

        Returns:
            List of FileReadResult for matching files

        """
        # Filter paths first
        filtered_paths = []

        for path in paths:
            path = Path(path)

            # Check extension
            if extensions and path.suffix not in extensions:
                continue

            # Check size
            if max_size and path.exists():
                if path.stat().st_size > max_size:
                    continue

            filtered_paths.append(path)

        # Read filtered paths
        return await self.read_batch(filtered_paths)  # type: ignore[arg-type]

    def close(self) -> None:
        """Close the threading manager."""
        self.manager.shutdown()

    def __enter__(self) -> "ParallelFileReader":
        """Context manager entry."""
        self.manager.start()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        """Context manager exit."""
        self.close()


# Convenience functions


async def batch_read_files(
    paths: list[str | Path], max_workers: int = 64, encoding: str = "utf-8",
) -> list[FileReadResult]:
    """Convenience function to read multiple files in parallel.

    Args:
        paths: List of file paths
        max_workers: Maximum concurrent reads
        encoding: File encoding

    Returns:
        List of FileReadResult objects

    Example:
        results = await batch_read_files([
            "file1.txt",
            "file2.txt",
            "file3.txt"
        ])

        for result in results:
            if result.success:
                logger.info(f"{result.path}: {len(result.content)} chars")

    """
    reader = ParallelFileReader(max_workers=max_workers, encoding=encoding)
    try:
        return await reader.read_batch(paths)
    finally:
        reader.close()


async def batch_read_files_dict(
    paths: list[str | Path], max_workers: int = 64,
) -> dict[str, str]:
    """Read multiple files and return as dictionary.

    Args:
        paths: List of file paths
        max_workers: Maximum concurrent reads

    Returns:
        Dictionary mapping path -> content

    """
    reader = ParallelFileReader(max_workers=max_workers)
    try:
        return await reader.read_batch_dict(paths)
    finally:
        reader.close()
