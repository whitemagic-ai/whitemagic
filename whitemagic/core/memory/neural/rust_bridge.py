"""🦀 Rust Bridge - High-Performance Memory Operations.

Wraps whitemagic_rs functions for use in the neural memory system.
Falls back to Python implementations if Rust is unavailable.

Created: December 2, 2025 (Hanuman Tuesday)
"""

from pathlib import Path
from typing import Any, cast

# Try to import Rust module
try:
    import whitemagic_rs as rs
    RUST_AVAILABLE = True
except ImportError:
    RUST_AVAILABLE = False


def is_rust_available() -> bool:
    """Check if Rust bridge is available."""
    return RUST_AVAILABLE


def fast_similarity(text1: str, text2: str) -> float:
    """Calculate text similarity using Rust (10-100x faster)."""
    if RUST_AVAILABLE:
        return float(rs.fast_similarity(text1, text2))

    # Python fallback
    words1 = set(text1.lower().split())
    words2 = set(text2.lower().split())
    if not words1 or not words2:
        return 0.0
    intersection = len(words1 & words2)
    union = len(words1 | words2)
    return intersection / union if union else 0.0


def parallel_grep(
    directory: str,
    pattern: str,
    extensions: list[str] | None = None,
    context_lines: int = 2,
) -> list[str]:
    """Parallel grep using Rust."""
    if RUST_AVAILABLE:
        exts = extensions or ["md", "txt", "json", "yaml"]
        return cast("list[str]", rs.parallel_grep(directory, pattern, exts, context_lines))

    # Python fallback (slower)
    import re
    results: list[str] = []
    path = Path(directory)
    exts = extensions or ["md", "txt", "json", "yaml"]

    for ext in exts:
        for file in path.rglob(f"*.{ext}"):
            try:
                content = file.read_text(errors="ignore")
                if re.search(pattern, content, re.IGNORECASE):
                    results.append(str(file))
            except IOError:
                pass

    return results


def build_word_index(
    directory: str,
    extensions: list[str] | None = None,
    min_word_length: int = 3,
) -> dict[str, list[str]]:
    """Build word index using Rust."""
    if RUST_AVAILABLE:
        exts = extensions or ["md"]
        return cast("dict[str, list[str]]", rs.build_word_index(directory, exts, min_word_length))

    # Python fallback
    import re
    from collections import defaultdict

    index: dict[str, list[str]] = defaultdict(list)
    path = Path(directory)
    exts = extensions or ["md"]

    for ext in exts:
        for file in path.rglob(f"*.{ext}"):
            try:
                content = file.read_text(errors="ignore").lower()
                words = re.findall(r"\b\w+\b", content)
                for word in set(words):
                    if len(word) >= min_word_length:
                        index[word].append(str(file))
            except Exception:
                pass

    return dict(index)


def scan_codebase(
    directory: str,
    extensions: list[str] | None = None,
    max_file_size: int = 1000000,
) -> list[dict[str, Any]]:
    """Scan codebase using Rust."""
    if RUST_AVAILABLE:
        exts = extensions or ["py"]
        return cast("list[dict[str, Any]]", rs.scan_codebase(directory, exts, max_file_size))

    # Python fallback
    results: list[dict[str, Any]] = []
    path = Path(directory)
    exts = extensions or ["py"]

    for ext in exts:
        for file in path.rglob(f"*.{ext}"):
            try:
                stat = file.stat()
                if stat.st_size <= max_file_size:
                    results.append({
                        "path": str(file),
                        "size": stat.st_size,
                    })
            except Exception:
                pass

    return results


def extract_todos(directory: str) -> list[str]:
    """Extract TODOs using Rust."""
    if RUST_AVAILABLE:
        return cast("list[str]", rs.extract_todos(directory))

    # Python fallback
    import re
    todos: list[str] = []
    path = Path(directory)

    for file in path.rglob("*.py"):
        try:
            content = file.read_text(errors="ignore")
            for match in re.finditer(r"#\s*TODO[:\s]*(.*)", content, re.IGNORECASE):
                todos.append(f"{file}: {match.group(1)}")
        except IOError:
            pass

    return todos


def extract_definitions(directory: str) -> list[dict[str, Any]]:
    """Extract function/class definitions using Rust."""
    if RUST_AVAILABLE:
        return cast("list[dict[str, Any]]", rs.extract_definitions(directory))

    # Python fallback
    import re
    defs: list[dict[str, Any]] = []
    path = Path(directory)

    for file in path.rglob("*.py"):
        try:
            content = file.read_text(errors="ignore")
            for match in re.finditer(r"^(def|class)\s+(\w+)", content, re.MULTILINE):
                defs.append({
                    "type": match.group(1),
                    "name": match.group(2),
                    "file": str(file),
                })
        except Exception:
            pass

    return defs


def audit_directory(directory: str) -> list[dict[str, Any]]:
    """Audit directory using Rust."""
    if RUST_AVAILABLE:
        return cast("list[dict[str, Any]]", rs.audit_directory(directory))

    # Python fallback
    results: list[dict[str, Any]] = []
    path = Path(directory)

    for file in path.rglob("*"):
        if file.is_file():
            try:
                stat = file.stat()
                content = file.read_text(errors="ignore")
                results.append({
                    "path": str(file),
                    "size": stat.st_size,
                    "lines": content.count("\n"),
                })
            except Exception:
                pass

    return results


def build_memory_graph(
    memories: list[tuple[str, str, list[str]]],
    min_similarity: float = 0.3,
    max_links: int = 10,
) -> dict[str, list[tuple[str, float]]]:
    """Build memory graph using Rust (massively parallel).

    Args:
        memories: List of (id, content, tags) tuples
        min_similarity: Minimum similarity threshold (0.0-1.0)
        max_links: Maximum links per node

    Returns:
        Adjacency list: {source_id: [(target_id, weight), ...]}

    """
    if RUST_AVAILABLE and hasattr(rs, "build_memory_graph"):
        return cast(
            "dict[str, list[tuple[str, float]]]",
            rs.build_memory_graph(memories, min_similarity, max_links),
        )

    # Python fallback (simplified, O(N^2) - slow!)
    graph = {}
    for i, (id1, content1, tags1) in enumerate(memories):
        links = []
        for j, (id2, content2, tags2) in enumerate(memories):
            if i == j:
                continue

            # Calculate similarity (basic Jaccard on tags + simple text overlap)
            sim = fast_similarity(content1, content2)

            # Boost for shared tags
            shared_tags = len(set(tags1) & set(tags2))
            if shared_tags > 0:
                sim += 0.1 * shared_tags

            if sim >= min_similarity:
                links.append((id2, min(1.0, sim)))

        # Sort by weight and limit
        links.sort(key=lambda x: x[1], reverse=True)
        graph[id1] = links[:max_links]

    return graph


def parallel_traverse(
    memories: list[tuple[str, str, list[str]]],
    start_id: str,
    max_depth: int = 3,
    min_similarity: float = 0.3,
) -> set[str]:
    """Traverse memory graph from start node to find connected cluster.

    Args:
        memories: List of (id, content, tags) tuples (needed to build graph if not cached)
        start_id: ID of starting node
        max_depth: How far to traverse
        min_similarity: Minimum weight to follow link

    Returns:
        Set of visited memory IDs (the cluster)

    """
    if RUST_AVAILABLE and hasattr(rs, "parallel_traverse"):
        return cast(
            "set[str]",
            rs.parallel_traverse(memories, start_id, max_depth, min_similarity),
        )

    # Python fallback (DFS)
    # First build graph (expensive!)
    graph = build_memory_graph(memories, min_similarity, max_links=20)

    visited = {start_id}
    stack = [(start_id, 0)]

    while stack:
        current_id, depth = stack.pop()
        if depth >= max_depth:
            continue

        neighbors = graph.get(current_id, [])
        for target_id, weight in neighbors:
            if weight >= min_similarity and target_id not in visited:
                visited.add(target_id)
                stack.append((target_id, depth + 1))

    return visited


# Export status
def get_status() -> dict[str, Any]:
    """Get Rust bridge status."""
    return {
        "rust_available": RUST_AVAILABLE,
        "functions": [
            "fast_similarity",
            "parallel_grep",
            "build_word_index",
            "scan_codebase",
            "extract_todos",
            "extract_definitions",
            "audit_directory",
        ] if RUST_AVAILABLE else [],
    }
