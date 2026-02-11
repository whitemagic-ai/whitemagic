"""Rust bridge tool handlers."""
from typing import Any, cast


def _load_rust() -> tuple[Any, Any]:
    from whitemagic.tools.unified_api import _load_rust
    return cast("tuple[Any, Any]", _load_rust())


def handle_rust_audit(**kwargs: Any) -> dict[str, Any]:
    path = kwargs.get("path", ".")
    pattern = kwargs.get("pattern", "*.py")
    rust, rust_error = _load_rust()
    if rust is None:
        return {"status": "error", "message": "Rust bridge unavailable", "rust_error": rust_error}
    files = rust.audit_directory(str(path), pattern, 1000)
    file_summaries = [
        {"path": info.path, "size": info.size, "lines": info.lines, "words": info.words, "summary": info.summary}
        for info in files[:100]
    ]
    return {"status": "success", "files_scanned": len(files), "files": file_summaries}


def handle_rust_compress(**kwargs: Any) -> dict[str, Any]:
    data = str(kwargs.get("data", ""))
    rust, rust_error = _load_rust()
    if rust is None:
        import gzip
        compressed = gzip.compress(data.encode("utf-8"))
        return {"status": "success", "compressed_size": len(compressed), "rust_error": rust_error}
    compressed = rust.fast_compress(data)
    return {"status": "success", "compressed_size": len(compressed)}


def handle_rust_similarity(**kwargs: Any) -> dict[str, Any]:
    text1 = kwargs.get("text1", "")
    text2 = kwargs.get("text2", "")
    rust, rust_error = _load_rust()
    if rust is not None and hasattr(rust, "rust_similarity"):
        similarity = rust.rust_similarity(text1, text2)
        return {"status": "success", "similarity": float(similarity)}
    if rust is not None and hasattr(rust, "fast_similarity"):
        similarity = rust.fast_similarity(text1, text2)
        return {"status": "success", "similarity": float(similarity)}
    from difflib import SequenceMatcher
    similarity = SequenceMatcher(None, text1, text2).ratio()
    return {"status": "success", "similarity": similarity, "rust_error": rust_error}


def handle_rust_status(**kwargs: Any) -> dict[str, Any]:
    rust, rust_error = _load_rust()
    if rust is None:
        return {"status": "success", "available": False, "rust_error": rust_error}
    functions = ["audit_directory", "read_file_fast", "read_files_fast", "fast_compress", "rust_similarity"]
    return {
        "status": "success",
        "available": True,
        "version": getattr(rust, "__version__", "unknown"),
        "functions": [name for name in functions if hasattr(rust, name)],
    }
