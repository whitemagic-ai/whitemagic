from __future__ import annotations

import builtins
import importlib
import logging
import sys
from collections.abc import Callable
from functools import wraps
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

# Add direct path for standalone imports
WHITE_MAGIC_PATH = Path(__file__).parent.parent
if str(WHITE_MAGIC_PATH) not in sys.path:
    sys.path.insert(0, str(WHITE_MAGIC_PATH))


class LazyImport:
    """Lazy import descriptor that defers import until first access.

    Usage:
        numpy = LazyImport('numpy')
        # numpy is not imported yet

        arr = numpy.array([1, 2, 3])  # Now numpy is imported
    """

    def __init__(self, module_name: str, attribute: str | None = None):
        self.module_name = module_name
        self.attribute = attribute
        self._module: Any | None = None

    def __getattr__(self, name: str) -> Any:
        if self._module is None:
            self._module = importlib.import_module(self.module_name)

        if self.attribute:
            return getattr(self._module, self.attribute)

        return getattr(self._module, name)

    def __call__(self, *args: Any, **kwargs: Any) -> Any:
        if self._module is None:
            self._module = importlib.import_module(self.module_name)

        if self.attribute:
            return getattr(self._module, self.attribute)(*args, **kwargs)

        module_obj = self._module
        if callable(module_obj):
            return module_obj(*args, **kwargs)
        raise TypeError(f"Module '{self.module_name}' is not callable")


def lazy_import(module_name: str, attribute: str | None = None) -> LazyImport:
    """Create a lazy import."""
    return LazyImport(module_name, attribute)


# Common heavy imports that should be lazy
HEAVY_IMPORTS = {
    # ML/AI libraries
    "numpy": lazy_import("numpy"),
    "pandas": lazy_import("pandas"),
    "sklearn": lazy_import("sklearn"),
    "torch": lazy_import("torch"),
    "transformers": lazy_import("transformers"),
    "sentence_transformers": lazy_import("sentence_transformers"),

    # Web/API libraries
    "requests": lazy_import("requests"),
    "aiohttp": lazy_import("aiohttp"),
    "fastapi": lazy_import("fastapi"),

    # Data processing
    "yaml": lazy_import("yaml"),
    "toml": lazy_import("toml"),
    "jsonschema": lazy_import("jsonschema"),

    # Visualization
    "matplotlib": lazy_import("matplotlib"),
    "plotly": lazy_import("plotly"),

    # Rust bridge (optional)
    "whitemagic_rs": lazy_import("whitemagic_rs"),
}


class SessionBootstrap:
    """Pre-warms essential modules and caches for faster cold starts.

    Strategy:
    1. Identify commonly used modules
    2. Pre-import them asynchronously
    3. Cache import results
    4. Reuse across sessions
    """

    def __init__(self) -> None:
        self._cache: dict[str, Any] = {}
        self._prewarmed: set[str] = set()

    def prewarm_modules(self, modules: list[str]) -> None:
        """Pre-warm a list of modules in the background.

        Args:
            modules: List of module names to pre-warm

        """
        for module in modules:
            try:
                if module in HEAVY_IMPORTS:
                    # Force import
                    _ = HEAVY_IMPORTS[module]._module
                    self._prewarmed.add(module)
            except ImportError:
                pass  # Module not available

    def get_prewarm_list(self) -> list[str]:
        """Get list of commonly used modules for pre-warming."""
        return [
            # Core dependencies
            "json",
            "pathlib",
            "datetime",
            "typing",

            # WhiteMagic core
            "whitemagic.core.exceptions",
            "whitemagic.fileio",
            "whitemagic.utils",

            # Frequently used optional
            "yaml",
            "requests",
        ]

    def is_prewarmed(self, module: str) -> bool:
        """Check if a module is pre-warmed."""
        return module in self._prewarmed


# Global bootstrap instance
_bootstrap = SessionBootstrap()


def get_bootstrap() -> SessionBootstrap:
    """Get the global session bootstrap instance."""
    return _bootstrap


def optimize_imports() -> None:
    """Apply import optimizations to the current session.

    This should be called early in the application startup.
    """
    # Pre-warm common modules
    common_modules = _bootstrap.get_prewarm_list()
    _bootstrap.prewarm_modules(common_modules)

    # Replace common heavy imports in sys.modules
    for name, lazy_import in HEAVY_IMPORTS.items():
        if name not in sys.modules and hasattr(lazy_import, "_module"):
            if lazy_import._module is not None:
                sys.modules[name] = lazy_import._module


def deferred_import(module_name: str) -> Callable[..., Callable[..., Any]]:
    """Decorator that defers import of a module until function is called.

    Usage:
        @deferred_import('numpy')
        def process_data():
            import numpy as np
            return np.array([1, 2, 3])
    """
    def decorator(func: Callable[..., Any]) -> Callable[..., Any]:
        @wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            # Import the module just before function execution
            try:
                importlib.import_module(module_name)
            except ImportError:
                pass  # Module not available, function will handle it

            return func(*args, **kwargs)
        return wrapper
    return decorator


class OptionalModule:
    """Safely handle optional module dependencies.

    Instead of:
        try:
            import torch
        except ImportError:
            torch = None

    Use:
        torch = OptionalModule('torch')
        if torch.available:
            torch.tensor([1, 2, 3])
    """

    def __init__(self, module_name: str):
        self.module_name = module_name
        self._module: Any | None = None
        self._checked = False

    @property
    def available(self) -> bool:
        """Check if module is available."""
        if not self._checked:
            try:
                self._module = importlib.import_module(self.module_name)
                self._checked = True
                return True
            except ImportError:
                self._checked = True
                return False
        return self._module is not None

    def __getattr__(self, name: str) -> Any:
        if not self.available:
            raise ImportError(f"Optional module '{self.module_name}' is not installed")
        return getattr(self._module, name)

    def __call__(self, *args: Any, **kwargs: Any) -> Any:
        if not self.available:
            raise ImportError(f"Optional module '{self.module_name}' is not installed")
        module_obj = self._module
        if callable(module_obj):
            return module_obj(*args, **kwargs)
        raise TypeError(f"Optional module '{self.module_name}' is not callable")


# Create optional module instances for common dependencies
torch = OptionalModule("torch")
transformers = OptionalModule("transformers")
sentence_transformers = OptionalModule("sentence_transformers")
whitemagic_rs = OptionalModule("whitemagic_rs")


def profile_imports(func: Callable[..., Any]) -> Callable[..., Any]:
    """Profile import time for a function.

    Usage:
        @profile_imports
        def my_function():
            # Function body
    """
    @wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        import time
        start = time.time()
        result = func(*args, **kwargs)
        duration = time.time() - start
        logger.info(f"[Import Profile] {func.__name__}: {duration:.3f}s")
        return result
    return wrapper


# Monkey patch to optimize common imports
def optimize_standard_imports() -> None:
    """Optimize imports in commonly used modules."""
    # Defer yaml import in utils
    try:
        import whitemagic.utils
        if hasattr(whitemagic.utils, "yaml"):
            # Make it a lazy import
            whitemagic.utils.yaml = lazy_import("yaml")
    except ImportError:
        pass

    # Defer heavy imports in other modules
    modules_to_optimize = [
        "whitemagic.embeddings.local_provider",
        "whitemagic.memory.neural.persistence",
        "whitemagic.edge.onnx_export",
    ]

    for module_name in modules_to_optimize:
        try:
            module: Any = importlib.import_module(module_name)
            # Add lazy loading for heavy imports
            if hasattr(module, "numpy"):
                module.numpy = lazy_import("numpy")
            if hasattr(module, "torch"):
                module.torch = lazy_import("torch")
        except ImportError:
            pass


def fast_import_exceptions() -> dict[str, type[BaseException]]:
    """Import exceptions without triggering package cascade.

    Returns:
        dict: All exception classes for fast access.

    """
    try:
        import whitemagic.core.exceptions as exc

        names = (
            "WhiteMagicError",
            "MemoryError",
            "ConfigError",
            "APIError",
            "ValidationError",
            "AuthenticationError",
            "RateLimitError",
            "NetworkError",
            "TimeoutError",
            "DatabaseError",
            "FileError",
            "PermissionError",
            "ImportError",
            "VersionError",
            "DependencyError",
            "SystemError",
            "RuntimeError",
            "LogicError",
            "ValueError",
            "TypeError",
            "IndexError",
            "KeyError",
            "AttributeError",
            "NotImplementedError",
            "AssertionError",
            "EOFError",
            "KeyboardInterrupt",
            "SystemExit",
            "StopIteration",
            "GeneratorExit",
            "Warning",
            "UserWarning",
            "DeprecationWarning",
            "PendingDeprecationWarning",
            "SyntaxWarning",
            "RuntimeWarning",
            "FutureWarning",
            "ImportWarning",
            "UnicodeWarning",
            "BytesWarning",
            "ResourceWarning",
        )
        resolved: dict[str, type[BaseException]] = {}
        for name in names:
            custom = getattr(exc, name, None)
            builtin = getattr(builtins, name, None)
            candidate = custom if custom is not None else builtin
            if isinstance(candidate, type) and issubclass(candidate, BaseException):
                resolved[name] = candidate
        return resolved
    except ImportError:
        return {}


# Export the fast exceptions function
__all__ = [
    "LazyImport",
    "fast_import_exceptions",
    "lazy_import",
    "optimize_imports",
    "optimize_standard_imports",
]

# Apply optimizations on import
# optimize_standard_imports()  # DISABLED: Causes circular imports during utils init
