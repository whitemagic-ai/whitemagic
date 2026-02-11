"""Dashboard interface package (optional dependencies)."""

# Dashboard is optional (requires flask + flask-cors).
try:
    from . import server
    __all__ = ["server"]
except ModuleNotFoundError:
    __all__ = []
