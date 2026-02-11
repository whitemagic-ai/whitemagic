"""Documentation module for WhiteMagic

Provides:
- Interactive API documentation
- Swagger UI integration
- ReDoc documentation
- OpenAPI specification generation
"""

from .interactive import (
    InteractiveDocumentation,
    create_docs_landing_page,
    setup_interactive_docs,
)

__all__ = [
    "InteractiveDocumentation",
    "create_docs_landing_page",
    "setup_interactive_docs",
]
