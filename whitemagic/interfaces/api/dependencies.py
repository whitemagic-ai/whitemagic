"""API dependencies."""

from typing import Annotated, Any, cast

from whitemagic.interfaces.api.database import Database, User

try:
    from fastapi import Depends, HTTPException, Request
    HAS_FASTAPI = True
except ImportError:
    HAS_FASTAPI = False
    Depends = None  # type: ignore
    HTTPException = None  # type: ignore
    Request = None  # type: ignore

_database: Database | None = None
_current_user: User | None = None

def get_database() -> Database:
    """Get database instance."""
    global _database
    if _database is None:
        _database = Database()
    return _database

def set_database(db: Database) -> None:
    """Set database instance."""
    global _database
    _database = db

async def get_current_user(request: Any) -> User:
    """Get current authenticated user from request state.

    Auth middleware sets request.state.user after validating API key.
    This dependency enforces that authentication succeeded.

    Raises:
        HTTPException(401): If user is not authenticated

    """
    if HTTPException is None:
        raise RuntimeError("FastAPI not available")

    user = getattr(request.state, "user", None)

    if user is None:
        raise HTTPException(
            status_code=401,
            detail="Authentication required. Provide valid API key in Authorization header.",
            headers={"WWW-Authenticate": "Bearer"},
        )

    return cast("User", user)

def set_current_user(user: User) -> None:
    """Set current user."""
    global _current_user
    _current_user = user

# FastAPI dependency type aliases
if HAS_FASTAPI and Depends is not None:
    CurrentUser = Annotated[User, Depends(get_current_user)]
    DBSession = Annotated[Database, Depends(get_database)]
else:
    CurrentUser = User  # type: ignore
    DBSession = Database  # type: ignore
