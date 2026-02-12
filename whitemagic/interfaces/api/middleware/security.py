"""Security middleware for WhiteMagic API.

Implements:
- CORS configuration
- Security headers
- Input validation
- Request logging
"""

import logging
import time
from collections.abc import Awaitable, Callable
from typing import Any

from fastapi import Request, Response
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.gzip import GZipMiddleware
from starlette.middleware.base import BaseHTTPMiddleware

logger = logging.getLogger(__name__)


# CORS configuration
def configure_cors(app: Any) -> None:
    """Configure CORS middleware."""
    origins = [
        "http://localhost:3000",
        "http://localhost:8000",
        "https://whitemagic.dev",
        "https://api.whitemagic.dev",
    ]

    app.add_middleware(
        CORSMiddleware,
        allow_origins=origins,
        allow_credentials=True,
        allow_methods=["GET", "POST", "PUT", "DELETE"],
        allow_headers=["*"],
        expose_headers=["X-Request-ID", "X-Response-Time"],
    )


# Security headers middleware
class SecurityHeadersMiddleware(BaseHTTPMiddleware):
    """Add security headers to all responses."""

    async def dispatch(
        self,
        request: Request,
        call_next: Callable[[Request], Awaitable[Response]],
    ) -> Response:
        response = await call_next(request)

        # Security headers
        response.headers["X-Content-Type-Options"] = "nosniff"
        response.headers["X-Frame-Options"] = "DENY"
        response.headers["X-XSS-Protection"] = "1; mode=block"
        response.headers["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains"
        response.headers["Content-Security-Policy"] = "default-src 'self'"
        response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"

        return response


# Request logging middleware
class RequestLoggingMiddleware(BaseHTTPMiddleware):
    """Log all requests with timing information."""

    async def dispatch(
        self,
        request: Request,
        call_next: Callable[[Request], Awaitable[Response]],
    ) -> Response:
        start_time = time.time()

        # Generate request ID
        request_id = f"{int(start_time * 1000000)}"

        # Log request
        logger.info(f"Request {request_id}: {request.method} {request.url.path}")

        # Process request
        response = await call_next(request)

        # Calculate duration
        duration = time.time() - start_time

        # Add timing headers
        response.headers["X-Request-ID"] = request_id
        response.headers["X-Response-Time"] = f"{duration:.3f}s"

        # Log response
        logger.info(
            f"Response {request_id}: {response.status_code} "
            f"({duration:.3f}s)",
        )

        return response


# Input validation middleware
class InputValidationMiddleware(BaseHTTPMiddleware):
    """Validate and sanitize input data."""

    async def dispatch(
        self,
        request: Request,
        call_next: Callable[[Request], Awaitable[Response]],
    ) -> Response:
        # Check content length
        content_length = request.headers.get("content-length")
        if content_length:
            try:
                size = int(content_length)
            except ValueError:
                size = 0
            if size > 10 * 1024 * 1024:  # 10MB limit
                return Response(
                    content="Request too large",
                    status_code=413,
                )

        # Process request
        response = await call_next(request)
        return response


def configure_security(app: Any) -> None:
    """Configure all security middleware."""
    # Add gzip compression
    app.add_middleware(GZipMiddleware, minimum_size=1000)

    # Add security headers
    app.add_middleware(SecurityHeadersMiddleware)

    # Add request logging
    app.add_middleware(RequestLoggingMiddleware)

    # Add input validation
    app.add_middleware(InputValidationMiddleware)

    # Configure CORS
    configure_cors(app)
