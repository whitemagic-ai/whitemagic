"""Content Security Policy (CSP) Middleware for WhiteMagic

Provides comprehensive security headers including:
- Content Security Policy (CSP)
- HTTP Strict Transport Security (HSTS)
- X-Frame-Options
- X-Content-Type-Options
- Referrer Policy
- Permissions Policy
"""

import secrets
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from fastapi import Request, Response
    from fastapi.middleware import Middleware
    from starlette.middleware.base import BaseHTTPMiddleware, RequestResponseEndpoint
    FASTAPI_AVAILABLE = True
else:
    try:
        from fastapi import Request, Response
        from fastapi.middleware import Middleware
        from starlette.middleware.base import (
            BaseHTTPMiddleware,
            RequestResponseEndpoint,
        )
        FASTAPI_AVAILABLE = True
    except ImportError:
        FASTAPI_AVAILABLE = False
        # Create stub for type checking
        class BaseHTTPMiddleware:
            def __init__(self, app: Any, **kwargs: Any) -> None:
                pass
        class Middleware:  # pragma: no cover - runtime stub when FastAPI isn't installed
            def __init__(self, cls: Any, *args: Any, **kwargs: Any) -> None:
                self.cls = cls
                self.args = args
                self.kwargs = kwargs
        class Request:
            pass
        class Response:
            pass
        class RequestResponseEndpoint:
            pass

from whitemagic.logging_config import get_logger

logger = get_logger(__name__)


@dataclass
class CSPConfig:
    """CSP configuration."""

    enabled: bool = True
    report_only: bool = False
    default_src: list[str] | None = None
    script_src: list[str] | None = None
    style_src: list[str] | None = None
    img_src: list[str] | None = None
    font_src: list[str] | None = None
    connect_src: list[str] | None = None
    media_src: list[str] | None = None
    object_src: list[str] | None = None
    child_src: list[str] | None = None
    frame_src: list[str] | None = None
    worker_src: list[str] | None = None
    manifest_src: list[str] | None = None
    upgrade_insecure_requests: bool = True
    block_all_mixed_content: bool = False
    report_uri: str | None = None
    report_to: str | None = None

    def __post_init__(self) -> None:
        """Set default values."""
        if self.default_src is None:
            self.default_src = ["'self'"]
        if self.script_src is None:
            self.script_src = ["'self'"]
        if self.style_src is None:
            self.style_src = ["'self'", "'unsafe-inline'"]
        if self.img_src is None:
            self.img_src = ["'self'", "data:", "https:"]
        if self.font_src is None:
            self.font_src = ["'self'", "data:"]
        if self.connect_src is None:
            self.connect_src = ["'self'"]
        if self.media_src is None:
            self.media_src = ["'self'"]
        if self.object_src is None:
            self.object_src = ["'none'"]
        if self.child_src is None:
            self.child_src = ["'self'"]
        if self.frame_src is None:
            self.frame_src = ["'none'"]
        if self.worker_src is None:
            self.worker_src = ["'self'"]
        if self.manifest_src is None:
            self.manifest_src = ["'self'"]


@dataclass
class SecurityHeadersConfig:
    """Security headers configuration."""

    csp: CSPConfig | None = None
    hsts_enabled: bool = True
    hsts_max_age: int = 31536000  # 1 year
    hsts_include_subdomains: bool = True
    hsts_preload: bool = False
    x_frame_options: str = "DENY"  # DENY, SAMEORIGIN, ALLOW-FROM
    x_content_type_options: bool = True
    referrer_policy: str = "strict-origin-when-cross-origin"
    permissions_policy: dict[str, list[str]] | None = None

    def __post_init__(self) -> None:
        """Set default values."""
        if self.csp is None:
            self.csp = CSPConfig()
        if self.permissions_policy is None:
            self.permissions_policy = {
                "geolocation": [],
                "microphone": [],
                "camera": [],
                "payment": [],
                "usb": [],
                "magnetometer": [],
                "gyroscope": [],
                "accelerometer": [],
                "ambient-light-sensor": [],
                "autoplay": ["self"],
                "encrypted-media": [],
                "fullscreen": ["self"],
                "picture-in-picture": ["self"],
            }


class CSPMiddleware(BaseHTTPMiddleware):
    """Middleware to add CSP and other security headers."""

    def __init__(self, app: Any, config: SecurityHeadersConfig) -> None:
        super().__init__(app)
        self.config = config
        self.nonce_cache: dict[str, float] = {}
        self.nonce_ttl = 3600  # 1 hour

    def generate_nonce(self) -> str:
        """Generate a cryptographic nonce for inline scripts/styles."""
        # Clean old nonces
        import time
        now = time.time()
        self.nonce_cache = {
            nonce: timestamp for nonce, timestamp in self.nonce_cache.items()
            if now - timestamp < self.nonce_ttl
        }

        # Generate new nonce
        nonce = secrets.token_urlsafe(16)
        self.nonce_cache[nonce] = now
        return nonce

    def build_csp_header(self, request: Request) -> str:
        """Build CSP header value."""
        assert self.config.csp is not None
        if not self.config.csp.enabled:
            return ""

        csp = CSPBuilder()

        # Add directives
        csp.add_directive("default-src", self.config.csp.default_src)
        csp.add_directive("script-src", self.config.csp.script_src)
        csp.add_directive("style-src", self.config.csp.style_src)
        csp.add_directive("img-src", self.config.csp.img_src)
        csp.add_directive("font-src", self.config.csp.font_src)
        csp.add_directive("connect-src", self.config.csp.connect_src)
        csp.add_directive("media-src", self.config.csp.media_src)
        csp.add_directive("object-src", self.config.csp.object_src)
        csp.add_directive("child-src", self.config.csp.child_src)
        csp.add_directive("frame-src", self.config.csp.frame_src)
        csp.add_directive("worker-src", self.config.csp.worker_src)
        csp.add_directive("manifest-src", self.config.csp.manifest_src)

        # Add special directives
        if self.config.csp.upgrade_insecure_requests:
            csp.add_directive("upgrade-insecure-requests")

        if self.config.csp.block_all_mixed_content:
            csp.add_directive("block-all-mixed-content")

        # Add reporting
        if self.config.csp.report_uri:
            csp.add_directive("report-uri", [self.config.csp.report_uri])

        if self.config.csp.report_to:
            csp.add_directive("report-to", [self.config.csp.report_to])

        # Add nonce for inline scripts if needed
        if "'unsafe-inline'" in (self.config.csp.script_src or []):
            nonce = self.generate_nonce()
            request.state.csp_nonce = nonce
            csp.add_nonce_to_script_src(nonce)

        return csp.build()

    async def dispatch(self, request: Request, call_next: RequestResponseEndpoint) -> Response:
        """Add security headers to response."""
        response = await call_next(request)
        assert self.config.csp is not None

        # Add CSP header
        if self.config.csp.enabled:
            csp_value = self.build_csp_header(request)
            if csp_value:
                header_name = "Content-Security-Policy-Report-Only" if self.config.csp.report_only else "Content-Security-Policy"
                response.headers[header_name] = csp_value

        # Add HSTS header
        if self.config.hsts_enabled and request.url.scheme == "https":
            hsts_parts = [f"max-age={self.config.hsts_max_age}"]
            if self.config.hsts_include_subdomains:
                hsts_parts.append("includeSubDomains")
            if self.config.hsts_preload:
                hsts_parts.append("preload")
            response.headers["Strict-Transport-Security"] = "; ".join(hsts_parts)

        # Add other security headers
        if self.config.x_frame_options:
            response.headers["X-Frame-Options"] = self.config.x_frame_options

        if self.config.x_content_type_options:
            response.headers["X-Content-Type-Options"] = "nosniff"

        if self.config.referrer_policy:
            response.headers["Referrer-Policy"] = self.config.referrer_policy

        # Add Permissions Policy header
        if self.config.permissions_policy:
            policies = []
            for feature, origins in self.config.permissions_policy.items():
                if origins:
                    policies.append(f"{feature}=({', '.join(origins)})")
                else:
                    policies.append(f"{feature}=()")
            if policies:
                response.headers["Permissions-Policy"] = ", ".join(policies)

        # Add additional security headers
        response.headers["X-XSS-Protection"] = "1; mode=block"
        response.headers["X-Permitted-Cross-Domain-Policies"] = "none"
        response.headers["Cross-Origin-Embedder-Policy"] = "require-corp"
        response.headers["Cross-Origin-Opener-Policy"] = "same-origin"
        response.headers["Cross-Origin-Resource-Policy"] = "same-origin"

        return response


class CSPBuilder:
    """Helper class to build CSP headers."""

    def __init__(self) -> None:
        self.directives: dict[str, list[str]] = {}

    def add_directive(self, name: str, values: list[str] | None = None) -> None:
        """Add a CSP directive."""
        if values:
            self.directives[name] = values

    def add_nonce_to_script_src(self, nonce: str) -> None:
        """Add nonce to script-src directive."""
        if "script-src" in self.directives:
            if "'nonce-" not in " ".join(self.directives["script-src"]):
                self.directives["script-src"].append(f"'nonce-{nonce}'")

    def build(self) -> str:
        """Build the CSP header string."""
        parts = []
        for directive, values in self.directives.items():
            if values:
                parts.append(f"{directive} {' '.join(values)}")
            else:
                parts.append(directive)
        return "; ".join(parts)


class CSPReporter:
    """Handles CSP violation reports."""

    def __init__(self, report_uri: str) -> None:
        self.report_uri = report_uri

    async def handle_report(self, report: dict[str, Any]) -> None:
        """Handle a CSP violation report."""
        # Log the violation
        logger.warning(f"CSP Violation: {report}")

        # Emit to Gan Ying for monitoring integration
        try:
            from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
            bus = get_bus()
            bus.emit(
                ResonanceEvent(
                    source="csp_reporter",
                    event_type=EventType.PATTERN_DETECTED,
                    data={
                        "type": "csp_violation",
                        "report": report,
                        "severity": "warning",
                    },
                ),
            )
        except ImportError:
            pass  # Gan Ying not available

    def get_report_endpoint_script(self) -> str:
        """Get JavaScript for reporting CSP violations."""
        return f"""
        document.addEventListener('securitypolicyviolation', function(e) {{
            fetch('{self.report_uri}', {{
                method: 'POST',
                headers: {{
                    'Content-Type': 'application/csp-report'
                }},
                body: JSON.stringify({{
                    'csp-report': {{
                        'document-uri': e.documentURI,
                        'referrer': e.referrer,
                        'violated-directive': e.violatedDirective,
                        'effective-directive': e.effectiveDirective,
                        'original-policy': e.originalPolicy,
                        'disposition': e.disposition,
                        'blocked-uri': e.blockedURI,
                        'line-number': e.lineNumber,
                        'column-number': e.columnNumber,
                        'source-file': e.sourceFile,
                        'status-code': e.statusCode,
                        'script-sample': e.sample
                    }}
                }})
            }});
        }});
        """


# Utility functions
def create_security_middleware(
    csp_config: CSPConfig | None = None,
    hsts_enabled: bool = True,
    environment: str = "production",
) -> Middleware:
    """Create security middleware with default configurations."""
    # Adjust config based on environment
    if environment == "development":
        csp_config = CSPConfig(
            enabled=True,
            report_only=True,
            script_src=["'self'", "'unsafe-inline'", "'unsafe-eval'"],
            style_src=["'self'", "'unsafe-inline'"],
            connect_src=["'self'", "ws:", "wss:"],
        )
        hsts_enabled = False

    config = SecurityHeadersConfig(
        csp=csp_config or CSPConfig(),
        hsts_enabled=hsts_enabled,
    )

    return Middleware(CSPMiddleware, config)


def get_default_csp_config() -> CSPConfig:
    """Get default CSP configuration for WhiteMagic."""
    return CSPConfig(
        enabled=True,
        report_only=False,
        default_src=["'self'"],
        script_src=["'self'"],  # No unsafe-inline in production
        style_src=["'self'"],  # Use hashed styles instead
        img_src=["'self'", "data:", "https:"],
        font_src=["'self'", "data:"],
        connect_src=["'self'"],
        media_src=["'self'"],
        object_src=["'none'"],
        child_src=["'self'"],
        frame_src=["'none'"],
        worker_src=["'self'"],
        manifest_src=["'self'"],
        upgrade_insecure_requests=True,
        block_all_mixed_content=True,
    )


if FASTAPI_AVAILABLE:
    from fastapi import APIRouter, Body, Depends

    router = APIRouter(prefix="/security", tags=["security"])
    reporter = CSPReporter(report_uri="/security/csp-report")

    @router.post("/csp-report")
    async def csp_violation_report(
        report: dict[str, Any] = Body(...),
        reporter: CSPReporter = Depends(lambda: reporter),
    ) -> dict[str, str]:
        """Receive CSP violation reports."""
        await reporter.handle_report(report)
        return {"status": "received"}

    @router.get("/csp-nonce")
    async def get_csp_nonce(request: Request) -> dict[str, str]:
        """Get a fresh CSP nonce."""
        # This is simplified - in practice, you'd get the middleware instance
        nonce = secrets.token_urlsafe(16)
        return {"nonce": nonce}


# Example usage
if __name__ == "__main__":
    # Example CSP header
    config = get_default_csp_config()
    builder = CSPBuilder()

    for directive in ["default-src", "script-src", "style-src"]:
        values = getattr(config, directive.replace("-", "_"))
        builder.add_directive(directive, values)

    logger.info("Example CSP Header:")
    logger.info(builder.build())
