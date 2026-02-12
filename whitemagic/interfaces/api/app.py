"""FastAPI application with all routes registered."""

import os
from collections.abc import Callable
from importlib.util import find_spec
from typing import Any

from whitemagic.config import VERSION
from whitemagic.config.validator import get_validator

app: Any

try:
    from fastapi import FastAPI
    from fastapi.middleware.cors import CORSMiddleware

    from .middleware_core import (  # type: ignore[import-not-found]
        AuthMiddleware,
        RequestLoggingMiddleware,
    )
    from .middleware_deployment import (  # type: ignore[import-not-found]
        DeploymentAuthMiddleware,
        RateLimitMiddleware,
    )

    app = FastAPI(title="WhiteMagic API", version=VERSION)

    # Validate configuration and get deployment mode
    validator = get_validator()
    validator.validate_or_create_secrets()
    deployment_mode = validator.validate_deployment_mode()

    # CORS configuration - mode-aware
    if deployment_mode == "local":
        # Local mode: permissive for localhost development
        origins = [
            "http://localhost:3000",
            "http://localhost:8000",
            "http://127.0.0.1:3000",
            "http://127.0.0.1:8000",
        ]
        cors_methods = ["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"]
        cors_headers = ["Content-Type", "Authorization", "X-Requested-With"]
    else:
        # Cloud mode: strict CORS from environment
        origins_env = os.getenv("ALLOWED_ORIGINS", "").strip()
        if not origins_env or origins_env == "*":
            raise RuntimeError(
                "Cloud mode requires explicit ALLOWED_ORIGINS. "
                "Set ALLOWED_ORIGINS environment variable (comma-separated).",
            )
        origins = [origin.strip() for origin in origins_env.split(",") if origin.strip()]
        cors_methods = ["GET", "POST", "PUT", "DELETE"]  # No PATCH in production
        cors_headers = ["Content-Type", "Authorization"]

    # Add middleware (reverse order of execution)
    app.add_middleware(
        CORSMiddleware,
        allow_origins=origins,
        allow_credentials=True,
        allow_methods=cors_methods,
        allow_headers=cors_headers,
    )
    app.add_middleware(RateLimitMiddleware, enabled=(deployment_mode != "local"), requests_per_minute=60)
    app.add_middleware(RequestLoggingMiddleware)
    app.add_middleware(DeploymentAuthMiddleware, mode=deployment_mode)
    app.add_middleware(AuthMiddleware)

    # Register all routes
    try:
        from .routes.metrics_api import router as metrics_router  # type: ignore[import-not-found]
        app.include_router(metrics_router)
    except ImportError:
        pass

    try:
        from .routes.keys_api import router as keys_router  # type: ignore[import-not-found]
        app.include_router(keys_router)
    except ImportError:
        pass

    try:
        from .routes.exec_enhanced import router as exec_router
        app.include_router(exec_router)
    except ImportError:
        pass

    try:
        from whitemagic.interfaces.api.routes.dashboard_api import (
            router as dashboard_router,
        )
        app.include_router(dashboard_router)
    except ImportError:
        pass

    # Universal REST Gateway - Tools API for any AI system
    try:
        from .routes.tools_gateway import router as tools_router
        app.include_router(tools_router)
    except ImportError:
        pass

    # OpenAI-Compatible API - For Codex, Ollama, and OpenAI SDK
    try:
        from .routes.openai_compat import router as openai_router
        app.include_router(openai_router)
    except ImportError:
        pass

    # Phase 4: Dharma (Ethical Reasoning) API
    try:
        from .routes.dharma_api import router as dharma_router  # type: ignore[import-not-found]
        app.include_router(dharma_router)
    except ImportError:
        pass

    # Phase 4: Local ML Inference API
    try:
        from .routes.local_ml_api import router as local_ml_router  # type: ignore[import-not-found]
        app.include_router(local_ml_router)
    except ImportError:
        pass

    # Mobile Webhook Triggers — REST endpoints for mobile automation
    try:
        from .routes.webhook_triggers import router as webhook_router
        app.include_router(webhook_router)
    except ImportError:
        pass

    @app.get("/")
    def root() -> dict[str, Any]:
        return {
            "message": f"WhiteMagic API v{VERSION}",
            "version": VERSION,
            "endpoints": {
                "tools_gateway": "/api/tools - Universal REST gateway for any AI",
                "openai_compat": "/v1 - OpenAI-compatible API for Codex/Ollama",
                "dharma": "/api/v1/dharma - Ethical reasoning (Phase 4)",
                "local_ml": "/api/v1/local-ml - Archived (disabled by default)",
                "health": "/health - Health check",
                "ready": "/ready - Readiness check",
                "docs": "/docs - Interactive API documentation",
            },
        }

    # WebSocket Live Feed for Nexus
    try:
        from .websocket import register_websocket_routes
        register_websocket_routes(app)
    except ImportError:
        pass

    @app.get("/health")
    def health() -> dict[str, Any]:
        """Liveness probe — is the process alive and responding?"""
        return {"status": "healthy", "version": VERSION}

    @app.get("/health/deep")
    def health_deep() -> dict[str, Any]:
        """Deep health check — tests all major components."""
        import time as _time
        start = _time.time()
        checks = {}

        # 1. Database
        try:
            import sqlite3

            from whitemagic.config.paths import DB_PATH
            conn = sqlite3.connect(str(DB_PATH), timeout=2)
            count = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
            conn.close()
            checks["database"] = {"status": "ok", "memories": count}
        except Exception as e:
            checks["database"] = {"status": "degraded", "error": str(e)}

        # 2. Rust bridge
        if find_spec("whitemagic_rs") is not None:
            checks["rust_bridge"] = {"status": "ok"}
        else:
            checks["rust_bridge"] = {"status": "unavailable"}

        # 3. Harmony Vector
        try:
            from whitemagic.harmony.vector import get_harmony_vector
            hv = get_harmony_vector()
            snap = hv.snapshot()
            checks["harmony"] = {"status": "ok", "balance": round(snap.balance, 3)}
        except Exception:
            checks["harmony"] = {"status": "unavailable"}

        # 4. Gan Ying bus
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import get_bus
            bus = get_bus()
            listener_count = getattr(bus, "listener_count", None)
            listeners = listener_count() if callable(listener_count) else 0
            checks["gan_ying"] = {"status": "ok", "listeners": listeners}
        except Exception:
            checks["gan_ying"] = {"status": "unavailable"}

        # 5. Redis (optional)
        try:
            import redis as _redis
            r = _redis.Redis.from_url(os.environ.get("REDIS_URL", "redis://localhost:6379"), socket_timeout=1)
            r.ping()
            checks["redis"] = {"status": "ok"}
        except Exception:
            checks["redis"] = {"status": "unavailable"}

        overall = "healthy"
        if checks.get("database", {}).get("status") != "ok":
            overall = "degraded"

        elapsed_ms = round((_time.time() - start) * 1000, 1)
        return {
            "status": overall,
            "version": VERSION,
            "checks": checks,
            "response_time_ms": elapsed_ms,
        }

    @app.get("/ready")
    def ready() -> dict[str, Any]:
        """Readiness probe — can this instance serve traffic?"""
        try:
            from whitemagic.config.paths import DB_PATH
            if not DB_PATH.exists():
                return {"status": "not_ready", "reason": "database not initialized"}
        except Exception:
            pass
        return {"status": "ready"}

except ImportError:
    class StubApp:
        def get(self, path: str) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
            def decorator(func: Callable[..., Any]) -> Callable[..., Any]:
                return func
            return decorator
        def include_router(self, router: Any) -> None:
            pass
        def add_middleware(self, middleware: Any, **kwargs: Any) -> None:
            pass
    app = StubApp()
