# mypy: disable-error-code=no-untyped-def
"""Interactive Documentation for WhiteMagic API

Provides:
- Swagger UI integration
- ReDoc documentation
- Interactive API explorer
- Code examples
- Try it out functionality
"""

import json
from typing import Any

try:
    from fastapi import FastAPI, Request, Response
    from fastapi.openapi.docs import get_redoc_html, get_swagger_ui_html
    FASTAPI_AVAILABLE = True
except ImportError:
    FASTAPI_AVAILABLE = False

from whitemagic.logging_config import get_logger

logger = get_logger(__name__)


class InteractiveDocumentation:
    """Manages interactive API documentation."""

    def __init__(self, app: FastAPI, docs_url: str = "/docs", redoc_url: str = "/redoc"):
        self.app = app
        self.docs_url = docs_url
        self.redoc_url = redoc_url
        self.custom_css: str | None = None
        self.custom_js: str | None = None
        self.swagger_ui_parameters: dict[str, Any] = {}

    def setup_swagger_ui(
        self,
        title: str = "WhiteMagic API",
        description: str | None = None,
        version: str = "2.0.0",
        oauth2_config: dict[str, Any] | None = None,
        custom_css: str | None = None,
        custom_js: str | None = None,
    ) -> None:
        """Set up Swagger UI documentation."""
        # Custom CSS for better branding
        default_css = """
        .swagger-ui .topbar { display: none }
        .swagger-ui .info { margin: 50px 0 }
        .swagger-ui .info .title { color: #2563eb }
        .swagger-ui .scheme-container { background: #f8fafc; padding: 20px; border-radius: 8px }
        .swagger-ui .opblock.opblock-post { border-color: #10b981; background: rgba(16, 185, 129, 0.1) }
        .swagger-ui .opblock.opblock-get { border-color: #3b82f6; background: rgba(59, 130, 246, 0.1) }
        .swagger-ui .opblock.opblock-put { border-color: #f59e0b; background: rgba(245, 158, 11, 0.1) }
        .swagger-ui .opblock.opblock-delete { border-color: #ef4444; background: rgba(239, 68, 68, 0.1) }
        """

        # Merge custom CSS
        final_css = default_css + (custom_css or "")
        self.custom_css = final_css
        self.custom_js = custom_js

        # OAuth2 configuration
        if oauth2_config:
            self.swagger_ui_parameters = {
                "oauth2RedirectUrl": f"{self.app.servers[0].url}{self.docs_url}/oauth2-redirect.html",  # type: ignore[attr-defined]
                **oauth2_config,
            }

        # Create custom Swagger UI route
        @self.app.get(self.docs_url, include_in_schema=False)
        async def custom_swagger_ui_html(req: Request):
            root = req.scope.get("root_path") or ""
            openapi_url = root + (self.app.openapi_url or "")
            resp = get_swagger_ui_html(
                openapi_url=openapi_url,
                title=f"{title} - Swagger UI",
                oauth2_redirect_url=self.swagger_ui_parameters.get("oauth2RedirectUrl"),
                init_oauth=self.swagger_ui_parameters,
                swagger_ui_parameters=self.swagger_ui_parameters,
                swagger_js_url="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js",
                swagger_css_url="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css",
                swagger_favicon_url="https://fastapi.tiangolo.com/img/favicon.png",
            )
            # Inject custom CSS/JS into the HTML response body
            raw_body = resp.body.decode("utf-8") if isinstance(resp.body, bytes) else str(resp.body)
            if self.custom_css:
                raw_body = raw_body.replace("</head>", f"<style>{self.custom_css}</style></head>")
            if self.custom_js:
                raw_body = raw_body.replace("</body>", f"<script>{self.custom_js}</script></body>")
            body = raw_body
            return Response(content=body, media_type="text/html")

        # OAuth2 redirect endpoint
        @self.app.get(f"{self.docs_url}/oauth2-redirect.html", include_in_schema=False)
        async def swagger_ui_redirect():
            return Response(
                content="""
                <!DOCTYPE html>
                <html>
                <head>
                    <title>Swagger UI: OAuth2 Redirect</title>
                </head>
                <body>
                    <script>
                        window.opener.postMessage({'authCode': new URLSearchParams(window.location.search).get('code')}, window.location.origin);
                        window.close();
                    </script>
                </body>
                </html>
                """,
                media_type="text/html",
            )

    def setup_redoc(
        self,
        title: str = "WhiteMagic API",
        description: str | None = None,
        version: str = "2.0.0",
        custom_css: str | None = None,
    ) -> None:
        """Set up ReDoc documentation."""
        # Custom CSS for ReDoc
        default_css = """
        redoc { font-family: Inter, sans-serif }
        .api-content { padding: 40px }
        .api-info { margin-bottom: 40px }
        h1 { color: #1f2937 }
        .react-json-view { padding: 20px; background: #f9fafb; border-radius: 8px }
        """

        final_css = default_css + (custom_css or "")

        @self.app.get(self.redoc_url, include_in_schema=False)
        async def redoc_html(req: Request):
            root = req.scope.get("root_path") or ""
            openapi_url = root + (self.app.openapi_url or "")
            resp = get_redoc_html(
                openapi_url=openapi_url,
                title=f"{title} - ReDoc",
                redoc_js_url="https://cdn.jsdelivr.net/npm/redoc@2.0.0/bundles/redoc.standalone.js",
                redoc_favicon_url="https://fastapi.tiangolo.com/img/favicon.png",
            )
            # Inject custom CSS into the HTML response body
            raw_body = resp.body.decode("utf-8") if isinstance(resp.body, bytes) else str(resp.body)
            if final_css:
                raw_body = raw_body.replace("</head>", f"<style>{final_css}</style></head>")
            body = raw_body
            return Response(content=body, media_type="text/html")

    def add_code_examples(self) -> None:
        """Add code examples to the OpenAPI schema."""

        def add_examples_to_schema():
            if not self.app.openapi_schema:
                return

            # Add examples to common endpoints
            paths = self.app.openapi_schema.get("paths", {})

            # Memory endpoints examples
            if "/memories" in paths:
                # POST /memories example
                post_operation = paths["/memories"].get("post", {})
                if "requestBody" in post_operation:
                    post_operation["requestBody"]["content"]["application/json"]["example"] = {
                        "title": "My First Memory",
                        "content": "This is the content of my memory",
                        "memory_type": "short_term",
                        "tags": ["personal", "important"],
                        "metadata": {"source": "web"},
                    }

                # GET /memories response example
                get_operation = paths["/memories"].get("get", {})
                if "responses" in get_operation and "200" in get_operation["responses"]:
                    get_operation["responses"]["200"]["content"]["application/json"]["example"] = {
                        "memories": [
                            {
                                "id": "123e4567-e89b-12d3-a456-426614174000",
                                "title": "Example Memory",
                                "content": "This is an example memory",
                                "memory_type": "short_term",
                                "tags": ["example"],
                                "created_at": "2024-01-01T00:00:00Z",
                                "updated_at": "2024-01-01T00:00:00Z",
                            },
                        ],
                        "total": 1,
                        "limit": 10,
                        "offset": 0,
                    }

        # Hook into schema generation
        self.app.openapi_schema = self.app.openapi()
        add_examples_to_schema()

    def add_postman_collection(self) -> None:
        """Add endpoint to download Postman collection."""

        @self.app.get("/docs/postman", include_in_schema=False)
        async def get_postman_collection(req: Request):
            """Generate Postman collection from OpenAPI spec."""
            openapi_spec = self.app.openapi()

            # Convert OpenAPI to Postman collection
            collection: dict[str, Any] = {
                "info": {
                    "name": "WhiteMagic API",
                    "description": openapi_spec.get("info", {}).get("description", ""),
                    "schema": "https://schema.getpostman.com/json/collection/v2.1.0/collection.json",
                },
                "auth": {
                    "type": "bearer",
                    "bearer": [
                        {
                            "key": "token",
                            "value": "{{accessToken}}",
                            "type": "string",
                        },
                    ],
                },
                "variable": [
                    {
                        "key": "baseUrl",
                        "value": self.app.servers[0].url if self.app.servers else "http://localhost:8000",  # type: ignore[attr-defined]
                    },
                ],
                "item": [],
            }

            # Convert endpoints
            for path, methods in openapi_spec.get("paths", {}).items():
                folder = {
                    "name": path,
                    "item": [],
                }

                for method, operation in methods.items():
                    if method.upper() in ["GET", "POST", "PUT", "DELETE", "PATCH"]:
                        item = {
                            "name": operation.get("summary", f"{method.upper()} {path}"),
                            "request": {
                                "method": method.upper(),
                                "header": [
                                    {
                                        "key": "Content-Type",
                                        "value": "application/json",
                                    },
                                ],
                                "url": {
                                    "raw": "{{baseUrl}}" + path,
                                    "host": ["{{baseUrl}}"],
                                    "path": path.strip("/").split("/") if path != "/" else [],
                                },
                            },
                        }

                        # Add request body if present
                        if "requestBody" in operation:
                            item["request"]["body"] = {
                                "mode": "raw",
                                "raw": json.dumps(
                                    operation["requestBody"]["content"]["application/json"].get("example", {}),
                                    indent=2,
                                ),
                                "options": {
                                    "raw": {
                                        "language": "json",
                                    },
                                },
                            }

                        folder["item"].append(item)

                collection["item"].append(folder)

            return Response(
                content=json.dumps(collection, indent=2),
                media_type="application/json",
                headers={
                    "Content-Disposition": 'attachment; filename="whitemagic.interfaces.api.postman_collection.json"',
                },
            )

    def add_openapi_json(self) -> None:
        """Add endpoint to download OpenAPI JSON."""

        @self.app.get("/docs/openapi.json", include_in_schema=False)
        async def get_openapi_json():
            """Return the OpenAPI specification as JSON."""
            return Response(
                content=json.dumps(self.app.openapi(), indent=2),
                media_type="application/json",
            )


def setup_interactive_docs(
    app: FastAPI,
    docs_url: str = "/docs",
    redoc_url: str = "/redoc",
    enable_postman: bool = True,
    enable_examples: bool = True,
) -> InteractiveDocumentation:
    """Set up interactive documentation for a FastAPI app."""
    docs = InteractiveDocumentation(app, docs_url, redoc_url)

    # Configure Swagger UI
    docs.setup_swagger_ui(
        title="WhiteMagic API",
        description="AI Memory Management API",
        version="2.0.0",
        oauth2_config={
            "useBasicAuthenticationWithAccessCodeGrant": False,
            "clientId": "your-client-id",
            "clientSecret": "your-client-secret",
            "realm": "your-realm",
            "appName": "WhiteMagic",
            "scopeSeparator": " ",
            "additionalQueryStringParams": {},
        },
    )

    # Configure ReDoc
    docs.setup_redoc(
        title="WhiteMagic API",
        description="AI Memory Management API",
        version="2.0.0",
    )

    # Add extras
    if enable_examples:
        docs.add_code_examples()

    if enable_postman:
        docs.add_postman_collection()

    docs.add_openapi_json()

    logger.info(f"Interactive documentation set up at {docs_url} and {redoc_url}")

    return docs


# Create documentation landing page
def create_docs_landing_page() -> str:
    """Create a landing page for documentation."""
    return """
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>WhiteMagic API Documentation</title>
        <style>
            * { margin: 0; padding: 0; box-sizing: border-box }
            body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif }
            .container { max-width: 1200px; margin: 0 auto; padding: 40px }
            .header { text-align: center; margin-bottom: 60px }
            .title { font-size: 3rem; color: #1f2937; margin-bottom: 20px }
            .subtitle { font-size: 1.5rem; color: #6b7280 }
            .cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 30px }
            .card { background: #f9fafb; border-radius: 12px; padding: 30px; text-align: center; transition: transform 0.2s }
            .card:hover { transform: translateY(-5px) }
            .card-icon { font-size: 3rem; margin-bottom: 20px }
            .card-title { font-size: 1.5rem; color: #1f2937; margin-bottom: 10px }
            .card-description { color: #6b7280; margin-bottom: 20px }
            .btn { display: inline-block; padding: 12px 24px; background: #3b82f6; color: white; text-decoration: none; border-radius: 8px; font-weight: 600 }
            .btn:hover { background: #2563eb }
            .btn-secondary { background: #e5e7eb; color: #1f2937 }
            .btn-secondary:hover { background: #d1d5db }
        </style>
    </head>
    <body>
        <div class="container">
            <div class="header">
                <h1 class="title">WhiteMagic API</h1>
                <p class="subtitle">AI Memory Management System Documentation</p>
            </div>

            <div class="cards">
                <div class="card">
                    <div class="card-icon">📚</div>
                    <h2 class="card-title">Swagger UI</h2>
                    <p class="card-description">Interactive API explorer with try-it-out functionality</p>
                    <a href="/docs" class="btn">Open Swagger UI</a>
                </div>

                <div class="card">
                    <div class="card-icon">📖</div>
                    <h2 class="card-title">ReDoc</h2>
                    <p class="card-description">Beautiful API documentation for reference</p>
                    <a href="/redoc" class="btn">Open ReDoc</a>
                </div>

                <div class="card">
                    <div class="card-icon">📦</div>
                    <h2 class="card-title">Postman</h2>
                    <p class="card-description">Download Postman collection for API testing</p>
                    <a href="/docs/postman" class="btn btn-secondary">Download Collection</a>
                </div>

                <div class="card">
                    <div class="card-icon">🔧</div>
                    <h2 class="card-title">OpenAPI Spec</h2>
                    <p class="card-description">Download the OpenAPI JSON specification</p>
                    <a href="/docs/openapi.json" class="btn btn-secondary">Download JSON</a>
                </div>
            </div>
        </div>
    </body>
    </html>
    """


if __name__ == "__main__":
    # Example usage
    if FASTAPI_AVAILABLE:
        app = FastAPI(title="WhiteMagic API")
        setup_interactive_docs(app)
        logger.info("Interactive documentation configured!")
    else:
        logger.info("FastAPI not available")
