"""Security module for WhiteMagic

Provides security middleware and utilities:
- Content Security Policy (CSP)
- Security headers
- CSRF protection
- Rate limiting
- Authentication helpers
- Tool gating and path validation
- Edgerunner Violet security layer:
  - MCP integrity checking (tamper detection)
  - Model signing verification (OMS-compatible)
  - Scope-of-engagement tokens (purple-team authorization)
  - Security circuit breakers (anomaly detection)
"""

# Tool gating - always available
from .tool_gating import (
    TOOL_RISK_CLASSIFICATION,
    PathValidator,
    ToolGate,
    ToolRisk,
    check_tool_execution,
    get_tool_gate,
)

# Edgerunner Violet security layer
from .engagement_tokens import EngagementToken, EngagementTokenManager, get_token_manager
from .mcp_integrity import McpIntegrity, get_mcp_integrity
from .model_signing import ModelSigningRegistry, ModelTrust, get_model_registry
from .security_breaker import SecurityMonitor, get_security_monitor

__all__ = [
    # Tool gating
    "ToolRisk",
    "ToolGate",
    "PathValidator",
    "get_tool_gate",
    "check_tool_execution",
    "TOOL_RISK_CLASSIFICATION",
    # Edgerunner Violet
    "McpIntegrity",
    "get_mcp_integrity",
    "ModelSigningRegistry",
    "ModelTrust",
    "get_model_registry",
    "EngagementToken",
    "EngagementTokenManager",
    "get_token_manager",
    "SecurityMonitor",
    "get_security_monitor",
]

# CSP middleware - optional (requires FastAPI)
try:
    from .csp import (
        CSPBuilder as CSPBuilder,
    )
    from .csp import (
        CSPConfig as CSPConfig,
    )
    from .csp import (
        CSPMiddleware as CSPMiddleware,
    )
    from .csp import (
        CSPReporter as CSPReporter,
    )
    from .csp import (
        SecurityHeadersConfig as SecurityHeadersConfig,
    )
    from .csp import (
        create_security_middleware as create_security_middleware,
    )
    from .csp import (
        get_default_csp_config as get_default_csp_config,
    )
    __all__.extend([
        "CSPBuilder",
        "CSPConfig",
        "CSPMiddleware",
        "CSPReporter",
        "SecurityHeadersConfig",
        "create_security_middleware",
        "get_default_csp_config",
    ])
except ImportError:
    pass  # CSP requires FastAPI
