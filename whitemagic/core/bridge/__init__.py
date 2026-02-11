"""Bridge Module — LEGACY MCP API dispatch layer.

.. deprecated:: 13.0.0
    ``core.bridge`` is the **v1 tool dispatch path** retained for backward
    compatibility.  All new tool development should target
    ``whitemagic.tools.handlers`` (the canonical v2 dispatch via
    ``unified_api.call_tool`` → ``dispatch_table``).

    This module will be removed in a future major release.
"""

import warnings as _warnings

_warnings.warn(
    "whitemagic.core.bridge is deprecated since v13.0.0. "
    "Use whitemagic.tools.handlers and whitemagic.tools.unified_api instead.",
    DeprecationWarning,
    stacklevel=2,
)
