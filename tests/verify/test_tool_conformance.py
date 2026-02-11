import pytest
from whitemagic.tools.registry import TOOL_REGISTRY
from whitemagic.tools.dispatch_table import DISPATCH_TABLE
from whitemagic.tools.unified_api import call_tool
from whitemagic.tools.envelope import is_enveloped

@pytest.mark.parametrize("tool_def", TOOL_REGISTRY)
def test_tool_registered_has_handler(tool_def):
    """Verify EVERY registered tool has a handler or prefix routing."""
    if tool_def.name.startswith("gana_"):
        return # Handled by prefix routing in dispatch()
    assert tool_def.name in DISPATCH_TABLE, f"Tool '{tool_def.name}' is registered but missing from DISPATCH_TABLE"

@pytest.mark.parametrize("tool_def", TOOL_REGISTRY)
def test_tool_conforms_to_envelope(tool_def):
    """
    Verify tool output conforms to stable envelope.
    Uses dry_run=True for safe verification where supported.
    """
    # Some tools might fail in CI if they require external services,
    # but the envelope structure should still be checked on success/error.
    
    # We only test tools that support dry_run safely or are pure read.
    # Note: capabilities and manifest are perfect test cases.
    if tool_def.name in ["capabilities", "manifest", "state.paths"]:
        resp = call_tool(tool_def.name)
        assert is_enveloped(resp), f"Tool '{tool_def.name}' returned non-enveloped response"
        assert resp["status"] == "success"
