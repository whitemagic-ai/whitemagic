import os
import sys
import tempfile

import pytest

# ---------------------------------------------------------------------------
# CRITICAL: Force test isolation BEFORE any whitemagic module is imported.
# paths.py evaluates DB_PATH / WM_ROOT at module-level on first import.
# If WM_STATE_ROOT already points to the real 107K-memory DB, every test
# that triggers get_unified_memory() will load the full production DB +
# holographic index, causing multi-minute hangs.
#
# We unconditionally override WM_STATE_ROOT to a fresh temp dir so that
# paths.py (and every singleton that depends on it) resolves to an empty DB.
# ---------------------------------------------------------------------------
_TEST_STATE_ROOT = tempfile.mkdtemp(prefix="wm_pytest_state_")
os.environ["WM_STATE_ROOT"] = _TEST_STATE_ROOT
os.environ["WM_SILENT_INIT"] = "1"
# Skip heavy holographic index loading during tests
os.environ["WM_SKIP_HOLO_INDEX"] = "1"

# If paths.py was already imported (e.g. by a plugin), force-reload it so
# the module-level constants pick up the new WM_STATE_ROOT.
if "whitemagic.config.paths" in sys.modules:
    import importlib
    importlib.reload(sys.modules["whitemagic.config.paths"])

# Ensure all subdirectories (memory/, data/, cache/, etc.) exist on disk.
# Without this, SQLite cannot create the DB file in CI where ~/.whitemagic
# doesn't pre-exist.
from whitemagic.config.paths import ensure_paths
ensure_paths()


def _reset_singletons():
    """Reset all known singletons so each test session starts clean."""
    _singleton_modules = [
        # --- Memory subsystem ---
        ("whitemagic.core.memory.unified", "_unified_memory"),
        ("whitemagic.core.memory.galactic_map", "_map_instance"),
        ("whitemagic.core.memory.consolidation", "_consolidator"),
        ("whitemagic.core.memory.lifecycle", "_manager"),
        ("whitemagic.core.memory.mindful_forgetting", "_forgetting"),
        ("whitemagic.core.memory.holographic", "_holographic_memory"),
        ("whitemagic.core.memory.constellations", "_detector_instance"),
        ("whitemagic.core.memory.association_miner", "_miner_instance"),
        ("whitemagic.core.memory.session_crystallizer", "_crystallizer"),
        # v14.0 Living Graph
        ("whitemagic.core.memory.graph_walker", "_walker"),
        ("whitemagic.core.memory.graph_engine", "_engine"),
        ("whitemagic.core.memory.surprise_gate", "_gate"),
        ("whitemagic.core.memory.bridge_synthesizer", "_synthesizer"),
        # --- Resonance / scheduling ---
        ("whitemagic.core.resonance.salience_arbiter", "_arbiter"),
        ("whitemagic.core.resonance.temporal_scheduler", "_scheduler"),
        ("whitemagic.core.resonance.gan_ying_enhanced", "_bus"),
        # --- Harmony / governance ---
        ("whitemagic.harmony.vector", "_harmony_vector"),
        ("whitemagic.harmony.homeostatic_loop", "_loop"),
        ("whitemagic.harmony.anomaly_detector", "_detector"),
        ("whitemagic.dharma.karma_ledger", "_ledger"),
        # --- Tools / dispatch ---
        ("whitemagic.tools.dependency_graph", "_graph"),
        ("whitemagic.tools.circuit_breaker", "_registry"),
        ("whitemagic.tools.rate_limiter", "_instance"),
        ("whitemagic.tools.tool_permissions", "_registry_instance"),
        ("whitemagic.tools.sandbox", "_sandbox"),
        # --- Intelligence ---
        ("whitemagic.core.intelligence.knowledge_graph", "_kg"),
        ("whitemagic.core.intelligence.bicameral", "_reasoner"),
        ("whitemagic.core.intelligence.emotion_drive", "_drive"),
        ("whitemagic.core.intelligence.self_model", "_model"),
        # --- Dreaming ---
        ("whitemagic.core.dreaming.dream_cycle", "_dream_cycle"),
        # --- Mesh ---
        ("whitemagic.mesh.awareness", "_awareness"),
    ]
    for mod_name, attr_name in _singleton_modules:
        mod = sys.modules.get(mod_name)
        if mod and hasattr(mod, attr_name):
            setattr(mod, attr_name, None)


@pytest.fixture(autouse=True)
def _reset_all_singletons():
    """Auto-reset singletons before each test to prevent cross-test leakage."""
    _reset_singletons()
    yield
    _reset_singletons()


@pytest.fixture
def fresh_state_root(tmp_path):
    """Provide a fresh WM_STATE_ROOT for tests that need total isolation."""
    old = os.environ.get("WM_STATE_ROOT")
    state_dir = tmp_path / "wm_state"
    state_dir.mkdir()
    os.environ["WM_STATE_ROOT"] = str(state_dir)
    yield state_dir
    if old is not None:
        os.environ["WM_STATE_ROOT"] = old
    else:
        os.environ.pop("WM_STATE_ROOT", None)


@pytest.fixture
def tool_caller():
    """Convenience wrapper for call_tool with assertion helpers."""
    from whitemagic.tools.unified_api import call_tool

    class ToolCaller:
        def __call__(self, tool_name: str, **kwargs):
            return call_tool(tool_name, **kwargs)

        def ok(self, tool_name: str, **kwargs):
            result = call_tool(tool_name, **kwargs)
            assert result["status"] == "success", f"{tool_name} failed: {result.get('message')}"
            return result

        def err(self, tool_name: str, **kwargs):
            result = call_tool(tool_name, **kwargs)
            assert result["status"] == "error", f"{tool_name} unexpectedly succeeded"
            return result

    return ToolCaller()


ENVELOPE_KEYS = {
    "status", "tool", "request_id", "idempotency_key", "message",
    "error_code", "details", "retryable", "writes", "artifacts",
    "metrics", "side_effects", "warnings", "timestamp",
    "envelope_version", "tool_contract_version",
}


def assert_envelope_shape(out: dict) -> None:
    """Assert that a tool response has the correct envelope shape."""
    import json
    missing = ENVELOPE_KEYS.difference(out.keys())
    assert not missing, f"missing envelope keys: {sorted(missing)}"
    assert isinstance(out["status"], str)
    assert isinstance(out["tool"], str)
    assert isinstance(out["request_id"], str)
    assert isinstance(out["details"], dict)
    # Must always be JSON-serializable.
    json.dumps(out)
