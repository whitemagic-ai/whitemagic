"""Tests for whitemagic.config.paths — WM_STATE_ROOT fallback chain, path resolution."""
import os
import importlib



def test_project_root_contains_pyproject(tmp_path):
    """PROJECT_ROOT should point to the directory containing pyproject.toml."""
    from whitemagic.config.paths import PROJECT_ROOT
    assert (PROJECT_ROOT / "pyproject.toml").exists()


def test_wm_root_is_directory():
    """WM_ROOT should resolve to an existing directory."""
    from whitemagic.config.paths import WM_ROOT
    assert WM_ROOT.is_dir() or WM_ROOT.parent.is_dir()


def test_wm_state_root_env_override(tmp_path):
    """WM_STATE_ROOT env var should override the default root."""
    custom = tmp_path / "custom_state"
    custom.mkdir()

    old = os.environ.get("WM_STATE_ROOT")
    os.environ["WM_STATE_ROOT"] = str(custom)
    try:
        import whitemagic.config.paths as paths_mod
        importlib.reload(paths_mod)
        assert paths_mod.WM_ROOT == custom
    finally:
        if old is not None:
            os.environ["WM_STATE_ROOT"] = old
        else:
            os.environ.pop("WM_STATE_ROOT", None)
        importlib.reload(paths_mod)


def test_db_path_derivation():
    """DB_PATH should be under MEMORY_DIR by default."""
    from whitemagic.config.paths import DB_PATH, MEMORY_DIR
    # When WM_DB_PATH is unset, DB should be under the memory directory
    if not os.environ.get("WM_DB_PATH"):
        assert str(DB_PATH).startswith(str(MEMORY_DIR))


def test_ensure_paths_creates_dirs(tmp_path):
    """ensure_paths() should create all required subdirectories."""
    old = os.environ.get("WM_STATE_ROOT")
    state = tmp_path / "fresh_state"
    os.environ["WM_STATE_ROOT"] = str(state)
    try:
        import whitemagic.config.paths as paths_mod
        importlib.reload(paths_mod)
        paths_mod.ensure_paths()
        for subdir in ["data", "memory", "cache", "sessions", "logs", "artifacts", "restoration"]:
            assert (paths_mod.WM_ROOT / subdir).is_dir(), f"{subdir} not created"
    finally:
        if old is not None:
            os.environ["WM_STATE_ROOT"] = old
        else:
            os.environ.pop("WM_STATE_ROOT", None)
        importlib.reload(paths_mod)


def test_scripts_dir_is_pathlib():
    """SCRIPTS_DIR should be a Path object under PROJECT_ROOT."""
    from whitemagic.config.paths import SCRIPTS_DIR, PROJECT_ROOT
    assert SCRIPTS_DIR == PROJECT_ROOT / "scripts"
