"""OMS (Optimized Memory States) tool handlers (v15.2)."""
from typing import Any


def handle_oms_export(**kwargs: Any) -> dict[str, Any]:
    """Export a Galaxy as a .mem package."""
    from whitemagic.oms import get_oms_manager
    mgr = get_oms_manager()
    return mgr.export_galaxy(
        galaxy=kwargs.get("galaxy", "default"),
        output_path=kwargs.get("output_path", ""),
        topic=kwargs.get("topic", ""),
        description=kwargs.get("description", ""),
        price_xrp=float(kwargs.get("price_xrp", 0.0)),
        license_str=kwargs.get("license", "CC-BY-4.0"),
    )


def handle_oms_import(**kwargs: Any) -> dict[str, Any]:
    """Import a .mem package into a Galaxy."""
    from whitemagic.oms import get_oms_manager
    mgr = get_oms_manager()
    path = kwargs.get("path", "")
    if not path:
        return {"status": "error", "reason": "path is required"}
    return mgr.import_mem(
        path=path,
        galaxy=kwargs.get("galaxy", ""),
        verify_first=kwargs.get("verify", True),
    )


def handle_oms_inspect(**kwargs: Any) -> dict[str, Any]:
    """Preview contents of a .mem package."""
    from whitemagic.oms import get_oms_manager
    mgr = get_oms_manager()
    path = kwargs.get("path", "")
    if not path:
        return {"status": "error", "reason": "path is required"}
    return mgr.inspect(path)


def handle_oms_verify(**kwargs: Any) -> dict[str, Any]:
    """Verify Merkle integrity of a .mem package."""
    from whitemagic.oms import get_oms_manager
    mgr = get_oms_manager()
    path = kwargs.get("path", "")
    if not path:
        return {"status": "error", "reason": "path is required"}
    return mgr.verify(path)


def handle_oms_price(**kwargs: Any) -> dict[str, Any]:
    """Estimate value of a .mem package."""
    from whitemagic.oms import get_oms_manager
    mgr = get_oms_manager()
    path = kwargs.get("path", "")
    if not path:
        return {"status": "error", "reason": "path is required"}
    return mgr.price(path)


def handle_oms_list(**kwargs: Any) -> dict[str, Any]:
    """List available .mem packages."""
    from whitemagic.oms import get_oms_manager
    mgr = get_oms_manager()
    return mgr.list_local(search_dir=kwargs.get("directory", ""))


def handle_oms_status(**kwargs: Any) -> dict[str, Any]:
    """Get OMS system stats."""
    from whitemagic.oms import get_oms_manager
    return get_oms_manager().get_stats()
