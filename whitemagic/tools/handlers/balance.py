"""Yin-Yang balance tool handlers."""
from typing import Any, cast


def handle_record_yin_yang_activity(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.harmony.yin_yang_tracker import get_tracker
    activity = kwargs.get("activity", "READ")
    tracker = get_tracker()
    metrics = tracker.record_activity(activity)
    return {
        "success": True,
        "balance_score": metrics.balance_score,
        "burnout_risk": metrics.burnout_risk,
        "recommendation": metrics.recommendation,
        "yang_ratio": metrics.yang_ratio,
        "yin_ratio": metrics.yin_ratio,
    }


def handle_get_yin_yang_balance(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.harmony.yin_yang_tracker import get_tracker
    report = dict(cast("dict[str, Any]", get_tracker().get_report()))
    # Rename tracker's "status" to avoid collision with envelope "status"
    if "status" in report:
        report["balance_status"] = report.pop("status")
    return {"status": "success", **report}


def handle_harmony_vector(**kwargs: Any) -> dict[str, Any]:
    """Return the full multi-dimensional Harmony Vector snapshot."""
    from whitemagic.harmony.vector import get_harmony_vector
    hv = get_harmony_vector()
    snap = hv.snapshot()
    return {
        "status": "success",
        "harmony_vector": snap.to_dict(),
    }

