"""Physical truth verification tool handlers.

Provides structured attestation tools for bridging the digital-physical divide.
See docs/PHYSICAL_TRUTH_ORACLE.md for the full concept.
"""
from datetime import datetime
from typing import Any


def handle_verification_request(**kwargs: Any) -> dict[str, Any]:
    """Create a structured request for physical-world verification.

    This stores a memory tagged with 'physical_verification' and 'pending'
    that can be searched by humans or QA agents who perform physical checks.
    """
    from whitemagic.tools.unified_api import call_tool

    claim = kwargs.get("claim", "")
    if not claim:
        return {"status": "error", "message": "claim is required — describe what needs physical verification"}

    location = kwargs.get("location", "unspecified")
    deadline = kwargs.get("deadline", "")
    evidence_required = kwargs.get("evidence_required", "photo + attestor signature")
    priority = kwargs.get("priority", "normal")

    importance = {"critical": 1.0, "high": 0.95, "normal": 0.85, "low": 0.7}.get(priority, 0.85)

    content = (
        f"PHYSICAL VERIFICATION NEEDED\n"
        f"{'=' * 40}\n"
        f"Claim: {claim}\n"
        f"Location: {location}\n"
        f"Deadline: {deadline or 'No deadline set'}\n"
        f"Evidence required: {evidence_required}\n"
        f"Priority: {priority}\n"
        f"Requested: {datetime.now().isoformat()}\n"
    )

    tags = ["physical_verification", "pending", f"priority_{priority}"]
    if location != "unspecified":
        tags.append(f"location_{location.lower().replace(' ', '_')[:30]}")

    result = call_tool(
        "create_memory",
        title=f"VERIFY: {claim[:80]}",
        content=content,
        tags=tags,
        importance=importance,
    )

    verification_id = result.get("details", result).get("memory_id", "unknown")

    return {
        "status": "success",
        "verification_id": verification_id,
        "claim": claim,
        "state": "pending",
        "message": f"Verification request created. A human or QA agent must verify and attest using verification.attest with id={verification_id}",
    }


def handle_verification_attest(**kwargs: Any) -> dict[str, Any]:
    """Record a human or sensor attestation for a physical verification claim.

    This creates a 'confirmed' or 'denied' memory linked to the original request.
    """
    from whitemagic.tools.unified_api import call_tool

    verification_id = kwargs.get("verification_id", kwargs.get("id", ""))
    if not verification_id:
        return {"status": "error", "message": "verification_id is required — the ID from verification.request"}

    outcome = kwargs.get("outcome", "confirmed")
    if outcome not in ("confirmed", "denied", "partial", "inconclusive"):
        return {"status": "error", "message": f"outcome must be one of: confirmed, denied, partial, inconclusive (got: {outcome})"}

    attestor = kwargs.get("attestor", kwargs.get("attested_by", "unknown"))
    evidence = kwargs.get("evidence", "")
    notes = kwargs.get("notes", "")

    content = (
        f"PHYSICAL VERIFICATION ATTESTATION\n"
        f"{'=' * 40}\n"
        f"Original request: {verification_id}\n"
        f"Outcome: {outcome.upper()}\n"
        f"Attestor: {attestor}\n"
        f"Evidence: {evidence or 'None provided'}\n"
        f"Notes: {notes or 'None'}\n"
        f"Attested: {datetime.now().isoformat()}\n"
    )

    tags = [
        "physical_verification",
        outcome,
        f"attested_by_{attestor.lower().replace(' ', '_')[:30]}",
        f"ref_{verification_id[:20]}",
    ]

    importance = 1.0 if outcome == "confirmed" else 0.9

    result = call_tool(
        "create_memory",
        title=f"ATTESTED ({outcome.upper()}): ref {verification_id[:20]}",
        content=content,
        tags=tags,
        importance=importance,
    )

    # Record in karma ledger as a physical truth event
    try:
        from whitemagic.dharma.karma_ledger import get_karma_ledger
        ledger = get_karma_ledger()
        ledger.record(
            tool="verification.attest",
            declared_safety="READ",
            actual_writes=0,
            success=outcome == "confirmed",
        )
    except Exception:
        pass  # Karma recording is best-effort

    return {
        "status": "success",
        "attestation_id": result.get("details", result).get("memory_id", "unknown"),
        "verification_id": verification_id,
        "outcome": outcome,
        "attestor": attestor,
        "message": f"Attestation recorded: {outcome}",
    }


def handle_verification_status(**kwargs: Any) -> dict[str, Any]:
    """Check the status of physical verification claims — pending, confirmed, denied."""
    from whitemagic.tools.unified_api import call_tool

    state_filter = kwargs.get("state", "all")
    limit = int(kwargs.get("limit", 20))

    tags = ["physical_verification"]
    if state_filter != "all":
        tags.append(state_filter)

    result = call_tool("search_memories", query="physical verification", limit=limit)

    claims: list[dict[str, Any]] = []
    details = result.get("details", result)
    memories = details.get("memories", details.get("results", []))

    for m in memories:
        content = m.get("content", "")
        mem_tags = m.get("tags", [])
        if isinstance(mem_tags, str):
            mem_tags = [mem_tags]

        # Determine state from tags
        state = "unknown"
        for s in ("pending", "confirmed", "denied", "partial", "inconclusive"):
            if s in mem_tags or f"physical_verification_{s}" in mem_tags:
                state = s
                break

        if state_filter != "all" and state != state_filter:
            continue

        claims.append({
            "id": m.get("id", "unknown"),
            "title": m.get("title", content[:60]),
            "state": state,
            "content_preview": content[:150],
        })

    return {
        "status": "success",
        "filter": state_filter,
        "count": len(claims),
        "claims": claims,
    }
