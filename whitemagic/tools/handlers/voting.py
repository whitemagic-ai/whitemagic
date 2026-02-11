"""Voting tool handlers — AI ensemble voting and consensus analysis.

Allows multiple AI agents to vote on solutions, with confidence scoring
and consensus detection.  All state is stored under WM_STATE_ROOT/votes/.
"""
import json
import os
import re
from datetime import datetime
from pathlib import Path
from typing import Any, cast
from uuid import uuid4


def _emit(event_type_name: str, data: dict) -> None:
    """Best-effort Gan Ying event emission."""
    try:
        from whitemagic.core.resonance import emit_event
        emit_event(event_type_name, data, source="voting")
    except Exception:
        pass


def _votes_dir() -> Path:
    """Return the votes directory under WM_STATE_ROOT."""
    from whitemagic.config.paths import WM_ROOT
    d = cast("Path", WM_ROOT) / "votes"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _vote_session_dir(session_id: str) -> Path:
    d = _votes_dir() / session_id
    d.mkdir(parents=True, exist_ok=True)
    return d


def _load_vote_session(session_id: str) -> dict[str, Any]:
    meta_path = _vote_session_dir(session_id) / "meta.json"
    if not meta_path.exists():
        raise FileNotFoundError(f"Vote session not found: {session_id}")
    return cast("dict[str, Any]", json.loads(meta_path.read_text(encoding="utf-8")))


def _save_vote_session(session: dict[str, Any]) -> None:
    session_id = session["id"]
    meta_path = _vote_session_dir(session_id) / "meta.json"
    meta_path.write_text(json.dumps(session, indent=2), encoding="utf-8")


# ---------------------------------------------------------------------------
# Confidence extraction (ported from Tools voting system)
# ---------------------------------------------------------------------------

def _extract_confidence(text: str) -> int:
    """Extract confidence percentage from vote text."""
    patterns = [
        r"confidence[:\s]+(\d+)%",
        r"(\d+)%\s+confiden",
        r"confidence[:\s]+(\d+)",
        r"(\d+)\s*(?:out of\s*)?(?:/\s*)?100",
    ]
    for pattern in patterns:
        match = re.search(pattern, text.lower())
        if match:
            val = int(match.group(1))
            if 0 <= val <= 100:
                return val
    return 70  # default if not specified


def _analyze_votes(votes: list[dict[str, Any]]) -> dict[str, Any]:
    """Analyze votes and determine consensus."""
    if not votes:
        return {
            "winner": None,
            "avg_confidence": 0,
            "consensus_strength": "none",
            "total_voters": 0,
            "rankings": [],
        }

    sorted_votes = sorted(votes, key=lambda x: x.get("confidence", 0), reverse=True)
    confidences = [v.get("confidence", 0) for v in sorted_votes]
    avg_confidence = sum(confidences) / len(confidences)

    spread = max(confidences) - min(confidences)
    if spread <= 10:
        strength = "strong"
    elif spread <= 25:
        strength = "moderate"
    else:
        strength = "weak"

    rankings = [
        {"voter": v.get("voter", "unknown"), "confidence": v.get("confidence", 0)}
        for v in sorted_votes
    ]

    return {
        "winner": sorted_votes[0] if sorted_votes else None,
        "avg_confidence": round(avg_confidence, 1),
        "consensus_strength": strength,
        "total_voters": len(votes),
        "rankings": rankings,
    }


# ---------------------------------------------------------------------------
# Handler: vote.create
# ---------------------------------------------------------------------------

def handle_vote_create(**kwargs: Any) -> dict[str, Any]:
    """Create a new voting session for a problem/question."""
    problem = kwargs.get("problem")
    if not problem:
        return {"status": "error", "error": "problem is required"}

    task_type = kwargs.get("task_type", "general")
    options = kwargs.get("options", [])

    session_id = f"vote-{uuid4().hex[:8]}"
    now = datetime.now().isoformat()

    session = {
        "id": session_id,
        "problem": problem,
        "task_type": task_type,
        "options": options,
        "votes": [],
        "status": "open",
        "created_at": now,
        "created_by": os.uname().nodename,
    }
    _save_vote_session(session)

    _emit("VOTE_SESSION_CREATED", {"session_id": session_id, "problem": problem[:120]})

    return {
        "status": "success",
        "message": f"Voting session {session_id} created",
        "session_id": session_id,
        "problem": problem,
    }


# ---------------------------------------------------------------------------
# Handler: vote.cast
# ---------------------------------------------------------------------------

def handle_vote_cast(**kwargs: Any) -> dict[str, Any]:
    """Cast a vote in a voting session."""
    session_id = kwargs.get("session_id")
    if not session_id:
        return {"status": "error", "error": "session_id is required"}

    voter = kwargs.get("voter")
    if not voter:
        return {"status": "error", "error": "voter is required"}

    solution = kwargs.get("solution", "")
    confidence = kwargs.get("confidence")
    reasoning = kwargs.get("reasoning", "")
    risks = kwargs.get("risks", "")

    try:
        session = _load_vote_session(session_id)
    except FileNotFoundError:
        return {"status": "error", "error": f"Vote session {session_id} not found", "error_code": "not_found"}

    if session.get("status") != "open":
        return {"status": "error", "error": f"Voting session is {session.get('status')}, not open"}

    # Auto-extract confidence from solution text if not provided
    if confidence is None:
        confidence = _extract_confidence(solution)

    vote = {
        "voter": voter,
        "solution": solution,
        "confidence": confidence,
        "reasoning": reasoning,
        "risks": risks,
        "cast_at": datetime.now().isoformat(),
    }

    session["votes"].append(vote)
    _save_vote_session(session)

    # Also save individual vote file for reference
    vote_file = _vote_session_dir(session_id) / f"vote_{voter}.json"
    vote_file.write_text(json.dumps(vote, indent=2), encoding="utf-8")

    _emit("VOTE_CAST", {"session_id": session_id, "voter": voter, "confidence": confidence})

    return {
        "status": "success",
        "message": f"Vote cast by {voter} (confidence: {confidence}%)",
        "vote": vote,
        "total_votes": len(session["votes"]),
    }


# ---------------------------------------------------------------------------
# Handler: vote.analyze
# ---------------------------------------------------------------------------

def handle_vote_analyze(**kwargs: Any) -> dict[str, Any]:
    """Analyze votes and determine consensus for a session."""
    session_id = kwargs.get("session_id")
    if not session_id:
        return {"status": "error", "error": "session_id is required"}

    close_session = kwargs.get("close", False)

    try:
        session = _load_vote_session(session_id)
    except FileNotFoundError:
        return {"status": "error", "error": f"Vote session {session_id} not found", "error_code": "not_found"}

    votes = session.get("votes", [])
    analysis = _analyze_votes(votes)

    if close_session:
        session["status"] = "closed"
        session["closed_at"] = datetime.now().isoformat()
        session["analysis"] = analysis
        _save_vote_session(session)
        _emit("VOTE_SESSION_CLOSED", {"session_id": session_id, "consensus": analysis["consensus_strength"]})

    if analysis.get("consensus_strength") == "strong":
        _emit("VOTE_CONSENSUS_REACHED", {"session_id": session_id, "winner": analysis.get("winner", {}).get("voter")})

    # Save analysis results
    results_file = _vote_session_dir(session_id) / "results.json"
    results_file.write_text(json.dumps({
        "session_id": session_id,
        "problem": session.get("problem"),
        "analysis": analysis,
        "analyzed_at": datetime.now().isoformat(),
    }, indent=2), encoding="utf-8")

    return {
        "status": "success",
        "session_id": session_id,
        "problem": session.get("problem"),
        "analysis": analysis,
        "session_status": session.get("status"),
    }


# ---------------------------------------------------------------------------
# Handler: vote.list
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Handler: vote.record_outcome
# ---------------------------------------------------------------------------

def handle_vote_record_outcome(**kwargs: Any) -> dict[str, Any]:
    """Record whether the winning solution from a vote actually worked."""
    session_id = kwargs.get("session_id")
    if not session_id:
        return {"status": "error", "error": "session_id is required"}

    success = kwargs.get("success", True)
    notes = kwargs.get("notes", "")

    try:
        session = _load_vote_session(session_id)
    except FileNotFoundError:
        return {"status": "error", "error": f"Vote session {session_id} not found", "error_code": "not_found"}

    # Record outcome
    session["outcome"] = {
        "success": success,
        "notes": notes,
        "recorded_at": datetime.now().isoformat(),
    }
    _save_vote_session(session)

    # Update per-voter accuracy in a ledger file
    accuracy_file = _votes_dir() / "voter_accuracy.json"
    accuracy = {}
    if accuracy_file.exists():
        try:
            accuracy = json.loads(accuracy_file.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError):
            accuracy = {}

    for vote in session.get("votes", []):
        voter = vote.get("voter", "unknown")
        if voter not in accuracy:
            accuracy[voter] = {"total_votes": 0, "outcomes_tracked": 0, "correct": 0}
        accuracy[voter]["outcomes_tracked"] += 1

        # The winner gets credit/blame for the outcome
        analysis = session.get("analysis", {})
        winner = analysis.get("winner", {})
        if isinstance(winner, dict) and winner.get("voter") == voter:
            if success:
                accuracy[voter]["correct"] += 1

    accuracy_file.write_text(json.dumps(accuracy, indent=2), encoding="utf-8")

    return {
        "status": "success",
        "message": f"Outcome recorded for {session_id}: {'success' if success else 'failure'}",
        "outcome": session["outcome"],
        "voter_accuracy": {v: round(d["correct"] / max(d["outcomes_tracked"], 1), 3)
                           for v, d in accuracy.items() if d["outcomes_tracked"] > 0},
    }


# ---------------------------------------------------------------------------
# Handler: vote.list
# ---------------------------------------------------------------------------

def handle_vote_list(**kwargs: Any) -> dict[str, Any]:
    """List voting sessions."""
    status_filter = kwargs.get("filter_status")
    limit = kwargs.get("limit", 20)

    vdir = _votes_dir()
    sessions = []
    for d in sorted(vdir.iterdir(), reverse=True):
        if not d.is_dir():
            continue
        meta = d / "meta.json"
        if not meta.exists():
            continue
        try:
            s = json.loads(meta.read_text(encoding="utf-8"))
            if status_filter and s.get("status") != status_filter:
                continue
            sessions.append({
                "id": s["id"],
                "problem": s.get("problem", "")[:100],
                "status": s.get("status"),
                "vote_count": len(s.get("votes", [])),
                "created_at": s.get("created_at"),
            })
        except (json.JSONDecodeError, OSError, KeyError):
            continue
        if len(sessions) >= limit:
            break

    return {
        "status": "success",
        "count": len(sessions),
        "sessions": sessions,
    }
