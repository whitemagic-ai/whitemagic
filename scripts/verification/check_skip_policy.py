#!/usr/bin/env python3
"""Validate pytest skip reasons/counts for a given CI profile.

Usage:
    python scripts/verification/check_skip_policy.py /tmp/pytest.out --profile baseline-dev
"""

from __future__ import annotations

import argparse
import re
import sys


SKIP_LINE_RE = re.compile(r"^SKIPPED \[(\d+)\] .*: (.+)$")

# Known skip reason substrings per profile.  Any skip whose reason text
# contains one of these substrings is considered expected.  Only *unknown*
# reasons (not matching any substring) cause a hard failure.  This avoids
# exact-count fragility when tests are added or removed.
KNOWN_REASONS_BY_PROFILE: dict[str, list[str]] = {
    # Baseline CI profile installs only `.[dev]`.
    "baseline-dev": [
        "cvxpy not installed",
        "fastapi/starlette not installed",
        "FastAPI not installed",
        "whitemagic_rs not installed",
        "No module named 'whitemagic_rs'",
        "Live network tests are opt-in.",
        "could not import 'numpy'",
        "could not import 'rich'",
    ],
    # Optional extras — tolerate their specific skip reasons.
    "optional-api": [
        "FastAPI not installed",
        "fastapi/starlette not installed",
    ],
    "optional-opt": [],
    "optional-network": [],
}


def parse_skips(path: str, known_reasons: list[str]) -> tuple[int, dict[str, int]]:
    """Return (total_known, {unknown_reason: count})."""
    total_known = 0
    unknown: dict[str, int] = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            m = SKIP_LINE_RE.match(line.strip())
            if not m:
                continue
            count = int(m.group(1))
            reason = m.group(2)
            if any(k in reason for k in known_reasons):
                total_known += count
            else:
                unknown[reason] = unknown.get(reason, 0) + count
    return total_known, unknown


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate pytest skip policy")
    parser.add_argument("report_path", help="Path to pytest output file")
    parser.add_argument(
        "--profile",
        default="baseline-dev",
        choices=sorted(KNOWN_REASONS_BY_PROFILE.keys()),
        help="Skip policy profile to enforce",
    )
    args = parser.parse_args()

    known = KNOWN_REASONS_BY_PROFILE[args.profile]
    total_known, unknown = parse_skips(args.report_path, known)

    if unknown:
        print("Unexpected skip reasons detected:")
        for reason in sorted(unknown):
            print(f"  - {reason!r}: {unknown[reason]}")
        return 1

    print(f"Skip policy OK for profile={args.profile}. Known skipped: {total_known}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
