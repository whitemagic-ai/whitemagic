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

# Each rule maps a stable substring -> expected count for that profile.
# We match by substring instead of full exact reason text to keep compatibility
# across pytest versions (reason preambles can vary).
EXPECTED_BY_PROFILE: dict[str, dict[str, int]] = {
    # Baseline CI profile installs only `.[dev]`.
    "baseline-dev": {
        "cvxpy not installed": 1,
        "fastapi/starlette not installed": 1,
        "FastAPI not installed": 2,
        "whitemagic_rs not installed": 4,
        "No module named 'whitemagic_rs'": 1,
        "Live network tests are opt-in.": 3,
    },
    # Optional extras lanes should not skip their targeted tests.
    "optional-api": {},
    "optional-opt": {},
    "optional-network": {},
}


def parse_skip_counts(path: str, expected_rules: dict[str, int]) -> tuple[dict[str, int], dict[str, int]]:
    counts: dict[str, int] = {rule: 0 for rule in expected_rules}
    unknown: dict[str, int] = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            m = SKIP_LINE_RE.match(line.strip())
            if not m:
                continue
            count = int(m.group(1))
            reason = m.group(2)
            matched = False
            for rule in expected_rules:
                if rule in reason:
                    counts[rule] += count
                    matched = True
                    break
            if not matched:
                unknown[reason] = unknown.get(reason, 0) + count
    return counts, unknown


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate pytest skip policy")
    parser.add_argument("report_path", help="Path to pytest output file")
    parser.add_argument(
        "--profile",
        default="baseline-dev",
        choices=sorted(EXPECTED_BY_PROFILE.keys()),
        help="Skip policy profile to enforce",
    )
    args = parser.parse_args()

    expected = EXPECTED_BY_PROFILE[args.profile]
    actual, unknown = parse_skip_counts(args.report_path, expected)

    unknown_reasons = sorted(unknown)
    missing_or_mismatch: list[str] = []
    for reason, exp_count in expected.items():
        got = actual.get(reason, 0)
        if got != exp_count:
            missing_or_mismatch.append(
                f"Reason mismatch: {reason!r} expected {exp_count}, got {got}"
            )

    if unknown_reasons:
        print("Unexpected skip reasons detected:")
        for reason in unknown_reasons:
            print(f"  - {reason!r}: {unknown[reason]}")

    if missing_or_mismatch:
        print("Expected skip reasons/counts mismatch:")
        for msg in missing_or_mismatch:
            print(f"  - {msg}")

    if unknown_reasons or missing_or_mismatch:
        return 1

    total = sum(actual.values())
    print(f"Skip policy OK for profile={args.profile}. Total skipped: {total}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
