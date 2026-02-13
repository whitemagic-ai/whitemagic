#!/usr/bin/env python3
"""dream_overnight.py — Overnight Dream Runner for WhiteMagic v15.4.

Runs the Dream Cycle continuously in a terminal, cycling through all
8 phases repeatedly. Designed to be left running overnight on a laptop,
generating insights and pouring them into a dedicated Dream Galaxy.

Usage:
    python3 scripts/dream_overnight.py                    # Run with defaults
    python3 scripts/dream_overnight.py --cycles 50        # Run 50 cycles then stop
    python3 scripts/dream_overnight.py --interval 30      # 30s between phases
    python3 scripts/dream_overnight.py --galaxy dreams     # Store insights in 'dreams' galaxy
    python3 scripts/dream_overnight.py --forever           # Never stop (Ctrl+C to quit)

Each cycle produces a DreamReport which is:
  1. Logged to stdout with a timestamp
  2. Persisted as a memory in the Dream Galaxy (if --galaxy is set)
  3. Appended to a JSONL log file for post-analysis
"""

from __future__ import annotations

import argparse
import json
import os
import signal
import sys
import time
from datetime import datetime
from pathlib import Path

# Ensure whitemagic is importable
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
os.environ.setdefault("WM_SILENT_INIT", "1")

# ── Globals ──────────────────────────────────────────────────────────
_shutdown = False


def _handle_signal(sig: int, frame: object) -> None:
    global _shutdown
    _shutdown = True
    print(f"\n{'─'*60}")
    print("☀️  Waking up... graceful shutdown requested.")
    print(f"{'─'*60}")


signal.signal(signal.SIGINT, _handle_signal)
signal.signal(signal.SIGTERM, _handle_signal)


def _now() -> str:
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def _banner(args: argparse.Namespace) -> None:
    print(f"""
{'━'*60}
  🌙  WhiteMagic Overnight Dream Runner  v15.4
{'━'*60}
  Interval:  {args.interval}s between phases
  Max cycles: {'∞ (forever)' if args.forever else args.cycles}
  Galaxy:    {args.galaxy or '(default — no separate galaxy)'}
  Log file:  {args.log}
  Started:   {_now()}
{'━'*60}
""")


def _ensure_dream_galaxy(galaxy_name: str) -> bool:
    """Create the dream galaxy if it doesn't exist."""
    try:
        from whitemagic.core.memory.galaxy_manager import get_galaxy_manager
        gm = get_galaxy_manager()
        existing = gm.list_galaxies()
        if galaxy_name not in [g.name for g in existing]:
            gm.create_galaxy(
                name=galaxy_name,
                description="Overnight dream insights — auto-generated during sleep cycles",
                tags=["dream", "overnight", "auto"],
            )
            print(f"  ✨ Created dream galaxy: {galaxy_name}")
        else:
            print(f"  📡 Using existing galaxy: {galaxy_name}")
        return True
    except Exception as e:
        print(f"  ⚠️  Could not create dream galaxy: {e}")
        return False


def _persist_to_galaxy(galaxy_name: str, report: dict, cycle_num: int) -> None:
    """Store a dream report as a memory in the dream galaxy."""
    try:
        from whitemagic.core.memory.galaxy_manager import get_galaxy_manager
        gm = get_galaxy_manager()
        um = gm._get_memory(galaxy_name)

        phase = report.get("phase", "unknown")
        details = report.get("details", {})

        # Build a readable summary
        summary_parts = [f"Dream Cycle #{cycle_num} — Phase: {phase}"]
        for k, v in details.items():
            if isinstance(v, (int, float, str, bool)):
                summary_parts.append(f"  {k}: {v}")
            elif isinstance(v, dict):
                summary_parts.append(f"  {k}: {json.dumps(v, default=str)[:200]}")
            elif isinstance(v, list) and len(v) <= 5:
                summary_parts.append(f"  {k}: {v}")

        content = "\n".join(summary_parts)

        um.store(
            content=content,
            title=f"Dream #{cycle_num}: {phase}",
            tags={"dream_insight", "overnight", f"phase:{phase}", "auto_generated"},
            importance=0.3,
            metadata={
                "cycle_number": cycle_num,
                "phase": phase,
                "generated_at": _now(),
                "source": "dream_overnight.py",
            },
        )
    except Exception as e:
        print(f"    ⚠️  Persist to galaxy failed: {e}")


def run(args: argparse.Namespace) -> None:
    """Main run loop."""
    from whitemagic.core.dreaming.dream_cycle import DreamCycle, DreamPhase

    _banner(args)

    # Set up dream galaxy if requested
    galaxy_ok = False
    if args.galaxy:
        galaxy_ok = _ensure_dream_galaxy(args.galaxy)

    # Open log file
    log_path = Path(args.log)
    log_path.parent.mkdir(parents=True, exist_ok=True)
    log_f = open(log_path, "a")

    # Create a DreamCycle instance (we won't use its thread — we drive manually)
    dc = DreamCycle(idle_threshold_seconds=0, cycle_interval_seconds=0)
    # Force it into dreaming mode so _run_phase works
    dc._dreaming = True
    dc._last_activity = 0  # ancient timestamp = always idle

    phases = list(DreamPhase)
    total_cycles = 0
    phase_stats: dict[str, int] = {p.value: 0 for p in phases}

    print(f"  🌙 Entering dream state... ({len(phases)} phases per full cycle)\n")

    try:
        while not _shutdown:
            if not args.forever and total_cycles >= args.cycles * len(phases):
                break

            # Run next phase
            phase = phases[total_cycles % len(phases)]
            cycle_num = total_cycles // len(phases) + 1
            phase_idx = total_cycles % len(phases)

            if phase_idx == 0:
                print(f"{'─'*60}")
                print(f"  🔄 Full Cycle #{cycle_num}  [{_now()}]")
                print(f"{'─'*60}")

            start = time.perf_counter()
            print(f"  [{_now()}] Phase {phase_idx+1}/{len(phases)}: {phase.value:15s} ", end="", flush=True)

            try:
                dc._run_phase()
                report = dc._history[-1] if dc._history else None

                elapsed = time.perf_counter() - start
                details = report.details if report else {}

                # Compact summary
                highlights = []
                if isinstance(details, dict):
                    for k, v in details.items():
                        if isinstance(v, (int, float)) and v > 0:
                            highlights.append(f"{k}={v}")
                        elif k == "hints" and isinstance(v, list) and v:
                            highlights.append(f"hints={len(v)}")

                summary = ", ".join(highlights[:6]) if highlights else "ok"
                print(f"✓ {elapsed:.1f}s  [{summary}]")

                # Log to JSONL
                log_entry = {
                    "timestamp": _now(),
                    "cycle": cycle_num,
                    "phase": phase.value,
                    "elapsed_s": round(elapsed, 2),
                    "details": details if isinstance(details, dict) else str(details),
                }
                log_f.write(json.dumps(log_entry, default=str) + "\n")
                log_f.flush()

                # Persist to dream galaxy
                if galaxy_ok and args.galaxy and details:
                    _persist_to_galaxy(args.galaxy, log_entry, total_cycles)

                phase_stats[phase.value] += 1

            except Exception as e:
                elapsed = time.perf_counter() - start
                print(f"✗ {elapsed:.1f}s  [error: {e}]")

            total_cycles += 1

            # Sleep between phases
            if not _shutdown:
                time.sleep(args.interval)

    finally:
        log_f.close()

    # Final summary
    full_cycles = total_cycles // len(phases)
    print(f"\n{'━'*60}")
    print(f"  ☀️  Dream session complete")
    print(f"{'━'*60}")
    print(f"  Total phases run:  {total_cycles}")
    print(f"  Full cycles:       {full_cycles}")
    print(f"  Phase breakdown:")
    for phase_name, count in phase_stats.items():
        print(f"    {phase_name:15s}: {count}")
    print(f"  Log file:          {log_path}")
    if args.galaxy and galaxy_ok:
        print(f"  Dream galaxy:      {args.galaxy}")
    print(f"  Ended:             {_now()}")
    print(f"{'━'*60}\n")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="WhiteMagic Overnight Dream Runner — continuous dream cycling",
    )
    parser.add_argument(
        "--cycles", type=int, default=10,
        help="Number of full cycles to run (default: 10, each has 8 phases)",
    )
    parser.add_argument(
        "--interval", type=float, default=45.0,
        help="Seconds between dream phases (default: 45)",
    )
    parser.add_argument(
        "--galaxy", type=str, default="dreams",
        help="Galaxy name to persist dream insights into (default: 'dreams')",
    )
    parser.add_argument(
        "--log", type=str, default="dream_log.jsonl",
        help="Path to JSONL log file (default: dream_log.jsonl)",
    )
    parser.add_argument(
        "--forever", action="store_true",
        help="Run indefinitely until Ctrl+C",
    )
    args = parser.parse_args()
    run(args)


if __name__ == "__main__":
    main()
