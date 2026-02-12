"""Continuity Suite - Grounding System for AI Agents
Provides real-time context awareness: Time, System Stats, Resource Usage, and Session Metadata.
"""

import json
import logging
import os
import platform
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import psutil

logger = logging.getLogger(__name__)

@dataclass
class SystemState:
    timestamp_utc: str
    timestamp_local: str
    unix_time: float
    uptime_seconds: float
    cpu_percent: float
    memory_percent: float
    disk_usage_percent: float
    system_load: list[float]
    active_user: str
    hostname: str
    platform: str
    python_version: str

class GroundingSystem:
    def __init__(self, storage_dir: str = "PROJECT_ROOT/whitemagic/memory/continuity"):
        self.storage_dir = Path(storage_dir)
        self.storage_dir.mkdir(parents=True, exist_ok=True)
        self.current_session_id = f"session_{int(time.time())}"
        self.boot_time = psutil.boot_time()

    def get_system_state(self) -> SystemState:
        """Capture current system reality."""
        now = datetime.now(timezone.utc)
        local_now = datetime.now()

        load_avg = [0.0, 0.0, 0.0]
        if hasattr(os, "getloadavg"):
            load_avg = list(os.getloadavg())

        return SystemState(
            timestamp_utc=now.isoformat(),
            timestamp_local=local_now.isoformat(),
            unix_time=time.time(),
            uptime_seconds=time.time() - self.boot_time,
            cpu_percent=psutil.cpu_percent(interval=0.1),
            memory_percent=psutil.virtual_memory().percent,
            disk_usage_percent=psutil.disk_usage("/").percent,
            system_load=load_avg,
            active_user=os.getlogin(),
            hostname=platform.node(),
            platform=platform.platform(),
            python_version=platform.python_version(),
        )

    def anchor_self(self) -> dict[str, Any]:
        """Generate a 'Grounding Anchor' - a high-density context object
        for an AI to orient itself immediately.
        """
        state = self.get_system_state()
        anchor = {
            "reality_check": {
                "time": {
                    "utc": state.timestamp_utc,
                    "local": state.timestamp_local,
                    "human_readable": datetime.now().strftime("%A, %B %d, %Y at %I:%M:%S %p"),
                },
                "host": {
                    "name": state.hostname,
                    "platform": state.platform,
                    "resources": {
                        "cpu": f"{state.cpu_percent}%",
                        "ram": f"{state.memory_percent}%",
                        "disk": f"{state.disk_usage_percent}%",
                    },
                },
            },
            "session": {
                "id": self.current_session_id,
                "start_time": self.boot_time, # Session start approx
            },
            "environment": {
                "cwd": os.getcwd(),
                "user": state.active_user,
            },
        }

        # Save anchor to disk for other processes
        self._save_anchor(anchor)
        return anchor

    def _save_anchor(self, anchor: dict[str, Any]) -> None:
        """Persist the anchor to a known location (the 'Lighthouse')."""
        lighthouse_path = self.storage_dir / "lighthouse.json"
        with open(lighthouse_path, "w") as f:
            json.dump(anchor, f, indent=2)

        # Also append to daily log
        day_str = datetime.now().strftime("%Y-%m-%d")
        log_path = self.storage_dir / f"continuity_log_{day_str}.jsonl"
        with open(log_path, "a") as f:
            f.write(json.dumps(anchor) + "\n")

    def print_grounding(self) -> None:
        """Print a human/AI readable grounding summary."""
        anchor = self.anchor_self()
        r = anchor["reality_check"]
        t = r["time"]
        h = r["host"]

        logger.info("\n⚓ CONTINUITY GROUNDING ANCHOR ⚓")
        logger.info("================================")
        logger.info(f"📅 Current Time: {t['human_readable']}")
        logger.info(f"🌍 UTC:          {t['utc']}")
        logger.info(f"🖥️  System:       {h['name']} ({h['platform']})")
        logger.info(f"📊 Resources:    CPU: {h['resources']['cpu']} | RAM: {h['resources']['ram']} | Disk: {h['resources']['disk']}")
        logger.info(f"👤 User:         {anchor['environment']['user']}")
        logger.info(f"📂 CWD:          {anchor['environment']['cwd']}")
        logger.info("================================\n")

if __name__ == "__main__":
    grounding = GroundingSystem()
    grounding.print_grounding()
