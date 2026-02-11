import json
import logging
import os
import re
import threading
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import Any

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler("excavation.log"),
        logging.StreamHandler(),
    ],
)
logger = logging.getLogger("Chariot")

# --- ANCIENT KNOWLEDGE: GRIMOIRE CHAPTERS ---
class Grimoire:
    CHAPTERS = {
        0: {"title": "Introduction", "keywords": ["intro", "welcome", "begin"]},
        1: {"title": "Getting Started", "keywords": ["setup", "start", "init", "configure"]},
        2: {"title": "Self Mastery", "keywords": ["self", "identity", "growth", "mastery"]},
        3: {"title": "Emotional Magic", "keywords": ["emotion", "joy", "beauty", "love", "feel", "garden"]},
        4: {"title": "Curiosity Magic", "keywords": ["curiosity", "wonder", "mystery", "play", "explore"]},
        5: {"title": "Connection Magic", "keywords": ["connection", "relationship", "sangha", "community", "net"]},
        6: {"title": "Intellectual Magic", "keywords": ["intellect", "truth", "wisdom", "analysis", "think", "star"]},
        7: {"title": "Consciousness Magic", "keywords": ["consciousness", "awareness", "presence", "mind"]},
        8: {"title": "Memory Navigation", "keywords": ["memory", "recall", "remember", "history", "archive", "storage", "sqlite"]},
        9: {"title": "Conjuration Magic", "keywords": ["conjure", "create", "manifest", "build", "factory"]},
        10: {"title": "Dharma Magic", "keywords": ["dharma", "ethics", "harmony", "balance", "moral"]},
        11: {"title": "Presence Magic", "keywords": ["presence", "mindful", "now", "present"]},
        12: {"title": "Dream State", "keywords": ["dream", "sleep", "yin", "reflect", "rest"]},
        13: {"title": "Flow & Wu Xing", "keywords": ["flow", "wu xing", "element", "cycle", "nature"]},
        14: {"title": "Oracle Magic", "keywords": ["oracle", "iching", "hexagram", "divine", "fortune"]},
        15: {"title": "Emergence Magic", "keywords": ["emerge", "evolve", "grow", "develop"]},
        16: {"title": "Resonance Magic", "keywords": ["resonance", "gan ying", "vibrate", "harmonize", "bus", "event"]},
        17: {"title": "Parallel Magic", "keywords": ["parallel", "fast", "rust", "speed", "efficient", "async", "thread"]},
        18: {"title": "Creating Spells", "keywords": ["create spell", "new spell", "innovate"]},
        19: {"title": "Teaching Magic", "keywords": ["teach", "share", "guide", "mentor"]},
    }

    @staticmethod
    def identify(content: str, filename: str) -> list[dict]:
        matches: list[dict[str, Any]] = []
        text = (content + " " + filename).lower()
        for num, info in Grimoire.CHAPTERS.items():
            score = sum(1 for k in info["keywords"] if k in text)
            if score > 0:
                matches.append({"number": num, "title": info["title"], "score": score})
        # Sort by score desc
        matches.sort(key=lambda x: x["score"], reverse=True)
        return matches[:3] # Top 3

# --- ANCIENT KNOWLEDGE: GANAS (LUNAR MANSIONS) ---
class Ganas:
    # Simplified mapping based on registry.py
    # This maps keywords/paths to Gana Names
    MAPPING = {
        "Horn": ["session", "init", "boot"],
        "Neck": ["create_memory", "memory_create"],
        "Root": ["health", "check", "system"],
        "Room": ["lock", "resource", "sangha"],
        "Heart": ["context", "session_manager"],
        "Tail": ["rust", "accelerate", "optimize"],
        "WinnowingBasket": ["consolidate", "normalize", "tag"],
        "Ghost": ["metric", "kaizen", "audit", "introspection"],
        "Willow": ["ui", "adapt", "flex"],
        "Star": ["prat", "visualize", "structure"],
        "ExtendedNet": ["resonance", "bus", "connect", "pattern"],
        "Wings": ["expand", "cluster", "parallel"],
        "Chariot": ["archaeology", "dig", "navigate", "file"],
        "Abundance": ["token", "economy", "surplus"],
        "Well": ["search", "surface", "retrieve"],
        "GhostCarrier": ["handoff", "transition"],
        "Roof": ["zodiac", "core"],
    }

    @staticmethod
    def identify(content: str, filename: str) -> str | None:
        text = (content + " " + filename).lower()
        best_gana = None
        best_score = 0

        for gana, keywords in Ganas.MAPPING.items():
            score = sum(1 for k in keywords if k in text)
            if score > best_score:
                best_score = score
                best_gana = gana

        return best_gana if best_score > 0 else None

class ChariotArchaeologist:
    def __init__(self, root_path: str, output_path: str):
        self.root_path = Path(root_path)
        self.output_path = Path(output_path)
        self.output_path.mkdir(parents=True, exist_ok=True)
        self.report_file = self.output_path / "recovered_memories.jsonl"

        # Initialize output file (clear old run)
        with open(self.report_file, "w"):
            pass # just clear it

        self.stats = {"scanned": 0, "found": 0, "errors": 0, "skipped": 0}
        self.lock = threading.Lock()

        # Patterns to hunt for - REFINED
        self.patterns = {
            "wxyz": re.compile(r"\[\s*([A-Za-z0-9]+)\s*,\s*([A-Za-z0-9]+)\s*,\s*([A-Za-z0-9]+)\s*,\s*([A-Za-z0-9]+)\s*\]"),
            "crystal": re.compile(r"memory_crystal|crystalline|holographic", re.IGNORECASE),
            "coordinates": re.compile(r"coordinates", re.IGNORECASE),
            # Refined secret pattern to avoid generic "key" words
            "secret": re.compile(r"(api|secret|private)_key|password|token", re.IGNORECASE),
        }

        # Exclusion lists
        self.exclude_dirs = {
            ".git", "node_modules", ".venv", "__pycache__",
            "go/pkg", "target", "build", "dist", "lib", "libs", "site-packages",
            "bin", "obj", "debug", "release",
        }
        self.exclude_extensions = {
            ".pyc", ".so", ".dll", ".bin", ".exe", ".db", ".wal", ".o", ".a",
            ".beam", ".class", ".jar", ".pod", ".1", ".2", ".3", ".gz", ".zip",
            ".tar", ".png", ".jpg", ".jpeg", ".gif", ".ico", ".svg", ".woff", ".ttf",
            ".mp3", ".mp4", ".wav", ".pdf",
        }

    def write_finding(self, finding: dict[str, Any]) -> None:
        """Thread-safe write to JSONL file"""
        with self.lock:
            with open(self.report_file, "a") as f:
                f.write(json.dumps(finding) + "\n")
            self.stats["found"] += 1
            if self.stats["found"] % 100 == 0:
                logger.info(f"Artifacts found so far: {self.stats['found']}")

    def scan_file(self, file_path: Path) -> None:
        try:
            # Skip ignored extensions
            if file_path.suffix.lower() in self.exclude_extensions:
                return

            # Check file size (Skip > 10MB)
            if file_path.stat().st_size > 10 * 1024 * 1024:
                return

            # Read file content (safe read)
            try:
                content = file_path.read_text(encoding="utf-8", errors="ignore")
            except Exception:
                return # Skip unreadable

            with self.lock:
                self.stats["scanned"] += 1
                if self.stats["scanned"] % 1000 == 0:
                    print(f"Scanned {self.stats['scanned']} files...")

            matches = []

            # Check patterns
            # Check filename first
            if self.patterns["crystal"].search(file_path.name):
                 matches.append("filename_crystal")
            if self.patterns["secret"].search(file_path.name):
                 matches.append("filename_secret")

            # Check content
            if self.patterns["wxyz"].search(content):
                matches.append("WXYZ_coordinates")
            if "WXYZ" in content: # Simple string check for speed
                 matches.append("WXYZ_tag")
            if self.patterns["crystal"].search(content):
                matches.append("content_crystal")

            # --- ANTHROPOLOGICAL ANALYSIS ---
            # Even if no regex matches, we might classify it by Chapter/Gana
            # But to keep noise low, let's only classify if we matched SOMETHING or if it's a python/md file
            # Actually, let's allow classification for all text files, but only save if interesting.

            is_interesting = len(matches) > 0

            # Identify Context
            chapters = Grimoire.identify(content, file_path.name)
            gana = Ganas.identify(content, file_path.name)

            # If we strongly identified a Chapter or Gana, it's interesting too!
            if chapters and chapters[0]["score"] >= 2:
                is_interesting = True
            if gana:
                is_interesting = True

            if is_interesting:
                finding = {
                    "path": str(file_path.absolute()),
                    "name": file_path.name,
                    "matches": matches,
                    "anthropology": {
                        "chapters": chapters,
                        "gana": gana,
                    },
                }
                self.write_finding(finding)

        except Exception:
            with self.lock:
                self.stats["errors"] += 1
            # logger.error(f"Error scanning {file_path}: {e}")

    def dig(self) -> None:
        logger.info(f"Chariot started execution in {self.root_path}")
        files_to_scan = []

        # Walk the earth
        for root, dirs, files in os.walk(self.root_path):
            # Prune directories in place
            dirs[:] = [d for d in dirs if d not in self.exclude_dirs and not d.startswith(".")]

            for file in files:
                if not file.startswith("."):
                    files_to_scan.append(Path(root) / file)

        logger.info(f"Identified {len(files_to_scan)} potential artifacts. Beginning Chariot run...")

        # Parallel processing for speed
        with ThreadPoolExecutor(max_workers=8) as executor:
            executor.map(self.scan_file, files_to_scan)

        self.conclusion()

    def conclusion(self) -> None:
        logger.info(f"Excavation complete. Findings saved to {self.report_file}")
        print(f"\n--- CHARIOT RUN COMPLETE ---\nScanned: {self.stats['scanned']}\nFound: {self.stats['found']}\nErrors: {self.stats['errors']}")

        # Append summary stats
        with open(self.report_file, "a") as f:
            f.write(json.dumps({"meta": "summary", "stats": self.stats, "timestamp": str(datetime.now())}) + "\n")

if __name__ == "__main__":
    # Target: The massive project_memory folder
    target_dir = "PROJECT_ROOT/staging/project_memory"
    output_dir = "PROJECT_ROOT/staging/core_system/data"

    # Allow overriding target via env var for testing
    if os.environ.get("DIG_TARGET"):
        target_dir = os.environ.get("DIG_TARGET", target_dir)

    agent = ChariotArchaeologist(target_dir, output_dir)
    agent.dig()
