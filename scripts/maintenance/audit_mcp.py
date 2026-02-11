import sys
import os
import json
from pathlib import Path
from rich.console import Console

# Allow running from repo root without requiring installation.
REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from whitemagic.mcp_api_bridge import (
    archaeology_scan_directory,
    archaeology_stats,
    detect_patterns,
    run_kaizen_analysis
)

console = Console()

def run_audit():
    console.print("[bold green]Starting MCP Deep Audit...[/bold green]")
    
    # 1. Archaeology Scan of Root
    console.print("\n[bold]1. Scanning Root Directory (Archaeology)[/bold]")
    scan_results = archaeology_scan_directory(
        directory=str(REPO_ROOT),
        depth=2,
        recursive=False
    )
    console.print(json.dumps(scan_results, indent=2))
    
    # 2. Stats
    console.print("\n[bold]2. Archaeology Stats[/bold]")
    stats = archaeology_stats()
    console.print(json.dumps(stats, indent=2))

    # 3. Kaizen Analysis
    console.print("\n[bold]3. Kaizen Analysis (Auto-Fix Candidates)[/bold]")
    kaizen = run_kaizen_analysis(auto_fix=False)
    console.print(json.dumps(kaizen, indent=2))

if __name__ == "__main__":
    run_audit()
