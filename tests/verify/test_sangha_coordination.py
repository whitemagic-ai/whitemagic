"""
Verification - Sangha Coordination
Tests distributed locking and heartbeating between multiple simulated agents.
"""

import pytest
rich = pytest.importorskip("rich")

from whitemagic.gardens.sangha.resources import get_resources
from rich.console import Console
import time

console = Console()

def test_sangha_locking():
    console.print("\n--- Testing Sangha Coordination: Distributed Locking ---")
    res = get_resources()
    resource_id = "test_coordination_resource"
    
    # 1. Agent A acquires lock
    a_ok = res.acquire_lock(resource_id, "Agent-A", "Working on task X", ttl_seconds=2)
    if a_ok:
        console.print("[green]✅ Agent-A successfully acquired lock.[/]")
    else:
        console.print("[red]❌ Agent-A failed to acquire lock.[/]")
        return

    # 2. Agent B attempts to acquire same lock (should fail)
    b_fail = res.acquire_lock(resource_id, "Agent-B", "Attempting same task", ttl_seconds=2)
    if not b_fail:
        console.print("[green]✅ Agent-B correctly denied access to locked resource.[/]")
    else:
        console.print("[red]❌ Agent-B incorrectly acquired locked resource![/]")

    # 3. Agent A heartbeats to extend
    h_ok = res.heartbeat(resource_id, "Agent-A", ttl_seconds=5)
    if h_ok:
        console.print("[green]✅ Agent-A successfully sent heartbeat.[/]")
    else:
        console.print("[red]❌ Agent-A heartbeat failed.[/]")

    # 4. Wait for original TTL (2s) to pass, heartbeat should keep it alive
    console.print("Waiting for original TTL to pass...")
    time.sleep(2.5)
    
    b_fail_again = res.acquire_lock(resource_id, "Agent-B", "Attempting again", ttl_seconds=2)
    if not b_fail_again:
        console.print("[green]✅ Heartbeat kept lock alive; Agent-B still denied.[/]")
    else:
        console.print("[red]❌ Lock expired prematurely despite heartbeat![/]")

    # 5. Agent A releases
    res.release_lock(resource_id, "Agent-A")
    console.print("Agent-A released lock.")

    # 6. Agent B should now succeed
    b_success = res.acquire_lock(resource_id, "Agent-B", "Taking over task", ttl_seconds=2)
    if b_success:
        console.print("[green]✅ Agent-B successfully acquired released lock.[/]")
    else:
        console.print("[red]❌ Agent-B failed to acquire released lock.[/]")

    res.release_lock(resource_id, "Agent-B")
    console.print("--- Coordination Test Complete ---\n")

if __name__ == "__main__":
    test_sangha_locking()
