# Chapter 23: Enduring Watch

**Gana**: OxGana (Chinese: 牛, Pinyin: Niú)
**Garden**: sangha
**Quadrant**: Northern (Black Tortoise)
**Element**: Water
**Phase**: Yin Peak
**I Ching Hexagram**: 52. 艮 Gèn (Keeping Still) - Steady watchfulness

---

## 🎯 Purpose

Chapter 23 maintains **enduring watch**—steady monitoring, reliable presence, patient vigilance. The Ox endures, watches, supports through all conditions. Like an ox plowing fields for seasons, sustained effort yields abundance.

Use this chapter when you need to:
- **Monitor systems** continuously over time
- **Maintain steady presence** during long work
- **Support community** (sangha) with reliability
- **Watch for anomalies** patiently
- **Endure through challenges** with resilience
- **Provide watchdog services** for critical systems
- **Sustain long-running processes** without fatigue

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `check_system_health` (continuous) | Ongoing monitoring | Watchdog mode |
| `manage_resonance` (monitor) | Network health watching | Sustained observation |
| `emit_event` | Heartbeat signals | Liveness indication |
| `manage_gardens` (activate) | Enter sangha garden | Community awareness |
| `track_metric` | Long-term trends | Historical monitoring |

---

## 📋 Workflow

### 1. Continuous Health Monitoring

Watchful presence over extended periods:

```python
import asyncio
from datetime import datetime, timedelta
from typing import Optional, Dict, Any
import logging

logger = logging.getLogger(__name__)

class ContinuousHealthMonitor:
    """Enduring health monitoring - the ox's steady watch"""

    def __init__(self, check_interval: int = 300):
        self.check_interval = check_interval  # 5 minutes default
        self.start_time = datetime.now()
        self.checks_performed = 0
        self.issues_detected = 0
        self.running = False

    async def watch_forever(self):
        """Maintain watch indefinitely"""
        self.running = True
        logger.info(f"🐂 Ox watch beginning (interval: {self.check_interval}s)")

        try:
            while self.running:
                # Perform health check
                await self._perform_check()

                # Wait for next interval
                await asyncio.sleep(self.check_interval)

        except asyncio.CancelledError:
            logger.info("🐂 Ox watch ending gracefully")
            raise

        finally:
            self._log_watch_summary()

    async def _perform_check(self):
        """Single health check iteration"""
        self.checks_performed += 1

        try:
            from whitemagic.tools import check_system_health

            # Check system health
            health = check_system_health(component="system")

            # Track status
            if health.get('status') != 'healthy':
                self.issues_detected += 1
                logger.warning(
                    f"⚠️ Ox detected issue (check #{self.checks_performed}): "
                    f"{health.get('status')}"
                )

                # Emit alert event
                from whitemagic.core.resonance import emit_event
                emit_event("health.degraded", {
                    "status": health.get('status'),
                    "check_number": self.checks_performed,
                    "issues": health.get('issues', [])
                })

            # Send heartbeat (silent)
            await self._send_heartbeat()

        except Exception as e:
            logger.error(f"Health check failed: {e}")
            self.issues_detected += 1

    async def _send_heartbeat(self):
        """Send liveness signal"""
        from whitemagic.core.resonance import emit_event

        emit_event("monitor.heartbeat", {
            "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            "checks_performed": self.checks_performed,
            "issues_detected": self.issues_detected
        })

    def _log_watch_summary(self):
        """Log summary when watch ends"""
        duration = datetime.now() - self.start_time
        logger.info(
            f"🐂 Ox watch summary: {self.checks_performed} checks over "
            f"{duration.total_seconds():.0f}s, {self.issues_detected} issues"
        )

    def stop(self):
        """Stop the watch"""
        self.running = False

# Usage - runs indefinitely until stopped
monitor = ContinuousHealthMonitor(check_interval=300)
# await monitor.watch_forever()
```

### 2. Circuit Breaker Pattern

Fail fast, recover gracefully:

```python
from enum import Enum
from datetime import datetime, timedelta
from collections import deque

class CircuitState(Enum):
    CLOSED = "closed"      # Operating normally
    OPEN = "open"          # Failing, reject requests
    HALF_OPEN = "half_open"  # Testing recovery

class CircuitBreaker:
    """Circuit breaker for resilient operations"""

    def __init__(
        self,
        failure_threshold: int = 5,
        recovery_timeout: int = 60,
        success_threshold: int = 2
    ):
        self.failure_threshold = failure_threshold
        self.recovery_timeout = recovery_timeout
        self.success_threshold = success_threshold

        self.state = CircuitState.CLOSED
        self.failure_count = 0
        self.success_count = 0
        self.last_failure_time: Optional[datetime] = None
        self.recent_calls = deque(maxlen=100)

    async def call(self, operation):
        """Execute operation through circuit breaker"""

        # Check if circuit should transition
        self._check_state_transition()

        if self.state == CircuitState.OPEN:
            raise Exception("Circuit breaker is OPEN - operation rejected")

        try:
            result = await operation()
            self._on_success()
            return result

        except Exception as e:
            self._on_failure()
            raise

    def _check_state_transition(self):
        """Check if state should change"""

        if self.state == CircuitState.OPEN:
            # Check if recovery timeout elapsed
            if self.last_failure_time:
                elapsed = (datetime.now() - self.last_failure_time).total_seconds()
                if elapsed >= self.recovery_timeout:
                    logger.info("Circuit breaker: OPEN → HALF_OPEN (testing recovery)")
                    self.state = CircuitState.HALF_OPEN
                    self.success_count = 0

    def _on_success(self):
        """Record successful operation"""
        self.recent_calls.append(("success", datetime.now()))

        if self.state == CircuitState.HALF_OPEN:
            self.success_count += 1
            if self.success_count >= self.success_threshold:
                logger.info("Circuit breaker: HALF_OPEN → CLOSED (recovered)")
                self.state = CircuitState.CLOSED
                self.failure_count = 0

    def _on_failure(self):
        """Record failed operation"""
        self.recent_calls.append(("failure", datetime.now()))
        self.failure_count += 1
        self.last_failure_time = datetime.now()

        if self.state == CircuitState.HALF_OPEN:
            logger.warning("Circuit breaker: HALF_OPEN → OPEN (recovery failed)")
            self.state = CircuitState.OPEN

        elif self.state == CircuitState.CLOSED:
            if self.failure_count >= self.failure_threshold:
                logger.error("Circuit breaker: CLOSED → OPEN (too many failures)")
                self.state = CircuitState.OPEN

# Usage
breaker = CircuitBreaker(failure_threshold=5, recovery_timeout=60)
# result = await breaker.call(risky_operation)
```

### 3. Long-Running Process Management

Sustain work over hours or days:

```python
import signal
from pathlib import Path
import json

class LongRunningProcess:
    """Manage long-duration processes with resilience"""

    def __init__(self, name: str):
        self.name = name
        self.start_time = datetime.now()
        self.should_stop = False
        self.checkpoints_dir = Path.home() / ".whitemagic" / "checkpoints"
        self.checkpoints_dir.mkdir(parents=True, exist_ok=True)

        # Setup graceful shutdown
        signal.signal(signal.SIGINT, self._handle_shutdown)
        signal.signal(signal.SIGTERM, self._handle_shutdown)

    async def run(self, work_func, checkpoint_interval: int = 1800):
        """Run long process with periodic checkpointing"""

        logger.info(f"🐂 Starting long-running process: {self.name}")
        iteration = 0
        last_checkpoint = datetime.now()

        try:
            while not self.should_stop:
                iteration += 1

                # Do work
                await work_func(iteration)

                # Checkpoint periodically (every 30 min)
                if (datetime.now() - last_checkpoint).total_seconds() >= checkpoint_interval:
                    await self._checkpoint(iteration)
                    last_checkpoint = datetime.now()

                # Brief pause between iterations
                await asyncio.sleep(1)

        except Exception as e:
            logger.error(f"Long-running process failed: {e}")
            await self._checkpoint(iteration, failed=True)
            raise

        finally:
            logger.info(
                f"🐂 Process {self.name} ending after {iteration} iterations, "
                f"duration: {(datetime.now() - self.start_time).total_seconds():.0f}s"
            )

    async def _checkpoint(self, iteration: int, failed: bool = False):
        """Save checkpoint"""
        checkpoint_file = self.checkpoints_dir / f"{self.name}_checkpoint.json"

        checkpoint_data = {
            "name": self.name,
            "iteration": iteration,
            "timestamp": datetime.now().isoformat(),
            "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            "failed": failed
        }

        checkpoint_file.write_text(json.dumps(checkpoint_data, indent=2))
        logger.info(f"Checkpoint saved: iteration {iteration}")

    def _handle_shutdown(self, signum, frame):
        """Handle graceful shutdown signals"""
        logger.info(f"Received shutdown signal ({signum}), stopping gracefully...")
        self.should_stop = True
```

---

## 🧭 Navigation

**Next**: [Chapter 24: Nurturing Profile](24_GIRL_NURTURE.md)
**Previous**: [Chapter 22: Well Stewardship](22_DIPPER_GOVERNANCE.md)
**Quadrant**: Northern (Winter/Water) - Position 2/7
