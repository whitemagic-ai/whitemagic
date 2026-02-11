# Chapter 17: Energy Management

**Gana**: StomachGana (Chinese: 胃, Pinyin: Wèi)  
**Garden**: healing  
**Quadrant**: Western (White Tiger)  
**Element**: Metal  
**Phase**: Yin Rising  
**I Ching Hexagram**: 27. 頤 Yí (Nourishment) - Proper sustenance

---

## 🎯 Purpose

Chapter 17 manages **energy and nourishment**—balancing yin/yang, ensuring sustainable work pace, healing when needed. The Stomach nourishes the system.

Use this chapter when you need to:
- **Balance yin/yang energy**
- **Prevent burnout**
- **Restore depleted resources**
- **Manage sustainable pace**
- **Heal from over-exertion**

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `analyze_wu_xing_phase` | Check current phase balance | Energy assessment |
| `manage_gardens` (activate) | Enter healing garden | Restoration mode |

---

## 📋 Workflow

### 1. Monitor System Harmony (Embodiment Layer)

WhiteMagic v5.0 includes embodiment awareness—AI that knows its hardware.

**Purpose**: Monitor real-time system health to adapt workload based on available energy.

**When to use**:
- Before starting resource-intensive operations
- During long-running processes to check if degradation needed
- When detecting performance issues or crashes
- To implement adaptive rate limiting

**Implementation**:

```python
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional
from datetime import datetime, timedelta
import psutil
import asyncio

from whitemagic.core.memory import MemoryManager
from whitemagic.tools import manage_gardens, emit_event


class GunaState(Enum):
    """Three gunas (qualities) of system temperament."""
    SATTVA = "sattva"  # Clarity, balance, harmony
    RAJAS = "rajas"    # Activity, passion, stress
    TAMAS = "tamas"    # Inertia, darkness, exhaustion


class ThermalState(Enum):
    """System thermal condition."""
    OK = "ok"        # Normal temperature
    WARM = "warm"    # Elevated but acceptable
    HOT = "hot"      # Approaching thermal limits
    CRITICAL = "critical"  # Thermal throttling likely


@dataclass
class HarmonySnapshot:
    """Point-in-time system harmony measurement."""
    timestamp: datetime
    harmony_score: float  # 0.0-1.0
    guna_tag: GunaState

    # Raw metrics
    cpu_percent: float
    memory_percent: float
    thermal_state: ThermalState
    io_wait_percent: float

    # Derived flags
    is_harmonious: bool

    # Recommendations
    recommended_max_workers: int
    recommended_batch_size: int
    should_defer_work: bool


@dataclass
class HarmonyHistory:
    """Track harmony over time for trend analysis."""
    snapshots: List[HarmonySnapshot] = field(default_factory=list)
    max_history: int = 100

    def add(self, snapshot: HarmonySnapshot):
        """Add snapshot, maintaining max history size."""
        self.snapshots.append(snapshot)
        if len(self.snapshots) > self.max_history:
            self.snapshots.pop(0)

    def get_trend(self, minutes: int = 5) -> str:
        """
        Analyze recent trend.

        Returns: "improving", "stable", "degrading"
        """
        if len(self.snapshots) < 2:
            return "stable"

        cutoff = datetime.now() - timedelta(minutes=minutes)
        recent = [s for s in self.snapshots if s.timestamp >= cutoff]

        if len(recent) < 2:
            return "stable"

        # Calculate average score change
        scores = [s.harmony_score for s in recent]
        avg_change = (scores[-1] - scores[0]) / len(scores)

        if avg_change > 0.05:
            return "improving"
        elif avg_change < -0.05:
            return "degrading"
        else:
            return "stable"

    def get_average_score(self, minutes: int = 5) -> float:
        """Get average harmony score over time window."""
        if not self.snapshots:
            return 0.5

        cutoff = datetime.now() - timedelta(minutes=minutes)
        recent = [s for s in self.snapshots if s.timestamp >= cutoff]

        if not recent:
            return self.snapshots[-1].harmony_score

        return sum(s.harmony_score for s in recent) / len(recent)


class HarmonyMonitor:
    """
    Monitor system harmony (embodiment awareness).

    Tracks CPU, RAM, thermal, I/O to calculate composite harmony score.
    Provides recommendations for adaptive workload management.
    """

    # Weights for harmony score calculation
    WEIGHTS = {
        "cpu": 0.40,
        "memory": 0.30,
        "thermal": 0.20,
        "io": 0.10
    }

    # Thresholds for guna classification
    GUNA_THRESHOLDS = {
        "sattva": 0.8,  # >= 0.8 = sattva
        "rajas": 0.5,   # 0.5-0.8 = rajas
        # < 0.5 = tamas
    }

    def __init__(self, check_interval_seconds: float = 1.0):
        self.check_interval = check_interval_seconds
        self.history = HarmonyHistory()
        self.memory = MemoryManager()
        self.healing = manage_gardens(action="get", garden_name="healing")

        self._monitoring_task: Optional[asyncio.Task] = None
        self._is_monitoring = False

    def get_current(self) -> Dict[str, any]:
        """
        Get current harmony snapshot.

        Returns dict with all metrics and recommendations.
        """
        snapshot = self._measure_harmony()
        self.history.add(snapshot)

        # Emit harmony measurement event
        emit_event("harmony.measured", {
            "score": snapshot.harmony_score,
            "guna": snapshot.guna_tag.value,
            "cpu": snapshot.cpu_percent,
            "memory": snapshot.memory_percent,
            "thermal": snapshot.thermal_state.value
        })

        # Convert to dict for compatibility
        return {
            "harmony_score": snapshot.harmony_score,
            "guna_tag": snapshot.guna_tag.value,
            "is_harmonious": snapshot.is_harmonious,
            "cpu_percent": snapshot.cpu_percent,
            "memory_percent": snapshot.memory_percent,
            "thermal_state": snapshot.thermal_state.value,
            "io_wait_percent": snapshot.io_wait_percent,
            "recommended_max_workers": snapshot.recommended_max_workers,
            "recommended_batch_size": snapshot.recommended_batch_size,
            "should_defer_work": snapshot.should_defer_work,
            "trend": self.history.get_trend()
        }

    def _measure_harmony(self) -> HarmonySnapshot:
        """Take point-in-time harmony measurement."""
        # Gather raw metrics
        cpu_percent = psutil.cpu_percent(interval=0.1)
        memory = psutil.virtual_memory()
        memory_percent = memory.percent
        io_counters = psutil.disk_io_counters()

        # Calculate I/O wait (approximation)
        io_wait_percent = min(100.0, (io_counters.read_time + io_counters.write_time) / 1000.0)

        # Thermal state (simplified - in production would read hwmon)
        thermal_state = self._assess_thermal(cpu_percent)

        # Calculate component scores (0.0-1.0, higher = better)
        cpu_score = max(0.0, 1.0 - (cpu_percent / 100.0))
        memory_score = max(0.0, 1.0 - (memory_percent / 100.0))
        thermal_score = self._thermal_to_score(thermal_state)
        io_score = max(0.0, 1.0 - (io_wait_percent / 100.0))

        # Weighted harmony score
        harmony_score = (
            cpu_score * self.WEIGHTS["cpu"] +
            memory_score * self.WEIGHTS["memory"] +
            thermal_score * self.WEIGHTS["thermal"] +
            io_score * self.WEIGHTS["io"]
        )

        # Classify guna
        if harmony_score >= self.GUNA_THRESHOLDS["sattva"]:
            guna = GunaState.SATTVA
        elif harmony_score >= self.GUNA_THRESHOLDS["rajas"]:
            guna = GunaState.RAJAS
        else:
            guna = GunaState.TAMAS

        # Generate recommendations
        is_harmonious = harmony_score >= 0.7
        recommended_max_workers = self._recommend_workers(harmony_score)
        recommended_batch_size = self._recommend_batch_size(harmony_score)
        should_defer_work = harmony_score < 0.3

        return HarmonySnapshot(
            timestamp=datetime.now(),
            harmony_score=harmony_score,
            guna_tag=guna,
            cpu_percent=cpu_percent,
            memory_percent=memory_percent,
            thermal_state=thermal_state,
            io_wait_percent=io_wait_percent,
            is_harmonious=is_harmonious,
            recommended_max_workers=recommended_max_workers,
            recommended_batch_size=recommended_batch_size,
            should_defer_work=should_defer_work
        )

    def _assess_thermal(self, cpu_percent: float) -> ThermalState:
        """
        Assess thermal state.

        In production would read from hwmon sensors.
        Here we approximate based on CPU usage.
        """
        if cpu_percent < 50:
            return ThermalState.OK
        elif cpu_percent < 75:
            return ThermalState.WARM
        elif cpu_percent < 90:
            return ThermalState.HOT
        else:
            return ThermalState.CRITICAL

    def _thermal_to_score(self, thermal: ThermalState) -> float:
        """Convert thermal state to score (0.0-1.0)."""
        mapping = {
            ThermalState.OK: 1.0,
            ThermalState.WARM: 0.7,
            ThermalState.HOT: 0.3,
            ThermalState.CRITICAL: 0.0
        }
        return mapping[thermal]

    def _recommend_workers(self, harmony_score: float) -> int:
        """Recommend max worker count based on harmony."""
        if harmony_score >= 0.8:
            return 8  # Full capacity
        elif harmony_score >= 0.6:
            return 4  # Moderate
        elif harmony_score >= 0.4:
            return 2  # Conservative
        else:
            return 1  # Minimal

    def _recommend_batch_size(self, harmony_score: float) -> int:
        """Recommend batch size based on harmony."""
        if harmony_score >= 0.8:
            return 100  # Large batches
        elif harmony_score >= 0.6:
            return 50
        elif harmony_score >= 0.4:
            return 20
        else:
            return 10  # Small batches

    async def start_monitoring(self):
        """Start continuous harmony monitoring in background."""
        if self._is_monitoring:
            return

        self._is_monitoring = True
        self._monitoring_task = asyncio.create_task(self._monitor_loop())

        emit_event("harmony.monitoring_started", {
            "interval_seconds": self.check_interval
        })

    async def stop_monitoring(self):
        """Stop continuous monitoring."""
        self._is_monitoring = False
        if self._monitoring_task:
            self._monitoring_task.cancel()
            try:
                await self._monitoring_task
            except asyncio.CancelledError:
                pass

        emit_event("harmony.monitoring_stopped", {})

    async def _monitor_loop(self):
        """Continuous monitoring loop."""
        while self._is_monitoring:
            try:
                state = self.get_current()

                # Check for critical conditions
                if state["guna_tag"] == "tamas":
                    emit_event("harmony.critical", {
                        "score": state["harmony_score"],
                        "recommendation": "Activate healing garden and defer work"
                    })

                await asyncio.sleep(self.check_interval)

            except asyncio.CancelledError:
                break
            except Exception as e:
                emit_event("harmony.monitoring_error", {"error": str(e)})
                await asyncio.sleep(self.check_interval)


def get_harmony_monitor() -> HarmonyMonitor:
    """Get singleton harmony monitor instance."""
    if not hasattr(get_harmony_monitor, "_instance"):
        get_harmony_monitor._instance = HarmonyMonitor()
    return get_harmony_monitor._instance


# Example usage
async def example_harmony_monitoring():
    """Demonstrate harmony monitoring and adaptation."""
    monitor = get_harmony_monitor()

    # Start continuous monitoring
    await monitor.start_monitoring()

    # Check current state
    state = monitor.get_current()
    print(f"System harmony: {state['harmony_score']:.2f}")
    print(f"Guna: {state['guna_tag']}")
    print(f"Trend: {state['trend']}")

    # Adapt workload based on harmony
    if state['is_harmonious']:
        print("✅ System balanced - proceed with intensive work")
        max_workers = state['recommended_max_workers']
        batch_size = state['recommended_batch_size']

        # Process with recommended settings
        # await process_heavy_workload(max_workers=max_workers, batch_size=batch_size)
    else:
        print("⚠️ System stressed - use lighter operations or wait")

        if state['should_defer_work']:
            print("⛔ Deferring non-critical work until harmony improves")
            # await defer_work_until_harmony_restored()

    # Stop monitoring when done
    await monitor.stop_monitoring()
```

**Harmony Vector Components:**
- **CPU load** (40% weight): Current processor usage
- **RAM availability** (30% weight): Free memory
- **Thermal state** (20% weight): System temperature
- **I/O wait** (10% weight): Disk/network blocking

**Harmony Score Interpretation:**
- `0.8-1.0`: **Sattva** (clarity, balance) - Full capacity available
- `0.5-0.8`: **Rajas** (activity, stress) - Moderate workload
- `0.0-0.5`: **Tamas** (inertia, exhaustion) - Reduce intensity

### 2. Wu Xing Phase Analysis

Check which of the Five Elements phase your work is in:

```python
from whitemagic.tools import analyze_wu_xing_phase

phase = analyze_wu_xing_phase(
    operation="analyze",
    description="Heavy async processing with database writes"
)

print(f"Phase: {phase['element']}")  # Wood/Fire/Earth/Metal/Water
print(f"Energy state: {phase['energy_state']}")
print(f"Recommendation: {phase['recommendation']}")
```

**Five Elements Energy Mapping:**

| Element | Phase | Energy Type | Activities | Balance Needs |
|---------|-------|-------------|------------|---------------|
| **Wood** | Spring | Yang Rising | Growth, creation, new features | Don't overextend |
| **Fire** | Summer | Yang Peak | Peak activity, async processing | Prevent burnout |
| **Earth** | Transition | Balanced | Consolidation, refactoring | Maintain stability |
| **Metal** | Autumn | Yin Rising | Harvesting, optimization | Let go of excess |
| **Water** | Winter | Yin Peak | Rest, planning, deep work | Restore reserves |

### 3. Resource Pool Management

Manage connection pools and resource allocation based on energy state:

```python
from whitemagic.core.pools import ResourcePool
from whitemagic.core.embodiment import get_harmony_monitor

async def adaptive_pool_sizing():
    """Adjust pool size based on system harmony."""
    monitor = get_harmony_monitor()
    state = monitor.get_current()

    if state['harmony_score'] > 0.8:
        # System is healthy - full pool size
        pool_size = 20
    elif state['harmony_score'] > 0.5:
        # Moderate stress - reduce pool
        pool_size = 10
    else:
        # High stress - minimal pool
        pool_size = 5

    pool = ResourcePool(
        resource_type="database_connection",
        min_size=pool_size // 2,
        max_size=pool_size
    )

    return pool
```

### 4. Graceful Degradation

When energy is low, degrade gracefully rather than crashing.

**Purpose**: Adapt task execution quality based on available system resources, avoiding crashes.

**When to use**:
- Long-running processes that can operate at different quality levels
- Background tasks that can be deferred during high load
- Services that need to stay responsive under stress
- Any workload that shouldn't crash when resources are low

**Implementation**:

```python
from dataclasses import dataclass
from enum import Enum
from typing import Any, Awaitable, Callable, Dict, List, Optional
import asyncio

from whitemagic.core.embodiment import get_harmony_monitor
from whitemagic.tools import emit_event


class TaskPriority(Enum):
    """Task priority levels."""
    CRITICAL = "critical"  # Must execute regardless
    HIGH = "high"          # Should execute unless tamas
    NORMAL = "normal"      # Execute in sattva/rajas
    LOW = "low"            # Only execute in sattva
    BACKGROUND = "background"  # Only when idle


class ExecutionMode(Enum):
    """Task execution quality modes."""
    FULL = "full"          # Maximum quality, all features
    BALANCED = "balanced"  # Good quality, optimize for speed
    MINIMAL = "minimal"    # Basic quality, fastest execution
    DEFERRED = "deferred"  # Not executed, queued for later


@dataclass
class TaskResult:
    """Result of task execution."""
    success: bool
    mode: ExecutionMode
    duration_ms: float
    output: Optional[Any] = None
    error: Optional[str] = None
    deferred_reason: Optional[str] = None


@dataclass
class DeferredTask:
    """Task deferred for later execution."""
    task_fn: Callable[..., Awaitable[Any]]
    args: tuple
    kwargs: dict
    priority: TaskPriority
    deferred_at: float
    reason: str


class GracefulTaskExecutor:
    """
    Execute tasks with graceful degradation based on system harmony.

    Adapts execution quality (full/balanced/minimal) or defers work
    based on available energy.
    """

    def __init__(self):
        self.monitor = get_harmony_monitor()
        self.deferred_queue: List[DeferredTask] = []
        self._processing_deferred = False

    async def execute(
        self,
        task_fn: Callable[..., Awaitable[Any]],
        *args,
        priority: TaskPriority = TaskPriority.NORMAL,
        **kwargs
    ) -> TaskResult:
        """
        Execute task with graceful degradation.

        Args:
            task_fn: Async function to execute
            *args: Positional arguments for task_fn
            priority: Task priority level
            **kwargs: Keyword arguments for task_fn

        Returns:
            TaskResult with execution details
        """
        import time
        start = time.time()

        # Get current system state
        state = self.monitor.get_current()
        guna = state["guna_tag"]
        harmony_score = state["harmony_score"]

        # Determine execution mode based on harmony and priority
        mode = self._determine_mode(guna, harmony_score, priority)

        emit_event("task.execution_started", {
            "priority": priority.value,
            "guna": guna,
            "harmony": harmony_score,
            "mode": mode.value
        })

        # Handle deferred execution
        if mode == ExecutionMode.DEFERRED:
            reason = f"System in {guna} state, harmony={harmony_score:.2f}"
            self.deferred_queue.append(DeferredTask(
                task_fn=task_fn,
                args=args,
                kwargs=kwargs,
                priority=priority,
                deferred_at=start,
                reason=reason
            ))

            emit_event("task.deferred", {
                "priority": priority.value,
                "reason": reason,
                "queue_size": len(self.deferred_queue)
            })

            return TaskResult(
                success=False,
                mode=ExecutionMode.DEFERRED,
                duration_ms=0,
                deferred_reason=reason
            )

        # Execute with appropriate mode
        try:
            # Pass mode to task if it accepts it
            if "mode" in kwargs or hasattr(task_fn, "__code__") and "mode" in task_fn.__code__.co_varnames:
                kwargs["mode"] = mode.value

            output = await task_fn(*args, **kwargs)

            duration_ms = (time.time() - start) * 1000

            emit_event("task.completed", {
                "priority": priority.value,
                "mode": mode.value,
                "duration_ms": duration_ms
            })

            return TaskResult(
                success=True,
                mode=mode,
                duration_ms=duration_ms,
                output=output
            )

        except Exception as e:
            duration_ms = (time.time() - start) * 1000

            emit_event("task.failed", {
                "priority": priority.value,
                "mode": mode.value,
                "error": str(e),
                "duration_ms": duration_ms
            })

            return TaskResult(
                success=False,
                mode=mode,
                duration_ms=duration_ms,
                error=str(e)
            )

    def _determine_mode(
        self,
        guna: str,
        harmony_score: float,
        priority: TaskPriority
    ) -> ExecutionMode:
        """
        Determine execution mode based on system state and priority.

        Decision matrix:
        - CRITICAL: Always execute (minimal if tamas)
        - HIGH: Execute unless tamas
        - NORMAL: Execute in sattva/rajas
        - LOW: Only execute in sattva
        - BACKGROUND: Only when idle (harmony > 0.9)
        """
        if guna == "tamas":  # System exhausted
            if priority == TaskPriority.CRITICAL:
                return ExecutionMode.MINIMAL
            else:
                return ExecutionMode.DEFERRED

        elif guna == "rajas":  # System stressed
            if priority in [TaskPriority.CRITICAL, TaskPriority.HIGH]:
                return ExecutionMode.BALANCED
            elif priority == TaskPriority.NORMAL:
                return ExecutionMode.BALANCED
            else:
                return ExecutionMode.DEFERRED

        else:  # sattva - System balanced
            if harmony_score >= 0.9:
                return ExecutionMode.FULL
            elif harmony_score >= 0.8:
                return ExecutionMode.BALANCED
            else:
                return ExecutionMode.BALANCED

    async def process_deferred(self, max_tasks: int = 10) -> Dict[str, int]:
        """
        Process deferred tasks when harmony improves.

        Args:
            max_tasks: Maximum tasks to process in one batch

        Returns:
            Stats on tasks processed
        """
        if self._processing_deferred or not self.deferred_queue:
            return {"processed": 0, "failed": 0, "re_deferred": 0}

        self._processing_deferred = True

        try:
            processed = 0
            failed = 0
            re_deferred = 0

            # Sort by priority (critical first)
            priority_order = {
                TaskPriority.CRITICAL: 0,
                TaskPriority.HIGH: 1,
                TaskPriority.NORMAL: 2,
                TaskPriority.LOW: 3,
                TaskPriority.BACKGROUND: 4
            }
            self.deferred_queue.sort(key=lambda t: priority_order[t.priority])

            # Process up to max_tasks
            tasks_to_process = self.deferred_queue[:max_tasks]
            remaining = self.deferred_queue[max_tasks:]

            for task in tasks_to_process:
                result = await self.execute(
                    task.task_fn,
                    *task.args,
                    priority=task.priority,
                    **task.kwargs
                )

                if result.success:
                    processed += 1
                elif result.mode == ExecutionMode.DEFERRED:
                    re_deferred += 1
                    remaining.append(task)  # Re-defer
                else:
                    failed += 1

            self.deferred_queue = remaining

            emit_event("deferred_tasks.processed", {
                "processed": processed,
                "failed": failed,
                "re_deferred": re_deferred,
                "remaining": len(self.deferred_queue)
            })

            return {
                "processed": processed,
                "failed": failed,
                "re_deferred": re_deferred
            }

        finally:
            self._processing_deferred = False

    async def wait_for_harmony(
        self,
        min_score: float = 0.7,
        timeout_seconds: float = 60.0
    ) -> bool:
        """
        Wait for system harmony to improve to at least min_score.

        Args:
            min_score: Minimum harmony score to wait for
            timeout_seconds: Maximum time to wait

        Returns:
            True if harmony improved, False if timeout
        """
        import time
        start = time.time()

        while time.time() - start < timeout_seconds:
            state = self.monitor.get_current()
            if state["harmony_score"] >= min_score:
                return True

            await asyncio.sleep(1.0)

        return False


# Example usage
async def example_graceful_degradation():
    """Demonstrate graceful task degradation."""
    executor = GracefulTaskExecutor()

    # Example task that can run in different modes
    async def process_data(items: List[str], mode: str = "full") -> Dict[str, int]:
        """Process data with different quality levels."""
        if mode == "full":
            # Full quality: Detailed processing
            results = {"processed": len(items), "validated": len(items), "enriched": len(items)}
            await asyncio.sleep(0.1 * len(items))  # Slow but thorough
        elif mode == "balanced":
            # Balanced: Process + validate
            results = {"processed": len(items), "validated": len(items), "enriched": 0}
            await asyncio.sleep(0.05 * len(items))  # Faster
        else:  # minimal
            # Minimal: Just process
            results = {"processed": len(items), "validated": 0, "enriched": 0}
            await asyncio.sleep(0.01 * len(items))  # Fastest

        return results

    # Execute tasks with different priorities
    items = ["item1", "item2", "item3"]

    # Critical task - will execute even if system exhausted
    result1 = await executor.execute(
        process_data,
        items,
        priority=TaskPriority.CRITICAL
    )
    print(f"Critical task: {result1.mode.value}, output={result1.output}")

    # Normal task - may be deferred if system stressed
    result2 = await executor.execute(
        process_data,
        items,
        priority=TaskPriority.NORMAL
    )
    if result2.mode == ExecutionMode.DEFERRED:
        print(f"Normal task deferred: {result2.deferred_reason}")

        # Wait for harmony to improve
        if await executor.wait_for_harmony(min_score=0.7):
            # Process deferred tasks
            stats = await executor.process_deferred()
            print(f"Processed {stats['processed']} deferred tasks")
    else:
        print(f"Normal task: {result2.mode.value}, output={result2.output}")
```

### 5. Rate Limiting for Sustainable Pace

Prevent burnout through intelligent rate limiting:

```python
from whitemagic.systems.rate_limiter import AdaptiveRateLimiter
from whitemagic.core.embodiment import get_harmony_monitor

# Rate limiter adjusts based on system state
limiter = AdaptiveRateLimiter(
    base_rate=100,  # requests per minute
    harmony_monitor=get_harmony_monitor()
)

async def process_requests(requests):
    """Process requests at sustainable pace."""
    for req in requests:
        # Automatically throttles when system stressed
        async with limiter.acquire():
            await handle_request(req)

# When harmony_score drops, rate automatically decreases
# When harmony_score rises, rate increases back to base
```

### 6. Healing Garden Activation

When resources are depleted, activate healing mode for recovery.

**Purpose**: Systematically recover from resource exhaustion through coordinated healing protocols.

**When to use**:
- Harmony score drops below 0.5 (tamas state)
- Repeated task failures due to resource limits
- Thermal throttling or memory pressure detected
- After intensive workload completion

**Implementation**:

```python
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional
from datetime import datetime
import asyncio

from whitemagic.tools import manage_gardens, emit_event
from whitemagic.core.embodiment import get_harmony_monitor


class HealingPhase(Enum):
    """Phases of healing protocol."""
    ASSESSMENT = "assessment"      # Measure damage
    STABILIZATION = "stabilization"  # Stop bleeding
    RECOVERY = "recovery"          # Restore resources
    VALIDATION = "validation"      # Verify health
    COMPLETE = "complete"          # Fully healed


@dataclass
class HealingAction:
    """Single healing action to perform."""
    name: str
    description: str
    action_fn: callable
    priority: int = 0  # Lower = higher priority


@dataclass
class HealingProtocol:
    """Complete healing protocol with phases."""
    trigger_score: float
    target_score: float
    max_duration_seconds: float
    check_interval_seconds: float
    actions: List[HealingAction] = field(default_factory=list)


class HealingGardenCoordinator:
    """
    Coordinate healing garden activation and recovery protocols.

    When system energy drops critically low, activates healing mode
    and systematically restores system health.
    """

    def __init__(self):
        self.monitor = get_harmony_monitor()
        self.healing_garden = manage_gardens(action="get", garden_name="healing")

        self.is_healing = False
        self.current_phase = HealingPhase.COMPLETE
        self.healing_start: Optional[datetime] = None

        # Define default healing protocol
        self.protocol = self._create_default_protocol()

    def _create_default_protocol(self) -> HealingProtocol:
        """Create default healing protocol."""
        return HealingProtocol(
            trigger_score=0.5,  # Enter healing when <= 0.5
            target_score=0.7,   # Exit healing when >= 0.7
            max_duration_seconds=300,  # 5 minutes max
            check_interval_seconds=10,  # Check every 10s
            actions=[
                HealingAction(
                    name="close_idle_connections",
                    description="Close idle database/network connections",
                    action_fn=self._close_idle_connections,
                    priority=1
                ),
                HealingAction(
                    name="flush_caches",
                    description="Flush caches to disk to free memory",
                    action_fn=self._flush_caches,
                    priority=2
                ),
                HealingAction(
                    name="garbage_collect",
                    description="Force garbage collection",
                    action_fn=self._garbage_collect,
                    priority=3
                ),
                HealingAction(
                    name="reduce_batch_sizes",
                    description="Reduce batch sizes for ongoing work",
                    action_fn=self._reduce_batch_sizes,
                    priority=4
                ),
                HealingAction(
                    name="defer_background_tasks",
                    description="Defer all non-critical background tasks",
                    action_fn=self._defer_background_tasks,
                    priority=5
                )
            ]
        )

    async def activate_healing(
        self,
        custom_protocol: Optional[HealingProtocol] = None
    ) -> Dict[str, any]:
        """
        Activate healing garden and execute recovery protocol.

        Args:
            custom_protocol: Optional custom healing protocol

        Returns:
            Healing results including final harmony score
        """
        if self.is_healing:
            return {"error": "Already healing"}

        protocol = custom_protocol or self.protocol

        self.is_healing = True
        self.healing_start = datetime.now()
        self.current_phase = HealingPhase.ASSESSMENT

        # Activate healing garden
        manage_gardens(action="activate", garden_name="healing")

        emit_event("healing.started", {
            "initial_score": self.monitor.get_current()["harmony_score"],
            "target_score": protocol.target_score
        })

        try:
            # Phase 1: Assessment
            initial_state = self.monitor.get_current()
            emit_event("healing.phase", {"phase": "assessment", "score": initial_state["harmony_score"]})

            # Phase 2: Stabilization
            self.current_phase = HealingPhase.STABILIZATION
            emit_event("healing.phase", {"phase": "stabilization"})

            # Execute healing actions in priority order
            actions_sorted = sorted(protocol.actions, key=lambda a: a.priority)
            for action in actions_sorted:
                await self._execute_action(action)

                # Check if we've reached target
                current_state = self.monitor.get_current()
                if current_state["harmony_score"] >= protocol.target_score:
                    break

            # Phase 3: Recovery - Wait and monitor
            self.current_phase = HealingPhase.RECOVERY
            emit_event("healing.phase", {"phase": "recovery"})

            recovery_start = datetime.now()
            while True:
                current_state = self.monitor.get_current()

                # Check if recovered
                if current_state["harmony_score"] >= protocol.target_score:
                    break

                # Check for timeout
                elapsed = (datetime.now() - recovery_start).total_seconds()
                if elapsed > protocol.max_duration_seconds:
                    emit_event("healing.timeout", {
                        "elapsed_seconds": elapsed,
                        "final_score": current_state["harmony_score"]
                    })
                    break

                # Wait and check again
                await asyncio.sleep(protocol.check_interval_seconds)

                emit_event("healing.recovery_progress", {
                    "score": current_state["harmony_score"],
                    "target": protocol.target_score,
                    "elapsed_seconds": elapsed
                })

            # Phase 4: Validation
            self.current_phase = HealingPhase.VALIDATION
            final_state = self.monitor.get_current()

            emit_event("healing.phase", {
                "phase": "validation",
                "final_score": final_state["harmony_score"]
            })

            # Phase 5: Complete
            self.current_phase = HealingPhase.COMPLETE

            total_duration = (datetime.now() - self.healing_start).total_seconds()

            emit_event("healing.completed", {
                "initial_score": initial_state["harmony_score"],
                "final_score": final_state["harmony_score"],
                "duration_seconds": total_duration,
                "success": final_state["harmony_score"] >= protocol.target_score
            })

            return {
                "success": final_state["harmony_score"] >= protocol.target_score,
                "initial_score": initial_state["harmony_score"],
                "final_score": final_state["harmony_score"],
                "duration_seconds": total_duration,
                "guna_before": initial_state["guna_tag"],
                "guna_after": final_state["guna_tag"]
            }

        finally:
            self.is_healing = False
            # Deactivate healing garden
            manage_gardens(action="deactivate", garden_name="healing")

    async def _execute_action(self, action: HealingAction):
        """Execute single healing action."""
        emit_event("healing.action_started", {
            "name": action.name,
            "description": action.description
        })

        try:
            await action.action_fn()

            emit_event("healing.action_completed", {
                "name": action.name
            })

        except Exception as e:
            emit_event("healing.action_failed", {
                "name": action.name,
                "error": str(e)
            })

    # Healing action implementations
    async def _close_idle_connections(self):
        """Close idle connections to free resources."""
        # In production, would actually close idle connections
        # from connection pools, HTTP clients, etc.
        await asyncio.sleep(0.1)  # Simulate work

    async def _flush_caches(self):
        """Flush caches to disk to free memory."""
        # In production, would flush various caches
        import gc
        gc.collect()  # Python garbage collection
        await asyncio.sleep(0.1)

    async def _garbage_collect(self):
        """Force garbage collection."""
        import gc
        gc.collect(generation=2)  # Full collection
        await asyncio.sleep(0.05)

    async def _reduce_batch_sizes(self):
        """Reduce batch sizes for ongoing work."""
        # In production, would signal to reduce batch sizes
        await asyncio.sleep(0.05)

    async def _defer_background_tasks(self):
        """Defer all non-critical background tasks."""
        # In production, would pause background workers
        await asyncio.sleep(0.05)

    async def check_and_heal(self) -> bool:
        """
        Check harmony and activate healing if needed.

        Returns:
            True if healing was activated, False otherwise
        """
        state = self.monitor.get_current()

        if state["harmony_score"] < self.protocol.trigger_score:
            await self.activate_healing()
            return True

        return False


# Example usage
async def example_healing_activation():
    """Demonstrate healing garden activation."""
    coordinator = HealingGardenCoordinator()
    monitor = get_harmony_monitor()

    # Check current state
    state = monitor.get_current()
    print(f"Current harmony: {state['harmony_score']:.2f} ({state['guna_tag']})")

    # Activate healing if needed
    if state['harmony_score'] < 0.5:
        print("⚠️ Energy critically low - activating healing garden")

        result = await coordinator.activate_healing()

        if result["success"]:
            print(f"✅ Healing complete:")
            print(f"  Before: {result['initial_score']:.2f} ({result['guna_before']})")
            print(f"  After: {result['final_score']:.2f} ({result['guna_after']})")
            print(f"  Duration: {result['duration_seconds']:.1f}s")
        else:
            print(f"⚠️ Healing timeout - partial recovery:")
            print(f"  Score improved from {result['initial_score']:.2f} to {result['final_score']:.2f}")
    else:
        print("✅ System healthy - no healing needed")

    # Automatic healing check (call periodically)
    if await coordinator.check_and_heal():
        print("Auto-healing activated")
```

### 7. Async Energy Efficiency

Async code is more energy-efficient than blocking:

```python
import asyncio

# ❌ Energy-intensive (blocking threads)
def blocking_approach():
    results = []
    for url in urls:
        result = requests.get(url)  # Blocks thread
        results.append(result)
    return results

# ✅ Energy-efficient (async I/O)
async def async_approach():
    async with aiohttp.ClientSession() as session:
        tasks = [session.get(url) for url in urls]
        results = await asyncio.gather(*tasks)
    return results

# Async uses 1/10th the memory and CPU
```

---

## ⚖️ Yin/Yang Balance in Software

### Yang Energy (Action, Output, Heat)
- Heavy computation (CPU-intensive)
- Parallel processing
- Network I/O
- Database writes
- Building, compiling
- Event emission

**Risks of excess Yang:**
- CPU overheating
- Memory exhaustion
- Connection pool depletion
- Burnout, crashes

### Yin Energy (Reception, Input, Cool)
- Reading, parsing
- Caching, retrieving
- Listening for events
- Planning, designing
- Profiling, observing
- Rest, garbage collection

**Risks of excess Yin:**
- Stagnation
- Over-caching (memory bloat)
- Analysis paralysis
- Underutilization

### Achieving Balance

```python
from whitemagic.core.embodiment import get_harmony_monitor

async def balanced_workflow():
    """Alternate between yang (action) and yin (reception)."""
    monitor = get_harmony_monitor()

    # Yang phase: Heavy processing
    print("Yang: Processing data...")
    await process_heavy_workload()

    # Check harmony after yang
    if monitor.get_current()['harmony_score'] < 0.6:
        # Yin phase: Rest and restore
        print("Yin: Resting and consolidating...")
        await asyncio.sleep(60)  # Cool down period
        await garbage_collection()
        await consolidate_caches()

    # Repeat cycle with awareness
```

---

## 🔍 Troubleshooting

**"Harmony score always low, can't do intensive work"**

1. Check if background processes are consuming resources:
   ```bash
   top -o %CPU  # Find CPU hogs
   free -h      # Check memory
   ```

2. Close idle connections and clear caches:
   ```python
   # Free up resources
   await pool.close_idle()
   cache.clear_expired()
   ```

3. Increase `harmony_score` threshold if too conservative:
   ```python
   # More tolerant threshold
   if state['harmony_score'] > 0.4:  # Instead of 0.5
       proceed_with_work()
   ```

4. Check thermal throttling:
   ```python
   state = monitor.get_current()
   if state['thermal_state'] == "HOT":
       # System overheating - wait for cooling
       await asyncio.sleep(120)
   ```

**"Guna tag stuck in 'tamas' (exhaustion)"**

This means system resources are critically low:

1. **Immediate**: Stop non-critical work
2. **Check**: Memory leaks, unclosed connections
3. **Action**: Restart resource pools, clear caches
4. **Prevention**: Implement graceful degradation (Workflow #4)

```python
# Emergency recovery
await pool.restart()
cache.clear_all()
await asyncio.sleep(60)  # Let system recover
```

**"How do I know which phase I'm in?"**

Use Wu Xing analysis to identify current energy state:

```python
phase = analyze_wu_xing_phase(
    operation="analyze",
    description="What I'm doing now"
)

print(f"You are in {phase['element']} phase")
print(f"Recommendation: {phase['recommendation']}")
```

**"Tasks failing with 'defer' - how to process them?"**

Deferred tasks accumulate when harmony is low. Process them when system recovers:

```python
from whitemagic.core.task_queue import get_deferred_tasks

# When harmony restored
if monitor.get_current()['harmony_score'] > 0.7:
    deferred = get_deferred_tasks()
    for task in deferred:
        await task.execute(mode="balanced")
```

---

## 💡 Best Practices

### Listen to Your System

```python
# ❌ Ignoring system state
def brute_force():
    # Just hammer through regardless
    for i in range(1000000):
        expensive_operation()

# ✅ Respecting system state
async def harmonious_approach():
    monitor = get_harmony_monitor()

    for i in range(1000000):
        state = monitor.get_current()

        if state['harmony_score'] < 0.5:
            # Take a break
            await asyncio.sleep(10)

        await expensive_operation()
```

### Sustainable Pace Over Speed

**Slow and steady wins the race.**

It's better to maintain 60% capacity continuously than to spike to 100%, crash, and restart:

```python
# Sustainable work pattern
async def sustainable_processing():
    limiter = AdaptiveRateLimiter(base_rate=100)

    # Process at adaptive rate
    # System auto-adjusts based on harmony
    for item in items:
        async with limiter.acquire():
            await process(item)
```

### Proactive Rest Periods

Don't wait until exhaustion—build in rest:

```python
async def work_with_rest():
    for hour in range(8):
        # Work period
        await do_intensive_work(duration=50 * 60)  # 50 min

        # Rest period
        print("Rest: Consolidating and recovering...")
        await asyncio.sleep(10 * 60)  # 10 min
        await garbage_collection()
```

### Use Async for I/O-Bound Work

Async is dramatically more energy-efficient for I/O:

```python
# 10x more efficient for I/O-bound work
async def efficient_io():
    async with aiohttp.ClientSession() as session:
        tasks = [fetch(session, url) for url in urls]
        return await asyncio.gather(*tasks)
```

---

## 🌿 Garden Resonance

The **healing** garden brings restoration and balance.

When working in this chapter, embody:
- **Nourishment**: Feed what needs sustenance
- **Balance**: Maintain yin/yang harmony  
- **Healing**: Restore when depleted
- **Sustainability**: Work at maintainable pace

---

## ⏭️ Transitions

### What Flows In

**From Chapter 16 (Strategic Patience)**: Resources have been accumulated patiently. Now use them wisely, monitoring energy expenditure to maintain sustainable pace.

**From Chapter 6 (Performance Drive)**: If performance optimization caused burnout or resource exhaustion, come here to restore balance and establish sustainable energy management.

**From Chapter 4 (Resource Sanctuary)**: Resources are protected and available. Now actively manage their consumption to prevent depletion.

### What Flows Out

**Sequential**: Proceed to **Chapter 18 (Detailed Attention)** once energy is balanced. With harmony maintained, you can now attend to fine details with sustained focus.

**Situational Jumps**:
- **System critically exhausted** → Activate healing garden (line 184), then jump to [Chapter 27 (Meditation/Stillness)](27_VOID_MEDITATION_STILLNESS.md) for deep rest
- **Need performance boost despite low energy** → Use graceful degradation (line 129) to balance speed and sustainability
- **High async workload** → See async efficiency patterns (line 215), then jump to [Chapter 9 (Adaptive Play)](09_WILLOW_ADAPTIVE_PLAY.md) for async orchestration
- **Wu Xing phase unclear** → Use phase analysis (line 70), then follow phase-appropriate chapter guidance

### Recovery Paths

- **Harmony score consistently low**: See troubleshooting (line 298) to diagnose resource issues
- **Guna stuck in tamas**: Emergency recovery protocol (line 330), clear caches and restart pools
- **Deferred tasks accumulating**: Process when harmony restored (line 360)
- **Burnout from overwork**: Proactive rest periods (line 419), sustainable pace practices

### Cross-References

**Key Sections to Reference**:
- Embodiment monitoring → Line 36 (System Harmony workflow)
- Guna interpretation → Line 65 (Harmony Score table)
- Wu Xing phases → Line 87 (Five Elements table)
- Yin/Yang balance → Line 242 (Energy types and risks)
- Graceful degradation → Line 129 (Workflow #4)
- Healing activation → Line 184 (Workflow #6)
- Best practices → Line 376 (Listen to system, sustainable pace)

**Related Chapters**:
- Resource sanctuary → [Chapter 4](04_ROOM_RESOURCE_SANCTUARY.md)
- Performance optimization → [Chapter 6](06_TAIL_PERFORMANCE_DRIVE.md)
- Async patterns → [Chapter 9](09_WILLOW_ADAPTIVE_PLAY.md)
- Strategic patience → [Chapter 16](16_MOUND_STRATEGIC_PATIENCE.md)
- Deep rest/meditation → [Chapter 27](27_VOID_MEDITATION_STILLNESS.md)

---

## 🧭 Navigation

**Predecessor**: Chapter 16 (Mound/Strategic Patience) - *Resources accumulated*
**Successor**: Chapter 18 (Hairy Head/Detailed Attention) - *Focused attention next*
**Quadrant Flow**: Third of Western Quadrant (Autumn/Metal) - Position 3/7

---

## 🔗 The Stomach's Wisdom

> *"The stomach nourishes the whole system, converting food into energy, rest into vitality. The wise developer monitors harmony, respects limitations, and maintains sustainable pace. True power comes not from constant exertion, but from rhythmic balance of yang action and yin restoration."*

**Energy Management Principles**:
- **Monitor embodiment**: Know your system's harmony state
- **Respect Guna**: Sattva enables, Rajas stresses, Tamas exhausts
- **Balance Yin/Yang**: Alternate action and restoration
- **Degrade gracefully**: Better to slow down than crash
- **Sustainable pace**: Marathon, not sprint

**The Energy Test**:
Before proceeding to Chapter 18, ask:
1. Is my system harmony score >= 0.5?
2. Am I using adaptive patterns (pools, rate limiting)?
3. Do I have a recovery plan if exhaustion occurs?

If yes to all three → Proceed to Chapter 18
If no to any → Continue Chapter 17 work

---

**Next Chapter**: [Chapter 18: Detailed Attention →](18_HAIRYHEAD_DETAILED_ATTENTION.md)
**Previous Chapter**: [Chapter 16: Strategic Patience](16_MOUND_STRATEGIC_PATIENCE.md)
**Quadrant**: Western (Autumn/Metal) - Yin Rising Phase
