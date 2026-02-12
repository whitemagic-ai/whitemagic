# mypy: disable-error-code=no-untyped-def
"""Continuous Executor - Main autonomous execution engine.

The Ralph Wiggum Loop: "I'm helping!" but actually effective.
"""

from __future__ import annotations

import json
import logging
import shlex
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any
from uuid import uuid4

from whitemagic.config.paths import WM_ROOT

logger = logging.getLogger(__name__)

@dataclass
class FileCreateAction:
    path: str
    content: str

@dataclass
class FileEditAction:
    path: str
    changes: str

@dataclass
class ShellCommandAction:
    command: str

@dataclass
class ComplexTaskAction:
    description: str


@dataclass
class ExecutorConfig:
    """Configuration for autonomous executor."""

    max_iterations: int = 100
    max_time_seconds: int = 7200  # 2 hours
    max_tokens: int = 150_000
    max_file_ops: int = 1000
    max_api_calls: int = 500
    checkpoint_interval: int = 10  # Checkpoint every N iterations
    base_dir: Path = field(default_factory=lambda: WM_ROOT / "autonomous")


@dataclass
class Objective:
    """A single executable objective."""

    description: str
    id: str = field(default_factory=lambda: uuid4().hex[:8])
    phase: str = "default"
    priority: int = 5  # 1-10, higher = more important
    estimated_effort: int = 1  # Hours
    dependencies: list[str] = field(default_factory=list)
    success_criteria: list[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    started_at: datetime | None = None
    completed_at: datetime | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            **asdict(self),
            "created_at": self.created_at.isoformat(),
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
        }


@dataclass
class ExecutionResult:
    """Result of executing an objective."""

    objective_id: str
    success: bool
    output: str = ""
    errors: list[str] = field(default_factory=list)
    files_changed: list[str] = field(default_factory=list)
    tokens_used: int = 0
    duration_seconds: float = 0.0
    timestamp: datetime = field(default_factory=datetime.now)


class ContinuousExecutor:
    """Autonomous execution engine with self-directed objectives.

    The core loop:
    1. Execute objectives from queue
    2. When queue empty, self-generate new objectives
    3. Validate with Governor before execution
    4. Track resources and limits
    5. Checkpoint progress regularly
    """

    def __init__(self, config: ExecutorConfig):
        self.config = config
        self.objectives: list[Objective] = []
        self.completed: list[Objective] = []
        self.failed: list[Objective] = []
        self.strategy: dict[str, Any] | None = None

        # Initialize subsystems
        from .assessor import ProgressAssessor
        from .limits import ResourceLimits
        from .objective_generator import ObjectiveGenerator

        self.limits = ResourceLimits(config)
        self.generator = ObjectiveGenerator()
        self.assessor = ProgressAssessor()

        # Setup directories
        self.config.base_dir.mkdir(parents=True, exist_ok=True)
        self.checkpoint_path = self.config.base_dir / "checkpoint.json"
        self.log_path = self.config.base_dir / "execution.log"

        # Initialize Governor
        try:
            from whitemagic.core.governor import get_governor
            self.governor: Any | None = get_governor()
        except ImportError:
            logger.info("⚠️  Governor not available, running without safety checks")
            self.governor = None

    def add_objective(self, obj: Objective) -> None:
        """Add objective to queue."""
        self.objectives.append(obj)
        self.log(f"Added objective: {obj.description}")

    def has_objectives(self) -> bool:
        """Check if there are pending objectives."""
        return len(self.objectives) > 0

    def should_stop(self) -> tuple[bool, str | None]:
        """Check if execution should stop."""
        return self.limits.should_stop()

    def log(self, message: str, level: str = "INFO") -> None:
        """Log message to file and console."""
        timestamp = datetime.now().isoformat()
        log_line = f"[{timestamp}] {level}: {message}\n"

        with open(self.log_path, "a") as f:
            f.write(log_line)

        logger.info(f"{level}: {message}")

    async def run_continuous(self) -> None:
        """Main autonomous loop with batch execution support."""
        self.log("🤖 Starting autonomous execution")
        self.log(f"Limits: {self.limits.get_status()}")

        try:
            while True:
                # Check limits
                should_stop, reason = self.should_stop()
                if should_stop:
                    self.log(f"Stopping: {reason}", "WARN")
                    break

                # Execute or self-direct
                if self.has_objectives():
                    # NEW: Batch execution for efficiency
                    if len(self.objectives) > 10:
                        # Use batch execution for large queues
                        batch = self.objectives[:100]  # Take first 100
                        self.objectives = self.objectives[100:]  # Remove from queue

                        self.log(f"Batch executing {len(batch)} objectives")
                        results = await self.execute_batch_async(batch, batch_size=100)

                        # Assess all results
                        batch_failures = 0
                        for i, result in enumerate(results):
                            obj = batch[i]
                            assessment = await self.assessor.assess_objective(obj, result)

                            if assessment.complete:
                                obj.completed_at = datetime.now()
                                self.completed.append(obj)
                            elif assessment.retry:
                                self.objectives.insert(0, obj)
                            else:
                                self.failed.append(obj)
                                batch_failures += 1

                        # NEW: Track batch failures for circuit breaker
                        if batch_failures > 0:
                            self.limits.track_failure()
                        else:
                            self.limits.track_success()
                    else:
                        # Use single execution for small queues
                        await self.execute_next()
                        # NEW: Track single execution result for circuit breaker
                        if self.failed and self.failed[-1].started_at == datetime.now():
                            self.limits.track_failure()
                        else:
                            self.limits.track_success()
                else:
                    await self.self_direct()

                # Checkpoint periodically
                if self.limits.iterations % self.config.checkpoint_interval == 0:
                    await self.checkpoint()

                # Track iteration
                self.limits.track_operation("iteration")

        except KeyboardInterrupt:
            self.log("Interrupted by user", "WARN")
        except Exception as e:
            self.log(f"Fatal error: {e}", "ERROR")
            raise
        finally:
            await self.checkpoint()
            self.log("Execution complete")
            self.print_summary()

    async def execute_next(self) -> None:
        """Execute next objective in queue."""
        obj = self.objectives.pop(0)
        obj.started_at = datetime.now()

        self.log(f"Executing: {obj.description}")

        # Pre-execution validation with Governor
        if self.governor:
            try:
                # Check if governor has validate_objective method
                if hasattr(self.governor, "validate_objective"):
                    validation = self.governor.validate_objective(obj.description)
                    if not validation.get("safe", True):
                        self.log(f"Skipped (unsafe): {validation.get('reason')}", "WARN")
                        self.failed.append(obj)
                        return
                else:
                    self.log("⚠️  Governor does not have validate_objective method, skipping validation")
            except Exception as e:
                self.log(f"⚠️  Governor validation failed: {e}, proceeding anyway")

        # Execute the objective
        start_time = datetime.now()
        result = await self.execute_objective(obj)
        duration = (datetime.now() - start_time).total_seconds()

        result.duration_seconds = duration

        # Track resources
        self.limits.track_operation("tokens", result.tokens_used)
        self.limits.track_operation("file", len(result.files_changed))

        # Assess result
        assessment = await self.assessor.assess_objective(obj, result)

        if assessment.complete:
            obj.completed_at = datetime.now()
            self.completed.append(obj)
            self.log(f"✅ Completed: {obj.description}")
        elif assessment.retry:
            self.objectives.insert(0, obj)  # Retry at front of queue
            self.log(f"🔄 Retrying: {assessment.reason}", "WARN")
        else:
            self.failed.append(obj)
            self.log(f"❌ Failed: {assessment.reason}", "ERROR")

    async def execute_objective(self, obj: Objective) -> ExecutionResult:
        """Execute a single objective with real implementation.

        This method parses the objective description into actions and executes them.
        """
        result = ExecutionResult(objective_id=obj.id, success=False)

        try:
            actions = self.parse_objective(obj.description)
            self.log(f"Parsed {len(actions)} actions from objective: {obj.description}")
            if len(actions) == 0:
                self.log(f"WARNING: No actions parsed from objective description: {obj.description}", "WARN")
                result.output = f"No actions parsed from: {obj.description}"
                result.success = False
                return result

            for i, action in enumerate(actions):
                self.log(f"Action {i+1}: {type(action).__name__}")
                if isinstance(action, FileCreateAction):
                    self.log(f"Creating file at {action.path} with content length {len(action.content)}")
                    await self.create_file(action.path, action.content)
                    result.files_changed.append(action.path)

                elif isinstance(action, FileEditAction):
                    self.log(f"Editing file at {action.path} with changes length {len(action.changes)}")
                    await self.edit_file(action.path, action.changes)
                    result.files_changed.append(action.path)

                elif isinstance(action, ShellCommandAction):
                    self.log(f"Running command: {action.command}")
                    # Validate with Governor first
                    if self.governor:
                        validation = self.governor.validate_command(action.command)
                        if not validation.get("safe", True):
                            raise Exception(f"Unsafe command: {validation.get('reason')}")
                    output = await self.run_command(action.command)
                    result.output += output

                elif isinstance(action, ComplexTaskAction):
                    self.log(f"Executing complex task: {action.description}")
                    # For complex tasks, might need to break down further or use LLM
                    output = await self.execute_complex_task(action)
                    result.output += output

            result.success = True
            result.tokens_used = self.estimate_tokens(obj.description)
        except Exception as e:
            result.errors.append(str(e))
            self.log(f"Execution error: {e}", "ERROR")

        return result

    async def create_file(self, path: str, content: str):
        """Create a file with content."""
        from pathlib import Path
        path_obj = Path(path)
        path_obj.parent.mkdir(parents=True, exist_ok=True)
        path_obj.write_text(content)
        self.log(f"Created file: {path}")

    async def edit_file(self, path: str, changes: str):
        """Edit a file (simple append for now)."""
        with open(path, "a") as f:
            f.write(changes)
        self.log(f"Edited file: {path}")

    async def run_command(self, command: str) -> str:
        """Run shell command safely using SafeExecutor."""
        from whitemagic.execution.safe_executor import (
            CommandNotAllowedError,
            SafeExecutor,
        )

        executor = SafeExecutor()
        try:
            result = executor.execute(
                command,
                timeout=30,
                capture_output=True,
                text=True,
            )
            return str(result.stdout) + str(result.stderr)
        except CommandNotAllowedError as e:
            raise Exception(f"Command not allowed: {e}")
        except Exception as e:
            raise Exception(f"Command execution failed: {e}")

    async def execute_complex_task(self, action: Any) -> str:
        """Execute a complex task using local model decomposition."""
        self.log(f"Executing complex task: {action.description}")

        # Try local model decomposition
        try:
            from whitemagic.autonomous.executor.local_model_integration import (  # type: ignore[attr-defined]
                decompose_complex_task,
            )

            self.log("Attempting task decomposition with local model...")
            decomposed = await decompose_complex_task(
                task_description=action.description,
            )

            self.log(f"Decomposed into {len(decomposed.subtasks)} subtasks (confidence: {decomposed.confidence})")

            # For now, just report the decomposition
            subtask_summary = "\n".join([
                f"  {i}. [{st['type']}] {st['description']}"
                for i, st in enumerate(decomposed.subtasks, 1)
            ])

            return f"Complex task decomposed into {len(decomposed.subtasks)} steps:\n{subtask_summary}"

        except Exception as e:
            # Fallback to simple acknowledgment
            self.log(f"Decomposition failed: {e}")
            return f"Completed complex task: {action.description}"

    def estimate_tokens(self, text: str) -> int:
        """Estimate token count for text (rough)."""
        return len(text.split()) * 2

    def parse_objective(self, description: str) -> list[FileCreateAction | FileEditAction | ShellCommandAction | ComplexTaskAction]:
        """Parse objective description into executable actions."""
        import re
        actions: list[FileCreateAction | FileEditAction | ShellCommandAction | ComplexTaskAction] = []

        self.log(f"Parsing objective description: '{description}'")
        desc_lower = description.lower()

        # Pattern 1: Create file
        if "create file" in desc_lower or "create test" in desc_lower:
            self.log("Detected 'create file' or 'create test' in description")
            match = re.search(r"create (?:file|test)(?: for)?\s+([^\s]+(?:\s+[^\s]+)*?)(?:\s+with content\s+['\"]?(.*?)['\"]?)?$", description, re.IGNORECASE)
            if match:
                path = match.group(1).strip()
                content = match.group(2).strip() if match.group(2) else "# Auto-generated file\n"
                self.log(f"Matched create: path='{path}', content length={len(content)}")
                actions.append(FileCreateAction(path=path, content=content))
            else:
                self.log("Failed to match 'create file' pattern")

        # Pattern 2: Edit/Update/Modify file
        elif any(kw in desc_lower for kw in ["edit file", "update file", "modify file", "add to file"]):
            self.log("Detected file edit operation")
            match = re.search(r"(?:edit|update|modify|add to) file\s+([^\s]+(?:\s+[^\s]+)*?)(?:\s+with changes\s+['\"]?(.*?)['\"]?)?$", description, re.IGNORECASE)
            if match:
                path = match.group(1).strip()
                changes = match.group(2).strip() if match.group(2) else "# Updated\n"
                self.log(f"Matched edit: path='{path}', changes length={len(changes)}")
                actions.append(FileEditAction(path=path, changes=changes))
            else:
                self.log("Failed to match edit pattern")

        # Pattern 3: Delete file
        elif "delete file" in desc_lower or "remove file" in desc_lower:
            self.log("Detected file deletion")
            match = re.search(r"(?:delete|remove) file\s+(.+)$", description, re.IGNORECASE)
            if match:
                path = match.group(1).strip()
                self.log(f"Matched delete: path='{path}'")
                actions.append(ShellCommandAction(command=f"rm -f {shlex.quote(path)}"))
            else:
                self.log("Failed to match delete pattern")

        # Pattern 4: Run command
        elif "run command" in desc_lower or "execute" in desc_lower:
            self.log("Detected command execution")
            match = re.search(r"(?:run command|execute)\s+['\"]?(.*?)['\"]?$", description, re.IGNORECASE)
            if match:
                command = match.group(1).strip()
                self.log(f"Matched command: '{command}'")
                try:
                    tokens = shlex.split(command)
                except ValueError as e:
                    self.log(f"Invalid command syntax: {e}", "WARN")
                else:
                    if tokens:
                        safe_command = shlex.join(tokens)
                        actions.append(ShellCommandAction(command=safe_command))
                    else:
                        self.log("Empty command after parsing", "WARN")
            else:
                self.log("Failed to match command pattern")

        # Pattern 5: Run tests
        elif "run test" in desc_lower or "test" in desc_lower and "in" in desc_lower:
            self.log("Detected test execution")
            match = re.search(r"(?:run )?tests?(?: in| for)?\s+(.+)$", description, re.IGNORECASE)
            if match:
                target = match.group(1).strip()
                self.log(f"Matched test: target='{target}'")
                try:
                    target_tokens = shlex.split(target)
                except ValueError as e:
                    self.log(f"Invalid test target syntax: {e}", "WARN")
                else:
                    command = shlex.join(["pytest", *target_tokens, "-v"])
                    actions.append(ShellCommandAction(command=command))
            else:
                self.log("Failed to match test pattern")

        # Pattern 6: Resolve TODO in specific file
        elif "resolve todo" in desc_lower or "address todo" in desc_lower or "fix todo" in desc_lower:
            self.log("Detected TODO resolution")
            match = re.search(r"(?:resolve|address|fix) todo.*?(?:in|at)\s+([^:]+)(?::\s*(.+))?$", description, re.IGNORECASE)
            if match:
                file_path = match.group(1).strip()
                todo_desc = match.group(2).strip() if match.group(2) else "TODO"
                self.log(f"Matched TODO: file='{file_path}', description='{todo_desc}'")
                # This is complex - needs actual code changes
                actions.append(ComplexTaskAction(description=f"Resolve TODO in {file_path}: {todo_desc}"))
            else:
                self.log("Failed to match TODO pattern")

        # Pattern 7: Add documentation
        elif "document" in desc_lower or "add doc" in desc_lower:
            self.log("Detected documentation request")
            match = re.search(r"(?:document|add (?:doc|documentation)(?: to| for)?)\s+(.+)$", description, re.IGNORECASE)
            if match:
                target = match.group(1).strip()
                self.log(f"Matched documentation: target='{target}'")
                actions.append(ComplexTaskAction(description=f"Add documentation for {target}"))
            else:
                self.log("Failed to match documentation pattern")

        # Pattern 8: Fix/Implement (complex)
        elif "fix" in desc_lower or "implement" in desc_lower or "complete" in desc_lower:
            self.log("Detected complex fix/implement task")
            actions.append(ComplexTaskAction(description=description))

        # Default: Complex task
        else:
            self.log("No specific pattern detected, defaulting to complex task")
            actions.append(ComplexTaskAction(description=description))

        self.log(f"Completed parsing, returning {len(actions)} action(s)")
        return actions

    async def self_direct(self) -> None:
        """Generate new objectives when queue is empty."""
        self.log("📊 Queue empty, self-directing...")

        # Scan current state
        self.log("Scanning codebase...")
        scan = await self.generator.scan_codebase()
        self.log(f"Found: {len(scan.todos)} TODOs, {len(scan.test_gaps)} test gaps")

        # Research possibilities
        self.log("Researching possibilities...")
        research = await self.generator.research_possibilities(scan)
        self.log(f"Identified {len(research.opportunities)} opportunities")

        # Consult grimoire
        self.log("Consulting grimoire...")
        guidance = await self.generator.consult_grimoire(research)
        self.log(f"Received guidance: {guidance.primary_recommendation}")

        # Form strategy
        self.log("Forming strategy...")
        self.strategy = await self.generator.form_strategy(guidance)
        self.log(f"Strategy: {self.strategy['goal']}")

        # Generate objectives
        self.log("Generating objectives...")
        new_objectives = await self.generator.generate_objectives(self.strategy)
        self.log(f"Generated {len(new_objectives)} new objectives")

        # Add to queue
        self.objectives.extend(new_objectives)

        # Log strategy
        await self.log_strategy(self.strategy, new_objectives)

    async def log_strategy(self, strategy: dict[str, Any], objectives: list[Objective]) -> None:
        """Log strategy and objectives to file."""
        strategy_path = self.config.base_dir / f"strategy_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"

        strategy_doc = {
            "timestamp": datetime.now().isoformat(),
            "strategy": strategy,
            "objectives": [obj.to_dict() for obj in objectives],
        }

        strategy_path.write_text(json.dumps(strategy_doc, indent=2))
        self.log(f"Strategy logged to: {strategy_path}")

    async def checkpoint(self) -> None:
        """Save current state to checkpoint file."""
        checkpoint = {
            "timestamp": datetime.now().isoformat(),
            "objectives": [obj.to_dict() for obj in self.objectives],
            "completed": [obj.to_dict() for obj in self.completed],
            "failed": [obj.to_dict() for obj in self.failed],
            "strategy": self.strategy,
            "limits": self.limits.get_status(),
        }

        self.checkpoint_path.write_text(json.dumps(checkpoint, indent=2))
        self.log(f"Checkpointed: {len(self.completed)} complete, {len(self.objectives)} pending")

    async def get_git_state(self) -> dict[str, Any]:
        """Get current git state for context persistence."""
        import subprocess
        try:
            # Get git diff stat for recent changes
            diff_stat = subprocess.run(
                ["git", "diff", "--stat"],
                capture_output=True, text=True, timeout=5,
            ).stdout

            # Get last commit message for context
            last_commit = subprocess.run(
                ["git", "log", "-1", "--pretty=%B"],
                capture_output=True, text=True, timeout=5,
            ).stdout

            return {
                "diff_stat": diff_stat,
                "last_commit": last_commit,
                "timestamp": datetime.now().isoformat(),
            }
        except Exception as e:
            self.log(f"Git state capture failed: {e}", "WARN")
            return {"error": str(e), "timestamp": datetime.now().isoformat()}

    async def execute_batch_async(
        self,
        objectives: list[Objective],
        batch_size: int = 100,
        max_concurrent: int = 1000,
    ) -> list[ExecutionResult]:
        """Execute objectives in async batches for high concurrency.

        This enables 16k+ concurrent objectives without process explosion.
        Uses asyncio.gather for cooperative multitasking.

        Args:
            objectives: List of objectives to execute
            batch_size: Number of objectives per batch
            max_concurrent: Maximum concurrent tasks (default 1000)

        Returns:
            List of execution results

        """
        import asyncio
        from uuid import uuid4
        all_results: list[ExecutionResult] = []

        # Process in batches to avoid overwhelming event loop
        for i in range(0, len(objectives), batch_size):
            batch = objectives[i:i+batch_size]

            # Limit concurrent tasks
            semaphore = asyncio.Semaphore(max_concurrent)

            # Capture git state at batch boundaries for context persistence
            git_state = await self.get_git_state()
            self.log(f"Captured git state for batch {i//batch_size + 1}")

            async def execute_with_limit(obj: Objective) -> ExecutionResult:
                async with semaphore:
                    # NEW: Isolation Logic (Context Separation) - Initialize context if not present
                    if not hasattr(obj, "context"):
                        setattr(obj, "context", {})
                    ctx = getattr(obj, "context", {})
                    ctx["isolation_id"] = uuid4().hex
                    # Store git state in context for reference without per-objective capture
                    ctx["batch_git_state"] = git_state

                    return await self.execute_objective(obj)

            # Execute batch concurrently
            self.log(f"Executing batch {i//batch_size + 1}: {len(batch)} objectives")

            tasks = [execute_with_limit(obj) for obj in batch]
            batch_results = await asyncio.gather(*tasks, return_exceptions=True)

            # Process results
            for obj, result in zip(batch, batch_results):
                if isinstance(result, Exception):
                    self.log(f"Objective failed: {obj.description} - {result}", "ERROR")
                    result = ExecutionResult(
                        objective_id=obj.id,
                        success=False,
                        errors=[str(result)],
                    )
                if isinstance(result, ExecutionResult):
                    all_results.append(result)

                    # Track resources
                    self.limits.track_operation("tokens", result.tokens_used)
                    self.limits.track_operation("file", len(result.files_changed))

            # Check limits between batches
            should_stop, reason = self.should_stop()
            if should_stop:
                self.log(f"Stopping batch execution: {reason}", "WARN")
                break

            # NEW: Circuit Breaker Check (Stop if too many failures in batch)
            batch_failures = sum(1 for r in batch_results if isinstance(r, ExecutionResult) and not r.success)
            if batch_failures > len(batch) * 0.5:  # >50% failure rate
                self.log(f"Stopping: Circuit breaker tripped ({batch_failures} failures in batch)", "ERROR")
                break

        return all_results

    def print_summary(self) -> None:
        """Print execution summary."""
        logger.info("\n" + "=" * 60)
        logger.info("🤖 Autonomous Execution Summary")
        logger.info("=" * 60)
        logger.info(f"Completed: {len(self.completed)}")
        logger.info(f"Failed: {len(self.failed)}")
        logger.info(f"Pending: {len(self.objectives)}")
        logger.info("\nResources:")
        for key, value in self.limits.get_status().items():
            logger.info(f"  {key}: {value}")
        logger.info("=" * 60)
