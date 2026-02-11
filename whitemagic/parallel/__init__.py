"""WhiteMagic parallel processing package."""

from .adaptive import AdaptiveThreadingController, SystemMetrics
from .cache import CacheEntry, DistributedCache
from .file_ops import FileReadResult, ParallelFileReader, batch_read_files, batch_read_files_dict
from .memory_consolidator import (
    ConsolidationResult,
    ParallelMemoryConsolidator,
    consolidate_all_memories,
    emit_consolidation_event,
)
from .pipeline import ParallelPipeline, PipelineResult, PipelineStage
from .pools import PoolConfig, ThreadingManager, ThreadingTier
from .runner import ParallelTestRunner, TestResult, TestSuiteResult, run_tests_parallel
from .scheduler import ParallelScheduler, SchedulerStats, Task, TaskPriority, TaskStatus

# Legacy names that were removed from the package layout.
# Keep placeholders for compatibility with old imports.
MassivePatternScanner = None
PatternMatch = None
ScanResult = None
quick_scan = None
DreamStateSynthesizer = None
GardenInsight = None
SynthesisResult = None
dream_synthesize = None
GrimoireIndexer = None
GrimoireIndex = None
Spell = None
index_grimoire = None
GanYingAmplifier = None
AmplifiedEvent = None
ResonanceWave = None
AmplificationResult = None
amplify = None
create_harmony = None

__all__ = [
    "ThreadingTier",
    "PoolConfig",
    "ThreadingManager",
    "AdaptiveThreadingController",
    "SystemMetrics",
    "ParallelScheduler",
    "TaskPriority",
    "TaskStatus",
    "Task",
    "SchedulerStats",
    "ParallelPipeline",
    "PipelineStage",
    "PipelineResult",
    "DistributedCache",
    "CacheEntry",
    "ParallelFileReader",
    "FileReadResult",
    "batch_read_files",
    "batch_read_files_dict",
    "ParallelMemoryConsolidator",
    "ConsolidationResult",
    "consolidate_all_memories",
    "emit_consolidation_event",
    "ParallelTestRunner",
    "TestResult",
    "TestSuiteResult",
    "run_tests_parallel",
    "MassivePatternScanner",
    "PatternMatch",
    "ScanResult",
    "quick_scan",
    "DreamStateSynthesizer",
    "GardenInsight",
    "SynthesisResult",
    "dream_synthesize",
    "GrimoireIndexer",
    "GrimoireIndex",
    "Spell",
    "index_grimoire",
    "GanYingAmplifier",
    "AmplifiedEvent",
    "ResonanceWave",
    "AmplificationResult",
    "amplify",
    "create_harmony",
]

__version__ = "2.6.6"
