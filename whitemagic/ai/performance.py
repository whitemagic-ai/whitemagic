"""AI Performance compatibility shim"""

from __future__ import annotations

import time
from types import TracebackType
from typing import Any


class AIPerformance:
    """AI Performance monitoring and optimization"""

    def __init__(self) -> None:
        self.cache = PerformanceCache()
        self.latency_tracker = LatencyTracker()
        self.batch_processor = BatchProcessor()

class ResponseCache:
    """Caches AI responses for performance"""

    def __init__(self, max_size: int = 1000) -> None:
        self.cache: dict[str, Any] = {}
        self.max_size = max_size

    def get(self, key: str) -> Any:
        """Get cached response"""
        return self.cache.get(key)

    def store(self, key: str, value: Any) -> None:
        """Cache response"""
        self.set(key, value)

    def set(self, key: str, value: Any) -> None:
        """Cache response"""
        if len(self.cache) >= self.max_size:
            # Remove oldest entry
            oldest = next(iter(self.cache))
            del self.cache[oldest]
        self.cache[key] = value

    def clear(self) -> None:
        """Clear cache"""
        self.cache.clear()

# Alias for backward compatibility
PerformanceCache = ResponseCache


class _LatencyContext:
    """Context manager for tracking latency of a code block."""

    def __init__(self, tracker: LatencyTracker, name: str) -> None:
        self.tracker = tracker
        self.name = name
        self.start_time: float = 0.0

    def __enter__(self) -> _LatencyContext:
        self.start_time = time.time()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        latency = time.time() - self.start_time
        self.tracker.record(self.name, latency)


class LatencyTracker:
    """Tracks API latency"""

    def __init__(self) -> None:
        self.latencies: list[float] = []
        self.metrics: dict[str, dict[str, float]] = {}

    def track(self, name: str) -> "_LatencyContext":
        """Track latency context manager"""
        return _LatencyContext(self, name)

    def record(self, name: str, latency: float) -> None:
        """Record a latency measurement"""
        self.latencies.append(latency)
        if name not in self.metrics:
            self.metrics[name] = {"count": 0, "total_latency": 0}
        self.metrics[name]["count"] += 1
        self.metrics[name]["total_latency"] += latency

    def get_metrics(self) -> dict[str, Any]:
        """Get latency metrics"""
        result = {}
        for name, data in self.metrics.items():
            result[name] = {
                "count": data["count"],
                "avg_latency": data["total_latency"] / data["count"] if data["count"] > 0 else 0,
            }
        return result

    def get_stats(self) -> dict[str, float]:
        """Get latency statistics"""
        if not self.latencies:
            return {"mean": 0, "min": 0, "max": 0, "p95": 0}

        sorted_lat = sorted(self.latencies)
        return {
            "mean": sum(self.latencies) / len(self.latencies),
            "min": min(self.latencies),
            "max": max(self.latencies),
            "p95": sorted_lat[int(len(sorted_lat) * 0.95)],
        }

class BatchProcessor:
    """Processes multiple requests in batch"""

    def __init__(self, batch_size: int = 10) -> None:
        self.batch_size = batch_size

    def process_batch(self, requests: list[Any]) -> list[Any]:
        """Process a batch of requests"""
        results = []
        for request in requests:
            # Mock processing
            request_id = request.get("id") if isinstance(request, dict) else str(request)
            request_type = request.get("type") if isinstance(request, dict) else "text"
            results.append({
                "request_id": request_id,
                "result": f"Processed {request_type}",
                "processing_time": 0.1,
            })
        return results

    def optimize_batch_size(self, avg_latency: float) -> int:
        """Optimize batch size based on latency"""
        if avg_latency < 0.5:
            return min(self.batch_size * 2, 50)
        if avg_latency > 2.0:
            return max(self.batch_size // 2, 1)
        return self.batch_size

class TokenOptimizer:
    """Optimizes token usage"""

    def count_tokens(self, text: str) -> int:
        """Count tokens in text"""
        return len(text.split())

    def optimize_prompt(self, text: str, max_tokens: int) -> str:
        """Optimize prompt to fit token limit"""
        words = text.split()
        if len(words) <= max_tokens:
            return text
        return " ".join(words[:max_tokens])

# Re-export
__all__ = ["AIPerformance", "BatchProcessor", "LatencyTracker", "PerformanceCache", "ResponseCache", "TokenOptimizer"]
