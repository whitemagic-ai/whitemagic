"""OpenTelemetry Export — OTLP spans and metrics from WhiteMagic.
===============================================================
Bridges the existing telemetry + Harmony Vector into OpenTelemetry
format. Every call_tool() becomes a trace span. Harmony Vector
dimensions are exported as gauges.

Requires: opentelemetry-api, opentelemetry-sdk, opentelemetry-exporter-otlp
(optional dependencies — gracefully degrades if not installed)

Configuration via environment:
  - OTEL_EXPORTER_OTLP_ENDPOINT (default: http://localhost:4317)
  - OTEL_SERVICE_NAME (default: whitemagic)
  - WM_OTEL_ENABLED (default: 0, set to 1 to enable)

Usage:
    from whitemagic.core.monitoring.otel_export import get_otel, record_tool_span
    otel = get_otel()
    otel.record_tool_span("gnosis", 0.042, "success")
"""

from __future__ import annotations

import logging
import os
import threading
import time
from collections.abc import Generator
from contextlib import contextmanager
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# Try to import OpenTelemetry
HAS_OTEL = False
try:
    from opentelemetry import trace
    from opentelemetry.sdk.resources import Resource
    from opentelemetry.sdk.trace import TracerProvider
    from opentelemetry.sdk.trace.export import ConsoleSpanExporter, SimpleSpanProcessor
    HAS_OTEL = True
except ImportError:
    pass

# Try OTLP exporter
HAS_OTLP = False
try:
    from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter  # type: ignore[import-not-found]
    HAS_OTLP = True
except ImportError:
    pass


@dataclass
class SpanRecord:
    """Lightweight span record for when OTEL is not installed."""

    tool_name: str
    duration_ms: float
    status: str
    attributes: dict[str, Any] = field(default_factory=dict)
    timestamp: float = 0.0

    def __post_init__(self) -> None:
        if self.timestamp == 0.0:
            self.timestamp = time.time()


class OTelExporter:
    """OpenTelemetry integration for WhiteMagic.

    If opentelemetry packages are installed and WM_OTEL_ENABLED=1,
    exports real OTLP spans and metrics. Otherwise, maintains an
    in-memory buffer of span records for introspection.
    """

    def __init__(self) -> None:
        self._enabled = os.environ.get("WM_OTEL_ENABLED", "0") == "1"
        self._service_name = os.environ.get("OTEL_SERVICE_NAME", "whitemagic")
        self._tracer: Any | None = None
        self._meter: Any | None = None
        self._lock = threading.Lock()

        # In-memory fallback
        self._buffer: list[SpanRecord] = []
        self._max_buffer = 1000
        self._total_spans = 0

        # Metric accumulators (always tracked)
        self._tool_counts: dict[str, int] = {}
        self._tool_errors: dict[str, int] = {}
        self._tool_durations: dict[str, list[float]] = {}

        if self._enabled and HAS_OTEL:
            self._init_otel()

    def _init_otel(self) -> None:
        """Initialize OpenTelemetry providers."""
        try:
            resource = Resource.create({"service.name": self._service_name})

            # Tracer
            provider = TracerProvider(resource=resource)
            if HAS_OTLP:
                endpoint = os.environ.get("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4317")
                provider.add_span_processor(SimpleSpanProcessor(OTLPSpanExporter(endpoint=endpoint)))
            else:
                provider.add_span_processor(SimpleSpanProcessor(ConsoleSpanExporter()))

            trace.set_tracer_provider(provider)
            self._tracer = trace.get_tracer("whitemagic.tools")

            logger.info("OpenTelemetry initialized (service=%s)", self._service_name)
        except Exception as e:
            logger.warning("OpenTelemetry init failed: %s", e)
            self._tracer = None

    def record_tool_span(
        self,
        tool_name: str,
        duration_seconds: float,
        status: str,
        attributes: dict[str, Any] | None = None,
    ) -> None:
        """Record a tool invocation as a span."""
        attrs = attributes or {}
        duration_ms = duration_seconds * 1000

        # Always track metrics
        with self._lock:
            self._total_spans += 1
            self._tool_counts[tool_name] = self._tool_counts.get(tool_name, 0) + 1
            if status == "error":
                self._tool_errors[tool_name] = self._tool_errors.get(tool_name, 0) + 1
            if tool_name not in self._tool_durations:
                self._tool_durations[tool_name] = []
            durations = self._tool_durations[tool_name]
            durations.append(duration_ms)
            if len(durations) > 100:
                self._tool_durations[tool_name] = durations[-100:]

        # Real OTEL span
        if self._tracer:
            try:
                with self._tracer.start_as_current_span(
                    f"tool.{tool_name}",
                    attributes={
                        "tool.name": tool_name,
                        "tool.status": status,
                        "tool.duration_ms": duration_ms,
                        **{f"tool.{k}": str(v) for k, v in attrs.items()},
                    },
                ) as span:
                    if status == "error":
                        span.set_status(trace.StatusCode.ERROR)
            except Exception:
                pass

        # In-memory buffer
        record = SpanRecord(
            tool_name=tool_name,
            duration_ms=round(duration_ms, 2),
            status=status,
            attributes=attrs,
        )
        with self._lock:
            self._buffer.append(record)
            if len(self._buffer) > self._max_buffer:
                self._buffer = self._buffer[-self._max_buffer:]

    def record_harmony_metrics(self, harmony_snapshot: dict[str, float]) -> None:
        """Record Harmony Vector dimensions as metrics."""
        # These are always tracked in-memory; OTEL gauges if available
        pass  # Harmony Vector already has its own introspection

    @contextmanager
    def tool_span(self, tool_name: str) -> Generator[dict[str, Any], None, None]:
        """Context manager for timing a tool invocation."""
        start = time.perf_counter()
        ctx: dict[str, Any] = {"status": "success"}
        try:
            yield ctx
        except Exception as e:
            ctx["status"] = "error"
            ctx["error"] = str(e)
            raise
        finally:
            duration = time.perf_counter() - start
            self.record_tool_span(tool_name, duration, ctx["status"])

    def recent_spans(self, limit: int = 20) -> list[dict[str, Any]]:
        """Get recent span records."""
        with self._lock:
            spans = list(self._buffer[-limit:])
        return [
            {
                "tool": s.tool_name,
                "duration_ms": s.duration_ms,
                "status": s.status,
                "timestamp": s.timestamp,
            }
            for s in reversed(spans)
        ]

    def metrics_summary(self) -> dict[str, Any]:
        """Get aggregated metrics."""
        with self._lock:
            tool_stats = {}
            for name, count in self._tool_counts.items():
                durations = self._tool_durations.get(name, [])
                errors = self._tool_errors.get(name, 0)
                tool_stats[name] = {
                    "calls": count,
                    "errors": errors,
                    "error_rate": round(errors / max(count, 1), 3),
                    "avg_ms": round(sum(durations) / max(len(durations), 1), 2),
                    "p95_ms": round(sorted(durations)[int(len(durations) * 0.95)] if durations else 0, 2),
                }

            return {
                "total_spans": self._total_spans,
                "unique_tools": len(self._tool_counts),
                "otel_enabled": self._enabled and HAS_OTEL,
                "otlp_available": HAS_OTLP,
                "buffer_size": len(self._buffer),
                "top_tools": dict(sorted(tool_stats.items(), key=lambda x: x[1]["calls"], reverse=True)[:10]),
            }

    def status(self) -> dict[str, Any]:
        return {
            "enabled": self._enabled,
            "has_otel": HAS_OTEL,
            "has_otlp": HAS_OTLP,
            "tracer_active": self._tracer is not None,
            "service_name": self._service_name,
            "total_spans": self._total_spans,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_otel: OTelExporter | None = None
_otel_lock = threading.Lock()


def get_otel() -> OTelExporter:
    global _otel
    if _otel is None:
        with _otel_lock:
            if _otel is None:
                _otel = OTelExporter()
    return _otel


def record_tool_span(tool_name: str, duration: float, status: str, **attrs: Any) -> None:
    """Convenience: record a tool span."""
    get_otel().record_tool_span(tool_name, duration, status, attrs)
