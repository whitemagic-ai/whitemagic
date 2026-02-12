"""Green Score Telemetry — Sustainability Metrics for AI Compute.
================================================================
Tracks the environmental efficiency of AI operations by measuring:

  - **Edge vs Cloud ratio**: What percentage of compute runs locally
    vs being sent to remote APIs (cloud LLMs, embedding services)
  - **Tokens saved**: Tokens avoided via local caching, context
    compression, and local model inference
  - **Inference locality**: Whether inference ran on-device (CPU/GPU),
    local network (Ollama), or remote cloud
  - **Compute efficiency**: Operations per watt-hour equivalent
    (estimated from CPU time × TDP)

The Green Score is a single 0-100 metric that summarizes sustainability:
  100 = Fully local, zero cloud calls, maximum efficiency
    0 = Fully cloud-dependent, no local processing

This feeds into the Harmony Vector and is visible via Gnosis introspection.

Gana: gana_mound (Metrics & Caching)

Usage:
    from whitemagic.core.monitoring.green_score import get_green_score
    gs = get_green_score()
    gs.record_inference(locality="edge", tokens_used=150, tokens_saved=50)
    report = gs.report()
"""

from __future__ import annotations

import logging
import threading
import time
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


class InferenceLocality:
    """Where inference was executed."""

    EDGE = "edge"           # On-device CPU/GPU (Rust, Zig, Mojo accelerators)
    LOCAL_LLM = "local_llm"  # Local Ollama or similar
    LOCAL_NET = "local_net"  # LAN-accessible service
    CLOUD = "cloud"          # Remote API (OpenAI, Anthropic, etc.)


@dataclass
class GreenSnapshot:
    """Point-in-time green score metrics."""

    score: float = 100.0  # 0-100 composite green score
    edge_calls: int = 0
    local_llm_calls: int = 0
    local_net_calls: int = 0
    cloud_calls: int = 0
    total_calls: int = 0
    edge_ratio: float = 1.0  # fraction of calls that are edge/local
    tokens_used: int = 0
    tokens_saved: int = 0
    token_savings_ratio: float = 0.0
    estimated_co2_saved_g: float = 0.0  # grams CO2 saved vs all-cloud
    timestamp: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "green_score": round(self.score, 1),
            "edge_calls": self.edge_calls,
            "local_llm_calls": self.local_llm_calls,
            "local_net_calls": self.local_net_calls,
            "cloud_calls": self.cloud_calls,
            "total_calls": self.total_calls,
            "edge_ratio": round(self.edge_ratio, 4),
            "tokens_used": self.tokens_used,
            "tokens_saved": self.tokens_saved,
            "token_savings_ratio": round(self.token_savings_ratio, 4),
            "estimated_co2_saved_g": round(self.estimated_co2_saved_g, 2),
            "timestamp": self.timestamp,
        }


class GreenScore:
    """Sustainability telemetry for AI compute operations.

    Tracks every inference call's locality and token usage to compute
    a composite "green score" reflecting environmental efficiency.
    """

    # CO2 estimates (grams per 1000 tokens)
    # Based on industry averages:
    #   Cloud GPT-4 class: ~4.3g CO2/1000 tokens
    #   Cloud smaller models: ~0.4g CO2/1000 tokens
    #   Local inference: ~0.05g CO2/1000 tokens (CPU only)
    #   Edge (Rust/Zig accelerated): ~0.01g CO2/1000 tokens
    CO2_PER_1K_TOKENS = {
        InferenceLocality.EDGE: 0.01,
        InferenceLocality.LOCAL_LLM: 0.05,
        InferenceLocality.LOCAL_NET: 0.1,
        InferenceLocality.CLOUD: 2.0,  # conservative average
    }

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._edge_calls = 0
        self._local_llm_calls = 0
        self._local_net_calls = 0
        self._cloud_calls = 0
        self._tokens_used = 0
        self._tokens_saved = 0
        self._co2_actual = 0.0   # grams CO2 from actual compute
        self._co2_avoided = 0.0  # grams CO2 avoided vs all-cloud baseline
        self._history: list[dict[str, Any]] = []
        self._max_history = 1000
        self._session_start = time.time()

    # ------------------------------------------------------------------
    # Recording
    # ------------------------------------------------------------------

    def record_inference(
        self,
        locality: str = InferenceLocality.EDGE,
        tokens_used: int = 0,
        tokens_saved: int = 0,
        model: str | None = None,
        tool: str | None = None,
        duration_ms: float = 0.0,
    ) -> None:
        """Record an inference operation.

        Args:
            locality: Where the inference ran (edge/local_llm/local_net/cloud).
            tokens_used: Tokens consumed by this operation.
            tokens_saved: Tokens avoided (via caching, compression, etc.).
            model: Model name if applicable.
            tool: Tool name that triggered the inference.
            duration_ms: Operation duration in milliseconds.
        """
        with self._lock:
            if locality == InferenceLocality.EDGE:
                self._edge_calls += 1
            elif locality == InferenceLocality.LOCAL_LLM:
                self._local_llm_calls += 1
            elif locality == InferenceLocality.LOCAL_NET:
                self._local_net_calls += 1
            else:
                self._cloud_calls += 1

            self._tokens_used += tokens_used
            self._tokens_saved += tokens_saved

            # CO2 accounting
            actual_co2 = (tokens_used / 1000.0) * self.CO2_PER_1K_TOKENS.get(
                locality, self.CO2_PER_1K_TOKENS[InferenceLocality.CLOUD],
            )
            cloud_baseline_co2 = (tokens_used / 1000.0) * self.CO2_PER_1K_TOKENS[
                InferenceLocality.CLOUD
            ]
            saved_co2 = (tokens_saved / 1000.0) * self.CO2_PER_1K_TOKENS[
                InferenceLocality.CLOUD
            ]

            self._co2_actual += actual_co2
            self._co2_avoided += (cloud_baseline_co2 - actual_co2) + saved_co2

            # History entry
            entry = {
                "locality": locality,
                "tokens_used": tokens_used,
                "tokens_saved": tokens_saved,
                "model": model,
                "tool": tool,
                "duration_ms": round(duration_ms, 1),
                "co2_g": round(actual_co2, 4),
                "timestamp": datetime.now().isoformat(),
            }
            self._history.append(entry)
            if len(self._history) > self._max_history:
                self._history = self._history[-self._max_history:]

    def record_cache_hit(self, tokens_saved: int = 0, tool: str | None = None) -> None:
        """Record a cache hit (avoided inference entirely)."""
        self.record_inference(
            locality=InferenceLocality.EDGE,
            tokens_used=0,
            tokens_saved=tokens_saved,
            tool=tool,
        )

    # ------------------------------------------------------------------
    # Scoring
    # ------------------------------------------------------------------

    def _compute_score(self) -> float:
        """Compute the composite green score (0-100).

        Weighted factors:
          - Locality ratio (40%): edge+local vs cloud
          - Token savings (30%): tokens saved vs total possible
          - CO2 efficiency (30%): actual vs cloud baseline
        """
        total = (
            self._edge_calls + self._local_llm_calls
            + self._local_net_calls + self._cloud_calls
        )
        if total == 0:
            return 100.0  # No compute = perfectly green

        # Locality score (0-1): edge=1.0, local_llm=0.9, local_net=0.7, cloud=0.0
        locality_score = (
            self._edge_calls * 1.0
            + self._local_llm_calls * 0.9
            + self._local_net_calls * 0.7
            + self._cloud_calls * 0.0
        ) / total

        # Token savings score (0-1)
        total_tokens = self._tokens_used + self._tokens_saved
        savings_score = (
            self._tokens_saved / total_tokens if total_tokens > 0 else 0.0
        )

        # CO2 efficiency score (0-1)
        total_co2 = self._co2_actual + self._co2_avoided
        co2_score = self._co2_avoided / total_co2 if total_co2 > 0 else 1.0

        # Weighted composite
        composite = (
            locality_score * 0.4
            + savings_score * 0.3
            + co2_score * 0.3
        )

        return round(min(100.0, max(0.0, composite * 100)), 1)

    # ------------------------------------------------------------------
    # Reporting
    # ------------------------------------------------------------------

    def report(self) -> dict[str, Any]:
        """Generate a full green score report."""
        with self._lock:
            total = (
                self._edge_calls + self._local_llm_calls
                + self._local_net_calls + self._cloud_calls
            )
            local_calls = self._edge_calls + self._local_llm_calls + self._local_net_calls
            edge_ratio = local_calls / max(1, total)

            total_tokens = self._tokens_used + self._tokens_saved
            savings_ratio = self._tokens_saved / max(1, total_tokens)

            snapshot = GreenSnapshot(
                score=self._compute_score(),
                edge_calls=self._edge_calls,
                local_llm_calls=self._local_llm_calls,
                local_net_calls=self._local_net_calls,
                cloud_calls=self._cloud_calls,
                total_calls=total,
                edge_ratio=edge_ratio,
                tokens_used=self._tokens_used,
                tokens_saved=self._tokens_saved,
                token_savings_ratio=savings_ratio,
                estimated_co2_saved_g=self._co2_avoided,
                timestamp=datetime.now().isoformat(),
            )

            result = snapshot.to_dict()
            result["co2_actual_g"] = round(self._co2_actual, 4)
            result["session_duration_s"] = round(time.time() - self._session_start, 1)
            result["recent_history"] = self._history[-10:]

            return result

    def snapshot(self) -> GreenSnapshot:
        """Get a lightweight snapshot for Harmony Vector integration."""
        with self._lock:
            total = (
                self._edge_calls + self._local_llm_calls
                + self._local_net_calls + self._cloud_calls
            )
            local_calls = self._edge_calls + self._local_llm_calls + self._local_net_calls
            total_tokens = self._tokens_used + self._tokens_saved

            return GreenSnapshot(
                score=self._compute_score(),
                edge_calls=self._edge_calls,
                local_llm_calls=self._local_llm_calls,
                local_net_calls=self._local_net_calls,
                cloud_calls=self._cloud_calls,
                total_calls=total,
                edge_ratio=local_calls / max(1, total),
                tokens_used=self._tokens_used,
                tokens_saved=self._tokens_saved,
                token_savings_ratio=self._tokens_saved / max(1, total_tokens),
                estimated_co2_saved_g=self._co2_avoided,
                timestamp=datetime.now().isoformat(),
            )


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_green_score: GreenScore | None = None
_gs_lock = threading.Lock()


def get_green_score() -> GreenScore:
    """Get the global Green Score telemetry singleton."""
    global _green_score
    if _green_score is None:
        with _gs_lock:
            if _green_score is None:
                _green_score = GreenScore()
    return _green_score
