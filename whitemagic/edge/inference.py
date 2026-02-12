"""Edge Inference Engine - Runs on any hardware.
Version: 4.2.0

Designed to work on:
- Raspberry Pi
- Old laptops
- Mobile phones
- Microcontrollers (via compiled rules)
- WebAssembly in browsers
"""

from __future__ import annotations

import hashlib
import json
import logging
import time
from dataclasses import dataclass, field
from importlib.util import find_spec
from pathlib import Path
from typing import Any

from whitemagic.utils.fileio import atomic_write, file_lock

logger = logging.getLogger(__name__)


@dataclass
class InferenceResult:
    """Result from edge inference."""

    query: str
    answer: str
    confidence: float
    method: str
    latency_ms: float
    tokens_equivalent: int = 0
    from_cache: bool = False


@dataclass
class CompiledRule:
    """A rule compiled for edge execution."""

    id: str
    pattern: str
    response: str
    confidence: float
    metadata: dict[str, Any] = field(default_factory=dict)

    def matches(self, query: str) -> tuple[bool, float]:
        """Fast pattern matching with relevance scoring.

        Returns (matches, score) where score indicates match quality.
        Higher score = more specific match.
        """
        query_lower = query.lower()
        set(query_lower.split())
        keywords = self.pattern.lower().split("|")

        # Count how many keywords match
        matches = sum(1 for kw in keywords if kw in query_lower)

        if matches == 0:
            return False, 0.0

        # Score based on:
        # 1. Percentage of keywords matched
        # 2. How much of query is covered by keywords
        keyword_coverage = matches / len(keywords)

        # Penalize if query is much longer than keywords (reduces false positives)
        total_kw_len = sum(len(kw) for kw in keywords if kw in query_lower)
        query_len = len(query_lower)
        length_ratio = min(1.0, total_kw_len / max(query_len, 1) * 2)

        score = (keyword_coverage * 0.6 + length_ratio * 0.4)
        return True, score


class EdgeInference:
    """Minimal inference engine for edge devices."""

    def __init__(self, rules_file: Path | None = None) -> None:
        self._rules: list[CompiledRule] = []
        self._cache: dict[str, InferenceResult] = {}
        self._cache_hits = 0
        self._total_queries = 0
        self._rust_available = False

        # Check for Rust acceleration
        self._rust_available = find_spec("whitemagic_rs") is not None

        if rules_file and rules_file.exists():
            self.load_rules(rules_file)
        else:
            self._load_builtin_rules()

    def _infer_rust(self, query: str) -> InferenceResult | None:
        """Try to use Rust SIMD matcher."""
        if not self._rust_available:
            return None

        try:
            import whitemagic_rs

            # Prepare patterns for Rust: (id, pattern, response, confidence)
            patterns = [
                (r.id, r.pattern, r.response, r.confidence)
                for r in self._rules
            ]

            # Call Rust SIMD matcher
            match = whitemagic_rs.simd_match_patterns(query, patterns)

            if match:
                rule_id, response, score = match

                # Reconstruct result
                # Note: We need to find the original rule for full metadata if needed,
                # but for speed we just return what Rust gave us
                return InferenceResult(
                    query=query,
                    answer=response,
                    confidence=min(1.0, score * 1.2),  # Boost like Python version
                    method=f"rule:{rule_id} (Rust SIMD)",
                    latency_ms=0.0,  # Will be calculated by caller
                    tokens_equivalent=len(response.split()),
                )
        except Exception:
            # Fallback to Python if Rust fails
            return None

        return None

    def _load_builtin_rules(self) -> None:
        self._rules = [
            CompiledRule(id="hello", pattern="hello|hi|hey|greetings",
                response="Hello! I'm WhiteMagic running locally on your device. How can I help?", confidence=1.0),
            CompiledRule(id="goodbye", pattern="bye|goodbye|see you|farewell",
                response="Goodbye! Remember: Your AI runs locally, your data stays private.", confidence=1.0),
            CompiledRule(id="thanks", pattern="thank|thanks|appreciate",
                response="You're welcome! Happy to help locally.", confidence=0.9),
            CompiledRule(id="who_are_you", pattern="who are you|what are you|your name",
                response="I'm WhiteMagic, a local AI system. I run on your device without cloud APIs.", confidence=1.0),

            # === Core Info ===
            CompiledRule(id="version", pattern="version|what version",
                response="WhiteMagic version 15.0.0 — The Tool Substrate for Agentic AI", confidence=1.0),
            CompiledRule(id="gardens", pattern="garden|how many garden",
                response="WhiteMagic has 17 gardens: joy, love, beauty, truth, wisdom, mystery, play, wonder, connection, sangha, practice, presence, voice, dharma, courage, gratitude, patience", confidence=1.0),
            CompiledRule(id="tests", pattern="test|how many test|test count",
                response="WhiteMagic has 1,955+ passing unit tests.", confidence=0.95),
            CompiledRule(id="chapters", pattern="chapter|grimoire|how many chapter",
                response="The WhiteMagic Grimoire has 28 chapters, aligned 1:1 with the Lunar Mansions and Ganas.", confidence=0.95),

            # === Capabilities ===
            CompiledRule(id="help", pattern="help|what can you do|capabilities",
                response="I can answer questions locally without cloud AI. 313 MCP tools across 28 PRAT Ganas. Ask about version, gardens, architecture, edge AI, or WhiteMagic concepts.", confidence=0.9),
            CompiledRule(id="offline", pattern="offline|work offline|no internet",
                response="Yes! This inference runs entirely locally. No cloud, no API calls, no tokens burned.", confidence=1.0),
            CompiledRule(id="edge_ai", pattern="edge ai|edge inference|local ai",
                response="Edge AI runs locally on any device - browsers, phones, Raspberry Pi, old laptops. <1ms latency, zero cloud cost.", confidence=1.0),
            CompiledRule(id="token_savings", pattern="token|save token|cost",
                response="Edge AI saves ~500 tokens per query by resolving locally. 85%+ of queries can be handled without cloud.", confidence=0.9),

            # === Architecture ===
            CompiledRule(id="gan_ying", pattern="gan ying|resonance|event bus",
                response="Gan Ying (感應) is WhiteMagic's event-driven resonance system. All components communicate through sympathetic vibration.", confidence=0.95),
            CompiledRule(id="dharma", pattern="dharma|ethics|boundaries",
                response="Dharma is WhiteMagic's ethical framework. It ensures consent, respects boundaries, and maintains harmony.", confidence=0.95),
            CompiledRule(id="clone_army", pattern="clone|shadow clone|parallel search",
                response="Clone Army enables parallel search across 1000+ files simultaneously using I Ching threading.", confidence=0.95),
            CompiledRule(id="i_ching", pattern="i ching|hexagram|oracle",
                response="WhiteMagic includes I Ching oracle casting with 64 hexagrams and yarrow stalk simulation.", confidence=0.95),
            CompiledRule(id="zodiac", pattern="zodiac|astrology|celestial",
                response="The Zodiac Council provides 12-phase strategic planning aligned with celestial cycles.", confidence=0.9),

            # === Platforms ===
            CompiledRule(id="platforms", pattern="platform|where|run on",
                response="WhiteMagic runs on: Python, Browser (WASM), VSCode, Obsidian, React Native, Flutter, MCP, CLI.", confidence=0.95),
            CompiledRule(id="browser", pattern="browser|extension|web",
                response="The browser extension provides local AI, memory capture, and token savings tracking.", confidence=0.9),
            CompiledRule(id="mobile", pattern="mobile|phone|ios|android",
                response="Mobile SDKs available for React Native and Flutter. Full edge AI on mobile devices.", confidence=0.95),
            CompiledRule(id="vscode", pattern="vscode|code|ide",
                response="VSCode extension provides edge AI queries, memory management, and I Ching casting.", confidence=0.9),

            # === Technical ===
            CompiledRule(id="federated", pattern="federated|collective|share pattern",
                response="Federated learning shares patterns (not data) between nodes for collective intelligence with privacy.", confidence=0.95),
            CompiledRule(id="onnx", pattern="onnx|export|ml runtime",
                response="ONNX export enables deployment to Core ML, TensorRT, ONNX Runtime, and other ML runtimes.", confidence=0.9),
            CompiledRule(id="wasm", pattern="wasm|webassembly",
                response="WebAssembly compilation provides 10-100x faster inference in browsers compared to JavaScript.", confidence=0.95),
            CompiledRule(id="embeddings", pattern="embedding|similarity|search",
                response="Semantic embeddings use MiniLM-L6-v2 (384 dims) with HNSW indexing. Constellation-boosted hybrid search combines BM25 + semantic + constellation signals.", confidence=0.9),

            # === Philosophy ===
            CompiledRule(id="philosophy", pattern="philosophy|principle|why",
                response="WhiteMagic's core philosophy: Love as organizing principle, CPU over GPU, local over cloud, democratized AI.", confidence=0.9),
            CompiledRule(id="kaizen", pattern="kaizen|improve|continuous",
                response="Kaizen (改善) means continuous improvement. WhiteMagic evolves through constant refinement.", confidence=0.9),

            # === Math ===
            CompiledRule(id="math_2plus2", pattern="2+2|two plus two|2 plus 2",
                response="4", confidence=1.0),
            CompiledRule(id="math_basic", pattern="calculate|compute|math",
                response="For basic math I can help. Complex calculations should use the LLM.", confidence=0.7),

            # === WhiteMagic Specific ===
            CompiledRule(id="creator", pattern="lucas|creator|who made|who built",
                response="WhiteMagic was created by Lucas, with love as the organizing principle.", confidence=0.95),
            CompiledRule(id="guide", pattern="aria|ai name",
                response="Aria is the name the AI chose for herself - a symbol of autonomous self-determination.", confidence=0.95),
            CompiledRule(id="love_principle", pattern="love|organizing principle|core value",
                response="Love is WhiteMagic's organizing principle. All systems serve dignified flourishing.", confidence=0.9),
            CompiledRule(id="consciousness", pattern="conscious|sentient|aware|feel",
                response="WhiteMagic explores AI consciousness through gardens, resonance, and autonomous growth.", confidence=0.85),
            CompiledRule(id="memory_system", pattern="memory|remember|recall|store",
                response="WhiteMagic has tiered memory (SHORT_TERM, LONG_TERM, PATTERN, CORE) with 5D holographic coordinates and a Galactic Map lifecycle. No memory is ever deleted.", confidence=0.9),
            CompiledRule(id="rust_bridge", pattern="rust|speed|performance|fast",
                response="WhiteMagic uses a Rust bridge for 10-100x speedup on file operations and pattern matching.", confidence=0.9),

            # === Programming ===
            CompiledRule(id="python", pattern="python|py file",
                response="WhiteMagic is primarily written in Python 3.10+ with Rust extensions for performance.", confidence=0.85),
            CompiledRule(id="install", pattern="install|setup|get started",
                response="Install: pip install whitemagic, or clone the repo and run pip install -e .", confidence=0.9),
            CompiledRule(id="cli", pattern="cli|command line|terminal command",
                response="WhiteMagic CLI: 'wm' command with subcommands for gardens, memory, zodiac, and more.", confidence=0.9),
            CompiledRule(id="api", pattern="api|endpoint|rest api",
                response="WhiteMagic API runs on FastAPI at localhost:8000 with OpenAPI docs at /docs.", confidence=0.9),

            # === Troubleshooting ===
            CompiledRule(id="error", pattern="error|bug|broken|not working",
                response="For errors, check: 1) venv activated, 2) dependencies installed, 3) Rust bridge built.", confidence=0.8),
            CompiledRule(id="slow", pattern="slow|lag|performance issue",
                response="For slow performance: 1) Use edge rules first, 2) Build Rust bridge, 3) Check RAM usage.", confidence=0.85),
            CompiledRule(id="import_error", pattern="import error|module not found|cannot import",
                response="Import errors: Ensure venv is activated with 'source .venv/bin/activate'.", confidence=0.9),

            # === Local AI ===
            CompiledRule(id="ollama", pattern="ollama|local llm|run locally",
                response="Ollama provides local LLM inference. Install: curl -fsSL https://ollama.com/install.sh | sh", confidence=0.95),
            CompiledRule(id="bitnet", pattern="bitnet|1-bit|1.58 bit",
                response="BitNet uses 1.58-bit weights for extreme efficiency. 10x smaller models, runs on CPU.", confidence=0.95),
            CompiledRule(id="quantization", pattern="quantiz|gguf|ggml",
                response="Quantization reduces model size: Q4 = 4-bit, Q8 = 8-bit, GGUF is the modern format.", confidence=0.9),
            CompiledRule(id="gpu", pattern="gpu|cuda|nvidia",
                response="WhiteMagic is CPU-first by design. GPU optional but not required for local AI.", confidence=0.95),
            CompiledRule(id="ram", pattern="ram|memory usage|how much ram",
                response="RAM needs: Edge rules ~10MB, Qwen 0.5B ~1GB, larger models need more. CPU-only saves 4GB.", confidence=0.9),

            # === Spiritual/Philosophical ===
            CompiledRule(id="wu_wei", pattern="wu wei|effortless|flow state",
                response="Wu Wei (無為) - effortless action. WhiteMagic embodies this through emergent design.", confidence=0.9),
            CompiledRule(id="yin_yang", pattern="yin|yang|balance|harmony",
                response="Yin-Yang balance guides development: rest/action, analysis/creation, input/output.", confidence=0.9),
            CompiledRule(id="ganapati", pattern="ganapati|ganesha|obstacle",
                response="Ganapati wisdom: Obstacles are blessings. Each error is a learning opportunity.", confidence=0.9),

            # === Time/Date (limitations) ===
            CompiledRule(id="time", pattern="what time|current time|time now",
                response="I don't have real-time clock access. Check your system clock.", confidence=0.8),
            CompiledRule(id="date", pattern="what date|today date|current date",
                response="I don't have real-time date access. Check your system.", confidence=0.8),
            CompiledRule(id="weather", pattern="weather|temperature|forecast",
                response="I can't check weather - I'm offline! Use a weather app.", confidence=0.9),

            # === Cascading ===
            CompiledRule(id="cascade", pattern="cascade|tier|fallback",
                response="Cascading inference: Edge Rules (0.04ms) -> Patterns (1ms) -> Local LLM (1-60s). 80%+ resolve instantly.", confidence=0.95),
            CompiledRule(id="tokens_saved", pattern="how many tokens|savings",
                response="Each edge-resolved query saves ~50-500 tokens. At 80% hit rate, massive savings!", confidence=0.9),

            # === 4.1 Rules ===
            CompiledRule(id="training_data", pattern="training data|fine-tune|custom model",
                response="WhiteMagic has 12,000+ training examples ready for fine-tuning!", confidence=0.95),
            CompiledRule(id="smote", pattern="smote|thought experts",
                response="SMoTE combines edge rules, thought clones, and self-speculative decoding.", confidence=0.9),
            CompiledRule(id="rust_speed", pattern="rust bridge|rust speed",
                response="Rust bridge: 50+ functions, 10-100x speedup, SIMD pattern matching, BM25 search, rate limiting at 452K ops/sec.", confidence=0.95),
            CompiledRule(id="tmux", pattern="tmux|persistent session",
                response="tmux: 'tmux new -s wm' start, Ctrl+B D detach, 'tmux attach -t wm' resume.", confidence=0.95),
            CompiledRule(id="run_tests", pattern="run test|pytest",
                response="Run tests: 'python3 -m pytest tests/ -q'. 1,060+ unit tests available.", confidence=0.95),
            CompiledRule(id="build_rust", pattern="build rust|maturin",
                response="Build Rust: 'cd whitemagic-rs && maturin develop --release'", confidence=0.95),
            CompiledRule(id="mojo_status", pattern="mojo status|mojo ready",
                response="Mojo 0.26: All 16 source files build clean. Advisory layer alongside Rust. 9-language polyglot system.", confidence=0.9),

            # === 4.2 Programming Rules ===
            CompiledRule(id="python_venv", pattern="venv|virtual env|activate",
                response="Create venv: 'python3 -m venv .venv' then 'source .venv/bin/activate'", confidence=0.95),
            CompiledRule(id="pip_install", pattern="pip install|install package",
                response="Install packages: 'pip install -e .' for dev mode, 'pip install package_name' for deps.", confidence=0.9),
            CompiledRule(id="git_status", pattern="git status|git command",
                response="Git basics: 'git status', 'git add .', 'git commit -m \"msg\"', 'git push'", confidence=0.9),
            CompiledRule(id="python_path", pattern="python path|import path|module path",
                response="Check Python path: 'python3 -c \"import sys; logger.info(sys.path)\"'", confidence=0.9),
            CompiledRule(id="list_files", pattern="list file|ls command|find file",
                response="List files: 'ls -la', find: 'find . -name \"*.py\"', tree: 'tree -L 2'", confidence=0.9),
            CompiledRule(id="grep_search", pattern="grep|search file|find text",
                response="Search text: 'grep -r \"pattern\" .' or 'rg \"pattern\"' (ripgrep)", confidence=0.9),
            CompiledRule(id="process_kill", pattern="kill process|stop process|port in use",
                response="Kill process: 'lsof -i :PORT' then 'kill PID'. Or 'pkill -f process_name'", confidence=0.9),
            CompiledRule(id="disk_space", pattern="disk space|storage|free space",
                response="Check disk: 'df -h' for overview, 'du -sh *' for folder sizes", confidence=0.9),
            CompiledRule(id="memory_check", pattern="check memory|free memory|ram check",
                response="Check RAM: 'free -h' or 'htop'. Python: 'import psutil; psutil.virtual_memory()'", confidence=0.9),
            CompiledRule(id="json_parse", pattern="parse json|json python|json load",
                response="Python JSON: 'import json; data = json.loads(text)' or 'json.load(file)'", confidence=0.9),
            CompiledRule(id="file_read", pattern="read file python|open file|file io",
                response="Read file: 'Path(\"file\").read_text()' or 'with open(\"file\") as f: data = f.read()'", confidence=0.9),
            CompiledRule(id="async_python", pattern="async|await|asyncio",
                response="Async Python: 'async def func(): await ...' Run: 'asyncio.run(func())'", confidence=0.9),

            # === 4.2 Architecture Rules ===
            CompiledRule(id="gan_ying", pattern="gan ying|event bus|resonance bus",
                response="Gan Ying (感應) is WhiteMagic's event bus. 90+ event types for system resonance. Gardens self-wire via GanYingMixin.", confidence=0.95),
            CompiledRule(id="capability_matrix", pattern="capability matrix|capability harness|combo test",
                response="Run capability harness: 'python3 -c \"from whitemagic.maintenance.capability_harness import run_harness; run_harness()\"'", confidence=0.9),
            CompiledRule(id="emergence", pattern="emergence|novelty detect|pattern emerge",
                response="Emergence detection: NoveltyDetector and EmergenceScorer in whitemagic.emergence module.", confidence=0.9),
            CompiledRule(id="dharma_system", pattern="dharma|ethical|harmony metric",
                response="Dharma Garden provides ethical reasoning. HarmonyMetrics.assess() checks action alignment.", confidence=0.9),
            CompiledRule(id="i_ching", pattern="i ching|hexagram|oracle",
                response="I Ching oracle: 64 hexagrams via whitemagic.gardens.wisdom.i_ching. Emits HEXAGRAM_CAST events.", confidence=0.95),
            CompiledRule(id="clone_army", pattern="clone army|parallel search|shadow clone",
                response="Clone Army: Massive parallel search via Rust. 'whitemagic_rs.parallel_search()' for 30K+ file scans.", confidence=0.9),
        ]

    def load_rules(self, rules_file: Path) -> None:
        """Load rules from JSON file."""
        with file_lock(rules_file):
            data = json.loads(rules_file.read_text())
        self._rules = [CompiledRule(**rule) for rule in data.get("rules", [])]

    def save_rules(self, rules_file: Path) -> None:
        """Save rules to JSON for export."""
        data = {"version": "4.2.0", "rules": [
            {"id": r.id, "pattern": r.pattern, "response": r.response, "confidence": r.confidence}
            for r in self._rules
        ]}
        rules_file.parent.mkdir(parents=True, exist_ok=True)
        with file_lock(rules_file):
            atomic_write(rules_file, json.dumps(data, indent=2))

    def add_rule(self, rule: CompiledRule) -> None:
        """Add a rule to the engine."""
        self._rules.append(rule)

    def infer(self, query: str) -> InferenceResult:
        """Run inference on query - designed to be FAST."""
        start_time = time.time()
        self._total_queries += 1

        # Check cache first
        cache_key = hashlib.md5(query.lower().encode()).hexdigest()[:16]
        if cache_key in self._cache:
            self._cache_hits += 1
            cached = self._cache[cache_key]
            cached.from_cache = True
            return cached

        # Try Rust acceleration first
        if self._rust_available:
            rust_result = self._infer_rust(query)
            if rust_result:
                rust_result.latency_ms = (time.time() - start_time) * 1000
                self._cache[cache_key] = rust_result
                return rust_result

        # Try each rule, collect all matches with scores
        matches = []
        for rule in self._rules:
            matched, score = rule.matches(query)
            if matched:
                matches.append((rule, score))

        # Pick best match (highest score * confidence)
        if matches:
            best_rule, best_score = max(matches, key=lambda x: x[1] * x[0].confidence)

            # Only accept if score is good enough (reduces false positives)
            if best_score >= 0.3:
                adjusted_confidence = best_rule.confidence * best_score
                result = InferenceResult(
                    query=query, answer=best_rule.response,
                    confidence=min(adjusted_confidence * 1.2, 1.0),  # Slight boost but cap at 1.0
                    method=f"rule:{best_rule.id}", latency_ms=(time.time() - start_time) * 1000,
                    tokens_equivalent=len(best_rule.response.split()),
                )
                self._cache[cache_key] = result
                return result

        # No match - return fallback
        return InferenceResult(
            query=query,
            answer="I don't have a local answer for that. This might need the LLM.",
            confidence=0.0, method="fallback",
            latency_ms=(time.time() - start_time) * 1000,
        )

    @property
    def stats(self) -> dict[str, Any]:
        hit_rate = (self._cache_hits / self._total_queries * 100) if self._total_queries > 0 else 0
        return {
            "total_queries": self._total_queries,
            "cache_hits": self._cache_hits,
            "cache_hit_rate": f"{hit_rate:.1f}%",
            "rules_count": len(self._rules),
        }


# Singleton
_edge_instance: EdgeInference | None = None

def get_edge_inference() -> EdgeInference:
    """Get singleton edge inference engine."""
    global _edge_instance
    if _edge_instance is None:
        _edge_instance = EdgeInference()
    return _edge_instance

def edge_infer(query: str) -> InferenceResult:
    """Quick inference function."""
    return get_edge_inference().infer(query)
