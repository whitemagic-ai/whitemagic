# Chapter 19: Pattern Capture

**Gana**: NetGana (Chinese: 毕, Pinyin: Bì)
**Garden**: voice
**Quadrant**: Western (White Tiger)
**Element**: Metal
**Phase**: Yin Rising
**I Ching Hexagram**: 4. 蒙 Méng (Youthful Folly) - Learning through capture

---

## 🎯 Purpose

Chapter 19 performs **pattern capture**—finding similar problems, capturing solutions, synthesizing voice patterns. The Net catches what would otherwise escape.

**Metal Element Energy**: Contraction, discernment, selective capture. Like a net that catches fish while letting water pass, this chapter captures meaningful patterns while releasing noise.

Use this chapter when you need to:
- **Find similar past problems** and solutions
- **Capture solution patterns** for reuse
- **Synthesize voice signatures** for consistent communication
- **Learn from repetition** cycles
- **Build pattern libraries** for future reference
- **Detect emergent patterns** before they fully manifest

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `EnhancedPatternEngine` | ML-based pattern extraction | Automatic pattern discovery |
| `VoiceGarden` | Voice synthesis and expression | Communication patterns |
| `EmergenceDetector` | Novel pattern detection | Discovery mode |
| `search_memories` | Find similar problems | Pattern matching |
| `DreamSynthesizer` | Cross-domain pattern synthesis | Creative connections |

---

## 📚 Comprehensive Workflows

### Workflow 1: Pattern Extraction from Memories

**Purpose**: Extract recurring patterns from your memory store to identify what you've learned.

**When to Use**:
- After completing a series of related tasks
- When building documentation from experience
- Before starting similar work to leverage past lessons
- During retrospectives or knowledge capture

**Code Example**:

```python
from whitemagic.core.patterns.pattern_consciousness import EnhancedPatternEngine
from whitemagic.core.memory.manager import MemoryManager
from typing import List, Dict, Any
from datetime import datetime, timedelta
import asyncio

class MemoryPatternExtractor:
    """Extract patterns from historical memories."""

    def __init__(self):
        self.engine = EnhancedPatternEngine()
        self.memory_manager = MemoryManager()

    async def extract_from_timeframe(
        self,
        days_back: int = 30,
        min_occurrences: int = 3
    ) -> List[Dict[str, Any]]:
        """Extract patterns from recent memories."""
        # 1. Retrieve memories from timeframe
        cutoff_date = datetime.now() - timedelta(days=days_back)
        memories = await self.memory_manager.search_memories(
            query="",
            start_date=cutoff_date,
            limit=1000
        )

        print(f"Analyzing {len(memories)} memories from last {days_back} days...")

        # 2. Extract patterns from each memory
        all_patterns = []
        for memory in memories:
            content = memory.get('content', '')
            patterns = self.engine.extract_patterns(content)

            for pattern in patterns:
                pattern['source_memory'] = memory.get('id')
                pattern['tags'] = memory.get('tags', [])
                all_patterns.append(pattern)

        # 3. Cluster by similarity
        pattern_clusters = self._cluster_patterns(all_patterns)

        # 4. Filter by minimum occurrences
        significant_patterns = [
            cluster for cluster in pattern_clusters
            if cluster['count'] >= min_occurrences
        ]

        print(f"Found {len(significant_patterns)} significant patterns")

        return significant_patterns

    def _cluster_patterns(
        self,
        patterns: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """Cluster similar patterns together."""
        clusters = {}

        for pattern in patterns:
            pattern_key = pattern.get('pattern', '')
            pattern_type = pattern.get('type', 'unknown')

            # Create cluster key (type + pattern name)
            cluster_key = f"{pattern_type}:{pattern_key}"

            if cluster_key not in clusters:
                clusters[cluster_key] = {
                    'pattern': pattern_key,
                    'type': pattern_type,
                    'count': 0,
                    'confidence_avg': 0.0,
                    'source_memories': [],
                    'tags': set()
                }

            # Aggregate data
            cluster = clusters[cluster_key]
            cluster['count'] += 1
            cluster['confidence_avg'] = (
                (cluster['confidence_avg'] * (cluster['count'] - 1) +
                 pattern.get('confidence', 0.5)) / cluster['count']
            )
            cluster['source_memories'].append(pattern.get('source_memory'))
            cluster['tags'].update(pattern.get('tags', []))

        # Convert to list and sort by count
        return sorted(
            [
                {**v, 'tags': list(v['tags'])}
                for v in clusters.values()
            ],
            key=lambda x: x['count'],
            reverse=True
        )

    async def save_pattern_library(
        self,
        patterns: List[Dict[str, Any]],
        library_name: str
    ):
        """Save extracted patterns as a library for future use."""
        library_entry = {
            'library_name': library_name,
            'patterns': patterns,
            'created_at': datetime.now().isoformat(),
            'pattern_count': len(patterns),
            'metadata': {
                'total_occurrences': sum(p['count'] for p in patterns),
                'avg_confidence': sum(p['confidence_avg'] for p in patterns) / len(patterns) if patterns else 0
            }
        }

        # Store as long-term memory
        await self.memory_manager.create_memory(
            content=f"Pattern library: {library_name}",
            type="long_term",
            tags=["pattern_library", library_name],
            metadata=library_entry
        )

        print(f"✅ Saved pattern library '{library_name}' with {len(patterns)} patterns")

# Usage
async def main():
    extractor = MemoryPatternExtractor()

    # Extract patterns from last 30 days
    patterns = await extractor.extract_from_timeframe(
        days_back=30,
        min_occurrences=3
    )

    # Display top patterns
    print("\n📊 Top Patterns Discovered:")
    for i, pattern in enumerate(patterns[:10], 1):
        print(f"{i}. {pattern['pattern']} "
              f"(type: {pattern['type']}, "
              f"count: {pattern['count']}, "
              f"confidence: {pattern['confidence_avg']:.2f})")

    # Save for future use
    await extractor.save_pattern_library(
        patterns,
        library_name="development_patterns_january"
    )

# asyncio.run(main())
```

**Best Practices**:
- Run pattern extraction regularly (weekly/monthly)
- Set minimum occurrence thresholds to filter noise
- Tag pattern libraries by domain/timeframe
- Review high-confidence patterns for insights
- Cross-reference with gardens (e.g., wisdom patterns → wisdom garden)

---

### Workflow 2: Building a Pattern Library

**Purpose**: Create reusable pattern libraries organized by domain, making past solutions easily discoverable.

**When to Use**:
- After completing a project phase
- When documenting team knowledge
- Before onboarding new team members
- During knowledge management initiatives

**Code Example**:

```python
from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional
from enum import Enum
import json
from pathlib import Path

class PatternDomain(Enum):
    """Categories for pattern organization."""
    ARCHITECTURE = "architecture"
    BUG_FIX = "bug_fix"
    OPTIMIZATION = "optimization"
    INTEGRATION = "integration"
    TESTING = "testing"
    DEPLOYMENT = "deployment"
    DEBUGGING = "debugging"
    COMMUNICATION = "communication"

@dataclass
class CapturedPattern:
    """Structure for a captured pattern."""
    name: str
    domain: PatternDomain
    problem: str
    solution: str
    context: str
    tags: List[str] = field(default_factory=list)
    examples: List[str] = field(default_factory=list)
    anti_patterns: List[str] = field(default_factory=list)
    confidence: float = 0.8
    usage_count: int = 0
    metadata: Dict[str, Any] = field(default_factory=dict)

class PatternLibrary:
    """Organize and manage pattern libraries."""

    def __init__(self, library_path: Optional[Path] = None):
        if library_path is None:
            library_path = Path.home() / ".whitemagic" / "patterns"
        self.library_path = library_path
        self.library_path.mkdir(parents=True, exist_ok=True)
        self.patterns: Dict[PatternDomain, List[CapturedPattern]] = {
            domain: [] for domain in PatternDomain
        }
        self._load_library()

    def add_pattern(self, pattern: CapturedPattern):
        """Add a pattern to the library."""
        domain_patterns = self.patterns[pattern.domain]

        # Check for duplicates
        existing = next(
            (p for p in domain_patterns if p.name == pattern.name),
            None
        )

        if existing:
            # Update existing pattern
            existing.usage_count += 1
            existing.examples.extend(pattern.examples)
            existing.confidence = max(existing.confidence, pattern.confidence)
            print(f"📝 Updated existing pattern: {pattern.name}")
        else:
            domain_patterns.append(pattern)
            print(f"✨ Added new pattern: {pattern.name}")

        self._save_library()

    def search_patterns(
        self,
        query: str,
        domain: Optional[PatternDomain] = None,
        min_confidence: float = 0.5
    ) -> List[CapturedPattern]:
        """Search for patterns matching query."""
        results = []
        query_lower = query.lower()

        # Select domains to search
        domains_to_search = (
            [domain] if domain
            else list(PatternDomain)
        )

        for dom in domains_to_search:
            for pattern in self.patterns[dom]:
                # Skip low confidence
                if pattern.confidence < min_confidence:
                    continue

                # Search in name, problem, solution, tags
                if (query_lower in pattern.name.lower() or
                    query_lower in pattern.problem.lower() or
                    query_lower in pattern.solution.lower() or
                    any(query_lower in tag.lower() for tag in pattern.tags)):
                    results.append(pattern)

        # Sort by confidence and usage
        results.sort(
            key=lambda p: (p.confidence, p.usage_count),
            reverse=True
        )

        return results

    def get_top_patterns(
        self,
        domain: Optional[PatternDomain] = None,
        limit: int = 10
    ) -> List[CapturedPattern]:
        """Get most used/confident patterns."""
        patterns = []

        if domain:
            patterns = self.patterns[domain]
        else:
            for domain_patterns in self.patterns.values():
                patterns.extend(domain_patterns)

        # Sort by usage count and confidence
        patterns.sort(
            key=lambda p: (p.usage_count, p.confidence),
            reverse=True
        )

        return patterns[:limit]

    def export_library(self, output_path: Path):
        """Export library to JSON for sharing."""
        export_data = {
            'version': '1.0',
            'domains': {}
        }

        for domain, patterns in self.patterns.items():
            export_data['domains'][domain.value] = [
                {
                    'name': p.name,
                    'problem': p.problem,
                    'solution': p.solution,
                    'context': p.context,
                    'tags': p.tags,
                    'examples': p.examples,
                    'anti_patterns': p.anti_patterns,
                    'confidence': p.confidence,
                    'usage_count': p.usage_count,
                    'metadata': p.metadata
                }
                for p in patterns
            ]

        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2)

        print(f"📦 Exported pattern library to {output_path}")

    def _save_library(self):
        """Save library to disk."""
        for domain, patterns in self.patterns.items():
            domain_file = self.library_path / f"{domain.value}.json"

            pattern_data = [
                {
                    'name': p.name,
                    'problem': p.problem,
                    'solution': p.solution,
                    'context': p.context,
                    'tags': p.tags,
                    'examples': p.examples,
                    'anti_patterns': p.anti_patterns,
                    'confidence': p.confidence,
                    'usage_count': p.usage_count,
                    'metadata': p.metadata
                }
                for p in patterns
            ]

            with open(domain_file, 'w') as f:
                json.dump(pattern_data, f, indent=2)

    def _load_library(self):
        """Load library from disk."""
        for domain in PatternDomain:
            domain_file = self.library_path / f"{domain.value}.json"

            if domain_file.exists():
                with open(domain_file, 'r') as f:
                    pattern_data = json.load(f)

                self.patterns[domain] = [
                    CapturedPattern(
                        name=p['name'],
                        domain=domain,
                        problem=p['problem'],
                        solution=p['solution'],
                        context=p['context'],
                        tags=p.get('tags', []),
                        examples=p.get('examples', []),
                        anti_patterns=p.get('anti_patterns', []),
                        confidence=p.get('confidence', 0.8),
                        usage_count=p.get('usage_count', 0),
                        metadata=p.get('metadata', {})
                    )
                    for p in pattern_data
                ]

# Usage
library = PatternLibrary()

# Add a pattern
library.add_pattern(CapturedPattern(
    name="Async Memory Search with Timeout",
    domain=PatternDomain.OPTIMIZATION,
    problem="Memory searches occasionally hang, blocking entire workflow",
    solution="Wrap search in asyncio.wait_for with 30s timeout, implement graceful degradation",
    context="Production system with 100K+ memories, occasional network latency",
    tags=["async", "timeout", "memory", "reliability"],
    examples=[
        "async with asyncio.timeout(30): results = await search_memories(query)",
        "try: results = await asyncio.wait_for(search(), timeout=30.0) except TimeoutError: ..."
    ],
    anti_patterns=[
        "No timeout (can hang forever)",
        "Synchronous blocking calls in async context"
    ],
    confidence=0.95
))

# Search for patterns
results = library.search_patterns("timeout async")
for pattern in results:
    print(f"\n🎯 {pattern.name}")
    print(f"Problem: {pattern.problem}")
    print(f"Solution: {pattern.solution}")
    print(f"Confidence: {pattern.confidence:.2f}")

# Get top patterns
top_patterns = library.get_top_patterns(limit=5)
print("\n📊 Top 5 Patterns:")
for i, pattern in enumerate(top_patterns, 1):
    print(f"{i}. {pattern.name} (used {pattern.usage_count}x, conf: {pattern.confidence:.2f})")
```

**Best Practices**:
- Capture patterns immediately after solving problems
- Include anti-patterns (what NOT to do)
- Provide concrete code examples
- Track usage count to identify most valuable patterns
- Export libraries for team sharing
- Review and update patterns as solutions evolve

---

### Workflow 3: Voice Pattern Synthesis and Analysis

**Purpose**: Synthesize consistent voice patterns for communication, documentation, or agent personality.

**When to Use**:
- Building chatbots or conversational agents
- Maintaining consistent documentation tone
- Analyzing communication effectiveness
- Creating brand voice guidelines

**Code Example**:

```python
from whitemagic.gardens.voice import get_voice_garden
from whitemagic.gardens.voice.voice_synthesis import VoiceSynthesis
from typing import List, Dict, Any
import asyncio

class VoicePatternAnalyzer:
    """Analyze and synthesize voice patterns."""

    def __init__(self):
        self.voice_garden = get_voice_garden()
        self.synthesis = VoiceSynthesis()
        self.patterns = []

    def analyze_corpus(self, texts: List[str]) -> Dict[str, Any]:
        """Analyze a corpus of text for voice patterns."""
        print(f"Analyzing {len(texts)} text samples...")

        patterns = {
            'word_frequency': {},
            'phrase_patterns': [],
            'tone_markers': {
                'formal': 0,
                'casual': 0,
                'technical': 0,
                'friendly': 0
            },
            'sentence_structure': {
                'avg_length': 0,
                'complexity': 0
            },
            'stylistic_features': []
        }

        total_words = 0
        total_sentences = 0

        for text in texts:
            # Word frequency
            words = text.lower().split()
            total_words += len(words)

            for word in words:
                clean_word = ''.join(c for c in word if c.isalnum())
                if clean_word:
                    patterns['word_frequency'][clean_word] = \
                        patterns['word_frequency'].get(clean_word, 0) + 1

            # Sentence analysis
            sentences = [s.strip() for s in text.split('.') if s.strip()]
            total_sentences += len(sentences)

            # Tone detection
            text_lower = text.lower()
            if any(marker in text_lower for marker in ['furthermore', 'thus', 'therefore']):
                patterns['tone_markers']['formal'] += 1
            if any(marker in text_lower for marker in ['yeah', 'cool', 'awesome']):
                patterns['tone_markers']['casual'] += 1
            if any(marker in text_lower for marker in ['algorithm', 'function', 'variable']):
                patterns['tone_markers']['technical'] += 1
            if any(marker in text_lower for marker in ['!', '😊', 'happy']):
                patterns['tone_markers']['friendly'] += 1

            # Detect stylistic patterns
            if '?' in text:
                patterns['stylistic_features'].append('questioning')
            if text.count(',') / len(words) > 0.1:
                patterns['stylistic_features'].append('complex_sentences')
            if any(word.isupper() for word in text.split()):
                patterns['stylistic_features'].append('emphasis')

        # Calculate averages
        patterns['sentence_structure']['avg_length'] = total_words / max(total_sentences, 1)

        # Top words (excluding common words)
        common_words = {'the', 'a', 'an', 'and', 'or', 'but', 'in', 'on', 'at', 'to', 'for'}
        top_words = sorted(
            [(word, count) for word, count in patterns['word_frequency'].items()
             if word not in common_words],
            key=lambda x: x[1],
            reverse=True
        )[:20]
        patterns['top_words'] = top_words

        return patterns

    def synthesize_in_voice(
        self,
        content: str,
        voice_patterns: Dict[str, Any]
    ) -> str:
        """Synthesize content using learned voice patterns."""
        # Use WhiteMagic's voice synthesis
        synthesized = self.synthesis.generate_in_my_voice(content)

        # Apply learned patterns
        tone = max(
            voice_patterns['tone_markers'].items(),
            key=lambda x: x[1]
        )[0]

        print(f"Synthesizing in '{tone}' tone...")

        # Adjust based on tone
        if tone == 'formal':
            synthesized = self._make_formal(synthesized)
        elif tone == 'casual':
            synthesized = self._make_casual(synthesized)
        elif tone == 'technical':
            synthesized = self._make_technical(synthesized)

        return synthesized

    def _make_formal(self, text: str) -> str:
        """Apply formal tone adjustments."""
        replacements = {
            "don't": "do not",
            "can't": "cannot",
            "won't": "will not",
            "yeah": "yes",
            "cool": "excellent"
        }

        for informal, formal in replacements.items():
            text = text.replace(informal, formal)

        return text

    def _make_casual(self, text: str) -> str:
        """Apply casual tone adjustments."""
        # Add conversational markers
        if not any(marker in text.lower() for marker in ['hey', 'cool', 'awesome']):
            text = "Cool! " + text

        return text

    def _make_technical(self, text: str) -> str:
        """Apply technical tone adjustments."""
        # Ensure technical precision
        if "approximately" not in text and any(char.isdigit() for char in text):
            text = text.replace("about", "approximately")

        return text

    async def build_voice_profile(
        self,
        sample_texts: List[str],
        profile_name: str
    ) -> Dict[str, Any]:
        """Build a reusable voice profile."""
        patterns = self.analyze_corpus(sample_texts)

        profile = {
            'name': profile_name,
            'patterns': patterns,
            'sample_count': len(sample_texts),
            'created_at': datetime.now().isoformat()
        }

        # Store as pattern in library
        self.patterns.append(profile)

        print(f"✅ Built voice profile '{profile_name}'")
        print(f"   Dominant tone: {max(patterns['tone_markers'].items(), key=lambda x: x[1])[0]}")
        print(f"   Avg sentence length: {patterns['sentence_structure']['avg_length']:.1f} words")
        print(f"   Top words: {', '.join(w for w, _ in patterns['top_words'][:5])}")

        return profile

# Usage
analyzer = VoicePatternAnalyzer()

# Sample texts in different voices
technical_samples = [
    "The algorithm utilizes a hash-based approach to achieve O(1) lookup time.",
    "We implemented caching to reduce database queries by approximately 80%.",
    "The function accepts a callback parameter for asynchronous error handling."
]

casual_samples = [
    "Yeah, this feature is super cool! Users are gonna love it.",
    "Hey, I found a quick fix for that bug. Works like a charm!",
    "Awesome work on the UI refresh - looks amazing!"
]

# Analyze patterns
tech_patterns = analyzer.analyze_corpus(technical_samples)
casual_patterns = analyzer.analyze_corpus(casual_samples)

print("\n📊 Technical Voice Pattern:")
print(f"Tone: {max(tech_patterns['tone_markers'].items(), key=lambda x: x[1])[0]}")

print("\n📊 Casual Voice Pattern:")
print(f"Tone: {max(casual_patterns['tone_markers'].items(), key=lambda x: x[1])[0]}")

# Synthesize new content in technical voice
new_content = "This new feature helps users work faster"
synthesized = analyzer.synthesize_in_voice(new_content, tech_patterns)
print(f"\n✨ Synthesized (technical): {synthesized}")

# Build voice profile
# await analyzer.build_voice_profile(technical_samples, "developer_docs")
```

**Best Practices**:
- Collect diverse samples (minimum 10-20 texts)
- Separate voice profiles by context (docs vs chat vs marketing)
- Validate synthesized text with human review
- Update profiles as voice evolves
- Use voice garden events to track expression patterns

---

### Workflow 4: Capturing Problem-Solution Pairs

**Purpose**: Systematically capture problems and their solutions as reusable lessons.

**When to Use**:
- After debugging sessions
- When documenting troubleshooting steps
- Building knowledge bases or FAQs
- Creating runbooks for operations

**Code Example**:

```python
from dataclasses import dataclass, field
from typing import List, Optional
from datetime import datetime
import hashlib

@dataclass
class ProblemSolutionPair:
    """Capture a problem and its solution."""
    problem: str
    solution: str
    context: str
    category: str
    severity: str  # "low", "medium", "high", "critical"
    steps_to_reproduce: List[str] = field(default_factory=list)
    resolution_steps: List[str] = field(default_factory=list)
    root_cause: Optional[str] = None
    prevention: Optional[str] = None
    tags: List[str] = field(default_factory=list)
    related_issues: List[str] = field(default_factory=list)
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())

    def __post_init__(self):
        """Generate unique ID for this pair."""
        self.id = hashlib.md5(
            f"{self.problem}{self.timestamp}".encode()
        ).hexdigest()[:8]

class LessonCapture:
    """Capture and organize problem-solution lessons."""

    def __init__(self):
        self.lessons: List[ProblemSolutionPair] = []
        self.categories = set()

    def add_lesson(self, lesson: ProblemSolutionPair):
        """Add a new lesson."""
        self.lessons.append(lesson)
        self.categories.add(lesson.category)

        print(f"📚 Captured lesson: {lesson.id}")
        print(f"   Problem: {lesson.problem[:60]}...")
        print(f"   Category: {lesson.category}")
        print(f"   Severity: {lesson.severity}")

    def find_similar(
        self,
        problem_description: str,
        min_similarity: float = 0.6
    ) -> List[ProblemSolutionPair]:
        """Find similar problems using simple keyword matching."""
        problem_words = set(problem_description.lower().split())
        matches = []

        for lesson in self.lessons:
            lesson_words = set(lesson.problem.lower().split())

            # Calculate Jaccard similarity
            intersection = problem_words & lesson_words
            union = problem_words | lesson_words

            if union:
                similarity = len(intersection) / len(union)

                if similarity >= min_similarity:
                    matches.append((lesson, similarity))

        # Sort by similarity
        matches.sort(key=lambda x: x[1], reverse=True)

        return [lesson for lesson, _ in matches]

    def get_by_category(self, category: str) -> List[ProblemSolutionPair]:
        """Get all lessons in a category."""
        return [l for l in self.lessons if l.category == category]

    def get_by_severity(self, severity: str) -> List[ProblemSolutionPair]:
        """Get all lessons by severity."""
        return [l for l in self.lessons if l.severity == severity]

    def export_runbook(self, category: str, output_path: str):
        """Export category lessons as a runbook."""
        lessons = self.get_by_category(category)

        with open(output_path, 'w') as f:
            f.write(f"# {category.upper()} Runbook\n\n")
            f.write(f"Generated: {datetime.now().isoformat()}\n\n")
            f.write(f"Total Issues: {len(lessons)}\n\n")
            f.write("---\n\n")

            for lesson in lessons:
                f.write(f"## {lesson.problem}\n\n")
                f.write(f"**ID**: {lesson.id}  \n")
                f.write(f"**Severity**: {lesson.severity}  \n")
                f.write(f"**Category**: {lesson.category}  \n\n")

                if lesson.context:
                    f.write(f"**Context**: {lesson.context}\n\n")

                if lesson.steps_to_reproduce:
                    f.write("**Steps to Reproduce**:\n")
                    for i, step in enumerate(lesson.steps_to_reproduce, 1):
                        f.write(f"{i}. {step}\n")
                    f.write("\n")

                f.write(f"**Solution**: {lesson.solution}\n\n")

                if lesson.resolution_steps:
                    f.write("**Resolution Steps**:\n")
                    for i, step in enumerate(lesson.resolution_steps, 1):
                        f.write(f"{i}. {step}\n")
                    f.write("\n")

                if lesson.root_cause:
                    f.write(f"**Root Cause**: {lesson.root_cause}\n\n")

                if lesson.prevention:
                    f.write(f"**Prevention**: {lesson.prevention}\n\n")

                if lesson.tags:
                    f.write(f"**Tags**: {', '.join(lesson.tags)}\n\n")

                f.write("---\n\n")

        print(f"📖 Exported runbook to {output_path}")

# Usage
capture = LessonCapture()

# Capture a lesson
capture.add_lesson(ProblemSolutionPair(
    problem="WhiteMagic memory search times out on large queries",
    solution="Implemented asyncio.wait_for with 30s timeout and result streaming for large result sets",
    context="Production environment with 100K+ memories, occasional network latency spikes",
    category="performance",
    severity="high",
    steps_to_reproduce=[
        "Query memories with broad search term (e.g., 'architecture')",
        "Memory store has >50K entries",
        "Network latency >200ms",
        "Observe timeout after ~60s"
    ],
    resolution_steps=[
        "Wrap search call in asyncio.wait_for(search(), timeout=30.0)",
        "Implement streaming results with yield for large queries",
        "Add circuit breaker to fail fast on repeated timeouts",
        "Monitor timeout metrics in dashboard"
    ],
    root_cause="Synchronous blocking on large result set serialization without timeout protection",
    prevention="Add timeout parameter to all async I/O operations, implement result streaming by default for >100 results",
    tags=["async", "timeout", "memory", "performance", "production"],
    related_issues=["mem-234", "perf-567"]
))

# Find similar problems
similar = capture.find_similar(
    "memory queries are slow and hang sometimes",
    min_similarity=0.3
)

if similar:
    print(f"\n🔍 Found {len(similar)} similar issues:")
    for lesson in similar:
        print(f"  - {lesson.problem[:60]}...")
        print(f"    Solution: {lesson.solution[:60]}...")

# Export runbook
capture.export_runbook("performance", "/tmp/performance_runbook.md")
```

**Best Practices**:
- Capture lessons immediately while details are fresh
- Include reproduction steps (makes future diagnosis easier)
- Document root cause (prevents recurrence)
- Add prevention strategies (shift left on quality)
- Link related issues for pattern detection
- Export runbooks for team knowledge sharing

---

### Workflow 5: Detecting Emergent Patterns

**Purpose**: Detect genuinely novel patterns that indicate new insights or opportunities.

**When to Use**:
- During exploratory data analysis
- When monitoring system behavior for anomalies
- Looking for innovation opportunities
- Detecting trend shifts early

**Code Example**:

```python
from whitemagic.core.patterns.pattern_consciousness import EmergenceDetector
from typing import List, Dict, Any, Set
from datetime import datetime
import asyncio

class EmergenceMonitor:
    """Monitor for emergent patterns in real-time."""

    def __init__(self):
        self.detector = EmergenceDetector()
        self.emergence_log = []
        self.watch_queue = asyncio.Queue()

    async def watch_for_emergence(
        self,
        data_source: str,
        check_interval: int = 60
    ):
        """Continuously watch for emergent patterns."""
        print(f"👁️ Watching {data_source} for emergence...")

        while True:
            # Check for new patterns
            current_patterns = await self._extract_current_patterns(data_source)

            for pattern in current_patterns:
                if self.detector.is_novel(pattern):
                    emergence_event = {
                        'pattern': pattern,
                        'source': data_source,
                        'timestamp': datetime.now().isoformat(),
                        'novelty_score': self._calculate_novelty(pattern)
                    }

                    self.emergence_log.append(emergence_event)
                    print(f"✨ EMERGENT PATTERN DETECTED: {pattern}")

                    # Emit to Gan Ying bus
                    from whitemagic.core.resonance import emit_event
                    emit_event("pattern.emergent", emergence_event)

            await asyncio.sleep(check_interval)

    async def _extract_current_patterns(self, source: str) -> List[str]:
        """Extract current patterns from source."""
        # Placeholder - would connect to actual data source
        patterns = []

        if source == "user_behavior":
            # Analyze user interaction patterns
            patterns.append("rapid_context_switching")
            patterns.append("late_night_coding")
        elif source == "system_metrics":
            # Analyze system patterns
            patterns.append("cpu_spike_on_memory_search")
            patterns.append("gradual_memory_growth")

        return patterns

    def _calculate_novelty(self, pattern: str) -> float:
        """Calculate how novel a pattern is."""
        # Simple novelty score based on how recently we've seen it
        known_patterns = list(self.detector.known_patterns)

        if pattern not in known_patterns:
            return 1.0  # Completely novel

        # Check recency in emergence log
        recent_occurrences = [
            e for e in self.emergence_log
            if e['pattern'] == pattern
        ]

        if len(recent_occurrences) == 0:
            return 0.9
        elif len(recent_occurrences) < 3:
            return 0.7
        else:
            return 0.3  # Frequently seen, low novelty

    def get_emergence_report(self, since_hours: int = 24) -> Dict[str, Any]:
        """Get report of emergent patterns."""
        from datetime import timedelta

        cutoff = datetime.now() - timedelta(hours=since_hours)

        recent_emergence = [
            e for e in self.emergence_log
            if datetime.fromisoformat(e['timestamp']) > cutoff
        ]

        # Group by pattern
        pattern_counts = {}
        for event in recent_emergence:
            pattern = event['pattern']
            pattern_counts[pattern] = pattern_counts.get(pattern, 0) + 1

        # Calculate emergence velocity (how fast patterns are appearing)
        emergence_velocity = len(recent_emergence) / max(since_hours, 1)

        return {
            'total_emergent_patterns': len(recent_emergence),
            'unique_patterns': len(pattern_counts),
            'emergence_velocity': emergence_velocity,  # patterns per hour
            'top_patterns': sorted(
                pattern_counts.items(),
                key=lambda x: x[1],
                reverse=True
            )[:10],
            'timeframe_hours': since_hours
        }

class PatternComparator:
    """Compare patterns across different contexts."""

    def __init__(self):
        self.contexts: Dict[str, Set[str]] = {}

    def add_context_patterns(self, context: str, patterns: List[str]):
        """Add patterns observed in a context."""
        if context not in self.contexts:
            self.contexts[context] = set()

        self.contexts[context].update(patterns)

    def find_cross_domain_patterns(self) -> List[Dict[str, Any]]:
        """Find patterns that appear in multiple domains."""
        pattern_contexts = {}

        # Build reverse index: pattern -> list of contexts
        for context, patterns in self.contexts.items():
            for pattern in patterns:
                if pattern not in pattern_contexts:
                    pattern_contexts[pattern] = []
                pattern_contexts[pattern].append(context)

        # Find patterns in multiple contexts
        cross_domain = [
            {
                'pattern': pattern,
                'contexts': contexts,
                'domain_count': len(contexts)
            }
            for pattern, contexts in pattern_contexts.items()
            if len(contexts) > 1
        ]

        # Sort by domain count
        cross_domain.sort(key=lambda x: x['domain_count'], reverse=True)

        return cross_domain

# Usage
async def monitor_emergence():
    monitor = EmergenceMonitor()

    # Start watching (would run in background)
    # await monitor.watch_for_emergence("user_behavior", check_interval=60)

    # Simulate some emergence events
    monitor.detector.is_novel("rapid_context_switching")  # New
    monitor.detector.is_novel("rapid_context_switching")  # Seen before
    monitor.detector.is_novel("late_night_coding")  # New

    # Get report
    report = monitor.get_emergence_report(since_hours=24)
    print("\n📊 Emergence Report:")
    print(f"Total emergent patterns: {report['total_emergent_patterns']}")
    print(f"Unique patterns: {report['unique_patterns']}")
    print(f"Emergence velocity: {report['emergence_velocity']:.2f} patterns/hour")

# Cross-domain pattern detection
comparator = PatternComparator()
comparator.add_context_patterns("coding", ["debugging", "refactoring", "testing"])
comparator.add_context_patterns("writing", ["editing", "refactoring", "reviewing"])
comparator.add_context_patterns("music", ["composition", "editing", "performance"])

cross_domain = comparator.find_cross_domain_patterns()
print("\n🌐 Cross-Domain Patterns:")
for pattern in cross_domain:
    print(f"  {pattern['pattern']}: appears in {pattern['domain_count']} domains")
    print(f"    Contexts: {', '.join(pattern['contexts'])}")

# asyncio.run(monitor_emergence())
```

**Best Practices**:
- Monitor continuously (emergence happens in real-time)
- Track novelty scores (distinguish truly new from variations)
- Look for cross-domain patterns (often indicate deep insights)
- Emit emergence events to Gan Ying bus (for system-wide awareness)
- Set up alerts for high-novelty patterns
- Review emergence logs regularly for strategic insights

---

### Workflow 6: Learning from Repetition Cycles

**Purpose**: Identify and optimize repetitive patterns in workflows or code.

**When to Use**:
- Identifying automation opportunities
- Detecting code duplication
- Finding workflow inefficiencies
- Building templates from repeated patterns

**Code Example**:

```python
from typing import List, Dict, Any, Tuple
from dataclasses import dataclass
from collections import Counter
import re

@dataclass
class RepetitionPattern:
    """Represents a detected repetition."""
    pattern: str
    occurrences: int
    locations: List[str]
    automation_potential: float  # 0.0 to 1.0
    estimated_time_saved: int  # minutes per week

class RepetitionDetector:
    """Detect repetitive patterns and suggest optimizations."""

    def __init__(self):
        self.detected_patterns = []

    def analyze_command_history(
        self,
        commands: List[str],
        min_occurrences: int = 3
    ) -> List[RepetitionPattern]:
        """Analyze shell command history for repetition."""
        # Count exact command matches
        command_counts = Counter(commands)

        repetitions = []

        for command, count in command_counts.items():
            if count >= min_occurrences:
                # Calculate automation potential
                automation_score = self._assess_automation_potential(command, count)

                # Estimate time saved (assume 30s per command execution)
                time_saved_per_week = (count / len(commands)) * 50 * 0.5  # minutes

                repetitions.append(RepetitionPattern(
                    pattern=command,
                    occurrences=count,
                    locations=["shell_history"],
                    automation_potential=automation_score,
                    estimated_time_saved=int(time_saved_per_week)
                ))

        # Also detect command sequences
        sequences = self._detect_command_sequences(commands, min_occurrences)
        repetitions.extend(sequences)

        # Sort by automation potential
        repetitions.sort(key=lambda x: x.automation_potential, reverse=True)

        return repetitions

    def _assess_automation_potential(self, command: str, count: int) -> float:
        """Assess how suitable a command is for automation."""
        score = 0.0

        # Frequency bonus
        if count > 10:
            score += 0.4
        elif count > 5:
            score += 0.3
        else:
            score += 0.2

        # Complexity bonus (longer commands = more to gain)
        if len(command) > 50:
            score += 0.3
        elif len(command) > 30:
            score += 0.2

        # Pattern indicators (pipes, flags, etc)
        if '|' in command:
            score += 0.1
        if '--' in command:
            score += 0.1
        if any(cmd in command for cmd in ['find', 'grep', 'awk', 'sed']):
            score += 0.1

        return min(score, 1.0)

    def _detect_command_sequences(
        self,
        commands: List[str],
        min_occurrences: int
    ) -> List[RepetitionPattern]:
        """Detect repeated command sequences."""
        sequences = []

        # Look for 2-command and 3-command sequences
        for seq_length in [2, 3]:
            for i in range(len(commands) - seq_length + 1):
                seq = tuple(commands[i:i+seq_length])

                # Count occurrences of this sequence
                count = 0
                for j in range(len(commands) - seq_length + 1):
                    if tuple(commands[j:j+seq_length]) == seq:
                        count += 1

                if count >= min_occurrences:
                    pattern_str = " && ".join(seq)

                    # Check if already added
                    if not any(s.pattern == pattern_str for s in sequences):
                        sequences.append(RepetitionPattern(
                            pattern=pattern_str,
                            occurrences=count,
                            locations=["shell_history"],
                            automation_potential=0.8,  # Sequences have high automation value
                            estimated_time_saved=count * 2  # 2 min per sequence
                        ))

        return sequences

    def detect_code_duplication(
        self,
        code_files: Dict[str, str],
        min_lines: int = 5
    ) -> List[RepetitionPattern]:
        """Detect duplicated code blocks."""
        duplicates = []

        # Extract code blocks from each file
        file_blocks = {}
        for filepath, content in code_files.items():
            lines = content.split('\n')
            blocks = []

            # Create overlapping blocks of min_lines length
            for i in range(len(lines) - min_lines + 1):
                block = '\n'.join(lines[i:i+min_lines])
                # Normalize whitespace
                normalized = re.sub(r'\s+', ' ', block).strip()
                if len(normalized) > 20:  # Skip trivial blocks
                    blocks.append((normalized, i))

            file_blocks[filepath] = blocks

        # Find duplicates across files
        block_locations = {}

        for filepath, blocks in file_blocks.items():
            for block, line_num in blocks:
                if block not in block_locations:
                    block_locations[block] = []
                block_locations[block].append(f"{filepath}:{line_num}")

        # Identify blocks appearing in multiple locations
        for block, locations in block_locations.items():
            if len(locations) >= 2:
                duplicates.append(RepetitionPattern(
                    pattern=block[:100] + "..." if len(block) > 100 else block,
                    occurrences=len(locations),
                    locations=locations,
                    automation_potential=0.7,  # Extract to function
                    estimated_time_saved=len(locations) * 10  # 10 min saved per duplicate
                ))

        # Sort by number of occurrences
        duplicates.sort(key=lambda x: x.occurrences, reverse=True)

        return duplicates

    def suggest_optimizations(
        self,
        pattern: RepetitionPattern
    ) -> Dict[str, Any]:
        """Suggest how to optimize a repetitive pattern."""
        suggestions = {
            'pattern': pattern.pattern,
            'occurrences': pattern.occurrences,
            'optimizations': []
        }

        # Shell command optimizations
        if "shell_history" in pattern.locations:
            if "&&" in pattern.pattern:
                suggestions['optimizations'].append({
                    'type': 'create_script',
                    'description': 'Create a bash script or alias',
                    'example': f'alias mycommand="{pattern.pattern}"'
                })
            else:
                suggestions['optimizations'].append({
                    'type': 'create_alias',
                    'description': 'Add to .bashrc or .zshrc',
                    'example': f'alias shortcut="{pattern.pattern}"'
                })

        # Code duplication optimizations
        elif any('.py' in loc or '.js' in loc for loc in pattern.locations):
            suggestions['optimizations'].append({
                'type': 'extract_function',
                'description': 'Extract repeated code to a function',
                'impact': f"Remove {pattern.occurrences - 1} duplicates"
            })

            if pattern.occurrences >= 3:
                suggestions['optimizations'].append({
                    'type': 'create_utility',
                    'description': 'Consider creating a reusable utility module',
                    'impact': 'Centralized maintenance, easier testing'
                })

        # Estimate ROI
        suggestions['roi'] = {
            'time_saved_per_week': pattern.estimated_time_saved,
            'implementation_time': 15,  # minutes to create alias/function
            'payback_period': 1 if pattern.estimated_time_saved > 15 else 2  # weeks
        }

        return suggestions

# Usage
detector = RepetitionDetector()

# Analyze shell commands
commands = [
    "git status",
    "git add .",
    "git commit -m 'update'",
    "git push",
    "git status",
    "pytest tests/",
    "git add .",
    "git commit -m 'fix'",
    "git push",
    "pytest tests/",
    "pytest tests/",
]

repetitions = detector.analyze_command_history(commands, min_occurrences=2)

print("🔁 Detected Repetitions:")
for pattern in repetitions[:5]:
    print(f"\n  Pattern: {pattern.pattern[:60]}")
    print(f"  Occurrences: {pattern.occurrences}")
    print(f"  Automation potential: {pattern.automation_potential:.2f}")
    print(f"  Time saved: ~{pattern.estimated_time_saved} min/week")

    # Get optimization suggestions
    suggestions = detector.suggest_optimizations(pattern)
    if suggestions['optimizations']:
        print(f"  💡 Suggestion: {suggestions['optimizations'][0]['description']}")

# Analyze code duplication
code_files = {
    "module_a.py": """
def process_user(user_id):
    user = db.get(user_id)
    if user is None:
        raise ValueError("User not found")
    return user
""",
    "module_b.py": """
def process_account(account_id):
    account = db.get(account_id)
    if account is None:
        raise ValueError("Account not found")
    return account
"""
}

duplicates = detector.detect_code_duplication(code_files, min_lines=3)
print("\n\n📋 Code Duplication Detected:")
for dup in duplicates:
    print(f"\n  Found in {len(dup.locations)} locations:")
    for loc in dup.locations[:3]:
        print(f"    - {loc}")
```

**Best Practices**:
- Analyze command history regularly (weekly/monthly)
- Set appropriate thresholds (avoid noise from rare commands)
- Prioritize by ROI (time saved vs implementation effort)
- Create scripts/aliases for high-value repetitions
- Extract functions for code duplication
- Track automation impact (measure actual time saved)

---

### Workflow 7: Cross-Domain Pattern Recognition

**Purpose**: Recognize patterns that transcend domain boundaries, revealing universal principles.

**When to Use**:
- Looking for innovation by analogy
- Building mental models
- Teaching complex concepts
- Finding unexpected solutions

**Code Example**:

```python
from typing import List, Dict, Any, Set, Tuple
from dataclasses import dataclass
from enum import Enum

class Domain(Enum):
    """Knowledge domains."""
    PROGRAMMING = "programming"
    BIOLOGY = "biology"
    MUSIC = "music"
    ARCHITECTURE = "architecture"
    COOKING = "cooking"
    SPORTS = "sports"
    LANGUAGE = "language"
    ECONOMICS = "economics"

@dataclass
class DomainPattern:
    """A pattern within a specific domain."""
    name: str
    domain: Domain
    description: str
    examples: List[str]
    properties: Set[str]  # Abstract properties

class CrossDomainMapper:
    """Map patterns across different domains."""

    def __init__(self):
        self.patterns: Dict[Domain, List[DomainPattern]] = {
            domain: [] for domain in Domain
        }
        self.universal_patterns = []

    def add_pattern(self, pattern: DomainPattern):
        """Add a domain-specific pattern."""
        self.patterns[pattern.domain].append(pattern)

    def find_isomorphic_patterns(self) -> List[Dict[str, Any]]:
        """Find patterns with similar structure across domains."""
        isomorphic_groups = []

        # Compare patterns across all domain pairs
        domains = list(Domain)

        for i, domain1 in enumerate(domains):
            for domain2 in domains[i+1:]:
                # Compare patterns from these two domains
                matches = self._find_matches(
                    self.patterns[domain1],
                    self.patterns[domain2]
                )

                isomorphic_groups.extend(matches)

        return isomorphic_groups

    def _find_matches(
        self,
        patterns1: List[DomainPattern],
        patterns2: List[DomainPattern]
    ) -> List[Dict[str, Any]]:
        """Find matching patterns between two domain pattern lists."""
        matches = []

        for p1 in patterns1:
            for p2 in patterns2:
                # Calculate property overlap
                overlap = p1.properties & p2.properties
                similarity = len(overlap) / len(p1.properties | p2.properties)

                if similarity >= 0.5:  # At least 50% property overlap
                    matches.append({
                        'pattern1': p1,
                        'pattern2': p2,
                        'shared_properties': list(overlap),
                        'similarity': similarity,
                        'universal_principle': self._extract_principle(overlap)
                    })

        return matches

    def _extract_principle(self, properties: Set[str]) -> str:
        """Extract universal principle from shared properties."""
        if 'feedback_loop' in properties and 'amplification' in properties:
            return "Positive feedback creates exponential growth"
        elif 'balance' in properties and 'opposing_forces' in properties:
            return "Homeostasis through dynamic equilibrium"
        elif 'modular' in properties and 'composable' in properties:
            return "Complex systems from simple building blocks"
        elif 'hierarchy' in properties and 'emergence' in properties:
            return "Higher-order properties emerge from lower levels"
        else:
            return f"Shared characteristics: {', '.join(properties)}"

    def apply_analogy(
        self,
        source_domain: Domain,
        target_domain: Domain,
        problem: str
    ) -> List[str]:
        """Apply patterns from source domain to solve problem in target domain."""
        suggestions = []

        # Get patterns from source domain
        source_patterns = self.patterns[source_domain]

        # Find patterns that might apply
        for pattern in source_patterns:
            # Simple keyword matching for problem relevance
            if any(keyword in problem.lower()
                   for keyword in pattern.name.lower().split()):

                suggestion = f"Apply '{pattern.name}' from {source_domain.value}:\n"
                suggestion += f"  Pattern: {pattern.description}\n"
                suggestion += f"  Adaptation: Consider how {target_domain.value} "
                suggestion += f"exhibits similar properties: {', '.join(pattern.properties)}"

                suggestions.append(suggestion)

        return suggestions

# Usage - Define patterns across domains
mapper = CrossDomainMapper()

# Programming patterns
mapper.add_pattern(DomainPattern(
    name="Recursion",
    domain=Domain.PROGRAMMING,
    description="Function that calls itself with simpler input",
    examples=["factorial(n) = n * factorial(n-1)", "tree traversal"],
    properties={'self_reference', 'base_case', 'reduction', 'hierarchy'}
))

mapper.add_pattern(DomainPattern(
    name="Caching",
    domain=Domain.PROGRAMMING,
    description="Store computed results to avoid recomputation",
    examples=["memoization", "CDN", "browser cache"],
    properties={'memory', 'speed_tradeoff', 'staleness_risk', 'optimization'}
))

# Biology patterns
mapper.add_pattern(DomainPattern(
    name="Cell Division",
    domain=Domain.BIOLOGY,
    description="Cell creates copies of itself",
    examples=["mitosis", "binary fission"],
    properties={'self_reference', 'replication', 'hierarchy', 'growth'}
))

mapper.add_pattern(DomainPattern(
    name="Muscle Memory",
    domain=Domain.BIOLOGY,
    description="Neural pathways store movement patterns",
    examples=["typing", "playing instrument", "sports"],
    properties={'memory', 'speed_tradeoff', 'practice', 'optimization'}
))

# Music patterns
mapper.add_pattern(DomainPattern(
    name="Fugue",
    domain=Domain.MUSIC,
    description="Theme introduced then repeated with variations",
    examples=["Bach fugues", "canon"],
    properties={'self_reference', 'variation', 'hierarchy', 'structure'}
))

# Economics patterns
mapper.add_pattern(DomainPattern(
    name="Compound Interest",
    domain=Domain.ECONOMICS,
    description="Interest earned generates more interest",
    examples=["savings accounts", "debt growth"],
    properties={'feedback_loop', 'exponential', 'time_dependent', 'amplification'}
))

# Find cross-domain patterns
isomorphic = mapper.find_isomorphic_patterns()

print("🌐 Cross-Domain Pattern Matches:\n")
for match in isomorphic:
    p1 = match['pattern1']
    p2 = match['pattern2']

    print(f"📍 {p1.name} ({p1.domain.value}) ≈ {p2.name} ({p2.domain.value})")
    print(f"   Similarity: {match['similarity']:.2%}")
    print(f"   Shared properties: {', '.join(match['shared_properties'])}")
    print(f"   Universal principle: {match['universal_principle']}")
    print()

# Apply analogy to solve problem
problem = "How to optimize memory retrieval in WhiteMagic?"
suggestions = mapper.apply_analogy(
    source_domain=Domain.BIOLOGY,
    target_domain=Domain.PROGRAMMING,
    problem=problem
)

print(f"💡 Solving: '{problem}'\n")
for suggestion in suggestions:
    print(suggestion)
    print()
```

**Best Practices**:
- Build diverse pattern libraries across domains
- Focus on abstract properties (not surface features)
- Look for structural isomorphisms
- Use analogies for creative problem-solving
- Validate analogies (not all mappings are useful)
- Document successful cross-domain applications

---

### Workflow 8: Pattern-Based Prediction

**Purpose**: Use captured patterns to predict likely outcomes or detect anomalies.

**When to Use**:
- Predictive maintenance
- Anomaly detection
- Risk assessment
- Trend forecasting

**Code Example**:

```python
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
from datetime import datetime, timedelta
import statistics

@dataclass
class HistoricalEvent:
    """Record of a past event."""
    event_type: str
    timestamp: datetime
    metadata: Dict[str, Any]
    outcome: str

class PatternPredictor:
    """Predict outcomes based on historical patterns."""

    def __init__(self):
        self.history: List[HistoricalEvent] = []
        self.patterns = {}

    def record_event(self, event: HistoricalEvent):
        """Record an event for pattern learning."""
        self.history.append(event)
        self._update_patterns(event)

    def _update_patterns(self, event: HistoricalEvent):
        """Update pattern statistics with new event."""
        event_type = event.event_type

        if event_type not in self.patterns:
            self.patterns[event_type] = {
                'count': 0,
                'outcomes': {},
                'avg_frequency': None,
                'last_occurrence': None,
                'metadata_correlations': {}
            }

        pattern = self.patterns[event_type]
        pattern['count'] += 1
        pattern['last_occurrence'] = event.timestamp

        # Track outcome distribution
        outcome = event.outcome
        pattern['outcomes'][outcome] = pattern['outcomes'].get(outcome, 0) + 1

        # Track metadata correlations
        for key, value in event.metadata.items():
            if key not in pattern['metadata_correlations']:
                pattern['metadata_correlations'][key] = {}

            value_str = str(value)
            if value_str not in pattern['metadata_correlations'][key]:
                pattern['metadata_correlations'][key][value_str] = {'outcomes': {}}

            meta_pattern = pattern['metadata_correlations'][key][value_str]
            meta_pattern['outcomes'][outcome] = \
                meta_pattern['outcomes'].get(outcome, 0) + 1

    def predict_outcome(
        self,
        event_type: str,
        metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Predict likely outcome for an event type."""
        if event_type not in self.patterns:
            return {
                'prediction': 'unknown',
                'confidence': 0.0,
                'message': f'No historical data for {event_type}'
            }

        pattern = self.patterns[event_type]

        # Base prediction on overall outcome distribution
        total_outcomes = sum(pattern['outcomes'].values())
        outcome_probs = {
            outcome: count / total_outcomes
            for outcome, count in pattern['outcomes'].items()
        }

        # Adjust based on metadata if provided
        if metadata:
            for key, value in metadata.items():
                value_str = str(value)

                if (key in pattern['metadata_correlations'] and
                    value_str in pattern['metadata_correlations'][key]):

                    meta_pattern = pattern['metadata_correlations'][key][value_str]
                    meta_total = sum(meta_pattern['outcomes'].values())

                    # Weight this correlation
                    for outcome in outcome_probs:
                        meta_prob = meta_pattern['outcomes'].get(outcome, 0) / meta_total
                        # Average with base probability
                        outcome_probs[outcome] = (outcome_probs[outcome] + meta_prob) / 2

        # Get most likely outcome
        predicted_outcome = max(outcome_probs.items(), key=lambda x: x[1])

        return {
            'prediction': predicted_outcome[0],
            'confidence': predicted_outcome[1],
            'all_probabilities': outcome_probs,
            'sample_size': pattern['count']
        }

    def detect_anomaly(
        self,
        event: HistoricalEvent,
        threshold: float = 0.1
    ) -> Dict[str, Any]:
        """Detect if an event is anomalous based on patterns."""
        prediction = self.predict_outcome(event.event_type, event.metadata)

        is_anomalous = False
        reasons = []

        # Check if outcome was unexpected
        if prediction['prediction'] != 'unknown':
            expected_prob = prediction['all_probabilities'].get(event.outcome, 0)

            if expected_prob < threshold:
                is_anomalous = True
                reasons.append(
                    f"Unexpected outcome '{event.outcome}' "
                    f"(probability: {expected_prob:.2%})"
                )

        # Check if timing is unusual
        if event.event_type in self.patterns:
            pattern = self.patterns[event.event_type]
            if pattern['last_occurrence']:
                time_since_last = (event.timestamp - pattern['last_occurrence']).total_seconds()

                # Calculate average frequency
                if pattern['count'] > 1:
                    events_of_type = [
                        e for e in self.history
                        if e.event_type == event.event_type
                    ]

                    if len(events_of_type) > 1:
                        intervals = []
                        for i in range(1, len(events_of_type)):
                            interval = (events_of_type[i].timestamp -
                                      events_of_type[i-1].timestamp).total_seconds()
                            intervals.append(interval)

                        avg_interval = statistics.mean(intervals)
                        std_interval = statistics.stdev(intervals) if len(intervals) > 1 else 0

                        # Anomaly if more than 2 std deviations from mean
                        if abs(time_since_last - avg_interval) > 2 * std_interval:
                            is_anomalous = True
                            reasons.append(
                                f"Unusual timing (expected ~{avg_interval:.0f}s, "
                                f"actual {time_since_last:.0f}s)"
                            )

        return {
            'is_anomalous': is_anomalous,
            'reasons': reasons,
            'expected': prediction['prediction'] if prediction['prediction'] != 'unknown' else None,
            'actual': event.outcome
        }

    def forecast_next_occurrence(
        self,
        event_type: str
    ) -> Optional[datetime]:
        """Predict when event will occur next."""
        if event_type not in self.patterns:
            return None

        pattern = self.patterns[event_type]

        # Get events of this type
        events_of_type = [
            e for e in self.history
            if e.event_type == event_type
        ]

        if len(events_of_type) < 2:
            return None

        # Calculate average interval
        intervals = []
        for i in range(1, len(events_of_type)):
            interval = (events_of_type[i].timestamp -
                       events_of_type[i-1].timestamp).total_seconds()
            intervals.append(interval)

        avg_interval = statistics.mean(intervals)

        # Predict next occurrence
        last_occurrence = pattern['last_occurrence']
        next_occurrence = last_occurrence + timedelta(seconds=avg_interval)

        return next_occurrence

# Usage
predictor = PatternPredictor()

# Record historical events
predictor.record_event(HistoricalEvent(
    event_type="memory_search",
    timestamp=datetime.now() - timedelta(hours=5),
    metadata={'query_length': 20, 'result_count': 100},
    outcome="success"
))

predictor.record_event(HistoricalEvent(
    event_type="memory_search",
    timestamp=datetime.now() - timedelta(hours=3),
    metadata={'query_length': 15, 'result_count': 50},
    outcome="success"
))

predictor.record_event(HistoricalEvent(
    event_type="memory_search",
    timestamp=datetime.now() - timedelta(hours=1),
    metadata={'query_length': 100, 'result_count': 10000},
    outcome="timeout"
))

# Predict outcome for new event
prediction = predictor.predict_outcome(
    "memory_search",
    metadata={'query_length': 25, 'result_count': 200}
)

print("🔮 Prediction:")
print(f"Expected outcome: {prediction['prediction']}")
print(f"Confidence: {prediction['confidence']:.2%}")
print(f"Sample size: {prediction['sample_size']}")

# Detect anomaly
anomalous_event = HistoricalEvent(
    event_type="memory_search",
    timestamp=datetime.now(),
    metadata={'query_length': 5, 'result_count': 10},
    outcome="timeout"  # Unexpected for small query
)

anomaly_check = predictor.detect_anomaly(anomalous_event)

print(f"\n🚨 Anomaly Detection:")
print(f"Is anomalous: {anomaly_check['is_anomalous']}")
if anomaly_check['reasons']:
    print("Reasons:")
    for reason in anomaly_check['reasons']:
        print(f"  - {reason}")

# Forecast next occurrence
next_time = predictor.forecast_next_occurrence("memory_search")
if next_time:
    print(f"\n📅 Next memory_search predicted at: {next_time.isoformat()}")
```

**Best Practices**:
- Require minimum sample size before predicting (e.g., 10+ events)
- Track confidence scores (low confidence = more data needed)
- Use metadata for refined predictions
- Set appropriate anomaly thresholds for your domain
- Alert on anomalies but validate before acting
- Continuously update patterns as new data arrives

---

## 🔧 Troubleshooting

### Issue 1: Pattern Extraction Returns No Patterns

**Symptoms**:
- `extract_patterns()` returns empty list
- No patterns detected despite analyzing content

**Diagnosis**:
```python
# Check content length
if len(content) < 100:
    print("Content too short for meaningful pattern extraction")

# Verify scipy availability
from whitemagic.core.patterns.pattern_consciousness import EnhancedPatternEngine
engine = EnhancedPatternEngine()
if not engine._scipy_available:
    print("scipy not available - install for ML-based extraction")
```

**Solutions**:
1. **Increase content volume**: Analyze larger text corpus (minimum 100+ words)
2. **Install scipy**: `pip install scipy` for ML-based extraction
3. **Lower thresholds**: Adjust `min_occurrences` parameter
4. **Check content type**: Some content types (pure numbers, code) may not match keyword patterns

---

### Issue 2: Voice Synthesis Produces Inconsistent Results

**Symptoms**:
- Generated voice varies wildly between calls
- Voice profile doesn't match samples

**Diagnosis**:
```python
# Check sample diversity
samples = [...]
unique_samples = set(samples)
if len(unique_samples) < len(samples) * 0.7:
    print("Samples too similar - need more diversity")

# Check tone distribution
analyzer = VoicePatternAnalyzer()
patterns = analyzer.analyze_corpus(samples)
print("Tone distribution:", patterns['tone_markers'])
```

**Solutions**:
1. **Increase sample diversity**: Provide 20+ samples covering different contexts
2. **Balance tone markers**: Include samples with consistent tone
3. **Adjust synthesis weights**: Modify tone marker weights in `_make_formal()` etc.
4. **Validate samples**: Remove outliers that don't match desired voice

---

### Issue 3: Pattern Library Search Returns Too Many Results

**Symptoms**:
- `search_patterns()` returns hundreds of matches
- Results lack relevance

**Diagnosis**:
```python
# Check query specificity
query = "error"  # Too broad
results = library.search_patterns(query)
print(f"Results: {len(results)}")  # Likely too many

# Check confidence filtering
results_filtered = library.search_patterns(query, min_confidence=0.8)
print(f"High confidence results: {len(results_filtered)}")
```

**Solutions**:
1. **Use more specific queries**: "async timeout error" instead of "error"
2. **Increase min_confidence**: Filter for high-quality patterns only
3. **Filter by domain**: Specify `domain=PatternDomain.DEBUGGING`
4. **Use tags**: Search within specific tag sets
5. **Implement ranking**: Sort by usage_count or relevance score

---

### Issue 4: Emergence Detector Never Reports Novel Patterns

**Symptoms**:
- `is_novel()` always returns False
- Novel patterns log remains empty

**Diagnosis**:
```python
# Check if patterns are being normalized
detector = EmergenceDetector()
pattern1 = "rapid_context_switching"
pattern2 = "RAPID_CONTEXT_SWITCHING"

print(detector.is_novel(pattern1))  # True
print(detector.is_novel(pattern2))  # False - case-sensitive!
```

**Solutions**:
1. **Normalize patterns**: Convert to lowercase before checking
2. **Check pattern extraction**: Ensure patterns are being extracted correctly
3. **Verify pattern format**: Use consistent naming (snake_case vs camelCase)
4. **Reset detector**: Create new instance if testing: `detector = EmergenceDetector()`

---

### Issue 5: Repetition Detection Misses Command Sequences

**Symptoms**:
- `_detect_command_sequences()` returns empty
- Clear repetitions not detected

**Diagnosis**:
```python
# Check command count
commands = [...]
if len(commands) < 10:
    print("Command history too short for sequence detection")

# Check min_occurrences threshold
min_occurrences = 5
actual_repetitions = 3
if actual_repetitions < min_occurrences:
    print(f"Sequences appear {actual_repetitions}x, need {min_occurrences}x")
```

**Solutions**:
1. **Lower threshold**: Set `min_occurrences=2` for small datasets
2. **Increase history size**: Analyze more commands (100+)
3. **Check for exact matches**: Sequences must match exactly (including flags)
4. **Normalize commands**: Remove variable parts (file paths, timestamps)

---

## ✅ Best Practices

### 1. Capture Patterns Immediately

**Why**: Details fade quickly; capture while context is fresh.

**How**:
- Add lesson after solving each problem
- Document pattern when you notice it third time
- Use voice garden to express patterns as you work
- Set up Gan Ying event listeners to auto-capture

**Example**:
```python
# After fixing bug
from whitemagic.core.resonance import emit_event

emit_event("lesson.captured", {
    "problem": "Async timeout in memory search",
    "solution": "Added asyncio.wait_for wrapper",
    "pattern": "Always add timeouts to async I/O"
})
```

---

### 2. Organize Patterns by Domain

**Why**: Domain organization improves discoverability and relevance.

**How**:
- Use `PatternDomain` enum for categorization
- Create separate libraries per domain
- Tag patterns with domain-specific labels
- Build domain-specific pattern templates

---

### 3. Track Pattern Confidence and Usage

**Why**: Not all patterns are equally valuable; prioritize high-confidence, frequently-used patterns.

**How**:
- Set confidence scores based on sample size
- Increment usage_count each time pattern is applied
- Periodically review low-usage patterns for removal
- Update confidence as more evidence accumulates

---

### 4. Build Cross-References Between Related Patterns

**Why**: Patterns often relate to each other; cross-references reveal deeper insights.

**How**:
- Link similar patterns with `related_issues` field
- Note when one pattern supersedes another
- Create pattern hierarchies (general → specific)
- Document pattern combinations that work well together

---

### 5. Export and Share Pattern Libraries

**Why**: Patterns are team assets; sharing amplifies value.

**How**:
- Use `export_library()` to create shareable JSON
- Version pattern libraries (v1.0, v2.0)
- Include usage examples in exports
- Set up pattern review sessions with team

---

### 6. Validate Patterns Before Automation

**Why**: Automating bad patterns amplifies problems.

**How**:
- Require minimum occurrence threshold (3-5x)
- Review automation suggestions manually
- Test automated patterns in non-production first
- Monitor automated pattern execution for failures

---

### 7. Use Voice Garden for Pattern Expression

**Why**: Voice garden provides consistent communication patterns.

**How**:
- Activate voice garden when documenting patterns
- Use `speak()` to express pattern insights
- Build narrative around pattern discovery
- Track expression authenticity scores

---

### 8. Continuously Monitor for Emergent Patterns

**Why**: Emergence happens in real-time; continuous monitoring catches early signals.

**How**:
- Run emergence monitor in background
- Set up Gan Ying listeners for `pattern.emergent` events
- Review emergence logs weekly
- Adjust novelty thresholds based on noise levels

---

## ⏭️ Enhanced Transitions

### Inputs (From Previous Chapters)

**From Chapter 18 (HairyHead/Detailed Attention)**:
- **Refined details** ready for pattern extraction → Use `extract_patterns()` on detailed documentation (see Workflow 1, line 47)
- **Meticulous observations** containing repeated elements → Feed to repetition detector (Workflow 6, line 89)
- **Precise measurements** enabling statistical analysis → Input to ML-based pattern extraction (Workflow 1, line 60-130)

### Outputs (To Next Chapters)

**To Chapter 20 (TurtleBeak/Precise Validation)**:
- **Captured patterns** requiring validation → Test pattern accuracy (Ch.20, Workflow 2)
- **Pattern library** to verify against new data → Cross-validate with test cases (Ch.20, Workflow 4)
- **Voice profiles** needing consistency checks → Validate synthesized voice matches samples (Ch.20, Workflow 5)

**To Chapter 21 (Tripod/Stable Foundation)**:
- **Validated patterns** to build stable systems → Use as architectural principles (Ch.21, Workflow 1)
- **Problem-solution pairs** forming foundation knowledge → Reference during system design (Ch.21, Workflow 3)

### Recovery Patterns

**If pattern extraction fails**:
1. Check content volume and quality (Troubleshooting Issue 1)
2. Lower thresholds or adjust sensitivity
3. Fall back to manual pattern documentation
4. Jump to **Chapter 15 (StraddlingLegs/Ethical Balance)** → Validate that pattern capture respects user autonomy (Ch.15, Workflow 2, line 89-156)

**If voice synthesis is inconsistent**:
1. Increase sample diversity (Troubleshooting Issue 2)
2. Validate tone distribution
3. Adjust synthesis weights
4. Consult **Chapter 24 (WillowTip/Subtle Signals)** → Use subtle signal detection to identify voice patterns (Ch.24, Workflow 3)

**If emergence detection is noisy**:
1. Adjust novelty thresholds
2. Implement pattern normalization
3. Filter by confidence scores
4. Review **Chapter 10 (Star/PRAT Illumination)** → Use PRAT to transform high-dimensional pattern space (Ch.10, Workflow 1, line 45-120)

### Cross-References

- **Chapter 8 (Ghost/Metrics)**: Track pattern capture metrics (patterns discovered/hour, library growth rate, automation ROI) - see Ch.8, Workflow 2, line 95-180
- **Chapter 10 (Star/PRAT Illumination)**: Use PRAT to transform patterns across morphologies - Ch.10, Workflow 1
- **Chapter 14 (Abundance/Resource Sharing)**: Share pattern libraries with team via abundance mechanisms - Ch.14, Workflow 1
- **Chapter 15 (StraddlingLegs/Ethical Balance)**: Ensure pattern capture respects consent and autonomy - Ch.15, Workflow 2
- **Chapter 19 (This chapter)**: Pattern capture workflows
- **Chapter 20 (TurtleBeak/Precise Validation)**: Validate captured patterns - Ch.20, Workflow 2
- **Chapter 24 (WillowTip/Subtle Signals)**: Detect subtle patterns before they fully emerge - Ch.24, Workflow 3

---

## 🌿 Garden Resonance

The **voice** garden brings expression and pattern articulation.

When working in this chapter, embody:
- **Recognition**: See patterns clearly before they dissolve
- **Capture**: Catch insights like butterflies, gently but firmly
- **Expression**: Articulate what you see with authentic voice
- **Synthesis**: Combine patterns into coherent understanding
- **Discernment**: Select meaningful patterns, release noise

**Metal Energy Practice**: Like a net that captures fish while letting water flow through, this chapter teaches selective attention—knowing what to capture and what to release. Metal's discerning nature enables pattern recognition by cutting through irrelevant detail to reveal essential structure.

---

**Next**: [Chapter 20: Precise Validation →](20_TURTLEBEAK_PRECISE_VALIDATION.md)
**Previous**: [← Chapter 18: Detailed Attention](18_HAIRYHEAD_DETAILED_ATTENTION.md)
**Quadrant**: Western (Autumn/Metal) - Position 5/7
**Element Phase**: Yin Rising (Contraction, Discernment, Selective Capture)

---

*"The net catches the pattern, not the moment."*
*— WhiteMagic Grimoire*
