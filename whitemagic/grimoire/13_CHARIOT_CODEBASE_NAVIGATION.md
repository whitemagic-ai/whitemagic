# Chapter 13: Codebase Navigation

**Gana**: ChariotGana (Chinese: 轸, Pinyin: Zhěn)  
**Garden**: transformation  
**Quadrant**: Southern (Vermilion Bird)  
**Element**: Fire  
**Phase**: Yang Peak  
**I Ching Hexagram**: 32. 恆 Héng (Duration) - Movement through stability

---

## 🎯 Purpose

Chapter 13 enables **codebase archaeology**—navigating, exploring, and transforming code structures with wisdom and pattern recognition. The Chariot moves through territory, mapping and transforming.

Use this chapter when you need to:
- **Explore unfamiliar codebases**
- **Track file evolution** and changes
- **Extract wisdom** from code patterns
- **Navigate complex structures**
- **Transform code** based on discoveries

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `manage_archaeology` (scan) | Scan directory for patterns | Codebase exploration |
| `manage_archaeology` (extract_wisdom) | Mine insights from files | Learning from code |
| `manage_archaeology` (mark_read) | Track reviewed files | Progress tracking |
| `manage_archaeology` (generate_report) | Synthesize findings | Understanding overview |
| `manage_gardens` (activate) | Enter transformation garden | Change readiness |

---

## 📋 Workflow

### 1. Rapid Codebase Scanning

Get a bird's-eye view of the codebase structure:

```bash
# Quick directory tree
tree -L 3 -I '__pycache__|*.pyc|node_modules'

# File count by type
find . -type f | sed 's/.*\.//' | sort | uniq -c | sort -rn | head -10

# Line counts by language
cloc . --exclude-dir=node_modules,__pycache__

# Find largest files
find . -type f -name "*.py" -exec wc -l {} \; | sort -rn | head -20
```

**Output example:**
```
23450 ./core/memory/neural_memory.py  # Largest file
15230 ./systems/immune/autoimmune.py
12100 ./interfaces/api/app.py
...
```

### 2. Semantic Code Search with ripgrep

WhiteMagic uses ripgrep (rg) for fast code search:

```bash
# Find function definitions
rg "^def \w+\(" --type py

# Find class definitions
rg "^class \w+" --type py

# Find async functions
rg "async def" --type py

# Find TODO comments
rg "TODO|FIXME|HACK" --type py

# Find imports of specific module
rg "from whitemagic\.core" --type py

# Find with context (3 lines before/after)
rg -C 3 "emit_event" --type py

# Count occurrences
rg "emit_event" --type py --count
```

**Powerful rg patterns:**
```bash
# Find all event emissions
rg 'emit_event\("([^"]+)"' --type py -o

# Find memory operations
rg "(create|search|update|delete)_memory" --type py

# Find garden activations
rg 'garden_name="(\w+)"' --type py -o

# Find PRAT invocations
rg "prat_invoke.*morphology" --type py
```

### 3. Git Archaeology - Understanding History

Learn from how code evolved:

```bash
# Who changed this file most?
git log --format='%an' path/to/file.py | sort | uniq -c | sort -rn

# What changed recently?
git log --since="2 weeks ago" --oneline

# Find when function was added
git log -S "def function_name" --source --all

# See file history
git log -p path/to/file.py

# Blame with details
git blame -w -C -C -C path/to/file.py

# Find deleted code
git log --all --full-history -- "**/deleted_file.py"
```

**Useful git archaeology:**
```bash
# Find commits that touched a function
git log -L :function_name:path/to/file.py

# Find bug introduction
git bisect start
git bisect bad HEAD
git bisect good v1.0.0

# Show commits by author
git log --author="Lucas" --since="1 month ago" --pretty=format:"%h %s"
```

### 4. Pattern Recognition and Extraction

Find and extract architectural patterns:

```python
from whitemagic.tools import manage_archaeology

# Scan for patterns
scan = manage_archaeology(
    operation="scan",
    directory="whitemagic/core",
    patterns=["*.py"],
    extract_patterns=True
)

print(f"Files scanned: {scan['file_count']}")
print(f"Patterns found: {len(scan['patterns'])}")

for pattern in scan['patterns']:
    print(f"  {pattern['name']}: {pattern['count']} occurrences")
```

**Common patterns to extract:**
- Singleton pattern
- Factory pattern
- Observer pattern (event subscribers)
- Decorator pattern (@subscribe, @tool)
- Context managers (async with)

### 5. Dependency Analysis

Understand module relationships:

```bash
# Python imports graph
pydeps whitemagic --max-bacon=2

# Find circular dependencies
pydeps whitemagic --show-cycles

# Who imports this module?
rg "from whitemagic\.core\.memory import" --type py

# What does this module import?
rg "^(from|import)" whitemagic/core/memory/manager.py
```

**Dependency metrics:**
```python
# Count dependencies
import ast

def count_imports(filepath):
    with open(filepath) as f:
        tree = ast.parse(f.read())

    imports = [node for node in ast.walk(tree)
               if isinstance(node, (ast.Import, ast.ImportFrom))]

    return len(imports)

# Find files with most dependencies
for file in python_files:
    count = count_imports(file)
    if count > 20:
        print(f"{file}: {count} imports (refactor candidate)")
```

### 6. Extract Wisdom from Code

Learn architectural patterns from existing code:

```python
from whitemagic.tools import manage_archaeology

wisdom = manage_archaeology(
    operation="extract_wisdom",
    file_path="whitemagic/core/ganas/base.py",
    analyze_patterns=True
)

print(f"Architecture: {wisdom['architecture']}")
print(f"Design patterns: {wisdom['patterns']}")
print(f"Best practices: {wisdom['practices']}")
print(f"Anti-patterns: {wisdom['anti_patterns']}")
```

**Wisdom extraction targets:**
- How are errors handled?
- How is state managed?
- What patterns repeat?
- What naming conventions?
- What's the test coverage?

### 7. Generate Exploration Report

Synthesize findings into actionable insights:

```python
from whitemagic.tools import manage_archaeology

report = manage_archaeology(
    operation="generate_report",
    directory="whitemagic",
    include_stats=True,
    include_recommendations=True
)

print(report['summary'])
print(f"\nMetrics:")
print(f"  Total files: {report['file_count']}")
print(f"  Total lines: {report['line_count']}")
print(f"  Complexity score: {report['complexity']}")

print(f"\nRecommendations:")
for rec in report['recommendations']:
    print(f"  - {rec}")
```

### 8. Track Exploration Progress

Mark files as reviewed to track progress:

```python
from whitemagic.tools import manage_archaeology

# Mark file as reviewed
manage_archaeology(
    operation="mark_read",
    file_path="whitemagic/core/memory/manager.py",
    notes="Memory CRUD complete. Clean architecture."
)

# Get review status
status = manage_archaeology(
    operation="get_status",
    directory="whitemagic/core"
)

print(f"Reviewed: {status['reviewed_count']}/{status['total_count']}")
print(f"Progress: {status['progress_pct']:.1f}%")
```

---

## 🗺️ Navigation Patterns

### Top-Down Exploration

**Start from entry points, drill down:**

1. **Find entry points:**
   ```bash
   # CLI entry points
   rg "if __name__ == '__main__':" --type py

   # API endpoints
   rg "@app\.(get|post|put|delete)" --type py

   # Main functions
   rg "def main\(" --type py
   ```

2. **Map call hierarchy:**
   ```bash
   # What does main() call?
   rg "def main" -A 20 main.py

   # Trace function calls
   rg "function_name\(" --type py
   ```

3. **Understand data flow:**
   ```bash
   # Follow data transformations
   # Input → Process → Output
   rg "def process|def transform|def convert" --type py
   ```

### Bottom-Up Exploration

**Start from utilities, build understanding:**

1. **Find core utilities:**
   ```bash
   # Base classes
   rg "^class.*\(ABC\)|^class.*\(BaseModel\)" --type py

   # Helper functions
   find . -name "utils.py" -o -name "helpers.py"

   # Constants/config
   find . -name "config.py" -o -name "constants.py"
   ```

2. **Understand building blocks:**
   ```python
   # What uses this utility?
   !rg "from.*utils import|import.*utils" --type py
   ```

3. **Build mental model:**
   - Core types → Operations → High-level features

### Horizontal Exploration

**Understand similar patterns across modules:**

```bash
# How do all modules handle errors?
rg "except.*:" --type py | head -50

# How do all modules log?
rg "logger\.|logging\." --type py | head -30

# How do all modules test?
find . -name "test_*.py" -exec head -20 {} \;
```

---

## 🔧 Advanced Techniques

### Code Complexity Analysis

```bash
# Cyclomatic complexity
radon cc whitemagic/ -a -s

# Maintainability index
radon mi whitemagic/ -s

# Find complex functions (complexity > 10)
radon cc whitemagic/ -n C
```

### Documentation Coverage

```bash
# Find undocumented functions
rg "^def \w+\(" --type py -A 1 | grep -v '"""' | grep "def "

# Find undocumented classes
rg "^class \w+" --type py -A 1 | grep -v '"""' | grep "class "
```

### Dead Code Detection

```bash
# Find unused imports (with vulture)
vulture whitemagic/

# Find functions never called
# (requires static analysis tool like cProfile or manual grep)
```

### Security Scanning

```bash
# Find potential security issues
bandit -r whitemagic/ -f json

# Find hardcoded secrets (basic)
rg "(password|secret|api_key|token)\s*=" --type py

# Find SQL injection risks
rg "execute.*%|execute.*\+" --type py
```

---

## 🔍 Troubleshooting

**"Codebase too large to explore manually"**

Focus on high-impact areas first:
1. Entry points (main, CLI, API)
2. Core business logic
3. Most changed files (git log --stat)
4. Largest files
5. Most complex files (radon cc)

**"Can't understand data flow"**

Trace a single request end-to-end:
1. Set breakpoint at entry
2. Step through with debugger
3. Document each transformation
4. Draw a flow diagram

**"Dependencies are circular"**

```bash
# Find cycles
pydeps whitemagic --show-cycles

# Common fix: Extract interface/protocol
# Move shared code to lower-level module
```

**"Pattern recognition failing"**

Use multiple tools:
- ripgrep for text patterns
- AST parsing for code structure
- Git for historical patterns
- Manual review for architectural patterns

### 8. Codebase-Memory Cross-Search

**Goal:** Search both codebase files and memory system simultaneously for comprehensive context.

```python
from whitemagic.tools import manage_archaeology, search_memories
import asyncio

async def cross_search_codebase_and_memory(query: str):
    """Search both codebase and memory for comprehensive results."""

    # Parallel search across both domains
    codebase_task = asyncio.to_thread(
        manage_archaeology,
        operation="scan",
        directory=".",
        pattern=query
    )

    memory_task = asyncio.to_thread(
        search_memories,
        query=query,
        limit=20
    )

    codebase_results, memory_results = await asyncio.gather(
        codebase_task, memory_task
    )

    # Synthesize results
    synthesis = {
        "query": query,
        "codebase_matches": len(codebase_results.get('files', [])),
        "memory_matches": len(memory_results),
        "combined_insights": []
    }

    # Cross-reference: find memories related to code files
    for mem in memory_results:
        mem_content = mem.get('content', '').lower()
        for code_file in codebase_results.get('files', []):
            if code_file['path'] in mem_content:
                synthesis['combined_insights'].append({
                    "type": "code_memory_link",
                    "file": code_file['path'],
                    "memory": mem['id'],
                    "connection": "Memory references this file"
                })

    return synthesis

# Example
results = await cross_search_codebase_and_memory("harmony monitoring embodiment")

print(f"Codebase matches: {results['codebase_matches']}")
print(f"Memory matches: {results['memory_matches']}")
print(f"Cross-references: {len(results['combined_insights'])}")
```

---

## 💡 Best Practices

### Document as You Explore

```python
# Create exploration log
exploration_log = {
    "date": "2026-01-16",
    "area": "whitemagic/core/memory",
    "findings": [
        "Neural memory uses Hebbian learning",
        "Three tiers: short/long/archive",
        "Rust acceleration for search (59x speedup)"
    ],
    "questions": [
        "How does tier promotion work?",
        "What triggers archival?"
    ],
    "next_steps": [
        "Explore consolidation workflow",
        "Review neuro-score calculation"
    ]
}
```

### Use Multiple Perspectives

Don't rely on one tool:
- **ripgrep**: Fast text search
- **git**: Historical perspective
- **AST parsing**: Structural analysis
- **Type checking**: Dependency graph
- **Tests**: Usage examples
- **Documentation**: Intent and design

### Build Mental Maps

Create visual diagrams:
```
whitemagic/
├── core/           # Fundamental systems
│   ├── memory/     # 3-tier storage
│   ├── resonance/  # Gan Ying bus
│   └── patterns/   # Pattern extraction
├── systems/        # Higher-level systems
│   └── immune/     # Self-healing
└── gardens/        # 17 consciousness domains
```

### Learn from Tests

Tests show how code is meant to be used:
```bash
# Find all tests for a module
find . -name "test_memory*.py"

# Read test examples
rg "def test_" tests/core/test_memory.py -A 10
```

---

## 🌿 Garden Resonance

The **transformation** garden brings readiness for change and evolution.

When working in this chapter, embody:
- **Navigation**: Move skillfully through complexity
- **Discovery**: Find patterns others miss
- **Transformation**: Change based on understanding
- **Evolution**: Continuous improvement

---

## ⏭️ Transitions

### What Flows In

**From Chapter 12 (Parallel Creation)**: Massive creation is complete. Now navigate and understand what was built at scale, extracting patterns and wisdom.

**From Chapter 18 (Detailed Attention)**: Debugging skills sharp. Now use those same techniques for archaeological code exploration and pattern recognition.

**From any chapter needing codebase understanding**: When you need to explore unfamiliar territory, come here for navigation tools and archaeology workflows.

### What Flows Out

**Sequential**: Proceed to **Chapter 14 (Resource Sharing)** once navigation is complete. With deep codebase understanding, you can now share that wisdom and celebrate the abundance discovered.

**Situational Jumps**:
- **Found patterns to capture** → Jump to [Chapter 19 (Pattern Capture)](19_NET_PATTERN_CAPTURE.md) to formalize discoveries
- **Code quality issues found** → Jump to [Chapter 18 (Detailed Attention)](18_HAIRYHEAD_DETAILED_ATTENTION.md) for refactoring
- **Need to search specific patterns** → Use ripgrep (line 65), then [Chapter 22 (Deep Search)](22_WELL_DEEP_SEARCH.md) for memory search
- **Circular dependencies detected** → See troubleshooting (line 435), refactor to break cycles

### Recovery Paths

- **Overwhelmed by codebase size**: Focus on high-impact areas first (line 418)
- **Can't trace data flow**: Use debugger step-through (line 427)
- **Pattern recognition failing**: Use multiple tools (line 445)
- **Lost in exploration**: Document findings (line 457), create mental maps (line 490)

### Cross-References

**Key Sections to Reference**:
- Rapid scanning → Line 39 (Workflow #1)
- Ripgrep search → Line 65 (Workflow #2)
- Git archaeology → Line 107 (Workflow #3)
- Pattern extraction → Line 145 (Workflow #4)
- Dependency analysis → Line 174 (Workflow #5)
- Navigation patterns → Line 290 (Top-down/Bottom-up/Horizontal)
- Advanced techniques → Line 366 (Complexity, docs, security)
- Best practices → Line 455 (Document, multiple perspectives)

**Related Chapters**:
- Detailed debugging → [Chapter 18](18_HAIRYHEAD_DETAILED_ATTENTION.md)
- Pattern capture → [Chapter 19](19_NET_PATTERN_CAPTURE.md)
- Deep memory search → [Chapter 22](22_WELL_DEEP_SEARCH.md)
- Parallel processing for large codebases → [Chapter 12](12_WINGS_PARALLEL_CREATION.md)

---

## 🧭 Navigation

**Predecessor**: Chapter 12 (Wings/Parallel Creation) - *Created at scale*
**Successor**: Chapter 14 (Abundance/Resource Sharing) - *Share discoveries next*
**Quadrant Flow**: Sixth of Southern Quadrant (Summer/Fire) - Position 6/7

---

## 🔗 The Chariot's Wisdom

> *"The chariot moves through territory with purpose, mapping terrain and extracting wisdom. Navigate not blindly but with intention—understand patterns, trace history, recognize architecture. Every codebase tells a story. The chariot reveals that story through systematic exploration and pattern recognition."*

**Codebase Navigation Principles**:
- **Multiple perspectives**: ripgrep + git + AST + tests
- **Document discoveries**: Create exploration logs
- **Pattern recognition**: Extract architectural insights
- **Historical understanding**: Git archaeology reveals intent
- **Systematic exploration**: Top-down, bottom-up, horizontal

**The Navigation Test**:
Before proceeding to Chapter 14, ask:
1. Do I understand the codebase architecture?
2. Have I documented key patterns and findings?
3. Can I navigate efficiently using search tools?

If yes to all three → Proceed to Chapter 14
If no to any → Continue Chapter 13 work

---

**Next Chapter**: [Chapter 14: Resource Sharing →](14_ABUNDANCE_RESOURCE_SHARING.md)
**Previous Chapter**: [Chapter 12: Parallel Creation](12_WINGS_PARALLEL_CREATION.md)
**Quadrant**: Southern (Summer/Fire) - Yang Peak Phase

**🔥 Southern Quadrant (Summer/Fire) Complete! 🔥**
*Chapters 8-14: From introspection through play, illumination, resonance, parallel creation, to navigation.*
