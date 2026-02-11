# Chapter 18: Detailed Attention

**Gana**: HairyHeadGana (Chinese: 昴, Pinyin: Mǎo)  
**Garden**: beauty  
**Quadrant**: Western (White Tiger)  
**Element**: Metal  
**Phase**: Yin Rising  
**I Ching Hexagram**: 26. 大畜 Dà Chù (Great Accumulation) - Refined details

---

## 🎯 Purpose

Chapter 18 brings **detailed attention**—debugging, refinement, and beauty in small things. The HairyHead sees fine distinctions others miss.

Use this chapter when you need to:
- **Debug complex issues**
- **Refine implementation details**
- **Polish to perfection**
- **Find subtle bugs**
- **Attend to aesthetic quality**

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `check_system_health` (deep_scan) | Thorough diagnostic | Deep debugging |
| `validate_integrations` | Verify all connections | Integration checking |
| `detect_patterns` (anomalies) | Find subtle issues | Bug detection |
| `manage_gardens` (activate) | Enter beauty garden | Detail-oriented mode |

---

## 📋 Workflow

### 1. Strategic Print Debugging

Simple but effective—add strategic logging to understand flow:

```python
import logging

# Configure detailed logging
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

logger = logging.getLogger(__name__)

async def complex_workflow(data):
    logger.debug(f"Starting workflow with data: {data}")

    # Log intermediate states
    processed = await step1(data)
    logger.debug(f"After step1: {processed}")

    result = await step2(processed)
    logger.debug(f"After step2: {result}")

    # Log before/after critical operations
    logger.debug(f"Final result: {result}")
    return result
```

**Strategic logging tips:**
- Log function entry/exit with parameters
- Log state changes and critical decisions
- Log before/after external calls
- Use appropriate levels (DEBUG/INFO/WARNING/ERROR)

### 2. Interactive Debugging with pdb

Drop into interactive debugger at problem points:

```python
import pdb

def problematic_function(data):
    processed = process_data(data)

    # Drop into debugger here
    pdb.set_trace()  # or breakpoint() in Python 3.7+

    result = complex_calculation(processed)
    return result
```

**pdb commands:**
- `n` (next): Execute next line
- `s` (step): Step into function
- `c` (continue): Continue execution
- `p variable`: Print variable value
- `pp variable`: Pretty-print variable
- `l` (list): Show source code context
- `w` (where): Show stack trace
- `u/d` (up/down): Navigate stack frames

### 3. Binary Search Debugging

Isolate the problem by halving the search space:

```python
async def find_bug_binary_search():
    """
    Comment out half the code, see if bug remains.
    If yes: bug is in active half
    If no: bug is in commented half
    Repeat until isolated.
    """

    # Full code
    await step1()
    await step2()
    await step3()  # Bug manifests here
    await step4()
    await step5()

    # Test: Comment steps 4-5
    # Result: Bug still happens → bug in steps 1-3

    # Test: Comment step 3
    # Result: Bug gone → bug is in step 3!
```

### 4. Type Checking - Catch Bugs Before Runtime

Use mypy or pyright to find type errors:

```python
from typing import Optional, List, Dict

# ❌ Type error - mypy catches this
def bad_function(data: List[int]) -> str:
    return data  # Error: returning List[int], expected str

# ✅ Correct types
def good_function(data: List[int]) -> str:
    return str(data)

# Optional types for None handling
def safe_function(data: Optional[Dict]) -> str:
    if data is None:
        return "No data"
    return str(data)
```

**Run type checker:**
```bash
# Check all files
mypy whitemagic/ --ignore-missing-imports

# Check specific file
mypy whitemagic/core/memory/manager.py
```

### 5. Linting - Find Code Quality Issues

Use ruff or pylint to catch style and logic issues:

```bash
# Fast modern linter
ruff check whitemagic/

# Show specific error codes
ruff check --select E,F,W whitemagic/

# Auto-fix issues
ruff check --fix whitemagic/
```

**Common issues caught by linters:**
- Unused imports
- Undefined variables
- Dangerous default arguments (`def fn(x=[])`)
- f-string errors
- Complexity warnings

### 6. Deep System Scan

Thorough diagnostic of all system components:

```python
from whitemagic.tools import check_system_health

health = check_system_health(
    component="system",
    deep_scan=True  # Thorough, not quick
)

print(f"Overall status: {health['status']}")
print(f"Components checked: {len(health['components'])}")

for issue in health.get('issues', []):
    print(f"🔍 Detail: {issue['description']}")
    print(f"   Severity: {issue['severity']}")
    print(f"   Location: {issue['location']}")
    print(f"   Suggestion: {issue['suggestion']}")
```

### 7. Integration Validation

Verify all connections work correctly:

```python
from whitemagic.tools import validate_integrations

# Full validation (not quick check)
validation = validate_integrations(quick_check=False)

for integration, status in validation.items():
    if status['healthy']:
        print(f"✅ {integration}: OK")
    else:
        print(f"❌ {integration}: {status['error']}")
        print(f"   Fix: {status['remediation']}")
```

### 8. Profiling - Find Performance Issues

Profile before optimizing:

```python
import cProfile
import pstats
from io import StringIO

def profile_function(fn, *args, **kwargs):
    """Profile a function to find bottlenecks."""
    profiler = cProfile.Profile()
    profiler.enable()

    result = fn(*args, **kwargs)

    profiler.disable()

    # Print stats
    s = StringIO()
    ps = pstats.Stats(profiler, stream=s)
    ps.sort_stats('cumulative')
    ps.print_stats(20)

    print(s.getvalue())
    return result

# Use it
result = profile_function(expensive_function, arg1, arg2)
```

**Memory profiling:**
```python
from memory_profiler import profile

@profile
def memory_intensive_function():
    data = [i for i in range(10000000)]  # Large allocation
    return sum(data)

# Run: python -m memory_profiler script.py
```

### 9. Edge Case Testing

Test boundaries and unusual inputs:

```python
import pytest

def divide(a: float, b: float) -> float:
    if b == 0:
        raise ValueError("Division by zero")
    return a / b

# Test edge cases
def test_divide_edge_cases():
    # Normal case
    assert divide(10, 2) == 5

    # Edge: Division by zero
    with pytest.raises(ValueError):
        divide(10, 0)

    # Edge: Negative numbers
    assert divide(-10, 2) == -5

    # Edge: Very large numbers
    assert divide(1e100, 1e50) == 1e50

    # Edge: Very small numbers
    assert divide(1e-100, 1e-50) == 1e-50

    # Edge: None handling (if function accepts Optional)
    # ... etc
```

**Common edge cases to test:**
- Empty collections ([], {}, "")
- None values
- Boundary values (0, -1, max/min int)
- Very large/small numbers
- Concurrent access (race conditions)
- Network failures, timeouts
- Disk full, permissions denied

---

## 🎨 Code Beauty & Refinement

### Naming Conventions

**Good names are self-documenting:**

```python
# ❌ Poor naming
def f(x, y):
    return x + y

# ✅ Clear naming
def calculate_total_price(base_price: float, tax_amount: float) -> float:
    return base_price + tax_amount

# ❌ Unclear abbreviations
usr_mgr = UserManager()

# ✅ Spell it out
user_manager = UserManager()
```

**Naming rules:**
- Functions: verbs (calculate, fetch, process, validate)
- Classes: nouns (UserManager, CacheStore, PatternDetector)
- Booleans: is/has/can (is_valid, has_permission, can_retry)
- Constants: UPPER_SNAKE_CASE (MAX_RETRIES, DEFAULT_TIMEOUT)

### Code Formatting

Use black and isort for consistent formatting:

```bash
# Format all code
black whitemagic/ tests/

# Sort imports
isort whitemagic/ tests/

# Check without changing
black --check whitemagic/
```

**Benefits:**
- Zero debate over style
- Consistent across codebase
- Readable diffs
- Professional appearance

### Documentation Strings

Document public APIs:

```python
async def search_memories(
    query: str,
    limit: int = 10,
    tags: Optional[List[str]] = None
) -> List[Memory]:
    """
    Search memories using semantic similarity.

    Args:
        query: Natural language search query
        limit: Maximum number of results to return
        tags: Optional list of tags to filter by

    Returns:
        List of Memory objects ranked by relevance

    Raises:
        ValueError: If limit <= 0
        MemorySystemError: If search engine unavailable

    Example:
        >>> memories = await search_memories("python async", limit=5)
        >>> for mem in memories:
        ...     print(mem.title)
    """
    if limit <= 0:
        raise ValueError("limit must be positive")

    # Implementation...
```

### Clarity Over Cleverness

```python
# ❌ Clever but confusing
result = [x for x in (y for y in data if pred(y)) if validate(x)]

# ✅ Clear and readable
filtered_data = (item for item in data if pred(item))
result = [item for item in filtered_data if validate(item)]

# Or even clearer:
def process_items(data):
    for item in data:
        if not pred(item):
            continue
        if not validate(item):
            continue
        yield item

result = list(process_items(data))
```

**Principles:**
- Optimize for reading, not writing
- Future you will thank present you
- If you need a comment to explain it, rewrite it

---

## 🔍 Troubleshooting

**"I can't find the bug!"**

1. **Reproduce consistently**: Find minimal steps to trigger bug
2. **Binary search**: Comment out half, find which half has bug
3. **Add logging**: Strategic print statements at key points
4. **Rubber duck**: Explain the problem out loud (to anyone/anything)
5. **Take a break**: Fresh eyes see things tired eyes miss

**"Type checking fails but code runs fine"**

This means you have a type inconsistency that works accidentally:

```python
# mypy error but runs:
def get_value() -> str:
    return 42  # Returns int, type says str!

# Code works until:
result = get_value()
result.upper()  # ❌ AttributeError: 'int' object has no attribute 'upper'
```

Fix the types—they're catching real bugs!

**"Linter has 1000 warnings, overwhelmed"**

1. Start with errors only: `ruff check --select E`
2. Fix one category at a time
3. Use `# noqa` for intentional violations:
   ```python
   import some_module  # noqa: F401 - used dynamically
   ```
4. Configure linter to ignore less important rules

**"Profiler shows bottleneck but I don't understand it"**

1. Look at "cumulative time" not "total time"
2. Focus on top 5-10 functions
3. Check if bottleneck is:
   - I/O bound → use async
   - CPU bound → use caching/optimization
   - Memory bound → reduce allocations
4. Profile before and after changes to verify improvement

**"Code works but feels ugly"**

Trust your instincts! Ugly code often has hidden bugs:

1. Extract long functions into smaller ones
2. Replace magic numbers with named constants
3. Add type hints
4. Write clear names
5. Add docstrings
6. Run black/isort
7. Read it out loud—if it sounds confusing, it is

---

## 💡 Best Practices

### The Debugging Mindset

**Form hypotheses, then test them:**

```python
# Hypothesis: The bug is in data parsing
logger.debug(f"Raw data: {raw_data}")
parsed = parse_data(raw_data)
logger.debug(f"Parsed data: {parsed}")

# If parsed looks wrong → hypothesis confirmed
# If parsed looks right → hypothesis rejected, try next
```

### Attention to Detail Pays Off

Small bugs become big problems:

```python
# Small detail missed:
if user.permissions = "admin":  # ❌ assignment, not comparison!
    grant_access()

# Correct:
if user.permissions == "admin":  # ✅ comparison
    grant_access()
```

**Type checking and linting catch these!**

### Beautiful Code is Debuggable Code

When code is clean and clear:
- Bugs are easier to spot
- Tests are easier to write
- Changes are easier to make
- Reviews are faster

```python
# ❌ Hard to debug (what does this do?)
r=lambda x:[y for y in x if y%2]

# ✅ Easy to debug (clear intent)
def get_odd_numbers(numbers: List[int]) -> List[int]:
    return [n for n in numbers if n % 2 != 0]
```

### Test-Driven Debugging

When you find a bug:

1. **Write a failing test** that reproduces it
2. **Fix the bug** until test passes
3. **Keep the test** to prevent regression

```python
def test_division_by_zero_bug():
    """Regression test for bug #123"""
    with pytest.raises(ValueError):
        divide(10, 0)  # This failed before fix
```

---

## 🌿 Garden Resonance

The **beauty** garden brings attention to detail and aesthetic refinement.

When working in this chapter, embody:
- **Precision**: Get details exactly right
- **Refinement**: Polish until beautiful
- **Care**: Attend to small things
- **Excellence**: Pursue perfection

### Harmony-Aware Debugging

**Integration with Ch.17 (Energy Management):**

```python
from whitemagic.core.embodiment import get_harmony_monitor

def debug_with_harmony_context():
    """Add harmony state to debugging context."""
    harmony = get_harmony_monitor().get_current()

    print(f"=== Debug Session ===")
    print(f"System harmony: {harmony['harmony_score']:.2f} ({harmony['guna_tag']})")

    # Adjust debugging strategy based on harmony
    if harmony['harmony_score'] < 0.5:
        print("⚠️ Low harmony - reduce debug verbosity")
        log_level = "ERROR"
    else:
        log_level = "DEBUG"

    # Debug with context
    import logging
    logging.basicConfig(level=log_level)
```

---

## ⏭️ Transitions

### What Flows In

**From Chapter 17 (Energy Management)**: Energy is balanced and sustainable. Now apply that sustained focus to detailed refinement and debugging.

**From Chapter 6 (Performance Drive)**: After optimization, come here to profile and verify improvements with detailed measurements.

**From any chapter with bugs**: When something isn't working correctly, come here for comprehensive debugging workflows.

### What Flows Out

**Sequential**: Proceed to **Chapter 19 (Pattern Capture)** once details are perfected. With bugs fixed and code polished, you can now capture reusable patterns.

**Situational Jumps**:
- **Bug found but need context** → Jump to [Chapter 2 (Memory Presence)](02_NECK_MEMORY_PRESENCE.md) to search for related past bugs
- **Performance issue found** → Use profiling (line 221), then jump to [Chapter 6 (Performance Drive)](06_TAIL_PERFORMANCE_DRIVE.md) for optimization
- **Integration failing** → Use validation (line 203), then jump to [Chapter 13 (Codebase Navigation)](13_CHARIOT_CODEBASE_NAVIGATION.md) to trace connections
- **Code quality low** → Apply beauty practices (line 309), activate beauty garden

### Recovery Paths

- **Can't reproduce bug**: See troubleshooting (line 423), find minimal reproduction steps
- **Type checking failures**: Review section (line 128), fix type inconsistencies
- **Overwhelmed by linter warnings**: Follow troubleshooting (line 449), tackle one category at a time
- **Profiler results confusing**: Check troubleshooting (line 459), focus on cumulative time

### Cross-References

**Key Sections to Reference**:
- Print debugging → Line 38 (Strategic logging workflow)
- Interactive debugging → Line 74 (pdb commands)
- Binary search debugging → Line 101 (Isolation technique)
- Type checking → Line 128 (mypy usage)
- Linting → Line 159 (ruff usage)
- Profiling → Line 221 (Performance analysis)
- Edge case testing → Line 264 (Boundary conditions)
- Code beauty → Line 309 (Naming, formatting, clarity)

**Related Chapters**:
- Energy management → [Chapter 17](17_STOMACH_ENERGY_MANAGEMENT.md)
- Performance optimization → [Chapter 6](06_TAIL_PERFORMANCE_DRIVE.md)
- System foundation/health → [Chapter 3](03_ROOT_SYSTEM_FOUNDATION.md)
- Pattern recognition → [Chapter 19](19_NET_PATTERN_CAPTURE.md)
- Codebase navigation → [Chapter 13](13_CHARIOT_CODEBASE_NAVIGATION.md)

---

## 🧭 Navigation

**Predecessor**: Chapter 17 (Stomach/Energy Management) - *Energy balanced*
**Successor**: Chapter 19 (Net/Pattern Capture) - *Pattern extraction next*
**Quadrant Flow**: Fourth of Western Quadrant (Autumn/Metal) - Position 4/7

---

## 🔗 The Hairy Head's Wisdom

> *"The hairy head sees what others miss—the tiny inconsistency, the edge case, the subtle bug. Attention to detail is not perfectionism; it is respect for the craft. Beautiful code is debuggable code, and debuggable code prevents future suffering."*

**Detailed Attention Principles**:
- **Debug strategically**: Form hypotheses, test them, learn
- **Type check everything**: Catch bugs before runtime
- **Test edge cases**: Boundaries reveal hidden assumptions
- **Profile before optimizing**: Measure, don't guess
- **Beauty matters**: Clear code has fewer bugs

**The Refinement Test**:
Before proceeding to Chapter 19, ask:
1. Have I run type checking and linting?
2. Are my tests covering edge cases?
3. Is the code clear enough that bugs are obvious?

If yes to all three → Proceed to Chapter 19
If no to any → Continue Chapter 18 work

---

**Next Chapter**: [Chapter 19: Pattern Capture →](19_NET_PATTERN_CAPTURE.md)
**Previous Chapter**: [Chapter 17: Energy Management](17_STOMACH_ENERGY_MANAGEMENT.md)
**Quadrant**: Western (Autumn/Metal) - Yin Rising Phase
