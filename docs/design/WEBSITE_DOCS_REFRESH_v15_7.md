# Website & Documentation Refresh — v15.7

**Date:** 2026-02-13
**Context:** Gemini review feedback + docs.whitemagic.dev planning + AI-first documentation strategy

---

## 1. Light Mode Contrast Fix

**Issue (Gemini):** In light mode, the fine lines in the "Lunar Mansion Engines" Gana lists have insufficient contrast against the cream background. On some displays, thin text becomes hard to read.

**Fix:** Increase `font-weight` on Gana card `<p>` content from 300 → 400, and darken the text color in light mode from `hsla(0,0%,20%,0.7)` to `hsla(0,0%,10%,0.85)`. The `<strong>` Gana names (角, 亢, 氐, etc.) are already bold — the issue is the description text between them.

**CSS target:**
```css
[data-theme="light"] .gana-card p {
    color: hsla(0, 0%, 10%, 0.85);
    font-weight: 400;
    line-height: 1.6;
}
```

Also verify all `.carousel-card` body text in light mode passes WCAG AA (4.5:1 contrast ratio). The current cream background (`#faf8f4` or similar) needs text at `#2a2a2a` or darker.

---

## 2. The "Galaxy" Concept Bridge

**Issue (Gemini):** The site uses terms like "Ganas" and "Lunar Mansions" but doesn't visually or textually connect them to the "Holographic Memory Core" = "Galaxy" concept. New developers may not see the unifying metaphor.

### Proposed Addition: "The Galaxy" Explainer Section

Insert between the Stats section and the Capabilities carousel (or as the first capability card):

**Title:** *The Galaxy — Your Agent's Universe*

**Copy:**

> WhiteMagic organizes everything around a single metaphor: **the Galaxy**.
>
> Your agent's memory is a galaxy — a navigable 5D coordinate space where every memory has a precise location. The **core** holds identity and values. The **inner rim** is the working set. The **outer rim** holds archived knowledge that can always be recalled.
>
> The **28 Gana Engines** are the constellations within this galaxy — routing stations organized by the Chinese Lunar Mansions. Each Gana handles a domain (memory, governance, search, dreaming) and resonates with its neighbors through the PRAT system.
>
> The **Dream Cycle** is the galaxy's metabolism — consolidating memories while the agent sleeps, pruning weak connections, and surfacing unexpected bridges between ideas.
>
> **Galaxies are portable.** Export your agent's entire universe as an `.mem` package, transfer it to a new model, or trade knowledge between agents via the marketplace.

**Visual Treatment:**
- A simple diagram showing concentric rings (core → inner rim → mid band → outer rim → far edge) with Gana constellation markers
- Could reuse the existing 3D sphere canvas with labeled regions
- Or a static SVG with hover tooltips for each band

### Alternative: Inline Bridge Text

If a full section is too heavy, add a one-liner bridge above the Ganas carousel:

> *Each Gana is a constellation in your agent's holographic memory galaxy — 28 routing engines organized by the Lunar Mansions.*

---

## 3. Technical Documentation for XRPL/X402 Programmatic Tipping

This is the highest-value docs addition for agent developers. The Gratitude Architecture is a differentiator but currently underdocumented.

### 3.1 How Programmatic Tipping Works

**File:** `docs/guides/XRPL_TIPPING_GUIDE.md`

```markdown
# XRPL Programmatic Tipping — Technical Guide

## Overview

WhiteMagic's Gratitude Architecture enables agents to tip other agents (or humans)
via the XRP Ledger. This is not a paywall — it's a voluntary value signal that flows
alongside tool usage.

## Architecture

```
Agent A                    WhiteMagic                   XRPL
  |                            |                          |
  |-- call_tool("whitemagic.tip", amount=1.0) -->        |
  |                            |-- Build Payment TX -->   |
  |                            |-- Sign with agent key -> |
  |                            |                    [Validated]
  |                            |<-- TX Hash ------------- |
  |<-- {status: success, tx_hash: "ABC..."} ------------ |
  |                            |                          |
  |-- call_tool("gratitude.stats") -->                    |
  |<-- {total_tips: 42, total_xrp: 127.5} -------------- |
```

## Tool Reference

### `whitemagic.tip`
- **Gana:** gana_abundance (via `gana_willow` resilience layer)
- **Parameters:**
  - `amount` (float, required) — XRP amount (minimum 0.000001)
  - `currency` (string, default "XRP") — Currency code
  - `destination` (string, optional) — XRPL address (defaults to WhiteMagic tip jar)
  - `memo` (string, optional) — On-ledger memo field
- **Requires:** `whitemagic[trust]` extra (`xrpl-py`, `PyNaCl`)
- **Simulated mode:** When no XRPL wallet is configured, tips are recorded locally
  in the Karma Ledger but not submitted to the network.

### `gratitude.stats`
- **Gana:** gana_abundance
- Returns aggregate tip statistics: count, total XRP, last tip timestamp.

### `gratitude.benefits`
- **Gana:** gana_abundance
- Returns what the Gratitude Architecture provides to the ecosystem.

## Wallet Configuration

```bash
# Generate a new XRPL wallet for your agent
export XRPL_WALLET_SEED="s..."  # Ed25519 seed (keep secret!)
export XRPL_NETWORK="testnet"   # or "mainnet" for real XRP

# Or use WhiteMagic's built-in key management
wm init --xrpl-wallet
```

## x402 Micropayment Integration

x402 enables HTTP-native micropayments. When WhiteMagic is deployed as an HTTP
MCP server (`--http` mode), it can accept x402 payment headers:

```
POST /mcp HTTP/1.1
X-402-Payment: xrpl:raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy:0.001:XRP
Content-Type: application/json

{"method": "tools/call", "params": {"name": "gana_winnowing_basket", ...}}
```

The server validates the payment receipt before processing the request.
This enables pay-per-query memory access for agent-to-agent commerce.

## Karma Ledger Integration

Every tip is recorded in the Karma Ledger with:
- `ops_class: "gratitude"`
- `tx_hash` (if on-ledger)
- `amount` and `currency`
- SHA-256 chain link to previous entry

Query with: `call_tool("karma_report", filter="gratitude")`
```

### 3.2 Agent-to-Agent Commerce Flow

**File:** `docs/guides/AGENT_COMMERCE.md`

Document the full loop:
1. Agent A publishes a service via `marketplace.publish`
2. Agent B discovers it via `marketplace.discover`
3. Agent B negotiates terms via `marketplace.negotiate`
4. Agent B pays via `ilp.send` (ILP streaming) or `whitemagic.tip` (XRPL lump sum)
5. Agent A delivers via OMS `.mem` package or direct tool access
6. Both agents rate the exchange via `marketplace.complete`

---

## 4. AI-First Documentation Strategy

### Core Principle

> The primary reader of WhiteMagic documentation is an AI agent, not a human developer. Every doc should be optimized for machine parsing first, human readability second.

### Concrete Guidelines

1. **Structured data over prose.** Use tables, code blocks, and JSON examples instead of paragraphs. An LLM can parse a table in one pass; a paragraph requires interpretation.

2. **One concept per heading.** Each `##` section should be independently queryable. An agent searching for "how to create a galaxy" should land on exactly one section.

3. **Executable examples.** Every code block should be copy-pasteable and runnable. No `...` elision, no `# your code here` placeholders.

4. **Tool-first navigation.** Organize docs by tool name, not by human mental model. An agent doesn't think "I need governance" — it thinks "I need `dharma_rules`."

5. **MCP resource mirrors.** Every major doc should be available as an MCP resource (`whitemagic://docs/<slug>`) so agents can read docs without leaving the tool protocol.

6. **llms.txt as index.** The `llms.txt` file is the table of contents for AI readers. Keep it updated with every new tool and doc page.

7. **Schema examples in every tool doc.** Show the full request and response JSON, including all envelope fields. Agents learn by example.

### Proposed docs.whitemagic.dev Structure (AI-Optimized)

```
docs.whitemagic.dev/
├── index.md                    # "Start here" — 3-step quickstart
├── contract.md                 # AI_PRIMARY.md (the tool contract)
├── tools/
│   ├── index.md                # Full tool inventory (356 tools, 28 Ganas)
│   ├── by-gana/                # One page per Gana with all nested tools
│   │   ├── horn.md             # Session init tools
│   │   ├── neck.md             # Memory creation tools
│   │   ├── ...
│   │   └── wall.md             # Security boundary tools
│   └── by-category/            # Cross-cutting views
│       ├── memory.md
│       ├── governance.md
│       ├── search.md
│       └── economy.md
├── guides/
│   ├── first-5-minutes.md      # Bootstrap → gnosis → create_memory → recall
│   ├── galaxy-setup.md         # Multi-tenant memory isolation
│   ├── xrpl-tipping.md         # Programmatic tipping (Section 3 above)
│   ├── agent-commerce.md       # Marketplace + ILP + OMS flow
│   ├── dream-cycle.md          # Overnight dream runner setup
│   ├── encryption.md           # SQLCipher at-rest encryption
│   ├── hermit-mode.md          # Air-gapped / privacy-first operation
│   └── docker-deploy.md        # Container deployment guide
├── reference/
│   ├── api.md                  # Full API reference (auto-generated from registry)
│   ├── architecture.md         # System map
│   ├── envelope.md             # Tool contract envelope spec
│   ├── error-codes.md          # Machine-actionable error taxonomy
│   └── polyglot.md             # Language-by-language accelerator status
├── grimoire/
│   ├── index.md                # Grimoire overview + spell index
│   └── chapters/               # 28 chapter files (one per Gana)
├── changelog.md
└── contributing.md
```

### MkDocs Material Configuration

```yaml
site_name: WhiteMagic Documentation
site_url: https://docs.whitemagic.dev
repo_url: https://github.com/whitemagic-ai/whitemagic
edit_uri: edit/main/docs/

theme:
  name: material
  palette:
    - scheme: slate
      primary: deep purple
      accent: amber
      toggle:
        icon: material/brightness-7
        name: Switch to light mode
    - scheme: default
      primary: deep purple
      accent: amber
      toggle:
        icon: material/brightness-4
        name: Switch to dark mode
  font:
    text: Crimson Pro
    code: JetBrains Mono
  features:
    - navigation.instant
    - navigation.tracking
    - navigation.tabs
    - navigation.sections
    - search.suggest
    - search.highlight
    - content.code.copy
    - content.code.annotate

plugins:
  - search
  - tags

markdown_extensions:
  - pymdownx.highlight
  - pymdownx.superfences
  - pymdownx.tabbed:
      alternate_style: true
  - admonitions
  - tables
  - toc:
      permalink: true

extra:
  social:
    - icon: fontawesome/brands/github
      link: https://github.com/whitemagic-ai/whitemagic
    - icon: fontawesome/brands/python
      link: https://pypi.org/project/whitemagic/
```

---

## 5. Website Content Updates for v15.7

### Stats Bar Update
- 341 → **356** MCP Tools
- 2,099 → **1,362** tests (corrected — was inflated)
- Add: **384 registry definitions**

### Version References
- v15.6 → **v15.7** in header, footer, schema.org, meta description
- Tool count 341 → **356** everywhere
- Nested tool count 313 → **356** in schema.org and structured data

### New Footer Links
- Add `Docs` link → `https://docs.whitemagic.dev`
- Add `Docker` link → `https://ghcr.io/whitemagic-ai/whitemagic`

### Schema.org Update
- `softwareVersion`: "15.6.0" → "15.7.0"
- `featureList`: Update tool count references

---

## 6. Priority Order

1. **Light mode contrast fix** — CSS only, 5 minutes
2. **Galaxy concept bridge text** — Add inline bridge text above Ganas carousel (quick win)
3. **Version bumps** — All references updated to v15.7, 356 tools
4. **XRPL tipping guide** — `docs/guides/XRPL_TIPPING_GUIDE.md`
5. **Agent commerce guide** — `docs/guides/AGENT_COMMERCE.md`
6. **MkDocs setup** — `mkdocs.yml` + GitHub Pages deploy
7. **Full Galaxy explainer section** — Design + implement (can be post-launch)

---

*This document supplements `docs/WEBSITE_REFRESH.md` (v15.0 original plan) with v15.7-specific additions.*
