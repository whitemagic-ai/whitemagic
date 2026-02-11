# Multi-stage Dockerfile for WhiteMagic v13.5+
# ================================================
# Stage 1: Rust Builder (optional — accelerators)
# Stage 2: Runtime (slim Python + MCP server)

# ---- Stage 1: Rust Builder ----
FROM python:3.12-slim AS builder

ENV PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PIP_NO_CACHE_DIR=1

WORKDIR /app

RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

RUN pip install maturin

COPY whitemagic-rust/ /app/whitemagic-rust/
WORKDIR /app/whitemagic-rust
RUN maturin build --release --out /app/dist

# ---- Stage 2: Runtime ----
FROM python:3.12-slim

LABEL org.opencontainers.image.title="WhiteMagic" \
      org.opencontainers.image.description="The Tool Substrate for Agentic AI" \
      org.opencontainers.image.source="https://github.com/lbailey94/whitemagic" \
      org.opencontainers.image.licenses="MIT"

WORKDIR /app

RUN apt-get update && apt-get install -y \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy project metadata first (layer caching)
COPY pyproject.toml README.md VERSION LICENSE /app/

# Copy Python package
COPY whitemagic/ /app/whitemagic/

# Install WhiteMagic with MCP + CLI support
RUN pip install --no-cache-dir ".[mcp,cli]"

# Install Rust accelerator wheel from builder stage
COPY --from=builder /app/dist/*.whl /tmp/
RUN pip install --no-cache-dir /tmp/*.whl && rm -f /tmp/*.whl

# Copy supporting files
COPY scripts/ /app/scripts/
COPY eval/ /app/eval/
COPY llms.txt /app/llms.txt
COPY skill.md /app/skill.md

# Create state directory with correct permissions
RUN mkdir -p /data/whitemagic/memory /data/whitemagic/gratitude

# Environment
ENV PYTHONPATH=/app \
    WM_STATE_ROOT=/data/whitemagic \
    WM_DB_PATH=/data/whitemagic/memory/whitemagic.db \
    WM_SILENT_INIT=1

# Health check: verify core tools are callable
HEALTHCHECK --interval=60s --timeout=10s --retries=3 \
    CMD python -c "from whitemagic.tools.unified_api import call_tool; assert call_tool('capabilities')['status']=='success'" || exit 1

# Expose API port (when running in API mode)
EXPOSE 8765

# Default: MCP stdio server in PRAT mode
# For MCP Classic: docker run whitemagic python -m whitemagic.run_mcp
# For CLI: docker run whitemagic wm status
# For API: docker run -p 8765:8765 whitemagic python -m whitemagic.interfaces.nexus_api
ENV WM_MCP_PRAT=1
CMD ["python", "-m", "whitemagic.run_mcp"]
