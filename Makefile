
WM_STATE_ROOT ?= /tmp/whitemagic_eval
WM_DB_PATH ?=
WM_SILENT_INIT ?= 1

.PHONY: install test smoke ship clean format eval lint typecheck clippy

install:
	pip install -e ".[dev,mcp,cli]"

test:
	WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) pytest -q tests/

smoke:
	WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) python3 audit/tool_smoke.py

ship:
	WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) python3 -c "import json; from whitemagic.tools.unified_api import call_tool; print(json.dumps(call_tool('ship.check')['details'], indent=2, sort_keys=True))"

eval:
	WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) python3 eval/run_eval.py

clean:
	rm -rf build/ dist/ *.egg-info .pytest_cache
	find . -name "__pycache__" -type d -exec rm -rf {} +
	find . -name "*.pyc" -delete

lint:
	ruff check whitemagic/ --select E,F,W --ignore E501

typecheck:
	mypy whitemagic/ --ignore-missing-imports --no-error-summary

clippy:
	cd whitemagic-rust && cargo clippy -- -W clippy::unwrap_used

format:
	black .
	isort .
