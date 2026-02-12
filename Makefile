
WM_STATE_ROOT ?= /tmp/whitemagic_eval
WM_DB_PATH ?=
WM_SILENT_INIT ?= 1

.PHONY: install test smoke smoke-installed verify-local ship clean format eval lint typecheck clippy build build-reproducible verify-reproducible

install:
	pip install -e ".[dev,mcp,cli]"

test:
	WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) pytest -q tests/

smoke:
	PYTHONPATH=. WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) python3 scripts/verify_install.py

smoke-installed:
	WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) python3 scripts/verify_install.py

verify-local:
	@set -eu; \
	REPORT="$$(mktemp /tmp/whitemagic_pytest_XXXX.out)"; \
	trap 'rm -f "$$REPORT"' EXIT; \
	echo "==> Python tests (with skip summary)"; \
	if WM_STATE_ROOT=$(WM_STATE_ROOT) WM_DB_PATH=$(WM_DB_PATH) WM_SILENT_INIT=$(WM_SILENT_INIT) pytest -q -rs tests/ > "$$REPORT"; then \
		cat "$$REPORT"; \
	else \
		cat "$$REPORT"; \
		exit 1; \
	fi; \
	python3 scripts/verification/check_skip_policy.py "$$REPORT" --profile baseline-dev; \
	echo "==> Rust tests"; \
	(cd whitemagic-rust && cargo test -q); \
	echo "==> Rust clippy"; \
	(cd whitemagic-rust && cargo clippy -- -D warnings)

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
	cd whitemagic-rust && cargo clippy -- -D warnings

build:
	rm -rf dist/
	python -m build --sdist --wheel

build-reproducible:
	rm -rf dist/
	SOURCE_DATE_EPOCH=$$(git log -1 --format=%ct) python -m build --sdist --wheel
	@echo "Computing build hashes..."
	@python3 -c "\
import hashlib, json, pathlib, datetime; \
files = sorted(pathlib.Path('dist').glob('*')); \
hashes = {f.name: hashlib.sha256(f.read_bytes()).hexdigest() for f in files}; \
data = {'hashes': hashes, 'source_date_epoch': '$$(git log -1 --format=%ct)', 'git_sha': '$$(git rev-parse HEAD)', 'built_at': datetime.datetime.now(datetime.timezone.utc).isoformat()}; \
pathlib.Path('BUILD_HASHES.json').write_text(json.dumps(data, indent=2) + '\\n'); \
print('BUILD_HASHES.json written'); \
[print(f'  {k}: {v}') for k,v in hashes.items()]"

verify-reproducible:
	@echo "Verifying reproducible build..."
	@python3 -c "\
import hashlib, json, pathlib, sys; \
expected = json.loads(pathlib.Path('BUILD_HASHES.json').read_text()); \
ok = True; \
for name, h in expected['hashes'].items(): \
    p = pathlib.Path('dist') / name; \
    actual = hashlib.sha256(p.read_bytes()).hexdigest() if p.exists() else 'MISSING'; \
    match = '✓' if actual == h else '✗'; ok = ok and (actual == h); \
    print(f'  {match} {name}: {actual[:16]}...'); \
print('All hashes match!' if ok else 'MISMATCH DETECTED'); \
sys.exit(0 if ok else 1)"

format:
	black .
	isort .
