# mypy: disable-error-code=no-untyped-def
import logging
import os
import subprocess
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class MojoBridge:
    """Bridge to the Mojo AI Accelerator.

    Executes Mojo scripts from the 'whitemagic-mojo' directory.
    Uses shell execution since Mojo FFI to Python is unidirectional (Mojo -> Python)
    mostly, and we want Core (Python) -> Mojo.
    """

    def __init__(self, project_path: str | None = None):
        # Look for the root whitemagic-mojo directory
        if project_path:
            self.project_path = project_path
        else:
            # Assume we are in whitemagic/core/polyglot
            # Root (core_system) is 4 levels up
            root = Path(__file__).parent.parent.parent.parent
            # "accelerator" is the new name for "whitemagic-mojo"
            self.project_path = str(root / "accelerator")

        self._check_project_path()

    def _check_project_path(self):
        if not os.path.exists(self.project_path):
            logger.warning(f"Mojo project not found at {self.project_path}")

    def run_script(self, script_name: str, args: list = []) -> str | None:
        """Run a Mojo script and return its stdout.

        Args:
            script_name: Name of script in whitemagic-mojo (e.g. 'hello.mojo')
            args: Command line arguments

        """
        script_path = os.path.join(self.project_path, script_name)
        if not os.path.exists(script_path):
            logger.error(f"Mojo script not found: {script_path}")
            return None

        try:
            cmd = ["mojo", script_path] + args
            logger.info(f"Running Mojo: {' '.join(cmd)}")

            result = subprocess.run(
                cmd, capture_output=True, text=True, cwd=self.project_path,
            )

            if result.returncode != 0:
                logger.error(f"Mojo execution failed: {result.stderr}")
                return None

            return result.stdout.strip()

        except FileNotFoundError:
            logger.error("Mojo executable not found. Is it in your PATH?")
            return None
        except Exception as e:
            logger.error(f"Error running Mojo script: {e}")
            return None

    def benchmark_simd(self) -> dict[str, Any]:
        """Run the SIMD benchmark."""
        # Assuming there's a benchmark script, or we use hello.mojo as a test
        output = self.run_script("hello.mojo")
        return {"success": output is not None, "output": output, "accelerator": "mojo"}


# Singleton
_bridge: MojoBridge | None = None


def get_mojo_bridge() -> MojoBridge:
    global _bridge
    if _bridge is None:
        _bridge = MojoBridge()
    return _bridge
