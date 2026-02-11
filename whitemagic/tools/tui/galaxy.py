"""Whitemagic v13 - Galaxy TUI
A terminal-based 4D holographic memory visualizer.
"""

import math
from typing import Any

from rich.align import Align
from rich.box import ROUNDED
from rich.console import Console
from rich.layout import Layout
from rich.panel import Panel
from rich.table import Table
from rich.text import Text


class GalaxyExplorer:
    def __init__(self, memories: list[dict[str, Any]]):
        self.memories = memories
        self.console = Console()
        self.selected_index = 0

    def generate_map(self, width: int, height: int) -> Text:
        """Render a scatter plot of memories based on X/Y coords."""
        chart = [[" " for _ in range(width)] for _ in range(height)]

        # Center of the map
        cx, cy = width // 2, height // 2

        for i, m in enumerate(self.memories):
            x = int(m.get("x", 0) * (width // 2.5)) + cx
            y = int(-m.get("y", 0) * (height // 2.5)) + cy

            if 0 <= x < width and 0 <= y < height:
                char = "●"
                style = "bold cyan"
                if i == self.selected_index:
                    char = "🎯"
                    style = "bold yellow"
                elif m.get("w", 0) > 0.8:
                    style = "bold white"

                chart[y][x] = f"[{style}]{char}[/]"

        lines = ["".join(row) for row in chart]
        return Text.from_markup("\n".join(lines))

    def get_layout(self) -> Layout:
        layout = Layout()
        layout.split_row(
            Layout(name="map", ratio=2),
            Layout(name="details", ratio=1),
        )
        return layout

    def render_details(self) -> Panel:
        if not self.memories:
            return Panel("No data")

        m = self.memories[self.selected_index]
        table = Table(box=None, show_header=False)
        table.add_row("ID", f"[cyan]{m.get('id', 'N/A')}[/]")
        table.add_row("Title", f"[bold white]{m.get('title', 'Untitled')}[/]")
        table.add_row("Quadrant", f"[green]{m.get('quadrant', 'Unknown')}[/]")
        table.add_row("Gravity", f"{m.get('w', 0):.4f}")
        table.add_row("Resonance", f"{m.get('resonance', 0):.4f}")

        return Panel(
            Align.center(table),
            title="[bold yellow]Memory Details[/]",
            border_style="yellow",
            box=ROUNDED,
        )

    def run_static(self) -> None:
        """Standard static render for CLI output."""
        layout = self.get_layout()
        layout["map"].update(Panel(self.generate_map(60, 20), title="[bold cyan]4D Holographic Galaxy[/]", border_style="cyan"))
        layout["details"].update(self.render_details())
        self.console.print(layout)

def wm_galaxy_main() -> None:
    """CLI Entry point for `wm galaxy`."""
    try:
        from whitemagic.core.intelligence.vector_lake import get_vector_lake  # type: ignore[import-not-found]
        lake = get_vector_lake()
        # Fetch a sample of memories for the map
        memories = lake.get_holographic_sample(limit=100)
    except Exception:
        # Fallback test data
        memories = [
            {"id": f"mem_{i}", "title": f"Test Memory {i}", "x": math.cos(i), "y": math.sin(i), "w": 0.5, "quadrant": "test"}
            for i in range(20)
        ]

    explorer = GalaxyExplorer(memories)
    explorer.run_static()

if __name__ == "__main__":
    wm_galaxy_main()
