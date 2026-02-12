"""WhiteMagic Galaxy TUI
Visual interface for the 4D Memory Space using Textual.
"""
from typing import Any

from textual.app import App, ComposeResult  # type: ignore[import-not-found]
from textual.containers import Horizontal, Vertical  # type: ignore[import-not-found]
from textual.reactive import reactive  # type: ignore[import-not-found]
from textual.widgets import (  # type: ignore[import-not-found]
    DataTable,
    Footer,
    Header,
    Label,
    Static,
)

from whitemagic import __version__

try:
    from whitemagic.core.memory.unified import get_unified_memory
    HAS_MEMORY = True
except ImportError:
    HAS_MEMORY = False

class GalaxyStats(Static):
    """Widget to display galaxy statistics."""

    total_stars = reactive(0)
    clusters = reactive(0)

    def compose(self) -> ComposeResult:
        yield Label("🌌 Galaxy Stats", classes="box-title")
        yield Label(f"Stars: {self.total_stars}", id="stat-stars")
        yield Label(f"Clusters: {self.clusters}", id="stat-clusters")

    def watch_total_stars(self, val: int) -> None:
        self.query_one("#stat-stars", Label).update(f"Stars: {val}")

    def watch_clusters(self, val: int) -> None:
        self.query_one("#stat-clusters", Label).update(f"Clusters: {val}")

class GalaxyMap(Static):
    """Widget to display the 4D holographic map."""

    def on_mount(self) -> None:
        self.update_map([])

    def update_map(self, memories: list[Any]) -> None:
        from whitemagic.tools.tui.galaxy import GalaxyExplorer
        explorer = GalaxyExplorer(memories)
        # Use full width/height for the widget
        width = self.size.width - 2
        height = self.size.height - 2
        if width <= 0:
            width = 60
        if height <= 0:
            height = 20

        self.update(explorer.generate_map(width, height))

class GalaxyTUI(App):
    """The 4D Memory Space Visualizer."""

    CSS = """
    Screen {
        layout: vertical;
    }

    #main-container {
        height: 100%;
    }

    #sidebar {
        width: 20%;
        height: 100%;
        border: solid green;
    }

    #view-area {
        width: 80%;
        height: 100%;
    }

    .box {
        border: solid green;
    }

    .box-title {
        text-align: center;
        background: $accent;
        color: $text;
        padding: 1;
    }

    GalaxyMap {
        height: 100%;
        border: double cyan;
        content-align: center middle;
    }

    DataTable {
        height: 100%;
        border: solid blue;
    }
    """

    BINDINGS = [
        ("q", "quit", "Quit"),
        ("r", "refresh_data", "Refresh"),
    ]

    def compose(self) -> ComposeResult:
        yield Header(show_clock=True)
        yield Horizontal(
            Vertical(
                GalaxyStats(classes="box"),
                Static("Visual Controls\n\n[r] Refresh\n[q] Quit", classes="box"),
                id="sidebar",
            ),
            Vertical(
                GalaxyMap(id="galaxy_map"),
                DataTable(id="memory_table"),
                id="view-area",
            ),
            id="main-container",
        )
        yield Footer()

    def on_mount(self) -> None:
        self.title = f"WhiteMagic Galaxy v{__version__}"
        self.sub_title = "4D Memory Space Visualizer"
        # Small delay to allow size resolution
        self.set_timer(0.2, self.action_refresh_data)

    def action_refresh_data(self) -> None:
        if not HAS_MEMORY:
            self.notify("Error: WhiteMagic Core not available", severity="error")
            return

        memory = get_unified_memory()

        # Update Stats
        stats = memory.get_stats()
        galaxy_stats = self.query_one(GalaxyStats)
        galaxy_stats.total_stars = stats.get("total_memories", 0)
        galaxy_stats.clusters = stats.get("total_topics", 0)

        # Fetch recent memories for Table and Map
        from whitemagic.core.intelligence.vector_lake import get_vector_lake  # type: ignore[import-not-found]
        lake = get_vector_lake()
        raw_results = lake.get_holographic_sample(limit=100)

        # Update Map
        self.query_one(GalaxyMap).update_map(raw_results)

        # Update Table
        table = self.query_one(DataTable)
        table.clear(columns=True)
        table.add_columns("ID", "Title", "X", "Y", "W")

        for m in raw_results:
            mid = str(m.get("id", ""))[:8]
            title = str(m.get("title", "Untitled"))[:30]
            x = f"{m.get('x', 0):.2f}"
            y = f"{m.get('y', 0):.2f}"
            w = f"{m.get('w', 0):.2f}"
            table.add_row(mid, title, x, y, w)

        self.notify(f"Synchronized {len(raw_results)} stars with the holographic core")

if __name__ == "__main__":
    GalaxyTUI().run()
