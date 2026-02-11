defmodule WhitemagicCore.DreamCycle do
  use GenServer
  require Logger
  alias WhitemagicCore.Brain

  @moduledoc """
  Autonomous process that wakes up periodically to consolidate memories.
  "Interconnection with local models... autonomous capabilities."
  """
  
  # 24 hours in milliseconds
  @interval 24 * 60 * 60 * 1000
  # For demo: 10 seconds startup delay
  @initial_delay 10_000

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @impl true
  def init(state) do
    Logger.info("🌙 Dream Cycle initialized. Sleeping...")
    Process.send_after(self(), :dream, @initial_delay)
    {:ok, state}
  end

  @impl true
  def handle_info(:dream, state) do
    Logger.info("🌌 Entering REM Sleep (Consolidating Memories)...")
    run_consolidation()

    # Schedule next dream
    Process.send_after(self(), :dream, @interval)
    {:noreply, state}
  end

  # A high-importance mesh signal nudges us into an early consolidation.
  # We honour it but do NOT reset the regular 24 h timer — that keeps ticking
  # independently so the schedule stays stable.
  @impl true
  def handle_call({:mesh_nudge, signal}, _from, state) do
    importance = get_in(signal, ["data", "importance"]) || 0.0
    Logger.info("🌌 Mesh nudge (importance=#{importance}) — running early consolidation")
    run_consolidation()
    {:reply, :ok, state}
  end

  # ---------------------------------------------------------------------------
  # Shared consolidation logic
  # ---------------------------------------------------------------------------
  defp run_consolidation do
    memory_path = Path.expand("../../memories", File.cwd!())

    case Brain.consolidate(memory_path) do
      %{"status" => "ok", "stats" => stats} ->
        Logger.info("✨ Consolidation Complete: #{inspect(stats)}")
      err ->
        Logger.warning("☁️ Dream interrupted: #{inspect(err)}")
    end
  end
end
