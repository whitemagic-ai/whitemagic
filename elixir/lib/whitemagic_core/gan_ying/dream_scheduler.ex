defmodule WhitemagicCore.GanYing.DreamScheduler do
  use GenServer
  require Logger

  @moduledoc """
  5-Phase Dream Cycle Scheduler — OTP GenServer.

  Matches Python's `core/dreaming/dream_cycle.py` with proper OTP supervision.
  Runs autonomously on the SLOW lane during idle time.

  Phases (each runs sequentially during a dream cycle):
    1. CONSOLIDATION — Cluster recent memories, synthesize strategies
    2. SERENDIPITY   — Surface unexpected cross-domain connections
    3. KAIZEN        — Continuous improvement proposals
    4. ORACLE        — I Ching consultation for system guidance
    5. DECAY         — Galactic drift for inactive memories

  The scheduler:
    - Publishes phase transitions to the GanYing EventBus
    - Resets idle timer on every `touch/0` call (tool invocation)
    - Supports manual `dream_now/1` to force a single phase
    - Tracks dream history for the gnosis portal
  """

  alias WhitemagicCore.GanYing.EventBus

  # ---------------------------------------------------------------------------
  # Configuration
  # ---------------------------------------------------------------------------

  # Idle threshold before entering dream state (5 minutes)
  @idle_threshold_ms 5 * 60 * 1_000

  # Interval between phase checks when dreaming
  @phase_interval_ms 30_000

  # Check idle status every 30 seconds
  @idle_check_ms 30_000

  # Maximum dream cycles per day
  @max_daily_cycles 12

  @phases [:consolidation, :serendipity, :kaizen, :oracle, :decay]

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Reset idle timer (call on every tool invocation)."
  @spec touch() :: :ok
  def touch do
    GenServer.cast(__MODULE__, :touch)
  end

  @doc "Force a single dream phase immediately."
  @spec dream_now(atom()) :: {:ok, map()} | {:error, term()}
  def dream_now(phase \\ :consolidation) when phase in @phases do
    GenServer.call(__MODULE__, {:dream_now, phase}, 60_000)
  end

  @doc "Start a full dream cycle."
  @spec start_dreaming() :: :ok | {:error, :already_dreaming}
  def start_dreaming do
    GenServer.call(__MODULE__, :start_dreaming)
  end

  @doc "Stop dreaming and return to idle."
  @spec stop_dreaming() :: :ok
  def stop_dreaming do
    GenServer.call(__MODULE__, :stop_dreaming)
  end

  @doc "Get current dream state."
  @spec status() :: map()
  def status do
    GenServer.call(__MODULE__, :status)
  end

  # ---------------------------------------------------------------------------
  # Server callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    Logger.info("🌙 DreamScheduler initialized")
    schedule_idle_check()

    {:ok, %{
      state: :idle,
      last_touch: System.monotonic_time(:millisecond),
      current_phase: nil,
      phase_index: 0,
      cycles_today: 0,
      cycle_start: nil,
      history: [],
      daily_reset: today_date()
    }}
  end

  @impl true
  def handle_cast(:touch, state) do
    new_state = %{state | last_touch: System.monotonic_time(:millisecond)}

    # If we were dreaming, wake up
    new_state = if new_state.state == :dreaming do
      Logger.info("🌙 Touch received — waking from dream")
      emit_event("dream_interrupted", %{phase: state.current_phase})
      %{new_state | state: :idle, current_phase: nil, phase_index: 0}
    else
      new_state
    end

    {:noreply, new_state}
  end

  @impl true
  def handle_call(:start_dreaming, _from, %{state: :dreaming} = state) do
    {:reply, {:error, :already_dreaming}, state}
  end

  def handle_call(:start_dreaming, _from, state) do
    state = maybe_reset_daily(state)

    if state.cycles_today >= @max_daily_cycles do
      {:reply, {:error, :daily_limit_reached}, state}
    else
      new_state = enter_dream(state)
      {:reply, :ok, new_state}
    end
  end

  @impl true
  def handle_call(:stop_dreaming, _from, state) do
    new_state = %{state | state: :idle, current_phase: nil, phase_index: 0}
    emit_event("dream_stopped", %{})
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:dream_now, phase}, _from, state) do
    result = run_phase(phase)
    record = %{phase: phase, result: result, timestamp: DateTime.utc_now()}
    new_state = %{state | history: [record | Enum.take(state.history, 99)]}
    {:reply, {:ok, result}, new_state}
  end

  @impl true
  def handle_call(:status, _from, state) do
    {:reply, %{
      state: state.state,
      current_phase: state.current_phase,
      phase_index: state.phase_index,
      cycles_today: state.cycles_today,
      idle_ms: idle_duration(state),
      history_count: length(state.history),
      last_phases: Enum.take(state.history, 5) |> Enum.map(&Map.take(&1, [:phase, :timestamp]))
    }, state}
  end

  # Periodic idle check
  @impl true
  def handle_info(:check_idle, %{state: :idle} = state) do
    state = maybe_reset_daily(state)

    new_state = if idle_duration(state) >= @idle_threshold_ms and
                   state.cycles_today < @max_daily_cycles do
      Logger.info("🌙 Idle threshold reached — entering dream state")
      enter_dream(state)
    else
      state
    end

    schedule_idle_check()
    {:noreply, new_state}
  end

  def handle_info(:check_idle, state) do
    schedule_idle_check()
    {:noreply, state}
  end

  # Phase progression during dreaming
  @impl true
  def handle_info(:next_phase, %{state: :dreaming} = state) do
    if state.phase_index >= length(@phases) do
      # Cycle complete
      Logger.info("🌙 Dream cycle complete (#{state.cycles_today + 1} today)")
      emit_event("dream_cycle_complete", %{cycle: state.cycles_today + 1})

      {:noreply, %{state |
        state: :idle,
        current_phase: nil,
        phase_index: 0,
        cycles_today: state.cycles_today + 1
      }}
    else
      phase = Enum.at(@phases, state.phase_index)
      Logger.info("🌙 Dream phase #{state.phase_index + 1}/#{length(@phases)}: #{phase}")
      emit_event("dream_phase_started", %{phase: phase, index: state.phase_index})

      result = run_phase(phase)
      record = %{phase: phase, result: result, timestamp: DateTime.utc_now()}

      emit_event("dream_phase_completed", %{phase: phase, result: result})
      schedule_next_phase()

      {:noreply, %{state |
        current_phase: phase,
        phase_index: state.phase_index + 1,
        history: [record | Enum.take(state.history, 99)]
      }}
    end
  end

  def handle_info(:next_phase, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  # ---------------------------------------------------------------------------
  # Internal helpers
  # ---------------------------------------------------------------------------

  defp enter_dream(state) do
    emit_event("dream_started", %{cycle: state.cycles_today + 1})
    schedule_next_phase()

    %{state |
      state: :dreaming,
      phase_index: 0,
      current_phase: nil,
      cycle_start: System.monotonic_time(:millisecond)
    }
  end

  defp run_phase(:consolidation) do
    Logger.info("  💭 Consolidation: clustering recent memories")
    # In production, calls Brain.consolidate() or Python bridge
    %{status: :ok, action: "memory_consolidation", simulated: true}
  end

  defp run_phase(:serendipity) do
    Logger.info("  🎲 Serendipity: surfacing cross-domain connections")
    %{status: :ok, action: "serendipity_surface", simulated: true}
  end

  defp run_phase(:kaizen) do
    Logger.info("  🔧 Kaizen: generating improvement proposals")
    %{status: :ok, action: "kaizen_analysis", simulated: true}
  end

  defp run_phase(:oracle) do
    Logger.info("  🔮 Oracle: consulting I Ching for guidance")
    %{status: :ok, action: "oracle_consultation", simulated: true}
  end

  defp run_phase(:decay) do
    Logger.info("  🌌 Decay: applying galactic drift to inactive memories")
    %{status: :ok, action: "galactic_decay_drift", simulated: true}
  end

  defp idle_duration(state) do
    System.monotonic_time(:millisecond) - state.last_touch
  end

  defp schedule_idle_check do
    Process.send_after(self(), :check_idle, @idle_check_ms)
  end

  defp schedule_next_phase do
    Process.send_after(self(), :next_phase, @phase_interval_ms)
  end

  defp emit_event(event_type, data) do
    EventBus.publish(%{
      "event_type" => event_type,
      "source" => "dream_scheduler",
      "data" => data,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    })
  rescue
    _ -> :ok  # EventBus might not be running yet
  end

  defp today_date, do: Date.utc_today()

  defp maybe_reset_daily(state) do
    today = today_date()
    if state.daily_reset != today do
      %{state | cycles_today: 0, daily_reset: today}
    else
      state
    end
  end
end
