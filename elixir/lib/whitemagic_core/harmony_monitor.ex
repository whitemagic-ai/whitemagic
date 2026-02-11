defmodule WhitemagicCore.HarmonyMonitor do
  @moduledoc """
  Harmony Monitor — OTP-supervised health monitoring GenServer.

  Continuously monitors the 7-dimension Harmony Vector and triggers
  homeostatic corrections when dimensions drift out of bounds.

  Dimensions monitored:
    1. Balance    — system equilibrium
    2. Throughput — tool execution rate
    3. Latency    — response time distribution
    4. Error Rate — failure frequency
    5. Dharma     — ethical compliance score
    6. Karma Debt — side-effect deficit
    7. Energy     — system vitality

  Corrections:
    - OBSERVE:    Log anomaly (dimension < 0.3 or > 0.9)
    - ADVISE:     Emit advisory event to Gan Ying bus
    - CORRECT:    Auto-adjust parameters (e.g., tighten rate limits)
    - INTERVENE:  Emergency actions (pause non-essential tools)

  Supervision:
    Should be started under the application supervisor with restart: :permanent.
  """

  use GenServer
  require Logger

  @default_name __MODULE__
  @check_interval_ms 10_000  # Check every 10 seconds
  @healthy_range {0.3, 0.85}

  # ---------------------------------------------------------------------------
  # Types
  # ---------------------------------------------------------------------------

  defmodule HarmonySnapshot do
    @moduledoc "A point-in-time snapshot of the Harmony Vector."
    defstruct [
      balance: 0.5,
      throughput: 0.5,
      latency: 0.5,
      error_rate: 0.5,
      dharma: 0.8,
      karma_debt: 0.0,
      energy: 0.7,
      timestamp: nil,
      overall: 0.5
    ]

    @type t :: %__MODULE__{
      balance: float(),
      throughput: float(),
      latency: float(),
      error_rate: float(),
      dharma: float(),
      karma_debt: float(),
      energy: float(),
      timestamp: DateTime.t() | nil,
      overall: float()
    }
  end

  defmodule Alert do
    @moduledoc "A harmony alert."
    defstruct [:dimension, :value, :threshold, :severity, :action, :timestamp]

    @type severity :: :observe | :advise | :correct | :intervene
    @type t :: %__MODULE__{
      dimension: atom(),
      value: float(),
      threshold: {float(), float()},
      severity: severity(),
      action: String.t(),
      timestamp: DateTime.t()
    }
  end

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, @default_name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "Get the current harmony snapshot."
  def snapshot(server \\ @default_name) do
    GenServer.call(server, :snapshot)
  end

  @doc "Update a harmony dimension."
  def update(server \\ @default_name, dimension, value) when is_atom(dimension) and is_float(value) do
    GenServer.cast(server, {:update, dimension, value})
  end

  @doc "Record a tool execution for throughput/latency tracking."
  def record_execution(server \\ @default_name, tool_name, duration_ms, success?) do
    GenServer.cast(server, {:record_execution, tool_name, duration_ms, success?})
  end

  @doc "Get recent alerts."
  def alerts(server \\ @default_name, limit \\ 20) do
    GenServer.call(server, {:alerts, limit})
  end

  @doc "Get overall health status."
  def health(server \\ @default_name) do
    GenServer.call(server, :health)
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    # Schedule periodic health checks
    Process.send_after(self(), :health_check, @check_interval_ms)

    state = %{
      harmony: %HarmonySnapshot{timestamp: DateTime.utc_now()},
      alerts: [],
      max_alerts: 100,
      executions: [],       # Recent execution records for throughput/latency
      max_executions: 1000,
      checks_performed: 0,
      corrections_applied: 0,
      started_at: DateTime.utc_now()
    }

    Logger.info("[HarmonyMonitor] Started — checking every #{@check_interval_ms}ms")
    {:ok, state}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    {:reply, state.harmony, state}
  end

  @impl true
  def handle_call({:alerts, limit}, _from, state) do
    {:reply, Enum.take(state.alerts, limit), state}
  end

  @impl true
  def handle_call(:health, _from, state) do
    h = state.harmony
    overall = compute_overall(h)
    status = cond do
      overall >= 0.7 -> :healthy
      overall >= 0.5 -> :degraded
      overall >= 0.3 -> :unhealthy
      true -> :critical
    end

    {:reply, %{
      status: status,
      overall: overall,
      harmony: h,
      alerts_active: length(state.alerts),
      checks_performed: state.checks_performed,
      corrections_applied: state.corrections_applied,
      uptime_seconds: DateTime.diff(DateTime.utc_now(), state.started_at)
    }, state}
  end

  @impl true
  def handle_cast({:update, dimension, value}, state) do
    harmony = Map.put(state.harmony, dimension, clamp(value, 0.0, 1.0))
    harmony = %{harmony | timestamp: DateTime.utc_now(), overall: compute_overall(harmony)}
    {:noreply, %{state | harmony: harmony}}
  end

  @impl true
  def handle_cast({:record_execution, _tool, duration_ms, success?}, state) do
    record = %{duration_ms: duration_ms, success: success?, at: System.monotonic_time(:millisecond)}
    executions = [record | state.executions] |> Enum.take(state.max_executions)

    # Update throughput and latency from recent executions
    recent = Enum.take(executions, 100)
    throughput = length(recent) / max(100, length(recent))  # Normalize
    avg_latency = if length(recent) > 0 do
      Enum.sum(Enum.map(recent, & &1.duration_ms)) / length(recent)
    else
      0
    end
    latency_score = clamp(1.0 - (avg_latency / 5000.0), 0.0, 1.0)  # 5s = worst

    error_count = Enum.count(recent, fn r -> not r.success end)
    error_rate = error_count / max(1, length(recent))
    error_score = clamp(1.0 - error_rate, 0.0, 1.0)

    harmony = state.harmony
    |> Map.put(:throughput, throughput)
    |> Map.put(:latency, latency_score)
    |> Map.put(:error_rate, error_score)
    harmony = %{harmony | timestamp: DateTime.utc_now(), overall: compute_overall(harmony)}

    {:noreply, %{state | harmony: harmony, executions: executions}}
  end

  @impl true
  def handle_info(:health_check, state) do
    # Schedule next check
    Process.send_after(self(), :health_check, @check_interval_ms)

    # Check each dimension
    {new_alerts, corrections} = check_dimensions(state.harmony)

    alerts = (new_alerts ++ state.alerts) |> Enum.take(state.max_alerts)

    {:noreply, %{state |
      alerts: alerts,
      checks_performed: state.checks_performed + 1,
      corrections_applied: state.corrections_applied + corrections
    }}
  end

  # ---------------------------------------------------------------------------
  # Internal
  # ---------------------------------------------------------------------------

  defp compute_overall(h) do
    weights = %{
      balance: 0.15, throughput: 0.15, latency: 0.15,
      error_rate: 0.15, dharma: 0.15, karma_debt: 0.10, energy: 0.15
    }

    total = Enum.reduce(weights, 0.0, fn {dim, weight}, acc ->
      val = Map.get(h, dim, 0.5)
      acc + val * weight
    end)

    clamp(total, 0.0, 1.0)
  end

  defp check_dimensions(harmony) do
    {min_healthy, max_healthy} = @healthy_range
    dimensions = [:balance, :throughput, :latency, :error_rate, :dharma, :energy]

    Enum.reduce(dimensions, {[], 0}, fn dim, {alerts, corrections} ->
      value = Map.get(harmony, dim, 0.5)
      cond do
        value < min_healthy - 0.2 ->
          alert = %Alert{
            dimension: dim, value: value,
            threshold: @healthy_range,
            severity: :intervene,
            action: "Emergency: #{dim} at #{Float.round(value, 2)}",
            timestamp: DateTime.utc_now()
          }
          Logger.warning("[HarmonyMonitor] INTERVENE: #{dim} = #{Float.round(value, 2)}")
          {[alert | alerts], corrections + 1}

        value < min_healthy ->
          alert = %Alert{
            dimension: dim, value: value,
            threshold: @healthy_range,
            severity: :correct,
            action: "Correcting: #{dim} at #{Float.round(value, 2)}",
            timestamp: DateTime.utc_now()
          }
          Logger.info("[HarmonyMonitor] CORRECT: #{dim} = #{Float.round(value, 2)}")
          {[alert | alerts], corrections + 1}

        value > max_healthy ->
          alert = %Alert{
            dimension: dim, value: value,
            threshold: @healthy_range,
            severity: :observe,
            action: "Observing: #{dim} at #{Float.round(value, 2)} (high)",
            timestamp: DateTime.utc_now()
          }
          {[alert | alerts], corrections}

        true ->
          {alerts, corrections}
      end
    end)
  end

  defp clamp(value, min_val, max_val) do
    value |> max(min_val) |> min(max_val)
  end
end
