defmodule WhitemagicCore.GanYing.EventBus do
  use GenServer
  require Logger

  @moduledoc """
  Gan Ying Event Bus — Actor-model event routing with backpressure.

  The central nervous system of the Elixir layer. Receives events from:
    - Redis pub/sub (via MeshConsumer)
    - Python bridge (via Brain)
    - Internal Elixir processes (DreamCycle, Heartbeat, etc.)

  Routes events to registered subscribers based on event lane and type.
  Implements 3-lane temporal scheduling matching Python's TemporalScheduler:
    - FAST: <100ms latency (system health, mesh signals)
    - MEDIUM: <1s latency (memory ops, garden events)
    - SLOW: best-effort (dream cycle, consolidation, sweeps)

  Backpressure: if a subscriber's mailbox exceeds @max_mailbox_size,
  events are dropped with a warning rather than overwhelming the process.
  """

  # ---------------------------------------------------------------------------
  # Types
  # ---------------------------------------------------------------------------

  @type event_lane :: :fast | :medium | :slow
  @type event_type :: String.t()
  @type subscriber :: {pid(), event_lane(), MapSet.t(event_type()) | :all}

  # Backpressure: max messages in a subscriber's mailbox before we drop
  @max_mailbox_size 1_000

  # Lane classification for known event types
  @fast_events ~w(
    system_started system_stopped system_health_changed system_state_change
    mesh_signal internal_state_changed
  )

  @medium_events ~w(
    memory_created memory_updated memory_recalled memory_accessed
    memory_consolidated pattern_in_reading search_completed
    garden_activated garden_resonance beauty_detected joy_triggered
    love_activated wisdom_integrated truth_spoken
  )

  # Everything else defaults to :slow

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Publish an event to the bus. Non-blocking (cast).

  Event must be a map with at least an "event_type" key.
  """
  @spec publish(map()) :: :ok
  def publish(%{"event_type" => _} = event) do
    GenServer.cast(__MODULE__, {:publish, event})
  end

  def publish(event) do
    Logger.warning("GanYing.EventBus: event missing event_type: #{inspect(event)}")
    :ok
  end

  @doc """
  Subscribe the calling process to events.

  Options:
    - lane: :fast | :medium | :slow | :all (default :all)
    - types: list of event_type strings, or :all (default :all)
  """
  @spec subscribe(keyword()) :: :ok
  def subscribe(opts \\ []) do
    lane = Keyword.get(opts, :lane, :all)
    types = Keyword.get(opts, :types, :all)
    GenServer.call(__MODULE__, {:subscribe, self(), lane, types})
  end

  @doc "Unsubscribe the calling process."
  @spec unsubscribe() :: :ok
  def unsubscribe do
    GenServer.call(__MODULE__, {:unsubscribe, self()})
  end

  @doc "Get bus statistics."
  @spec stats() :: map()
  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  # ---------------------------------------------------------------------------
  # Server callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    Logger.info("⚡ GanYing EventBus initialized")
    {:ok, %{
      subscribers: [],
      stats: %{
        published: 0,
        delivered: 0,
        dropped: 0,
        by_lane: %{fast: 0, medium: 0, slow: 0}
      }
    }}
  end

  @impl true
  def handle_cast({:publish, event}, state) do
    event_type = event["event_type"] || "unknown"
    lane = classify_lane(event_type)
    timestamp = System.system_time(:millisecond)

    enriched = Map.merge(event, %{
      "_lane" => Atom.to_string(lane),
      "_bus_ts" => timestamp
    })

    # Route to matching subscribers
    {delivered, dropped} = route(enriched, lane, event_type, state.subscribers)

    # Update stats
    stats = state.stats
    new_stats = %{stats |
      published: stats.published + 1,
      delivered: stats.delivered + delivered,
      dropped: stats.dropped + dropped,
      by_lane: Map.update(stats.by_lane, lane, 1, &(&1 + 1))
    }

    {:noreply, %{state | stats: new_stats}}
  end

  @impl true
  def handle_call({:subscribe, pid, lane, types}, _from, state) do
    # Monitor the subscriber so we can clean up on crash
    Process.monitor(pid)
    type_set = if types == :all, do: :all, else: MapSet.new(types)
    sub = {pid, lane, type_set}
    {:reply, :ok, %{state | subscribers: [sub | state.subscribers]}}
  end

  @impl true
  def handle_call({:unsubscribe, pid}, _from, state) do
    new_subs = Enum.reject(state.subscribers, fn {p, _, _} -> p == pid end)
    {:reply, :ok, %{state | subscribers: new_subs}}
  end

  @impl true
  def handle_call(:stats, _from, state) do
    {:reply, Map.put(state.stats, :subscriber_count, length(state.subscribers)), state}
  end

  # Clean up dead subscribers
  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    new_subs = Enum.reject(state.subscribers, fn {p, _, _} -> p == pid end)
    {:noreply, %{state | subscribers: new_subs}}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  # ---------------------------------------------------------------------------
  # Internal routing
  # ---------------------------------------------------------------------------

  defp route(event, lane, event_type, subscribers) do
    Enum.reduce(subscribers, {0, 0}, fn {pid, sub_lane, sub_types}, {del, drop} ->
      if matches?(lane, sub_lane, event_type, sub_types) do
        if mailbox_ok?(pid) do
          send(pid, {:gan_ying, event})
          {del + 1, drop}
        else
          {del, drop + 1}
        end
      else
        {del, drop}
      end
    end)
  end

  defp matches?(event_lane, sub_lane, event_type, sub_types) do
    lane_ok = sub_lane == :all or sub_lane == event_lane
    type_ok = sub_types == :all or MapSet.member?(sub_types, event_type)
    lane_ok and type_ok
  end

  defp mailbox_ok?(pid) do
    case Process.info(pid, :message_queue_len) do
      {:message_queue_len, len} -> len < @max_mailbox_size
      nil -> false  # process is dead
    end
  end

  defp classify_lane(event_type) when event_type in @fast_events, do: :fast
  defp classify_lane(event_type) when event_type in @medium_events, do: :medium
  defp classify_lane(_), do: :slow
end
