defmodule WhitemagicCore.GardenPubSub do
  @moduledoc """
  Garden PubSub — Event-driven garden activation system.

  WhiteMagic's 28 Ganas are organized into 4 quadrant gardens
  (East/South/West/North). This module provides an Elixir PubSub
  system for garden-level event propagation:

    - Tool invocations broadcast to their garden's topic
    - Cross-garden resonance events propagate between adjacent quadrants
    - Seasonal (Wu Xing) phase changes trigger garden-wide reconfigurations
    - Constellation formation events cluster related tool activity

  Topics:
    "garden:east"   — Wood element, growth & creativity tools
    "garden:south"  — Fire element, transformation & analysis tools
    "garden:west"   — Metal element, precision & refinement tools
    "garden:north"  — Water element, memory & wisdom tools
    "garden:center" — Earth element, balance & governance tools
    "resonance:*"   — Cross-garden resonance propagation
    "wuxing:phase"  — Wu Xing phase change notifications
    "constellation:formed" — New constellation detected

  Architecture:
    Uses Registry-based PubSub (no external deps like Phoenix.PubSub).
  """

  use GenServer
  require Logger

  @registry __MODULE__.Registry
  @default_name __MODULE__

  # ---------------------------------------------------------------------------
  # Types
  # ---------------------------------------------------------------------------

  defmodule Event do
    @moduledoc "A garden event."
    defstruct [
      :id,
      :topic,
      :type,       # :tool_invocation | :resonance | :phase_change | :constellation | :custom
      :payload,
      :source_gana,
      :quadrant,
      :timestamp,
      :ttl         # Time-to-live in seconds (events expire)
    ]

    @type t :: %__MODULE__{
      id: String.t(),
      topic: String.t(),
      type: atom(),
      payload: map(),
      source_gana: String.t() | nil,
      quadrant: :east | :south | :west | :north | :center | nil,
      timestamp: DateTime.t(),
      ttl: non_neg_integer()
    }
  end

  # Garden → Gana mapping (28 Ganas across 4 quadrants + center)
  @garden_ganas %{
    east: ~w(ashvini bharani krittika rohini mrigashira ardra punarvasu),
    south: ~w(pushya ashlesha magha purvaphalguni uttaraphalguni hasta chitra),
    west: ~w(swati vishakha anuradha jyeshtha mula purvashada uttarashada),
    north: ~w(shravana dhanishta shatabhisha purvabhadra uttarabhadra revati abhijit)
  }

  @wu_xing_gardens %{
    wood: :east,
    fire: :south,
    metal: :west,
    water: :north,
    earth: :center
  }

  # Adjacent quadrants for resonance propagation
  @adjacency %{
    east: [:south, :north],
    south: [:east, :west],
    west: [:south, :north],
    north: [:west, :east],
    center: [:east, :south, :west, :north]
  }

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, @default_name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "Subscribe to a garden topic. Returns :ok."
  def subscribe(topic, pid \\ self()) do
    Registry.register(@registry, topic, pid)
    :ok
  end

  @doc "Unsubscribe from a garden topic."
  def unsubscribe(topic, pid \\ self()) do
    Registry.unregister_match(@registry, topic, pid)
    :ok
  end

  @doc "Publish an event to a garden topic."
  def publish(server \\ @default_name, topic, event_type, payload, opts \\ []) do
    event = %Event{
      id: generate_id(),
      topic: topic,
      type: event_type,
      payload: payload,
      source_gana: Keyword.get(opts, :gana),
      quadrant: Keyword.get(opts, :quadrant),
      timestamp: DateTime.utc_now(),
      ttl: Keyword.get(opts, :ttl, 300)
    }
    GenServer.cast(server, {:publish, event})
    {:ok, event.id}
  end

  @doc "Broadcast a tool invocation to the appropriate garden."
  def broadcast_tool(server \\ @default_name, tool_name, gana_name, result) do
    quadrant = gana_to_quadrant(gana_name)
    topic = "garden:#{quadrant}"

    publish(server, topic, :tool_invocation, %{
      tool: tool_name,
      gana: gana_name,
      result_status: Map.get(result, :status, "ok"),
      summary: Map.get(result, :summary, "")
    }, gana: gana_name, quadrant: quadrant)
  end

  @doc "Broadcast a Wu Xing phase change."
  def broadcast_phase_change(server \\ @default_name, new_phase, old_phase) do
    publish(server, "wuxing:phase", :phase_change, %{
      new_phase: new_phase,
      old_phase: old_phase,
      active_garden: Map.get(@wu_xing_gardens, new_phase, :center)
    })
  end

  @doc "Get event history for a topic."
  def history(server \\ @default_name, topic, limit \\ 50) do
    GenServer.call(server, {:history, topic, limit})
  end

  @doc "Get garden status summary."
  def status(server \\ @default_name) do
    GenServer.call(server, :status)
  end

  @doc "Look up which quadrant a Gana belongs to."
  def gana_to_quadrant(gana_name) do
    gana_lower = String.downcase(to_string(gana_name))
    Enum.find_value(@garden_ganas, :center, fn {quadrant, ganas} ->
      if gana_lower in ganas, do: quadrant
    end)
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    # Start the Registry for PubSub
    {:ok, _} = Registry.start_link(keys: :duplicate, name: @registry)

    state = %{
      event_log: %{},       # topic => [events] (bounded)
      event_counts: %{},    # topic => count
      max_history: 100,
      total_events: 0,
      started_at: DateTime.utc_now()
    }

    Logger.info("[GardenPubSub] Started with #{map_size(@garden_ganas)} quadrants, #{Enum.sum(Enum.map(@garden_ganas, fn {_, g} -> length(g) end))} Ganas")
    {:ok, state}
  end

  @impl true
  def handle_cast({:publish, event}, state) do
    # Dispatch to all subscribers of this topic
    dispatch_to_subscribers(event)

    # Store in event log
    new_log = Map.update(
      state.event_log,
      event.topic,
      [event],
      fn events ->
        [event | events] |> Enum.take(state.max_history)
      end
    )

    new_counts = Map.update(state.event_counts, event.topic, 1, &(&1 + 1))

    # Propagate resonance to adjacent gardens if tool invocation
    if event.type == :tool_invocation and event.quadrant do
      propagate_resonance(event)
    end

    {:noreply, %{state |
      event_log: new_log,
      event_counts: new_counts,
      total_events: state.total_events + 1
    }}
  end

  @impl true
  def handle_call({:history, topic, limit}, _from, state) do
    events = Map.get(state.event_log, topic, []) |> Enum.take(limit)
    {:reply, events, state}
  end

  @impl true
  def handle_call(:status, _from, state) do
    status = %{
      total_events: state.total_events,
      topics: Map.keys(state.event_counts),
      event_counts: state.event_counts,
      uptime_seconds: DateTime.diff(DateTime.utc_now(), state.started_at),
      garden_ganas: @garden_ganas,
      wu_xing_mapping: @wu_xing_gardens
    }
    {:reply, status, state}
  end

  # ---------------------------------------------------------------------------
  # Internal
  # ---------------------------------------------------------------------------

  defp dispatch_to_subscribers(event) do
    Registry.dispatch(@registry, event.topic, fn entries ->
      for {pid, _value} <- entries do
        send(pid, {:garden_event, event})
      end
    end)
  end

  defp propagate_resonance(event) do
    adjacent = Map.get(@adjacency, event.quadrant, [])
    Enum.each(adjacent, fn adj_quadrant ->
      resonance_event = %Event{
        id: generate_id(),
        topic: "resonance:#{adj_quadrant}",
        type: :resonance,
        payload: %{
          source_quadrant: event.quadrant,
          source_gana: event.source_gana,
          source_tool: Map.get(event.payload, :tool, "unknown"),
          resonance_strength: 0.3  # Decayed cross-garden signal
        },
        source_gana: event.source_gana,
        quadrant: adj_quadrant,
        timestamp: DateTime.utc_now(),
        ttl: 60
      }
      dispatch_to_subscribers(resonance_event)
    end)
  end

  defp generate_id do
    :crypto.strong_rand_bytes(8) |> Base.hex_encode32(case: :lower, padding: false)
  end
end
