defmodule WhitemagicCore.MeshConsumer do
  use GenServer
  require Logger

  @moduledoc """
  Subscribes to the Redis "ganying" channel and dispatches inbound
  mesh signals into the Elixir supervision tree.

  Go Mesh publishes HolographicSignal events as JSON on "ganying".
  This process receives them and routes high-importance signals to
  DreamCycle for accelerated consolidation, and logs everything for
  the Ingestor's awareness.
  """

  @channel "ganying"
  # Signals above this importance trigger an early consolidation nudge
  @importance_threshold 0.6

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  # ---------------------------------------------------------------------------
  # Server callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    redis_url = System.get_env("REDIS_URL", "redis://localhost:6379/0")

    case Redix.PubSub.start_link(redis_url, name: :redix_pubsub) do
      {:ok, _pid} ->
        # Subscribe; messages arrive as handle_info messages keyed on self()
        {:ok, _ref} = Redix.PubSub.subscribe(:redix_pubsub, [@channel], self())
        Logger.info("🌐 MeshConsumer subscribed to Redis channel '#{@channel}'")
        {:ok, %{}}

      {:error, reason} ->
        Logger.warning("🌐 MeshConsumer: Redis unavailable (#{inspect(reason)}). Mesh signals will not be received.")
        # Start anyway — the rest of the app doesn't depend on mesh
        {:ok, %{redis: :unavailable}}
    end
  end

  # Redix PubSub delivers messages as:
  #   {:redix_pubsub, _conn, _ref, :message, channel, payload}
  @impl true
  def handle_info({:redix_pubsub, _conn, _ref, :message, @channel, raw}, state) do
    case Jason.decode(raw) do
      {:ok, event} -> dispatch(event)
      {:error, _}  -> Logger.warning("🌐 MeshConsumer: malformed JSON on ganying")
    end
    {:noreply, state}
  end

  # Subscription confirmation — just log it
  @impl true
  def handle_info({:redix_pubsub, _conn, _ref, :subscribed, @channel, _count}, state) do
    Logger.debug("🌐 MeshConsumer: subscription confirmed for '#{@channel}'")
    {:noreply, state}
  end

  # Catch-all for unexpected messages
  @impl true
  def handle_info(msg, state) do
    Logger.debug("🌐 MeshConsumer: unexpected message #{inspect(msg)}")
    {:noreply, state}
  end

  # ---------------------------------------------------------------------------
  # Dispatch logic
  # ---------------------------------------------------------------------------

  defp dispatch(%{"event_type" => "MESH_SIGNAL", "data" => data} = event) do
    importance = data["importance"] || 0.0
    signal_id  = data["signal_id"]  || "unknown"

    Logger.info("🌐 MeshConsumer: signal #{signal_id} (importance=#{importance})")

    # High-importance signals nudge the DreamCycle into an early consolidation
    if importance >= @importance_threshold do
      Logger.info("🌐 High-importance mesh signal — nudging DreamCycle")
      nudge_dream_cycle(event)
    end
  end

  # Ignore any event_type we don't understand yet
  defp dispatch(event) do
    Logger.debug("🌐 MeshConsumer: unhandled event_type #{inspect(event["event_type"])}")
  end

  # Send a one-off :mesh_nudge to the DreamCycle process.
  # DreamCycle handles it gracefully even if a full consolidation is already
  # in progress (the Brain call will just queue behind the existing one).
  defp nudge_dream_cycle(signal) do
    case GenServer.call(WhitemagicCore.DreamCycle, {:mesh_nudge, signal}, 5_000) do
      :ok -> :ok
      _   -> Logger.debug("🌐 DreamCycle nudge returned non-ok (likely busy)")
    end
  rescue
    _ -> Logger.debug("🌐 DreamCycle not ready for nudge yet")
  end
end
