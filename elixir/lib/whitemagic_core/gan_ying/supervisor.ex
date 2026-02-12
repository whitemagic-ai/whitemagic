defmodule WhitemagicCore.GanYing.Supervisor do
  use Supervisor

  @moduledoc """
  Supervises the Gan Ying subsystem: EventBus + DreamScheduler.

  Restart strategy: :rest_for_one — if the EventBus crashes, the
  DreamScheduler (which depends on it) is also restarted.
  """

  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      WhitemagicCore.GanYing.EventBus,
      WhitemagicCore.GanYing.DreamScheduler
    ]

    Supervisor.init(children, strategy: :rest_for_one)
  end
end
