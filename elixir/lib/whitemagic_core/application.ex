defmodule WhitemagicCore.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    redis_url = System.get_env("REDIS_URL", "redis://localhost:6379/0")

    children = [
      WhitemagicCore.Repo,
      # WhitemagicCore.Librarian,
      # Redix connection pool for outbound Redis publishes (Ingestor broadcasts)
      {Redix, {redis_url, [name: :redix]}},
      WhitemagicCore.Brain,
      WhitemagicCore.Zodiac.Heartbeat,
      WhitemagicCore.DreamCycle,
      # Gan Ying event bus + 5-phase dream scheduler (v0.2)
      WhitemagicCore.GanYing.Supervisor,
      # Mesh signal consumer — subscribes to "ganying" and nudges DreamCycle
      WhitemagicCore.MeshConsumer
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: WhitemagicCore.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
