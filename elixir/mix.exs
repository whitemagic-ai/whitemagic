defmodule WhitemagicCore.MixProject do
  use Mix.Project

  def project do
    [
      app: :whitemagic_core,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {WhitemagicCore.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ecto_sql, "~> 3.10"},
      {:ecto_sqlite3, "~> 0.22"},
      {:jason, "~> 1.4"},
      {:file_system, "~> 1.0"},
      {:yaml_elixir, "~> 2.9"},
      {:redix, "~> 1.0"}
    ]
  end
end
