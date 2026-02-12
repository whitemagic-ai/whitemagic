defmodule WhitemagicCore.Repo do
  use Ecto.Repo,
    otp_app: :whitemagic_core,
    adapter: Ecto.Adapters.SQLite3
end
