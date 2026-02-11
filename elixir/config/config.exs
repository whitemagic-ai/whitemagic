import Config

# Path configuration
# Override with environment variables: WM_STATE_ROOT, WM_MEMORY_ROOT, WM_WISDOM_PATH
# These are computed at runtime if not set here:
# config :whitemagic_core,
#   memory_root: "/custom/path/to/memory",
#   wisdom_path: "/custom/path/to/wisdom.json"

config :whitemagic_core, WhitemagicCore.Repo,
  database: "whitemagic.db",
  pool_size: 5,
  stacktrace: true,
  show_sensitive_data_on_connection_error: true

config :logger, level: :info
