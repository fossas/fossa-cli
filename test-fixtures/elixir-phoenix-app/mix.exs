defmodule PhoenixApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :phoenix_app,
      version: "0.1.0",
      elixir: "~> 1.15",
      deps: deps()
    ]
  end

  defp deps do
    [
      {:phoenix, "~> 1.7"},
      {:bcrypt_elixir, "~> 3.1"},
      {:argon2_elixir, "~> 4.0"},
      {:pbkdf2_elixir, "~> 2.2"},
      {:comeonin, "~> 5.4"},
      {:jason, "~> 1.4"}
    ]
  end
end
