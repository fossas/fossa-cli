# Elixir Analysis

When developing in Elixir, [Mix](https://hexdocs.pm/mix/Mix.html) and [Hex](https://hex.pm/) are most commonly used to manage dependencies. 

| Strategy | Direct Deps        | Deep Deps          | Edges              | Container Scanning (experimental) |
| -------- | ------------------ | ------------------ | ------------------ | --------------------------------- |
| mix deps | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |

## Project Discovery

In order to find elixir projects, we look for `mix.exs` file, which specifies the root of an elixir project. When we find an elixir project, we do not descend into any `deps` or `_build` subdirectories to look for more elixir projects.

## Analysis

1. Run `mix deps.tree --format plain --only prod` and generate output similar to:
```
├─ one─1.0.0 (github.com/dep/one)
│  ├─ two─2.0.0 (hex package)
│  │  └─ three─3.0.0 (hex package)
│  └─ four─4.0.0 (github.com/dep/four)
└─ five─5.0.0 (hex package)
```
2. Run `mix deps --all` and generate output similar to:
```
* one 1.0.0 (Hex package) (mix)
  locked at 1.0.0 (one) 3ad58ae7
  ok
* two 1.0.0 (Hex package) (mix)
  locked at 1.0.0 (two) eaf3c2aa
  ok
* three 1.0.0 (Hex package) (mix)
  locked at 1.0.0 (three) ce708e5f
  ok
* four 1.0.0 (Hex package) (mix)
  locked at 1.0.0 (four) 08eb32d6
  ok
* four 4.0.0 (github.com/dep/four) (mix)
  locked at 9554589
  ok
* five 1.0.0 (Hex package) (mix)
  locked at 1.0.0 (five) 4964996d
  ok
  .....
```
3. Parse these outputs to determine the dependency graph and the locations of each dependency. 

## Limitations

* Dependencies from private hex repositories are not supported currently.
* Dependencies must be sourced from git, or from hex.
* Dependencies must have been resolved `mix deps.get` and compiled `mix compile` (or at least `mix deps.compile`) prior to performing the analyses.

## Example 

1. Create a new mix project, using `mix init hello_world`. Now modify `mix.exs` to add dependencies (final file shown below) 

```elixir
defmodule HelloWorld.MixProject do
  use Mix.Project

  def project do
    [
      app: :hello_world,
      version: "0.1.0",
      elixir: "~> 1.12",
      elixirc_options: [warnings_as_errors: false],
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :public_key]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:phoenix, git: "https://github.com/phoenixframework/phoenix", tag: "v1.5.1"},
      {:plug, ">= 0.4.0"},
      {:ecto, "~> 2.0"},
      {:postgrex, ">= 0.8.1 and <3.0.0"},
      {:faker, "~> 0.16.0"},
      {:ex_doc, "~> 0.23"},
      {:jason, "~> 1.0"}
    ]
  end
end
```

You may need to add `config/config.exs` file with following content to ensure phoenix does not throw complier warnings due to missing configs.

```elixir
use Mix.Config

config :phoenix, :json_library, Jason
```

1. Run `mix deps.get` to get and resolve dependencies 
2. Run `mix compile` to compile project - you may see few compiler warnings for deprecations and configs

Corresponding mix.lock file should now be generated in the project directory (example file shown below)
```elixir
%{
  "connection": {:hex, :connection, "1.0.4", "a1cae72211f0eef17705aaededacac3eb30e6625b04a6117c1b2db6ace7d5976", [:mix], [], "hexpm", "4a0850c9be22a43af9920a71ab17c051f5f7d45c209e40269a1938832510e4d9"},
  "db_connection": {:hex, :db_connection, "1.1.3", "89b30ca1ef0a3b469b1c779579590688561d586694a3ce8792985d4d7e575a61", [:mix], [{:connection, "~> 1.0.2", [hex: :connection, repo: "hexpm", optional: false]}, {:poolboy, "~> 1.5", [hex: :poolboy, repo: "hexpm", optional: true]}, {:sbroker, "~> 1.0", [hex: :sbroker, repo: "hexpm", optional: true]}], "hexpm", "5f0a16a58312a610d5eb0b07506280c65f5137868ad479045f2a2dc4ced80550"},
  "decimal": {:hex, :decimal, "1.9.0", "83e8daf59631d632b171faabafb4a9f4242c514b0a06ba3df493951c08f64d07", [:mix], [], "hexpm", "b1f2343568eed6928f3e751cf2dffde95bfaa19dd95d09e8a9ea92ccfd6f7d85"},
  "earmark_parser": {:hex, :earmark_parser, "1.4.13", "0c98163e7d04a15feb62000e1a891489feb29f3d10cb57d4f845c405852bbef8", [:mix], [], "hexpm", "d602c26af3a0af43d2f2645613f65841657ad6efc9f0e361c3b6c06b578214ba"},
  "ecto": {:hex, :ecto, "2.2.12", "ce7f619c1451daad0f59e8f01fd8e2584226171dc273e3346444446a13d93943", [:mix], [{:db_connection, "~> 1.1", [hex: :db_connection, repo: "hexpm", optional: true]}, {:decimal, "~> 1.2", [hex: :decimal, repo: "hexpm", optional: false]}, {:mariaex, "~> 0.8.0", [hex: :mariaex, repo: "hexpm", optional: true]}, {:poison, "~> 2.2 or ~> 3.0", [hex: :poison, repo: "hexpm", optional: true]}, {:poolboy, "~> 1.5", [hex: :poolboy, repo: "hexpm", optional: false]}, {:postgrex, "~> 0.13.0", [hex: :postgrex, repo: "hexpm", optional: true]}, {:sbroker, "~> 1.0", [hex: :sbroker, repo: "hexpm", optional: true]}], "hexpm", "d5d01f3ec33e3853ac8ca80dcaf9b154a348b9eaa70009d2b9ad25c45262fdea"},
  "ex_doc": {:hex, :ex_doc, "0.24.2", "e4c26603830c1a2286dae45f4412a4d1980e1e89dc779fcd0181ed1d5a05c8d9", [:mix], [{:earmark_parser, "~> 1.4.0", [hex: :earmark_parser, repo: "hexpm", optional: false]}, {:makeup_elixir, "~> 0.14", [hex: :makeup_elixir, repo: "hexpm", optional: false]}, {:makeup_erlang, "~> 0.1", [hex: :makeup_erlang, repo: "hexpm", optional: false]}], "hexpm", "e134e1d9e821b8d9e4244687fb2ace58d479b67b282de5158333b0d57c6fb7da"},
  "faker": {:hex, :faker, "0.16.0", "1e2cf3e8d60d44a30741fb98118fcac18b2020379c7e00d18f1a005841b2f647", [:mix], [], "hexpm", "fbcb9bf1299dff3c9dd7e50f41802bbc472ffbb84e7656394c8aa913ec315141"},
  "makeup": {:hex, :makeup, "1.0.5", "d5a830bc42c9800ce07dd97fa94669dfb93d3bf5fcf6ea7a0c67b2e0e4a7f26c", [:mix], [{:nimble_parsec, "~> 0.5 or ~> 1.0", [hex: :nimble_parsec, repo: "hexpm", optional: false]}], "hexpm", "cfa158c02d3f5c0c665d0af11512fed3fba0144cf1aadee0f2ce17747fba2ca9"},
  "makeup_elixir": {:hex, :makeup_elixir, "0.15.1", "b5888c880d17d1cc3e598f05cdb5b5a91b7b17ac4eaf5f297cb697663a1094dd", [:mix], [{:makeup, "~> 1.0", [hex: :makeup, repo: "hexpm", optional: false]}, {:nimble_parsec, "~> 1.1", [hex: :nimble_parsec, repo: "hexpm", optional: false]}], "hexpm", "db68c173234b07ab2a07f645a5acdc117b9f99d69ebf521821d89690ae6c6ec8"},
  "makeup_erlang": {:hex, :makeup_erlang, "0.1.1", "3fcb7f09eb9d98dc4d208f49cc955a34218fc41ff6b84df7c75b3e6e533cc65f", [:mix], [{:makeup, "~> 1.0", [hex: :makeup, repo: "hexpm", optional: false]}], "hexpm", "174d0809e98a4ef0b3309256cbf97101c6ec01c4ab0b23e926a9e17df2077cbb"},
  "mime": {:hex, :mime, "1.6.0", "dabde576a497cef4bbdd60aceee8160e02a6c89250d6c0b29e56c0dfb00db3d2", [:mix], [], "hexpm", "31a1a8613f8321143dde1dafc36006a17d28d02bdfecb9e95a880fa7aabd19a7"},
  "nimble_parsec": {:hex, :nimble_parsec, "1.1.0", "3a6fca1550363552e54c216debb6a9e95bd8d32348938e13de5eda962c0d7f89", [:mix], [], "hexpm", "08eb32d66b706e913ff748f11694b17981c0b04a33ef470e33e11b3d3ac8f54b"},
  "phoenix": {:git, "https://github.com/phoenixframework/phoenix", "cfd5a6e91b0d60c5dc84ad5874506444f5d65251", [tag: "v1.5.8"]},
  "phoenix_pubsub": {:hex, :phoenix_pubsub, "2.0.0", "a1ae76717bb168cdeb10ec9d92d1480fec99e3080f011402c0a2d68d47395ffb", [:mix], [], "hexpm", "c52d948c4f261577b9c6fa804be91884b381a7f8f18450c5045975435350f771"},
  "plug": {:hex, :plug, "1.11.1", "f2992bac66fdae679453c9e86134a4201f6f43a687d8ff1cd1b2862d53c80259", [:mix], [{:mime, "~> 1.0", [hex: :mime, repo: "hexpm", optional: false]}, {:plug_crypto, "~> 1.1.1 or ~> 1.2", [hex: :plug_crypto, repo: "hexpm", optional: false]}, {:telemetry, "~> 0.4", [hex: :telemetry, repo: "hexpm", optional: false]}], "hexpm", "23524e4fefbb587c11f0833b3910bfb414bf2e2534d61928e920f54e3a1b881f"},
  "plug_crypto": {:hex, :plug_crypto, "1.2.2", "05654514ac717ff3a1843204b424477d9e60c143406aa94daf2274fdd280794d", [:mix], [], "hexpm", "87631c7ad914a5a445f0a3809f99b079113ae4ed4b867348dd9eec288cecb6db"},
  "poolboy": {:hex, :poolboy, "1.5.2", "392b007a1693a64540cead79830443abf5762f5d30cf50bc95cb2c1aaafa006b", [:rebar3], [], "hexpm", "dad79704ce5440f3d5a3681c8590b9dc25d1a561e8f5a9c995281012860901e3"},
  "postgrex": {:hex, :postgrex, "0.13.5", "3d931aba29363e1443da167a4b12f06dcd171103c424de15e5f3fc2ba3e6d9c5", [:mix], [{:connection, "~> 1.0", [hex: :connection, repo: "hexpm", optional: false]}, {:db_connection, "~> 1.1", [hex: :db_connection, repo: "hexpm", optional: false]}, {:decimal, "~> 1.0", [hex: :decimal, repo: "hexpm", optional: false]}], "hexpm", "a19b61193379cdee04b5b2361bf93d1eb170cd2eec0b18042617b07e1e15fbfb"},
  "telemetry": {:hex, :telemetry, "0.4.3", "a06428a514bdbc63293cd9a6263aad00ddeb66f608163bdec7c8995784080818", [:rebar3], [], "hexpm", "eb72b8365ffda5bed68a620d1da88525e326cb82a75ee61354fc24b844768041"},
}
```

3. To perform the analyses, run `fossa analyze ./../myElixirProjectDir/` 
4. To perform the analyses and view it's output only, run `fossa analyze ./../myElixirProjectDir/ --output` 

Here is the dependency graph produced from the above project. Dependencies in yellow are direct dependencies. 

![Mix Dependency Graph](mix-resolved-graph.svg)

## F.A.Q

### How do I exclude downloaded rebar3 dependency artifacts from the analyses?

You can explicitly specify analyses target in `.fossa.yml` file. 

Example below, will exclude all analyses targets except mix. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: mix
```

Or, you can exclude all rebar3 targets under specific directory.

```yaml
# .fossa.yml 
# Exclude rebar3 targets found in ./deps/ directory from the analyses. 

version: 3
targets:
  exclude:
    - type: rebar3
      target: ./deps/
```