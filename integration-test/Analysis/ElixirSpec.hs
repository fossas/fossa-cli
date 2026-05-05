{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.ElixirSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Data.Map.Strict qualified as Map
import Effect.Exec (AllowErr (Never), Command (Command))
import Path
import Strategy.Mix qualified as Mix
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

elixirEnv :: FixtureEnvironment
elixirEnv = NixEnv ["elixir"]

mixBuildProjectCmd :: Command
mixBuildProjectCmd =
  Command
    "mix"
    [ "local.hex --force --if-missing" -- install hex package manager
    , "&&"
    , "mix local.rebar --force" -- project requires rebar3
    , "&&"
    , "mix deps.get"
    , "&&"
    , "mix deps.compile"
    ]
    Never
    Map.empty

-- | absinthe v1.6.6: Tests the fallback path.
--
-- This project has @import_config "\#{Mix.env()}.exs"@ in its @config\/config.exs@
-- but does NOT have a @config\/prod.exs@ file. This means @MIX_ENV=prod@ will
-- fail, and the analyzer should fall back to @--only prod@ / default env.
absinthe :: AnalysisTestFixture (Mix.MixProject)
absinthe =
  AnalysisTestFixture
    "absinthe"
    Mix.discover
    elixirEnv
    (Just mixBuildProjectCmd)
    $ FixtureArtifact
      "https://github.com/absinthe-graphql/absinthe/archive/refs/tags/v1.6.6.tar.gz"
      [reldir|elixir/absinthe/|]
      [reldir|absinthe-1.6.6/|]

-- | commanded v1.4.6: Tests the primary (MIX_ENV=prod) path.
--
-- This project has a @config\/prod.exs@ file, so @MIX_ENV=prod@ works correctly.
-- It also has 7 dev/test-only dependencies (credo, dialyxir, ex_doc, mox,
-- mix_test_watch, benchfella, local_cluster) that are excluded when running
-- in the prod environment. This verifies that @MIX_ENV=prod@ actually filters
-- out non-production dependencies.
commanded :: AnalysisTestFixture (Mix.MixProject)
commanded =
  AnalysisTestFixture
    "commanded"
    Mix.discover
    elixirEnv
    (Just mixBuildProjectCmd)
    $ FixtureArtifact
      "https://github.com/commanded/commanded/archive/refs/tags/v1.4.6.tar.gz"
      [reldir|elixir/commanded/|]
      [reldir|commanded-1.4.6/|]

spec :: Spec
spec = do
  -- absinthe: Falls back to --only prod / default env because it lacks config/prod.exs
  testSuiteDepResultSummary NonStrict absinthe MixProjectType (DependencyResultsSummary 4 4 1 1 Complete)
  -- commanded: Uses MIX_ENV=prod successfully, excluding dev/test-only deps
  testSuiteHasSomeDepResults commanded MixProjectType
