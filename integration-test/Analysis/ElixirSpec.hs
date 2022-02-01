{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.ElixirSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import Effect.Exec (AllowErr (Never), Command (Command))
import Path
import Strategy.Mix qualified as Mix
import Test.Hspec
import Types (DiscoveredProjectType (..), GraphBreadth (Complete))

elixirEnv :: FixtureEnvironment
elixirEnv = NixEnv ["elixir"]

mixBuildProjectCmd :: Command
mixBuildProjectCmd = Command "mix" ["deps.get", "&&", "mix deps.compile"] Never

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

spec :: Spec
spec = do
  testSuiteDepResultSummary absinthe MixProjectType (DependencyResultsSummary 4 4 1 1 Complete)
