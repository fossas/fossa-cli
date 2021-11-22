{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.MixSpec (spec) where

import Analysis.Utils
import Effect.Exec (AllowErr (Never), Command (..))
import Path
import Strategy.Mix qualified as Mix
import Test.Hspec (Spec)
import Types (GraphBreadth (..))
import Strategy.Elixir.MixTree (MixProject (..))

withElixir :: FixtureEnvironment
withElixir = NixEnvSimpleConfig ["elixir"]

mkAbsintheSuite :: FixtureEnvironment -> AnalysisIntegrationCase MixProject
mkAbsintheSuite env =
  AnalysisIntegrationCase
    { testCategory = AnalysisIntegrationElixir
    , testName = "absinthe"
    , discover = Mix.discover
    , environment = env
    , buildCmd = Just (Command "mix" ["local.hex", "--force", "&&", "mix", "local.rebar", "--force", "&&", "mix", "deps.get", "&&", "mix compile"] Never, [reldir|absinthe/absinthe-1.6.6/|])
    , artifact = FixtureArtifact "https://github.com/absinthe-graphql/absinthe/archive/refs/tags/v1.6.6.tar.gz" [reldir|absinthe/|]
    , assertions =
        [ Assertion "mix" [reldir|absinthe/absinthe-1.6.6/|] (DependencyResultsTabulated 4 4 5 Complete 1)
        ]
    }

spec :: Spec
spec = do
  testSuiteOf . mkAbsintheSuite $ withElixir