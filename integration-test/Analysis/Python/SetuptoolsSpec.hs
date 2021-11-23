{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.SetuptoolsSpec (spec) where
import Analysis.Utils
import Path
import Strategy.Python.Setuptools qualified as Setuptools
import Test.Hspec (Spec)
import Types (GraphBreadth (..))

spec :: Spec
spec = do
  testSuiteOf
    AnalysisIntegrationCase
      { testName = "theFuck"
      , discover = Setuptools.discover
      , environment = LocalEnvironment
      , buildCmd = Nothing
      , artifact = FixtureArtifact "https://github.com/nvbn/thefuck/archive/refs/tags/3.31.tar.gz" [reldir|thefuck/|]
      , assertions =
          [ Assertion "setuptools" [reldir|thefuck/thefuck-3.31/|] (DependencyResultsTabulated 16 16 16 Partial 2)
          ]
      }