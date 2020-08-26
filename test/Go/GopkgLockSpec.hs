{-# LANGUAGE TemplateHaskell #-}

module Go.GopkgLockSpec
  ( spec
  ) where

import Data.Function ((&))
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Go.GopkgLock
import Strategy.Go.Types (graphingGolang)
import Test.Hspec
import qualified Toml

projects :: [Project]
projects =
  [ Project
      { projectName = "repo/name/A"
      , projectSource = Nothing
      , projectRevision = "3012a1dbe2e4bd1391d42b32f0577cb7bbc7f005"
      }
  , Project
      { projectName = "repo/name/B"
      , projectSource = Nothing
      , projectRevision = "12345"
      }
  , Project
      { projectName = "repo/name/C"
      , projectSource = Just "https://someotherlocation/"
      , projectRevision = "12345"
      }
  ]

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency
             { dependencyType = GoType
             , dependencyName = "repo/name/A"
             , dependencyVersion = Just (CEq "3012a1dbe2e4bd1391d42b32f0577cb7bbc7f005")
             , dependencyLocations = []
             , dependencyEnvironments = []
             , dependencyTags = M.empty
             }
  direct $ Dependency
             { dependencyType = GoType
             , dependencyName = "repo/name/B"
             , dependencyVersion = Just (CEq "12345")
             , dependencyLocations = []
             , dependencyEnvironments = []
             , dependencyTags = M.empty
             }
  direct $ Dependency
             { dependencyType = GoType
             , dependencyName = "repo/name/C"
             , dependencyVersion = Just (CEq "12345")
             , dependencyLocations = ["https://someotherlocation/"]
             , dependencyEnvironments = []
             , dependencyTags = M.empty
             }

spec :: Spec
spec = do
  contents <- runIO (TIO.readFile "test/Go/testdata/Gopkg.lock")

  describe "analyze" $
    it "should produce expected output" $ do
      case Toml.decode golockCodec contents of
        Left err -> expectationFailure ("decode failed: " <> show err)
        Right golock -> do
          let result = buildGraph (lockProjects golock) & graphingGolang & run
          result `shouldBe` expected

  describe "buildGraph" $
    it "should produce expected output" $ do
      let result = buildGraph projects & graphingGolang & run

      result `shouldBe` expected
