module Go.GopkgLockSpec (
  spec,
) where

import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Go.GopkgLock
import Strategy.Go.Types (graphingGolang)
import Test.Hspec
import Toml qualified

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
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "repo/name/A"
      , dependencyVersion = Just (CEq "3012a1dbe2e4bd1391d42b32f0577cb7bbc7f005")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "repo/name/B"
      , dependencyVersion = Just (CEq "12345")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "repo/name/C"
      , dependencyVersion = Just (CEq "12345")
      , dependencyLocations = ["https://someotherlocation/"]
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  contents <- runIO (TIO.readFile "test/Go/testdata/Gopkg.lock")

  describe "analyze" $
    it "should produce expected output" $ do
      case Toml.decode contents of
        Toml.Failure err -> expectationFailure ("decode failed: " <> show err)
        Toml.Success warnings golock -> do
          let result = buildGraph (lockProjects golock) & graphingGolang & run
          result `shouldBe` expected
          warnings
            `shouldBe` [ "5:3: unexpected key: digest in projects[0]"
                       , "7:3: unexpected key: packages in projects[0]"
                       , "8:3: unexpected key: pruneopts in projects[0]"
                       , "10:3: unexpected key: version in projects[0]"
                       , "13:3: unexpected key: digest in projects[1]"
                       , "15:3: unexpected key: packages in projects[1]"
                       , "23:3: unexpected key: pruneopts in projects[1]"
                       , "25:3: unexpected key: version in projects[1]"
                       , "29:3: unexpected key: branch in projects[2]"
                       , "30:3: unexpected key: digest in projects[2]"
                       , "32:3: unexpected key: packages in projects[2]"
                       , "33:3: unexpected key: pruneopts in projects[2]"
                       ]

  describe "buildGraph" $
    it "should produce expected output" $ do
      let result = buildGraph projects & graphingGolang & run

      result `shouldBe` expected
