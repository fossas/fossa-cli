module Go.GopkgTomlSpec (
  spec,
) where

import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Go.GopkgToml
import Strategy.Go.Types (graphingGolang)
import Test.Hspec
import Toml qualified

gopkg :: Gopkg
gopkg =
  Gopkg
    { pkgConstraints =
        [ PkgConstraint
            { constraintName = "cat/fossa"
            , constraintSource = Just "https://someotherlocation/"
            , constraintVersion = Just "v3.0.0"
            , constraintBranch = Nothing
            , constraintRevision = Nothing
            }
        , PkgConstraint
            { constraintName = "repo/name/A"
            , constraintSource = Nothing
            , constraintVersion = Just "v1.0.0"
            , constraintBranch = Nothing
            , constraintRevision = Nothing
            }
        , PkgConstraint
            { constraintName = "repo/name/B"
            , constraintSource = Nothing
            , constraintVersion = Nothing
            , constraintBranch = Nothing
            , constraintRevision = Just "12345"
            }
        , PkgConstraint
            { constraintName = "repo/name/C"
            , constraintSource = Nothing
            , constraintVersion = Nothing
            , constraintBranch = Just "branchname"
            , constraintRevision = Nothing
            }
        ]
    , pkgOverrides =
        [ PkgConstraint
            { constraintName = "repo/name/B"
            , constraintSource = Nothing
            , constraintVersion = Nothing
            , constraintBranch = Just "overridebranch"
            , constraintRevision = Nothing
            }
        ]
    }

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "cat/fossa"
      , dependencyVersion = Just (CEq "v3.0.0")
      , dependencyLocations = ["https://someotherlocation/"]
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "repo/name/A"
      , dependencyVersion = Just (CEq "v1.0.0")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "repo/name/B"
      , dependencyVersion = Just (CEq "overridebranch")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "repo/name/C"
      , dependencyVersion = Just (CEq "branchname")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  contents <- runIO (TIO.readFile "test/Go/testdata/Gopkg.toml")

  describe "analyze" $
    it "should produce expected output" $ do
      case Toml.decode contents of
        Toml.Failure err -> expectationFailure ("decode failed: " <> show err)
        Toml.Success warnings pkg -> do
          let result = buildGraph pkg & graphingGolang & run
          result `shouldBe` expected
          warnings `shouldBe` []

  describe "buildGraph" $
    it "should produce expected output" $ do
      let result = buildGraph gopkg & graphingGolang & run

      result `shouldBe` expected
