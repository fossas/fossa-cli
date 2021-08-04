module Go.GlideLockSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Yaml

import Control.Algebra
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Go.GlideLock

import Test.Hspec

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "github.com/pkg/one"
      , dependencyVersion = Just (CEq "1234")
      , dependencyLocations = []
      , dependencyEnvironments = []
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = GoType
      , dependencyName = "github.com/pkg/three/v3"
      , dependencyVersion = Just (CEq "4bd8")
      , dependencyLocations = []
      , dependencyEnvironments = []
      , dependencyTags = Map.empty
      }

glideLockfile :: GlideLockfile
glideLockfile =
  GlideLockfile
    { hash = "123"
    , updated = "now"
    , imports =
        [ GlideDep
            { depName = "github.com/pkg/one"
            , depVersion = "1234"
            , depRepo = Just "testRepo"
            }
        , GlideDep
            { depName = "github.com/pkg/three/v3"
            , depVersion = "4bd8"
            , depRepo = Just "testRepo"
            }
        ]
    }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/Go/testdata/glide.lock")

  describe "glide lock analyzer" $ do
    it "produces the expected output" $ do
      let result = buildGraph glideLockfile
      result `shouldBe` expected

    it "works end to end" $ do
      case decodeEither' testFile of
        Right res -> buildGraph res `shouldBe` expected
        Left err -> expectationFailure $ "failed to parse: " <> show err
