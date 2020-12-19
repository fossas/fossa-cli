module Go.GlideLockSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Data.Yaml

import Control.Algebra
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Go.GlideLock

import Test.Hspec

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency
               { dependencyType = GoType
               , dependencyName = "github.com/pkg/one"
               , dependencyVersion = Just (CEq "100")
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }
  direct $ Dependency
               { dependencyType = GoType
               , dependencyName = "github.com/pkg/three/v3"
               , dependencyVersion = Just (CEq "300")
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

glideLockfile :: GlideLockfile
glideLockfile = 
  GlideLockfile { hash = "123"
  , updated = "now"
  , imports = 
    [ GlideDep 
        { depName = "github.com/pkg/one"
        , depVersion = 100
        , depRepo = Just "testRepo"
    }
    , GlideDep 
        { depName = "github.com/pkg/three/v3"
        , depVersion = 300
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
        Left _ -> expectationFailure "failed to parse"
