{-# LANGUAGE QuasiQuotes #-}

module Go.GostdSpec (spec) where

import Control.Algebra (run)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (DepType (GoType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (direct, evalGrapher)
import Graphing (Graphing (..))
import Strategy.Go.Gostd (GoStdlibDep (..), filterGoStdlibPackages, parseGoStdlibPackages)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)
import Text.RawString.QQ (r)

-- Actual test logic.

spec :: Spec
spec = do
  describe "go command interop" $ do
    it "should parse @go list std@" $ do
      goListStdOutputTruncated `shouldParseOutputInto` goStdlibList

  describe "filters graph" $ do
    it "filters stdlib packages" $ do
      filterGoStdlibPackages goStdlibList goProjectGraph `shouldBe` goProjectGraphFilteredExpected

    it "filters additional packages" $ do
      filterGoStdlibPackages goStdlibList goProjectGraphWithC `shouldBe` goProjectGraphFilteredExpected

-- Helpers for building fixtures.

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseOutputInto :: Text -> [GoStdlibDep] -> Expectation
shouldParseOutputInto = parseMatch parseGoStdlibPackages

stddep :: Text -> GoStdlibDep
stddep = GoStdlibDep GoType

dep :: Text -> Text -> Dependency
dep name revision =
  Dependency
    { dependencyType = GoType
    , dependencyName = name
    , dependencyVersion = Just (CEq revision)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

-- Fixtures for testing.

-- | The actual @go list std@ output is quite long, and very repetitive, so it's not worth exactly reproducing.
-- Instead, we keep one example of each "kind" of package:
--
-- * plain (e.g. @bytes@)
-- * nested simple (e.g. @archive/zip@)
-- * nested deep (e.g. @crypto/ed25519/internal/edwards25519@)
-- * addressed (e.g. @vendor/golang.org/x/crypto/chacha20@)
--
-- Future Go versions may add another kind of reported package; if so feel free to add it here.
goListStdOutputTruncated :: Text
goListStdOutputTruncated =
  [r|archive/zip
bytes
crypto/ed25519/internal/edwards25519
vendor/golang.org/x/crypto/chacha20
|]

goStdlibList :: [GoStdlibDep]
goStdlibList =
  [ stddep "archive/zip"
  , stddep "bytes"
  , stddep "crypto/ed25519/internal/edwards25519"
  , stddep "vendor/golang.org/x/crypto/chacha20"
  ]

goProjectGraph :: Graphing Dependency
goProjectGraph = run . evalGrapher $ do
  direct $ dep "github.com/pkg/one" "v1.0.0"
  direct $ dep "github.com/pkg/three/v3" "v3.0.0"
  direct $ dep "archive/zip" ""
  direct $ dep "bytes" ""
  direct $ dep "crypto/ed25519/internal/edwards25519" ""
  direct $ dep "vendor/golang.org/x/crypto/chacha20" ""

goProjectGraphWithC :: Graphing Dependency
goProjectGraphWithC = run . evalGrapher $ do
  direct $ dep "github.com/pkg/one" "v1.0.0"
  direct $ dep "github.com/pkg/three/v3" "v3.0.0"
  direct $ dep "archive/zip" ""
  direct $ dep "bytes" ""
  direct $ dep "crypto/ed25519/internal/edwards25519" ""
  direct $ dep "vendor/golang.org/x/crypto/chacha20" ""
  direct $ dep "C" ""

goProjectGraphFilteredExpected :: Graphing Dependency
goProjectGraphFilteredExpected = run . evalGrapher $ do
  direct $ dep "github.com/pkg/one" "v1.0.0"
  direct $ dep "github.com/pkg/three/v3" "v3.0.0"
