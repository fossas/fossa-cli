module Conan.VersionSpec (spec) where

import Data.SemVer (version)
import Data.Text (Text)
import Data.Void (Void)
import Strategy.Conan.Version (conanVersion)
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  it,
 )
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (
  Parsec,
  parse,
 )

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

spec :: Spec
spec = do
  describe "parse version" $ do
    let shouldParseInto = parseMatch conanVersion

    it "should parse version" $ do
      "Conan version 2.0.0-dev" `shouldParseInto` (version 2 0 0 [] [])
      "Conan version 2.0.0" `shouldParseInto` (version 2 0 0 [] [])
      "Conan version 1.0.0" `shouldParseInto` (version 1 0 0 [] [])
      "Conan version 1.63.0" `shouldParseInto` (version 1 63 0 [] [])
