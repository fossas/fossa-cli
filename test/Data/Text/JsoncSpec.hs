{-# LANGUAGE QuasiQuotes #-}

module Data.Text.JsoncSpec (
  spec,
) where

import Data.Aeson (eitherDecodeStrict)
import Data.Aeson qualified as Aeson
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import Data.Text.Jsonc (stripJsonc)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "stripJsonc" $ do
  describe "stripping" $ do
    it "strips single-line comments" $
      stripJsonc
        [r|{
// comment
"a": 1
}|]
        `shouldBeRight` [r|{
"a": 1
}|]

    it "strips line comments at end of file" $
      stripJsonc
        [r|{"a": 1}
// trailing|]
        `shouldBeRight` [r|{"a": 1}
|]

    it "strips block comments" $
      stripJsonc [r|{"a": /* inline */ 1}|]
        `shouldBeRight` [r|{"a":  1}|]

    it "strips multi-line block comments" $
      stripJsonc
        [r|{
/* line 1
   line 2 */
"a": 1
}|]
        `shouldBeRight` [r|{

"a": 1
}|]

    it "strips trailing commas before }" $
      stripJsonc [r|{"a": 1,}|]
        `shouldBeRight` [r|{"a": 1}|]

    it "strips trailing commas before ]" $
      stripJsonc [r|[1, 2,]|]
        `shouldBeRight` [r|[1, 2]|]

    -- Uses escaped strings because trailing spaces are invisible in raw strings.
    it "strips trailing commas with whitespace" $
      stripJsonc "{\"a\": 1 , \n}"
        `shouldBeRight` "{\"a\": 1  \n}"

    it "preserves commas between elements" $
      stripJsonc [r|{"a": 1, "b": 2}|]
        `shouldBeRight` [r|{"a": 1, "b": 2}|]

    it "preserves // inside strings" $
      stripJsonc [r|{"url": "https://example.com"}|]
        `shouldBeRight` [r|{"url": "https://example.com"}|]

    it "preserves commas inside strings" $
      stripJsonc [r|{"msg": "hello, world"}|]
        `shouldBeRight` [r|{"msg": "hello, world"}|]

    it "preserves escaped quotes inside strings" $
      stripJsonc [r|{"msg": "say \"hello\""}|]
        `shouldBeRight` [r|{"msg": "say \"hello\""}|]

    it "preserves /* */ inside strings" $
      stripJsonc [r|{"a": "/* not a comment */"}|]
        `shouldBeRight` [r|{"a": "/* not a comment */"}|]

    it "preserves trailing comma patterns inside strings" $
      stripJsonc [r|{"a": "value,}"}|]
        `shouldBeRight` [r|{"a": "value,}"}|]

    it "handles comments and trailing commas together" $
      stripJsonc
        [r|{
// comment
"a": 1,
"b": 2,
}|]
        `shouldBeRight` [r|{
"a": 1,
"b": 2
}|]

  describe "aeson round-trip" $ do
    it "parses JSONC with comments and trailing commas" $
      shouldRoundTrip
        [r|{
// comment
"name": "test",
"version": 1,
/* block */
"items": [1, 2, 3,],
}|]
        [r|{"name": "test", "version": 1, "items": [1, 2, 3]}|]

    it "parses JSONC with comment-like strings" $
      shouldRoundTrip
        [r|{"url": "https://example.com/path", "pattern": "/* glob */",}|]
        [r|{"url": "https://example.com/path", "pattern": "/* glob */"}|]

shouldRoundTrip :: Text -> Text -> Expectation
shouldRoundTrip input expectedJson =
  case stripJsonc input of
    Left err -> expectationFailure $ "stripJsonc failed: " ++ err
    Right stripped -> do
      let actual = eitherDecodeStrict @Aeson.Value (encodeUtf8 stripped)
      let expected = eitherDecodeStrict @Aeson.Value (encodeUtf8 expectedJson)
      case (actual, expected) of
        (Right a, Right e) -> a `shouldBe` e
        (Left err, _) -> expectationFailure $ "Failed to parse stripped output: " ++ err
        (_, Left err) -> expectationFailure $ "Failed to parse expected JSON: " ++ err

shouldBeRight :: (Show a, Show b, Eq b) => Either a b -> b -> Expectation
shouldBeRight (Left err) _ = expectationFailure $ "Expected Right, got Left: " ++ show err
shouldBeRight (Right actual) expected
  | actual == expected = pure ()
  | otherwise = expectationFailure $ "Expected: " ++ show expected ++ "\nGot: " ++ show actual
