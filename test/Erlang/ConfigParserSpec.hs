module Erlang.ConfigParserSpec
  ( spec,
  )
where

import qualified Data.Char as C
import Data.Text ( Text )
import qualified Data.Text.IO as TIO
import Data.Void ( Void )
import Strategy.Erlang.ConfigParser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

atom :: Text -> ErlValue
atom = ErlAtom . AtomText

spec :: Spec
spec = do
  describe "Erlang config parser" $ do
    rawText <- runIO $ TIO.readFile "test/Erlang/testdata/rebar.config"
    oneWithEverything <- runIO $ TIO.readFile "test/Erlang/testdata/stresstest.config"
    it "should succeed on a real input file" $
      parse parseConfig "" `shouldSucceedOn` rawText
    
    it "should parse an atom" $ do
      let shouldParseInto input = parseMatch parseAtom input

      "a" `shouldParseInto` atom "a"
      "a_" `shouldParseInto` atom "a_"
      "a@" `shouldParseInto` atom "a@"
      "a9" `shouldParseInto` atom "a9"
      "'a'" `shouldParseInto` atom "a"
      "'a b'" `shouldParseInto` atom "a b"
      "'1two3'" `shouldParseInto` atom "1two3"
      "'A b'" `shouldParseInto` atom "A b"
      --
      parse parseAtom "" `shouldFailOn` "Abc"
      parse parseAtom "" `shouldFailOn` "4five6"
      parse parseAtom "" `shouldFailOn` "_atom"
    
    it "should parse a string" $ do
      let shouldParseInto input = parseMatch parseErlString input

      "\"hello\"" `shouldParseInto` ErlString "hello"
      "\"'a'\"" `shouldParseInto` ErlString "'a'"
      "\"\\n\"" `shouldParseInto` ErlString "\n"
      "\"\\\"\"" `shouldParseInto` ErlString "\"" -- in source, LHS would appear as -> "\""
      "\"a\" \"b\"" `shouldParseInto` ErlString "ab" -- run-on strings -> "a" "b"

    it "should parse an array" $ do
      let shouldParseInto input = parseMatch parseErlArray input

      "[]" `shouldParseInto` ErlArray []
      "[[], []]" `shouldParseInto` ErlArray [ErlArray [], ErlArray []]
      "[a, b, '234']" `shouldParseInto` ErlArray (map atom ["a", "b", "234"])

    it "should parse a tuple" $ do
      let shouldParseInto input = parseMatch parseTuple input

      "{a}" `shouldParseInto` ErlTuple [atom "a"]
      "{a, {b, c}}" `shouldParseInto` ErlTuple [atom "a", ErlTuple [atom "b", atom "c"]]

    it "should parse an integer" $ do
      let shouldParseInto input = parseMatch parseIntLiteral input

      "1" `shouldParseInto` ErlInt 1
      "+56" `shouldParseInto` ErlInt 56
      "-21" `shouldParseInto` ErlInt (-21)

    it "should parse a float" $ do
      let shouldParseInto input = parseMatch parseFloatLiteral input

      "1.0" `shouldParseInto` ErlFloat 1.0
      "0.0" `shouldParseInto` ErlFloat 0.0
      "3.0" `shouldParseInto` ErlFloat 3.0
      "1.23456789101112" `shouldParseInto` ErlFloat 1.23456789101112
      "+1.2" `shouldParseInto` ErlFloat 1.2
      "-3.14" `shouldParseInto` ErlFloat (-3.14)

    it "should parse a numeric char" $ do
      let shouldParseInto input = parseMatch parseCharNum input

      "$A" `shouldParseInto` ErlInt (C.ord 'A')
      "$\\n" `shouldParseInto` ErlInt 10
      "$\\r" `shouldParseInto` ErlInt 13
      -- This one fails for now, doesn't seem very useful
      parse parseCharNum "" `shouldFailOn` "-$A"

    it "should parse a radix str" $ do
      let shouldParseInto input = parseMatch parseRadixLiteral input

      "2#1" `shouldParseInto` ErlInt 1
      "2#11001" `shouldParseInto` ErlInt 25
      "8#1234" `shouldParseInto` ErlInt 0o1234
      "10#1234" `shouldParseInto` ErlInt 1234
      "16#abcd" `shouldParseInto` ErlInt 0xabcd
      "36#1z" `shouldParseInto` ErlInt 71 -- (36 * 2 - 1)
      --
      parse parseRadixLiteral "" `shouldFailOn` "-2#10"

    it "should parse everything at once" $
      parse parseConfig "stresstest.config" oneWithEverything `shouldParse` 
        [ErlTuple [
          atom "rawAtom",
          atom "quotedAtom",
          ErlString "Regular String",
          ErlString "Escaped \" String",
          ErlInt 1234, -- Literal
          ErlFloat 3.14159,
          ErlInt 120, -- '$x'
          ErlInt 35338, -- 'wes' in base 33
          ErlArray [atom "arr1"],
          ErlArray [ErlTuple [atom "key", ErlString "value"]],
          ErlTuple [atom "number", ErlInt 5678] -- Literal
        ]]

  describe "radix parser" $
    it "should parse number strings correctly" $ do
      intLiteralInBase 2 "11001" `shouldBe` 25
      intLiteralInBase 8 "1234" `shouldBe` 0o1234
      intLiteralInBase 10 "1234" `shouldBe` 1234
      intLiteralInBase 16 "abcd" `shouldBe` 0xabcd
      intLiteralInBase 36 "1z" `shouldBe` 71 -- (36 * 2 - 1)

  describe "alphaNumToInt" $
    it "should provide the correct value for chars" $ do
      alphaNumToInt 'a' `shouldBe` 10
      alphaNumToInt 'B' `shouldBe` 11
      alphaNumToInt 'z' `shouldBe` 35
      --
      alphaNumToInt 'W' `shouldBe` 32
      alphaNumToInt 'e' `shouldBe` 14
      alphaNumToInt 's' `shouldBe` 28
