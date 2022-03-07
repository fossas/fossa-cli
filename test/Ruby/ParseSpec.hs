module Ruby.ParseSpec (spec) where

import Data.Char (isSeparator)
import Data.Foldable (for_)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Strategy.Ruby.Parse (Assignment (Assignment), Symbol (Symbol), parseRubyArray, parseRubyAssignment, parseRubyDict, parseRubySymbol, parseRubyWordsArray, rubyString)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, Parsec, runParser, takeWhile1P)

-- I'm not sure about these helpers. Get guidance on whether they help readability.
shouldParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
shouldParse parser = runParser parser ""

to :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> IO ()
to parsed expected = either (fail . show) (`shouldBe` expected) parsed

mitLicense :: Assignment Text
mitLicense = Assignment "s.license" "MIT"

delimiterPairs :: [(Text, Text, Text)]
delimiterPairs =
  [ ("", "'", "'")
  , ("", "\"", "\"")
  , ("%", "~", "~")
  , ("%", "^", "^")
  , ("%q", "*", "*")
  , ("%Q", "#", "#")
  , ("%q", "{", "}")
  , ("%Q", "<", ">")
  ]

stringParseSpec :: Spec
stringParseSpec =
  describe "Ruby String parsing test" $ do
    for_ delimiterPairs $ \(prefix, d1, d2) -> do
      let baseStr = prefix <> d1 <> "Hello" <> d2
      it ("Parses string enclosed in " <> toString d1 <> toString d2) $
        strParse `shouldParse` baseStr `to` "Hello"
      it "Consumes a '.freeze' on the end of a string" $ do
        strParse `shouldParse` (baseStr <> ".freeze") `to` "Hello"
      it "Consumes a '.freeze()' on the end of a string" $ do
        strParse `shouldParse` (baseStr <> ".freeze()") `to` "Hello"
      it "Respects escaped delimiters" $ do
        let expected = "\\" <> d1 <> "Hello" <> "\\" <> d2 <> " world"
            escapedText = ("\"\\" <> d1 <> "Hello\\" <> d2 <> " world\"")
        strParse `shouldParse` escapedText `to` expected
  where
    strParse = rubyString <* eof -- make sure it consumes all input

rubyStringArray :: Text
rubyStringArray = "[ \"hello\",\"world\" ,\t\"foo\",\"bar\"]"

rubyWordArray :: Text
rubyWordArray = "%w( hello world \t foo \nbar)"

expectedArray :: [Text]
expectedArray = ["hello", "world", "foo", "bar"]

escapedRubyWordArray :: Text
escapedRubyWordArray = "%w[ [hello\\] world \t foo \nbar]"

escapedExpectedArray :: [Text]
escapedExpectedArray = ["[hello\\]", "world", "foo", "bar"]

arrayParseSpec :: Spec
arrayParseSpec =
  describe "Parsing arrays of items in ruby" $ do
    it "Can parse an array of strings" $
      parseRubyArray rubyString `shouldParse` rubyStringArray `to` expectedArray
    it "Can parse an array of words" $
      parseRubyWordsArray `shouldParse` rubyWordArray `to` expectedArray
    it "Can parse an array of words with escaped ending delimiter" $
      parseRubyWordsArray `shouldParse` escapedRubyWordArray `to` escapedExpectedArray

symbolParseSpec :: Spec
symbolParseSpec =
  describe "Parsing ruby symbols" $ do
    it "Can parse a symbol " $
      parseRubySymbol `shouldParse` ":he1l_o" `to` Symbol "he1l_o"
    it "Can parse a symbol made from a string literal with '\"'" $
      parseRubySymbol `shouldParse` ":\"f\\\"o o\"" `to` Symbol "f\\\"o o"
    it "Can parse a symbol made from a string literal with \"'\"" $
      parseRubySymbol `shouldParse` ":\'f\\'o o\'" `to` Symbol "f\\'o o"

assignmentParseSpec :: Spec
assignmentParseSpec =
  describe "Ruby assignment parsing test" $ do
    let parseAssignmentAnyRHS = parseRubyAssignment $ takeWhile1P Nothing (not . isSeparator)
    it "Parses an assignment" $
      parseAssignmentAnyRHS `shouldParse` "foo= bar" `to` Assignment "foo" "bar"
    it "Parses an assignment, no spaces" $
      parseAssignmentAnyRHS `shouldParse` "foo=bar" `to` Assignment "foo" "bar"

    for_ ["'", "\""] $ \delim ->
      context ("It parses strings with delimiter: " <> toString delim) $ do
        let mitStr = delim <> "MIT" <> delim
        let licenseStr = ("s.license \t =   " <> mitStr)
        it "Parses a basic string assignment" $
          parseRubyAssignment rubyString `shouldParse` licenseStr `to` mitLicense
        it "Parses a string with '.freeze' on the end" $
          parseRubyAssignment rubyString `shouldParse` (licenseStr <> ".freeze") `to` mitLicense

-- TODO: Parse when there isn't a space between the fat arrow and the key/value
multiKeyDict :: Text
multiKeyDict = "{ :key => \"val\", :\"key\" => \"hello\"}"

expectedMultiKeyDict :: [(Symbol, Text)]
expectedMultiKeyDict =
  [ (Symbol "key", "val")
  , (Symbol "key", "hello")
  ]

dictLiteralParseSpec :: Spec
dictLiteralParseSpec =
  describe "Ruby dictionary literal parse spec" $ do
    it "Parses a dictionary with a single item" $
      parseRubyDict rubyString `shouldParse` "{ :key => \"val\"}" `to` [(Symbol "key", "val")]
    it "Parses a dictionary with a several items" $
      parseRubyDict rubyString `shouldParse` multiKeyDict `to` expectedMultiKeyDict

spec :: Spec
spec = context "Ruby GemSpec tests" $ do
  stringParseSpec
  assignmentParseSpec
  arrayParseSpec
  symbolParseSpec
  dictLiteralParseSpec
