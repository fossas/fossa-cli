module Ruby.GemspecSpec (spec) where

import Data.Char (isSeparator)
import Strategy.Ruby.Gemspec (Assignment (Assignment), parseRubyAssignment, rubyString)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, takeWhile1P)

-- I'm not sure about these helpers. Get guidance on whether they help readability.
shouldParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
shouldParse parser = runParser parser ""

to :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> IO ()
to parsed expected = either (fail . show) (`shouldBe` expected) parsed

stringParseTest :: Spec
stringParseTest =
  describe "Ruby string parsing test" $ do
    it "Parses string enclosed in \"" $
      rubyString `shouldParse` "\"Hello\"" `to` "Hello"
    it "Parses string enclosed in \'" $
      rubyString `shouldParse` "\'Hello\'" `to` "Hello"

assignmentParseTest :: Spec
assignmentParseTest =
  describe "Ruby assignment parsing test" $ do
    let parseAssignmentAnyRHS = parseRubyAssignment $ takeWhile1P Nothing (not . isSeparator)
    it "Parses an assignment" $
      parseAssignmentAnyRHS `shouldParse` "foo= bar" `to` Assignment "foo" "bar"
    it "Parses an assignment, no spaces" $
      parseAssignmentAnyRHS `shouldParse` "foo=bar" `to` Assignment "foo" "bar"
    it "Parses an assignment, leading spaces" $
      parseAssignmentAnyRHS `shouldParse` " foo =bar" `to` Assignment "foo" "bar"
    it "Parses a basic string assignment" $
      parseRubyAssignment rubyString `shouldParse` "s.license =   \"MIT\"" `to` Assignment "s.license" "MIT"
    it "Consumes spaces at the beginning of an assignment" $
      parseRubyAssignment rubyString `shouldParse` " \t s.license =   \"MIT\"" `to` Assignment "s.license" "MIT"

spec :: Spec
spec = context "" $ do
  stringParseTest
  assignmentParseTest
