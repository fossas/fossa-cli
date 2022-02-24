{-# LANGUAGE TemplateHaskell #-}

module Ruby.GemspecSpec (spec) where

import Data.Char (isSeparator)
import Data.Foldable (for_)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Path (mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Ruby.Gemspec (Assignment (Assignment), parseRubyAssignment, readAssignments, rubyString)
import Test.Hspec (Spec, context, describe, it, runIO, shouldBe)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, takeWhile1P)

-- I'm not sure about these helpers. Get guidance on whether they help readability.
shouldParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
shouldParse parser = runParser parser ""

to :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> IO ()
to parsed expected = either (fail . show) (`shouldBe` expected) parsed

mitLicense :: Assignment Text
mitLicense = Assignment "s.license" "MIT"

stringAssignmentResult :: [Assignment Text]
stringAssignmentResult =
  [ Assignment "s.name" "bar"
  , mitLicense
  ]

stringParseSpec :: Spec
stringParseSpec =
  describe "Ruby string parsing test" $ do
    it "Parses string enclosed in \"" $
      rubyString `shouldParse` "\"Hello\"" `to` "Hello"
    it "Parses string enclosed in \'" $
      rubyString `shouldParse` "\'Hello\'" `to` "Hello"

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
        it "Parses a basic string assignment" $
          parseRubyAssignment rubyString `shouldParse` ("s.license \t =   " <> mitStr) `to` mitLicense

gemspecFileParseSpec :: Spec
gemspecFileParseSpec = do
  currDir <- runIO getCurrentDir
  simpleSpec <- runIO . Text.readFile . toFilePath $ (currDir </> $(mkRelFile "test/Ruby/testdata/simple_spec.gemspec"))
  describe "Reading .gemspecs" $
    it "Reads string assignments out of a full gemspec file" $
      readAssignments rubyString `shouldParse` simpleSpec `to` stringAssignmentResult

spec :: Spec
spec = context "Ruby GemSpec tests" $ do
  stringParseSpec
  assignmentParseSpec
  gemspecFileParseSpec
