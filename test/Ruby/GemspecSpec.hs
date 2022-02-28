{-# LANGUAGE TemplateHaskell #-}

module Ruby.GemspecSpec (spec) where

import Data.Char (isSeparator)
import Data.Foldable (for_)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Path (Abs, File, Path, Rel, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Bundler (findLicenses)
import Strategy.Ruby.Gemspec (Assignment (Assignment), parseRubyArray, parseRubyAssignment, parseRubyWordsArray, readAssignments, rubyString)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, context, describe, it, runIO, shouldBe)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, Parsec, runParser, takeWhile1P)
import Types (License (License), LicenseResult (LicenseResult), LicenseType (UnknownType))

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

mkLicensesResult :: Path Abs File -> [LicenseResult]
mkLicensesResult specPath =
  [ LicenseResult
      (toFilePath specPath)
      [ License UnknownType "AGPL"
      , License UnknownType "BSD"
      , License UnknownType "MIT"
      ]
  ]

stringParseSpec :: Spec
stringParseSpec =
  for_ ["'", "\'"] $ \delim ->
    describe "Ruby string parsing test" $ do
      let baseStr = delim <> "Hello" <> delim
      it ("Parses string enclosed in " <> toString delim) $
        strParse `shouldParse` baseStr `to` "Hello"
      it "Consumes a '.freeze' on the end of a string" $ do
        strParse `shouldParse` (baseStr <> ".freeze") `to` "Hello"
  where
    strParse = rubyString <* eof -- make sure it consumes all input

rubyStringArray :: Text
rubyStringArray = "[ \"hello\",\"world\" ,\t\"foo\",\"bar\"]"

rubyWordArray :: Text
rubyWordArray = "%w( hello world \t foo \nbar)"

expectedArray :: [Text]
expectedArray = ["hello", "world", "foo", "bar"]

arrayParseSpec :: Spec
arrayParseSpec =
  describe "Parsing arrays of items in ruby" $ do
    it "Can parse an array of strings" $
      parseRubyArray rubyString `shouldParse` rubyStringArray `to` expectedArray

    it "Can parse an array of words" $
      parseRubyWordsArray `shouldParse` rubyWordArray `to` expectedArray

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

simpleSpecPath :: Path Rel File
simpleSpecPath = $(mkRelFile "test/Ruby/testdata/simple_spec.gemspec")

complexSpecPath :: Path Rel File
complexSpecPath = $(mkRelFile "test/Ruby/testdata/complex.gemspec")

licensesSpecPath :: Path Rel File
licensesSpecPath = $(mkRelFile "test/Ruby/testdata/licenses.gemspec")

wordArraySpecPath :: Path Rel File
wordArraySpecPath = $(mkRelFile "test/Ruby/testdata/licenses_word_array.gemspec")

gemspecFileParseSpec :: Spec
gemspecFileParseSpec = do
  currDir <- runIO getCurrentDir
  simpleSpec <- runIO . Text.readFile . toFilePath $ (currDir </> simpleSpecPath)
  describe "Reading .gemspecs" $
    it "Reads string assignments out of a full gemspec file" $
      readAssignments rubyString `shouldParse` simpleSpec `to` stringAssignmentResult

gemspecLicenseAnalyzeSpec :: Spec
gemspecLicenseAnalyzeSpec =
  describe "License analysis from gemspec files" $ do
    currDir <- runIO getCurrentDir
    it' "Can extract licenses from the 'license' key out of a simple gemspec" $ do
      let specPath = currDir </> simpleSpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` [LicenseResult (toFilePath specPath) [License UnknownType "MIT"]]
    it' "Can extract licenses from the 'license' key out of a more complicated gemspec" $ do
      let specPath = currDir </> complexSpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` [LicenseResult (toFilePath specPath) [License UnknownType "AGPL"]]
    it' "Can extract licenses from the 'licenses' key" $ do
      let specPath = currDir </> licensesSpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` mkLicensesResult specPath
    it' "Can extract licenses from the 'licenses' key with a word array" $ do
      let specPath = currDir </> wordArraySpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` mkLicensesResult specPath

spec :: Spec
spec = context "Ruby GemSpec tests" $ do
  stringParseSpec
  assignmentParseSpec
  arrayParseSpec
  gemspecFileParseSpec
  gemspecLicenseAnalyzeSpec
