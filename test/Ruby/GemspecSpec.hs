{-# LANGUAGE TemplateHaskell #-}

module Ruby.GemspecSpec (spec) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Data.Char (isSeparator)
import Data.Foldable (for_)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Bundler (BundlerProject (..), findLicenses)
import Strategy.Ruby.Gemspec (Assignment (Assignment), parseRubyArray, parseRubyAssignment, parseRubyWordsArray, rubyString)
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

mkLicensesResult :: Path Abs File -> [LicenseResult]
mkLicensesResult specPath =
  [ LicenseResult
      (toFilePath specPath)
      [ License UnknownType "AGPL"
      , License UnknownType "BSD"
      , License UnknownType "MIT"
      , License UnknownType "Apache"
      ]
  ]

wordLicensesResult :: Path Abs File -> [LicenseResult]
wordLicensesResult specPath =
  [ LicenseResult
      (toFilePath specPath)
      [ License UnknownType "AGPL"
      , License UnknownType "BSD"
      , License UnknownType "MIT"
      ]
  ]

multiLicenseResult :: Path Abs File -> Path Abs File -> [LicenseResult]
multiLicenseResult specPath1 specPath2 =
  [ LicenseResult
      (toFilePath specPath1)
      [License UnknownType "Ruby"]
  , LicenseResult
      (toFilePath specPath2)
      [ License UnknownType "AGPL"
      , License UnknownType "BSD"
      , License UnknownType "MIT"
      , License UnknownType "Apache"
      ]
  ]

delimiterPairs :: [(Text, Text)]
delimiterPairs =
  [ ("'", "'")
  , ("\'", "\'")
  , ("%~", "~")
  , ("%^", "^")
  , ("%q*", "*")
  , ("%Q#", "#")
  , ("%q{", "}")
  , ("%Q<", ">")
  ]

stringParseSpec :: Spec
stringParseSpec =
  for_ delimiterPairs $ \(d1, d2) ->
    describe "Ruby String parsing test" $ do
      let baseStr = d1 <> "Hello" <> d2
      it ("Parses string enclosed in " <> toString d1 <> toString d2) $
        strParse `shouldParse` baseStr `to` "Hello"
      it "Consumes a '.freeze' on the end of a string" $ do
        strParse `shouldParse` (baseStr <> ".freeze") `to` "Hello"
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

specDir :: Path Rel Dir
specDir = $(mkRelDir "test/Ruby/testdata/gemspecs")

singleLicenseSpecPath :: Path Rel File
singleLicenseSpecPath = specDir </> $(mkRelFile "single_license.gemspec")

licensesSpecPath :: Path Rel File
licensesSpecPath = specDir </> $(mkRelFile "licenses.gemspec")

wordArraySpecPath :: Path Rel File
wordArraySpecPath = specDir </> $(mkRelFile "licenses_word_array.gemspec")

gemspecLicenseAnalyzeSpec :: Spec
gemspecLicenseAnalyzeSpec =
  describe "License analysis from gemspec files" $ do
    currDir <- runIO getCurrentDir
    it' "Can extract licenses from the 'license' key out of a more complicated gemspec" $ do
      let specPath = currDir </> singleLicenseSpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` [LicenseResult (toFilePath specPath) [License UnknownType "Ruby"]]
    it' "Can extract licenses from the 'licenses' key" $ do
      let specPath = currDir </> licensesSpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` mkLicensesResult specPath
    it' "Can extract licenses from the 'licenses' key with a word array" $ do
      let specPath = currDir </> wordArraySpecPath
      licenses <- findLicenses specPath
      licenses `shouldBe'` wordLicensesResult specPath

gemspecProjectLicenseScanningSpec :: Spec
gemspecProjectLicenseScanningSpec = do
  currDir <- runIO getCurrentDir
  let specPath1 = currDir </> singleLicenseSpecPath
      specPath2 = currDir </> licensesSpecPath
  describe "Project license discovery from a project" $
    it' "Discovers licenses from a project with multiple gemspec files" $ do
      let proj =
            BundlerProject
              { bundlerGemfile = currDir </> $(mkRelFile "not-tested")
              , bundlerGemfileLock = Nothing
              , bundlerDir = currDir </> specDir
              , bundlerGemSpec =
                  [ specPath1
                  , specPath2
                  ]
              }
      licenses <- licenseAnalyzeProject proj
      licenses `shouldBe'` multiLicenseResult specPath1 specPath2

spec :: Spec
spec = context "Ruby GemSpec tests" $ do
  stringParseSpec
  assignmentParseSpec
  arrayParseSpec
  gemspecLicenseAnalyzeSpec
  gemspecProjectLicenseScanningSpec
