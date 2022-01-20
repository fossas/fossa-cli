{-# LANGUAGE CPP #-}

#ifndef mingw32_HOST_OS
{-# LANGUAGE TemplateHaskell #-}
#endif

module App.Fossa.VSI.DynLinked.InternalSpec (spec) where

import Test.Hspec qualified as Hspec

#ifdef mingw32_HOST_OS

spec :: Hspec.Spec
spec = pure ()

#else

import App.Fossa.VSI.DynLinked.Internal (LocalDependency (..), parseLine, parseLocalDependencies)
import Data.Text (Text)
import Data.Void (Void)
import Path (mkAbsFile)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)

spec :: Hspec.Spec
spec = do
  Hspec.describe "parse ldd output" $ do
    Hspec.it "should parse a single line" $ do
      singleLine `shouldParseLineInto` singleLineExpected
      singleLineMoreSpaces `shouldParseLineInto` singleLineExpected

    Hspec.it "should parse output with a single line" $ do
      singleLine `shouldParseOutputInto` [singleLineExpected]
      singleLineMoreSpaces `shouldParseOutputInto` [singleLineExpected]

    Hspec.it "should parse output with multiple lines" $ do
      multipleLine `shouldParseOutputInto` multipleLineExpected

    Hspec.it "should parse output with multiple lines while ignoring system" $ do
      multipleLineSystemPresent `shouldParseOutputInto` multipleLineSystemPresentExpected

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Hspec.Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseLineInto :: Text -> LocalDependency -> Hspec.Expectation
shouldParseLineInto = parseMatch parseLine

shouldParseOutputInto :: Text -> [LocalDependency] -> Hspec.Expectation
shouldParseOutputInto = parseMatch parseLocalDependencies

singleLine :: Text
singleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

singleLineMoreSpaces :: Text
singleLineMoreSpaces = "\t\tlibc.so.6\t\t=>\t\t/lib/x86_64-linux-gnu/libc.so.6\t\t(0x00007fbea9a88000)\t\t"

singleLineExpected :: LocalDependency
singleLineExpected = LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")

multipleLine :: Text
multipleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\n\tlibc2.so.6 => /lib/x86_64-linux-gnu/libc2.so.6 (0x00007fbea9a88000)"

multipleLineExpected :: [LocalDependency]
multipleLineExpected =
  [ LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")
  , LocalDependency "libc2.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc2.so.6")
  ]

multipleLineSystemPresent :: Text
multipleLineSystemPresent = "linux-vdso.so.1 =>  (0x00007ffc28d59000)\n\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\n\t/lib64/ld-linux-x86-64.so.2 (0x00007fbea9e52000)"

multipleLineSystemPresentExpected :: [LocalDependency]
multipleLineSystemPresentExpected = [LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

#endif
