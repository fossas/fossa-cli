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
import Data.Maybe (catMaybes)

spec :: Hspec.Spec
spec = do
  Hspec.describe "parse ldd output" $ do
    Hspec.it "should parse a single line" $ do
      singleLine `shouldParseLineInto` singleLineExpected
      singleLineMoreSpaces `shouldParseLineInto` singleLineExpected

    Hspec.it "should parse output with a single line" $ do
      singleLine `shouldParseOutputInto` catMaybes [singleLineExpected]
      singleLineMoreSpaces `shouldParseOutputInto` catMaybes [singleLineExpected]

    Hspec.it "should parse output with multiple lines" $ do
      multipleLine `shouldParseOutputInto` multipleLineExpected

    Hspec.it "should parse output with multiple lines while ignoring things we don't care about" $ do
      syscallPresent `shouldParseOutputInto` syscallPresentExpected
      linkerPresent `shouldParseOutputInto` linkerPresentExpected
      syscallAndLinkerPresent `shouldParseOutputInto` syscallAndLinkerPresentExpected

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Hspec.Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseLineInto :: Text -> (Maybe LocalDependency) -> Hspec.Expectation
shouldParseLineInto = parseMatch parseLine

shouldParseOutputInto :: Text -> [LocalDependency] -> Hspec.Expectation
shouldParseOutputInto = parseMatch parseLocalDependencies

singleLine :: Text
singleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

singleLineMoreSpaces :: Text
singleLineMoreSpaces = "\t\tlibc.so.6\t\t=>\t\t/lib/x86_64-linux-gnu/libc.so.6\t\t(0x00007fbea9a88000)\t\t"

singleLineExpected :: Maybe LocalDependency
singleLineExpected = Just $ LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")

multipleLine :: Text
multipleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\n\tlibc2.so.6 => /lib/x86_64-linux-gnu/libc2.so.6 (0x00007fbea9a88000)"

multipleLineExpected :: [LocalDependency]
multipleLineExpected =
  [ LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")
  , LocalDependency "libc2.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc2.so.6")
  ]

syscallPresent :: Text
syscallPresent = "linux-vdso.so.1 =>  (0x00007ffc28d59000)\n\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

syscallPresentExpected :: [LocalDependency]
syscallPresentExpected = [LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

linkerPresent :: Text
linkerPresent = "/lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)\n\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

linkerPresentExpected :: [LocalDependency]
linkerPresentExpected = [LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

syscallAndLinkerPresent :: Text
syscallAndLinkerPresent = "linux-vdso.so.1 =>  (0x00007ffc28d59000)\n\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\n\t/lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)"

syscallAndLinkerPresentExpected :: [LocalDependency]
syscallAndLinkerPresentExpected = [LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]


#endif
