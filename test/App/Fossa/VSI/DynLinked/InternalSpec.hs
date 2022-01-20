{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.DynLinked.InternalSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal (LocalDependency (..), parseLocalDependencies)
import Data.Text (Text)
import Data.Void (Void)
import Path (mkAbsFile)
import Test.Hspec (Expectation, Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseInto :: Text -> [LocalDependency] -> Expectation
shouldParseInto = parseMatch parseLocalDependencies

spec :: Spec
spec = do
  describe "parse ldd output" $ do
    it "should parse single line" $ do
      singleLine `shouldParseInto` singleLineExpected
      singleLineMoreSpaces `shouldParseInto` singleLineExpected

    it "should parse multiple lines" $ do
      multipleLine `shouldParseInto` multipleLineExpected

    it "should parse multiple lines while ignoring linker" $ do
      multipleLineSystemPresent `shouldParseInto` multipleLineSystemPresentExpected

singleLine :: Text
singleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

singleLineMoreSpaces :: Text
singleLineMoreSpaces = "\t\tlibc.so.6\t\t=>\t\t/lib/x86_64-linux-gnu/libc.so.6\t\t(0x00007fbea9a88000)\t\t"

singleLineExpected :: [LocalDependency]
singleLineExpected = [LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

multipleLine :: Text
multipleLine =
  "  libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\
  \  libc2.so.6 => /lib/x86_64-linux-gnu/libc2.so.6 (0x00007fbea9a88000)"

multipleLineExpected :: [LocalDependency]
multipleLineExpected =
  [ LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")
  , LocalDependency "libc2.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc2.so.6")
  ]

multipleLineSystemPresent :: Text
multipleLineSystemPresent =
  "  linux-vdso.so.1 =>  (0x00007ffc28d59000)\
  \  libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\
  \  /lib64/ld-linux-x86-64.so.2 (0x00007fbea9e52000)"

multipleLineSystemPresentExpected :: [LocalDependency]
multipleLineSystemPresentExpected = [LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]
