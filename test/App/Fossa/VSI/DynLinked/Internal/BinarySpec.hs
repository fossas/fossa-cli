{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

#ifndef mingw32_HOST_OS
{-# LANGUAGE TemplateHaskell #-}
#endif

module App.Fossa.VSI.DynLinked.Internal.BinarySpec (spec) where

import Test.Hspec qualified as Hspec

-- Windows isn't happy with our `/` shaped abs paths, which are built at compile time with TH.
-- Since Windows isn't going to be parsing ldd output anyway, just skip it.
#ifdef mingw32_HOST_OS

spec :: Hspec.Spec
spec = pure ()

#else

import App.Fossa.VSI.DynLinked.Internal.Binary qualified as Binary
import Data.Text (Text)
import Data.Void (Void)
import Path (mkAbsFile)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)
import Data.Maybe (catMaybes)
import Text.RawString.QQ (r)

spec :: Hspec.Spec
spec = do
  Hspec.describe "parse ldd-shaped output" $ do
    Hspec.it "should parse a single line" $ do
      singleLine `shouldParseLineInto` singleLineExpected
      singleLineMoreSpaces `shouldParseLineInto` singleLineExpected

    Hspec.it "should parse output with a single line" $ do
      singleLine `shouldParseOutputInto` catMaybes [singleLineExpected]
      singleLineMoreSpaces `shouldParseOutputInto` catMaybes [singleLineExpected]
      singleLineNotFound `shouldParseOutputInto` []

    Hspec.it "should parse output with multiple lines" $ do
      multipleLine `shouldParseOutputInto` multipleLineExpected

    Hspec.it "should parse output with multiple lines while ignoring things we don't care about" $ do
      syscallPresent `shouldParseOutputInto` syscallPresentExpected
      linkerPresent `shouldParseOutputInto` linkerPresentExpected
      realOutputUbuntu `shouldParseOutputInto` realOutputUbuntuExpected
      realOutputAlpine `shouldParseOutputInto` realOutputAlpineExpected

    Hspec.it "should parse output with multiple lines, ignoring appropriately, with optional => literal" $ do
      syscallPresentVariant `shouldParseOutputInto` syscallPresentExpected
      linkerPresent `shouldParseOutputInto` linkerPresentExpected
      realOutputUbuntu `shouldParseOutputInto` realOutputUbuntuExpected
      realOutputAlpine `shouldParseOutputInto` realOutputAlpineExpected

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Hspec.Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseLineInto :: Text -> (Maybe Binary.LocalDependency) -> Hspec.Expectation
shouldParseLineInto = parseMatch Binary.lddParseDependency

shouldParseOutputInto :: Text -> [Binary.LocalDependency] -> Hspec.Expectation
shouldParseOutputInto = parseMatch Binary.lddParseLocalDependencies

singleLine :: Text
singleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

singleLineNotFound :: Text
singleLineNotFound = "        libprotobuf.so.22 => not found"

singleLineMoreSpaces :: Text
singleLineMoreSpaces = "    libc.so.6   =>    /lib/x86_64-linux-gnu/libc.so.6    (0x00007fbea9a88000)    "

singleLineExpected :: Maybe Binary.LocalDependency
singleLineExpected = Just $ Binary.LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")

multipleLine :: Text
multipleLine =
  [r|  libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)
       libc2.so.6 => /lib/x86_64-linux-gnu/libc2.so.6 (0x00007fbea9a88000)
  |]


multipleLineExpected :: [Binary.LocalDependency]
multipleLineExpected =
  [ Binary.LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")
  , Binary.LocalDependency "libc2.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc2.so.6")
  ]

syscallPresent :: Text
syscallPresent =
  [r|  linux-vdso.so.1 =>  (0x00007ffc28d59000)
       libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)
  |]

syscallPresentVariant :: Text
syscallPresentVariant =
  [r|  linux-vdso.so.1 (0x00007ffc28d59000)
       libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)
  |]

syscallPresentExpected :: [Binary.LocalDependency]
syscallPresentExpected = [Binary.LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

linkerPresent :: Text
linkerPresent =
  [r|  /lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)
       libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)
  |]

linkerPresentExpected :: [Binary.LocalDependency]
linkerPresentExpected = [Binary.LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

realOutputUbuntu :: Text
realOutputUbuntu =
  [r|  linux-vdso.so.1 =>  (0x00007ffc28d59000)
       libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)
       /lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)
  |]


realOutputAlpine :: Text
realOutputAlpine =
  [r|  /lib/ld-musl-x86_64.so.1 (0x7f2ca52da000)
       libc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7f2ca52da000)
  |]

realOutputUbuntuExpected :: [Binary.LocalDependency]
realOutputUbuntuExpected = [Binary.LocalDependency "libc.so.6" $(mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

realOutputAlpineExpected :: [Binary.LocalDependency]
realOutputAlpineExpected = [Binary.LocalDependency "libc.musl-x86_64.so.1" $(mkAbsFile "/lib/ld-musl-x86_64.so.1")]

#endif
