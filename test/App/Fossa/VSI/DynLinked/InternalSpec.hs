{-# LANGUAGE CPP #-}

#ifndef mingw32_HOST_OS
{-# LANGUAGE TemplateHaskell #-}
#endif

module App.Fossa.VSI.DynLinked.InternalSpec (spec) where

import Test.Hspec qualified as Hspec

-- Windows isn't happy with our `/` shaped abs paths.
-- Since Windows isn't going to be parsing ldd output anyway, just skip it.
#ifdef mingw32_HOST_OS

spec :: Hspec.Spec
spec = pure ()

#else

import App.Fossa.VSI.DynLinked.Internal qualified as DL
import Data.Text (Text)
import Data.Void (Void)
import Path qualified as P
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)
import Data.Maybe (catMaybes)

-- While parsing ldd-shaped output works on every platform, only Linux can actually run ldd.
#ifdef linux_HOST_OS
import Path.IO qualified as PIO
import Data.Set qualified as S
import Control.Carrier.Diagnostics (runDiagnostics)
import Effect.Exec (runExecIO)
#endif

spec :: Hspec.Spec
spec = do
  Hspec.describe "parse ldd-shaped output" $ do
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

#ifdef linux_HOST_OS
  Hspec.describe "parse ldd output" $ do
    executableTarget <- Hspec.runIO localExecutable
    targetDependencies <- Hspec.runIO . runDiagnostics . runExecIO $ DL.listLocalDependencies executableTarget

    Hspec.it "should parse actual ldd output" $ case targetDependencies of
      Left _ -> Hspec.expectationFailure "could not check file: ensure you've run `make build-test-data` locally"
      Right result -> result `Hspec.shouldBe` localExecutableExpected
#endif

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Hspec.Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseLineInto :: Text -> (Maybe DL.LocalDependency) -> Hspec.Expectation
shouldParseLineInto = parseMatch DL.parseLine

shouldParseOutputInto :: Text -> [DL.LocalDependency] -> Hspec.Expectation
shouldParseOutputInto = parseMatch DL.parseLocalDependencies

singleLine :: Text
singleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

singleLineMoreSpaces :: Text
singleLineMoreSpaces = "\t\tlibc.so.6\t\t=>\t\t/lib/x86_64-linux-gnu/libc.so.6\t\t(0x00007fbea9a88000)\t\t"

singleLineExpected :: Maybe DL.LocalDependency
singleLineExpected = Just $ DL.LocalDependency "libc.so.6" $(P.mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")

multipleLine :: Text
multipleLine = "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\n\tlibc2.so.6 => /lib/x86_64-linux-gnu/libc2.so.6 (0x00007fbea9a88000)"

multipleLineExpected :: [DL.LocalDependency]
multipleLineExpected =
  [ DL.LocalDependency "libc.so.6" $(P.mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")
  , DL.LocalDependency "libc2.so.6" $(P.mkAbsFile "/lib/x86_64-linux-gnu/libc2.so.6")
  ]

syscallPresent :: Text
syscallPresent = "linux-vdso.so.1 =>  (0x00007ffc28d59000)\n\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

syscallPresentExpected :: [DL.LocalDependency]
syscallPresentExpected = [DL.LocalDependency "libc.so.6" $(P.mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

linkerPresent :: Text
linkerPresent = "/lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)\n\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)"

linkerPresentExpected :: [DL.LocalDependency]
linkerPresentExpected = [DL.LocalDependency "libc.so.6" $(P.mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

syscallAndLinkerPresent :: Text
syscallAndLinkerPresent = "linux-vdso.so.1 =>  (0x00007ffc28d59000)\n\tlibc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbea9a88000)\n\t/lib64/ld-linux-x86-64.so.2 (0x00007f4232cc1000)"

syscallAndLinkerPresentExpected :: [DL.LocalDependency]
syscallAndLinkerPresentExpected = [DL.LocalDependency "libc.so.6" $(P.mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")]

#ifdef linux_HOST_OS

localExecutable :: IO (P.Path P.Abs P.File)
localExecutable = PIO.resolveFile' "test/App/Fossa/VSI/DynLinked/testdata/hello_standard"

localExecutableExpected :: S.Set (P.Path P.Abs P.File)
localExecutableExpected = S.singleton $(P.mkAbsFile "/lib/x86_64-linux-gnu/libc.so.6")

#endif

#endif
