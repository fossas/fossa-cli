{-# LANGUAGE TemplateHaskell #-}

module Container.TarballReadFSSpec (
  spec,
) where

import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.TarballReadFs (runTarballReadFSIO)
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Stack (StackC, runStack)
import Control.Effect.Lift (Lift, sendIO)
import Data.FileTree.IndexFileTree (SomeFileTree, empty, insert, toSomePath)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Diag.Result (Result (Failure, Success), renderFailure)
import Effect.Logger (IgnoreLoggerC, ignoreLogger, renderIt)
import Effect.ReadFS (ReadFSIOC, listDir, readContentsText)
import Path (Abs, Dir, File, Path, mkRelFile, (</>))
import Path.IO qualified as PIO
import Path.Internal (Path (..))
import Test.Hspec (
  Spec,
  SpecWith,
  describe,
  expectationFailure,
  it,
  runIO,
  shouldBe,
  shouldMatchList,
 )
import Type.Operator (type ($))

spec :: Spec
spec = do
  tarFile <- runIO tarAbsFile
  let it' = itEff tarFile

  describe "readContentText" $ do
    it' "should read content of file in tarball" $ do
      let feb2 :: Path Abs File = Path "logs-archive/feb/2.txt"
      feb2Content <- readContentsText feb2
      feb2Content `shouldBe'` "2\n"

      let jan1 :: Path Abs File = Path "logs-archive/jan/1.txt"
      jan1Content <- readContentsText jan1
      jan1Content `shouldBe'` "1\n"

      let lastFile :: Path Abs File = Path "logs-archive/last.txt"
      lastContent <- readContentsText lastFile
      lastContent `shouldBe'` "01-01-2022\n"

    it' "should read content of symbolic link's target in tarball" $ do
      let healthFile :: Path Abs File = Path "logs-archive/feb/last_health"
      healthFileContent <- readContentsText healthFile
      healthFileContent `shouldBe'` "OK\n"

      let lastFile :: Path Abs File = Path "last"
      lastContent <- readContentsText lastFile
      lastContent `shouldBe'` "01-01-2022\n"

  describe "listDir" $
    it' "should list directories and files" $ do
      let logsArchive :: Path Abs Dir = Path "logs-archive/"
      (listedDirs, listedFiles) <- listDir logsArchive

      listedDirs `shouldMatchList'` [Path "logs-archive/jan/", Path "logs-archive/feb/"]
      listedFiles `shouldMatchList'` [Path "logs-archive/last.txt"]
  where
    itEff :: Path Abs File -> String -> EffStack () -> SpecWith ()
    itEff tarFile msg = it msg . (runWithTarFsEff tarFile minimalTarFsTree)

    shouldBe' :: (Has (Lift IO) sig m, Show a, Eq a) => a -> a -> m ()
    shouldBe' left right = sendIO $ shouldBe left right

    shouldMatchList' :: (Has (Lift IO) sig m, Show a, Eq a) => [a] -> [a] -> m ()
    shouldMatchList' a b = sendIO $ shouldMatchList a b

-- * Effect Stack For Testing

type EffStack = ReadFSIOC $ DiagnosticsC $ IgnoreLoggerC $ StackC IO

runWithTarFsEff :: Path Abs File -> SomeFileTree TarEntryOffset -> EffStack () -> IO ()
runWithTarFsEff tarPath tarTree = do
  runStack . ignoreLogger . handleDiag . runTarballReadFSIO (tarTree) (tarPath)
  where
    handleDiag :: (Has (Lift IO) sig m) => DiagnosticsC m () -> m ()
    handleDiag diag =
      runDiagnostics diag >>= \case
        Failure ws eg ->
          expectationFailure'
            . toString
            . renderIt
            $ renderFailure ws eg "An issue occurred"
        Success _ _ -> pure ()

    expectationFailure' :: (Has (Lift IO) sig m) => String -> m ()
    expectationFailure' = sendIO . expectationFailure

mkTree :: [(Text, Maybe TarEntryOffset)] -> SomeFileTree TarEntryOffset
mkTree = foldr (\(p, ref) tree -> insert (toSomePath p) ref tree) empty

tarAbsFile :: IO (Path Abs File)
tarAbsFile = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Container/testdata/changeset_example.tar"))

minimalTarFsTree :: SomeFileTree TarEntryOffset
minimalTarFsTree =
  mkTree
    [ ("logs-archive/feb/2.txt", Just 10)
    , ("logs-archive/jan/1.txt", Just 14)
    , ("logs-archive/last.txt", Just 16)
    , ("logs-archive/feb/last_health", Just 66)
    , ("health.txt", Just 76)
    , ("last", Just 63)
    ]
