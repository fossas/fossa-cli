{-# LANGUAGE TemplateHaskell #-}

module Container.TarballReadFSSpec (
  spec,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (GenEntryContent (NormalFile))
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Tarball (TarEntries (..), mkEntries)
import Container.TarballReadFs (runTarballReadFSIO)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Control.Carrier.Stack (StackC, runStack)
import Control.Effect.Lift (sendIO)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.FileTree.IndexFileTree (SomeFileTree, empty, insert, toSomePath)
import Data.Functor (($>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Sequence (ViewL (..))
import Data.Sequence qualified as Seq
import Data.String.Conversion (ToString (toString), toText)
import Data.Text (Text)
import Discovery.Filters (AllFilters (..))
import Discovery.Walk (WalkStep (..))
import Discovery.Walk.Broadcast (WalkConsumer (..), withWalkBroadcast)
import Effect.Logger (IgnoreLoggerC, ignoreLogger)
import Effect.ReadFS (ReadFSIOC, listDir, readContentsBS, readContentsText, resolveDir, resolveFile)
import GHC.Exception (throw)
import Path (Abs, Dir, File, Path, mkRelFile, toFilePath, (</>))
import Path.IO qualified as PIO
import Path.Internal (Path (..))
import Test.Effect (handleDiag, shouldBe', shouldMatchList')
import Test.Hspec (
  Spec,
  SpecWith,
  describe,
  it,
  runIO,
 )
import Type.Operator (type ($))
import Types (DiscoveredProjectType (..))

spec :: Spec
spec = do
  tarFile <- runIO tarAbsFile
  emptyPathTarFile <- runIO tarEmptyRootFile
  emptyPathTarFileTree <- runIO $ readTree emptyPathTarFile

  let withStandardTarFileIt = itEff tarFile minimalTarFsTree
  let withEmptyRootTarFileIt = itEff emptyPathTarFile emptyPathTarFileTree

  describe "readContentText" $ do
    withStandardTarFileIt "should read content of file in tarball" $ do
      let feb2 :: Path Abs File = Path "logs-archive/feb/2.txt"
      feb2Content <- readContentsText feb2
      feb2Content `shouldBe'` "2\n"

      let jan1 :: Path Abs File = Path "logs-archive/jan/1.txt"
      jan1Content <- readContentsText jan1
      jan1Content `shouldBe'` "1\n"

      let lastFile :: Path Abs File = Path "logs-archive/last.txt"
      lastContent <- readContentsText lastFile
      lastContent `shouldBe'` "01-01-2022\n"

    withStandardTarFileIt "should read content of symbolic link's target in tarball" $ do
      let healthFile :: Path Abs File = Path "logs-archive/feb/last_health"
      healthFileContent <- readContentsText healthFile
      healthFileContent `shouldBe'` "OK\n"

      let lastFile :: Path Abs File = Path "last"
      lastContent <- readContentsText lastFile
      lastContent `shouldBe'` "01-01-2022\n"

    withEmptyRootTarFileIt "should read content of file in tarball when root has no name" $ do
      let awsLambdaRie :: Path Abs File = Path "usr/local/bin/aws-lambda-rie"
      awsLambdaRieContent <- readContentsBS awsLambdaRie
      -- This is a binary, so just check the length rather than embedding the content in code.
      (ByteString.length awsLambdaRieContent) `shouldBe'` 5373952

  describe "listDir" $ do
    withStandardTarFileIt "should list directories and files" $ do
      let logsArchive :: Path Abs Dir = Path "logs-archive/"
      (listedDirs, listedFiles) <- listDir logsArchive

      listedDirs `shouldMatchList'` [Path "logs-archive/jan/", Path "logs-archive/feb/"]
      listedFiles `shouldMatchList'` [Path "logs-archive/last.txt"]

    withEmptyRootTarFileIt "should list directories and files when root has no name" $ do
      let parent :: Path Abs Dir = Path "usr/local/bin/"
      (parentDirs, parentFiles) <- listDir parent
      parentDirs `shouldMatchList'` []
      parentFiles `shouldMatchList'` [Path "usr/local/bin/aws-lambda-rie"]

      let grandParent :: Path Abs Dir = Path "usr/local/"
      (grandParentDirs, grandParentFiles) <- listDir grandParent
      grandParentDirs `shouldMatchList'` [Path "usr/local/bin/"]
      grandParentFiles `shouldMatchList'` []

  describe "resolveFile" $ do
    withStandardTarFileIt "should resolve file" $ do
      let logsArchive :: Path Abs Dir = Path "logs-archive/"
      resolvedFile <- resolveFile logsArchive "last.txt"
      resolvedFile `shouldBe'` Path "logs-archive/last.txt"

    withEmptyRootTarFileIt "should resolve file when root has no name" $ do
      let parent :: Path Abs Dir = Path "usr/local/bin"
      resolvedFile <- resolveFile parent "aws-lambda-rie"
      resolvedFile `shouldBe'` Path "usr/local/bin/aws-lambda-rie"

  describe "resolveDir" $ do
    withStandardTarFileIt "should resolve directory" $ do
      let logsArchive :: Path Abs Dir = Path "logs-archive/"
      resolvedDir <- resolveDir logsArchive "jan"
      resolvedDir `shouldBe'` Path "logs-archive/jan/"

    withEmptyRootTarFileIt "should resolve directory when root has no name" $ do
      let parent :: Path Abs Dir = Path "usr/local/"
      resolvedDir <- resolveDir parent "bin"
      resolvedDir `shouldBe'` Path "usr/local/bin/"

  describe "shared walk over the tar filesystem" $ do
    -- IndexFileTree.lookupDir strips the vfs-root prefix from its listings:
    -- the walk root's children arrive as "node_modules/", not
    -- "vfs-root/node_modules/".  A strategy's root-level skip (Maven skips
    -- target/ and node_modules/ at every listing) must still match those
    -- root-relative children, and per-visitor pruning must leave a sibling
    -- visitor seeing the skipped subtree — in one broadcast.
    let tree =
          insert (toSomePath "Cargo.toml") (Just 0)
            . insert (toSomePath "node_modules/m1/package.json") (Just 1)
            . insert (toSomePath "target/tgt/placeholder.txt") (Just 2)
            $ empty
        root = Path "vfs-root"

    itEff tarFile tree "a root-level skip matches root-relative children for its own visitor only, in one broadcast" $ do
      mavenRef <- sendIO (newIORef ([] :: [Path Abs Dir]))
      cargoRef <- sendIO (newIORef ([] :: [Path Abs Dir]))
      -- Maven-like: at every listing it skips its target/ and node_modules/
      -- subtrees.  Cargo-like: descends everywhere.
      let mavenLike dir _ _ = sendIO (modifyIORef' mavenRef (dir :)) $> ((), WalkSkipSome ["node_modules/", "target/"])
          cargoLike dir _ _ = sendIO (modifyIORef' cargoRef (dir :)) $> ((), WalkContinue)
          mavenConsumer = WalkConsumer MavenProjectType mavenLike
          cargoConsumer = WalkConsumer CargoProjectType cargoLike
      withWalkBroadcast (mempty :: AllFilters) Nothing False root [mavenConsumer, cargoConsumer]

      -- Maven's root-level skip matched the root-relative children: it saw
      -- only the root.
      mavenVisited <- sendIO (readIORef mavenRef)
      mavenVisited `shouldMatchList'` [Path "vfs-root"]

      -- Cargo still sees the whole tree, including both skipped subtrees.
      cargoVisited <- sendIO (readIORef cargoRef)
      cargoVisited
        `shouldMatchList'` [ Path "vfs-root"
                           , Path "node_modules/"
                           , Path "node_modules/m1/"
                           , Path "target/"
                           , Path "target/tgt/"
                           ]
  where
    itEff :: Path Abs File -> SomeFileTree TarEntryOffset -> String -> EffStack () -> SpecWith ()
    itEff tarFile tree msg = it msg . (runWithTarFsEff tarFile tree)

-- * Effect Stack For Testing

type EffStack = ReadFSIOC $ DiagnosticsC $ IgnoreLoggerC $ StackC IO

runWithTarFsEff :: Path Abs File -> SomeFileTree TarEntryOffset -> EffStack () -> IO ()
runWithTarFsEff tarPath tarTree =
  runStack
    . ignoreLogger
    . handleDiag
    . runTarballReadFSIO tarTree tarPath

-- | Create a tree for the tar file in code, as though it had been read via @Tarball.mkFsFromChangeset@.
mkTree :: [(Text, Maybe TarEntryOffset)] -> SomeFileTree TarEntryOffset
mkTree = foldr (\(p, ref) tree -> insert (toSomePath p) ref tree) empty

-- | Read a file tree directly from a tar file, as though it had been read via @Tarball.mkFsFromChangeset@.
-- Normalizes paths to forward slashes regardless of platform to make testing simpler.
readTree :: Path Abs File -> IO (SomeFileTree TarEntryOffset)
readTree file = do
  content <- ByteStringLazy.readFile $ toFilePath file
  case mkEntries $ Tar.read content of
    Left err -> throw . userError $ "read tar at " <> toString file <> ": " <> show err
    Right entries -> pure $ mkTreeFromEntries empty entries
  where
    mkTreeFromEntries :: SomeFileTree TarEntryOffset -> TarEntries -> SomeFileTree TarEntryOffset
    mkTreeFromEntries tree (TarEntries entries baseOffset) = case Seq.viewl $ Seq.filter (isFile . fst) entries of
      EmptyL -> tree
      (entry, offset) :< rest -> case Tar.entryContent entry of
        (NormalFile _ _) -> do
          let path = toText . normalizeSlash $ Tar.entryPath entry
          let tree' = insert (toSomePath path) (Just offset) tree
          mkTreeFromEntries tree' (TarEntries rest baseOffset)

        -- Tar files can have multiple kinds of entries, but only handle the ones needed for test data here.
        -- Skip anything else.
        _ -> mkTreeFromEntries tree (TarEntries rest baseOffset)

    -- True if tar entry is for a file with content, otherwise False.
    isFile :: Tar.Entry -> Bool
    isFile (TarEntry.Entry _ (NormalFile _ _) _ _ _ _) = True
    isFile _ = False

    normalizeSlash :: FilePath -> FilePath
    normalizeSlash = map (\c -> if c == '\\' then '/' else c)

-- | This tar has no name for the root directory:
--
-- @
-- ; tar -xf emptypath.tar
-- tar: Substituting `.' for empty member name
-- ; lt
-- .
-- └── usr
--    └── local
--       └── bin
--          └── aws-lambda-rie
-- @
tarEmptyRootFile :: IO (Path Abs File)
tarEmptyRootFile = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Container/testdata/emptypath.tar"))

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
