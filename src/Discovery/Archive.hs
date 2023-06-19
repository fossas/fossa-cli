module Discovery.Archive (
  discover,
  withArchive,
  withArchive',
  extractRpm,
  extractTar,
  extractTarGz,
  extractTarXz,
  extractTarBz2,
  extractZip,
  selectUnarchiver,
  unpackFailurePath,
) where

import App.Util (ancestryDerived)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Zip qualified as Zip
import Codec.Compression.BZip qualified as BZip
import Codec.Compression.GZip qualified as GZip
import Conduit (runConduit, runResourceT, sourceFileBS, (.|))
import Control.Carrier.Diagnostics (fromEither)
import Control.Effect.Diagnostics (Diagnostics, Has, ToDiagnostic (renderDiagnostic), context, warnOnSomeException)
import Control.Effect.Exception (SomeException)
import Control.Effect.Finally (Finally, onExit)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.TaskPool (TaskPool, forkTask)
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Binary (sinkLbs)
import Data.Conduit.Lzma qualified as CLzma
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import Data.String.Conversion (toText)
import Discovery.Archive.RPM (extractRpm)
import Discovery.Walk (WalkStep (WalkContinue), fileName, walk)
import Effect.ReadFS (ReadFS)
import Path (
  Abs,
  Dir,
  File,
  Path,
  fromAbsDir,
  fromAbsFile,
  toFilePath,
  Rel,
  (</>),
 )
import Path.IO qualified as PIO
import Path qualified as P
import Prettyprinter (Pretty (pretty), hsep, viaShow, vsep)
import Prelude hiding (zip)

data ArchiveUnpackFailure = ArchiveUnpackFailure (Path Abs File) SomeException

unpackFailurePath :: ArchiveUnpackFailure -> Path Abs File
unpackFailurePath (ArchiveUnpackFailure path _) = path

instance ToDiagnostic ArchiveUnpackFailure where
  renderDiagnostic (ArchiveUnpackFailure file exc) =
    vsep
      [ "An error occurred while attempting to unpack an archive."
      , hsep ["Archive path:", pretty $ toText file]
      , hsep ["Error text:", viaShow exc]
      ]

-- | Converts a relative file path into a relative directory, where the passed in file path is suffixed by the archive suffix literal.
-- In other words, this:
--
-- > "external/lib.zip" :: Path Rel File
--
-- Becomes this:
--
-- > "external/lib.zip :: Path Rel Dir
convertArchiveToDir :: (Has Diagnostics sig m) => Path Rel File -> m (Path Rel Dir)
convertArchiveToDir file = do
  name <- fromEither . P.parseRelDir $ P.toFilePath (P.filename file)
  pure $ P.parent file </> name

-- | Given a function to run over unarchived contents, recursively unpack archives
discover ::
  (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Finally sig m, Has TaskPool sig m) =>
  -- | Callback to run on the discovered file
  (Path Abs Dir -> Maybe (Path Rel Dir) -> m ()) ->
  -- | Path to the archive
  Path Abs Dir ->
  -- | Path rendering
  (Path Abs Dir -> Path Abs File -> m (Path Rel File)) ->
  m ()
discover go dir renderAncestry = context "Finding archives" $ do
  flip walk dir $ \_ _ files -> do
    -- To process an unpacked archive, run the provided function on the archive
    -- contents, and recursively call discover
    let process file unpackedDir = context (toText (fileName file)) $ do
          logicalPath <- renderAncestry dir file
          logicalParent <- convertArchiveToDir logicalPath
          go unpackedDir (Just logicalParent)
          discover go unpackedDir $ ancestryDerived logicalParent

    traverse_ (\file -> forkTask $ withArchive' file (process file)) files
    pure WalkContinue

-- |Given a file extension, return an extraction function for that file type.
selectUnarchiver :: Has (Lift IO) sig m => String -> Maybe (Path Abs Dir -> Path Abs File -> m ())
selectUnarchiver file
  | ".tar" `isSuffixOf` file = Just extractTar
  | ".tar.gz" `isSuffixOf` file = Just extractTarGz
  | ".tar.xz" `isSuffixOf` file = Just extractTarXz
  | ".tar.bz2" `isSuffixOf` file = Just extractTarBz2
  | ".zip" `isSuffixOf` file = Just extractZip
  | ".jar" `isSuffixOf` file = Just extractZip
  | ".rpm" `isSuffixOf` file = Just extractRpm
  | ".aar" `isSuffixOf` file = Just extractZip
  | otherwise = Nothing

-- | Extract an archive to a temporary directory, and run the provided callback
-- on the temporary directory. Archive contents are removed when the callback
-- finishes.
--
-- The archive extraction function is automatically selected from the
-- file extension of the target archive.
--
-- If the file is not supported for extraction, results in 'Nothing'.
withArchive' ::
  (Has (Lift IO) sig m, Has Finally sig m, Has Diagnostics sig m) =>
  -- | Path to archive
  Path Abs File ->
  -- | Callback
  (Path Abs Dir -> m c) ->
  m (Maybe c)
withArchive' file go =
  case selectUnarchiver (fileName file) of
    Just extract -> withArchive extract file go
    Nothing -> pure Nothing

-- | Extract an archive to a temporary directory, and run the provided callback
-- on the temporary directory. Archive contents are removed when the callback
-- finishes.
--
-- Exceptions thrown during archive extraction are emitted as warnings and 'Nothing' is returned.
withArchive ::
  (Has (Lift IO) sig m, Has Diagnostics sig m, Has Finally sig m) =>
  -- | Archive extraction function
  (Path Abs Dir -> Path Abs File -> m ()) ->
  -- | Path to archive
  Path Abs File ->
  -- | Callback
  (Path Abs Dir -> m c) ->
  m (Maybe c)
withArchive extract file go = warnOnSomeException (ArchiveUnpackFailure file) "withArchive" $ do
  tmpDir <- mkTempDir (fileName file)
  extract tmpDir file
  go tmpDir

-- | Make a temporary directory, deleting it on exit
mkTempDir :: (Has (Lift IO) sig m, Has Finally sig m) => String -> m (Path Abs Dir)
mkTempDir name = do
  systemTmpDir <- sendIO PIO.getTempDir
  dir <- sendIO $ PIO.createTempDir systemTmpDir name
  onExit . sendIO . PIO.removeDirRecur $ dir
  pure dir

---------- Tar files

extractTar :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
extractTar dir tarFile =
  sendIO $ Tar.unpack (fromAbsDir dir) . removeTarLinks . Tar.read =<< BL.readFile (fromAbsFile tarFile)

extractTarGz :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
extractTarGz dir tarGzFile =
  sendIO $ Tar.unpack (fromAbsDir dir) . removeTarLinks . Tar.read . GZip.decompress =<< BL.readFile (fromAbsFile tarGzFile)

extractTarXz :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
extractTarXz dir tarXzFile = do
  decompressed <- sendIO (runResourceT . runConduit $ sourceFileBS (toFilePath tarXzFile) .| CLzma.decompress Nothing .| sinkLbs)
  sendIO $ Tar.unpack (fromAbsDir dir) . removeTarLinks . Tar.read $ decompressed

extractTarBz2 :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
extractTarBz2 dir tarGzFile =
  sendIO $ Tar.unpack (fromAbsDir dir) . removeTarLinks . Tar.read . BZip.decompress =<< BL.readFile (fromAbsFile tarGzFile)

-- The tar unpacker dies when tar files reference files outside of the archive root
removeTarLinks :: Tar.Entries e -> Tar.Entries e
removeTarLinks (Tar.Next x xs) =
  case Tar.entryContent x of
    Tar.HardLink _ -> removeTarLinks xs
    Tar.SymbolicLink _ -> removeTarLinks xs
    _ -> Tar.Next x (removeTarLinks xs)
removeTarLinks Tar.Done = Tar.Done
removeTarLinks (Tar.Fail e) = Tar.Fail e

---------- Zip files

extractZip :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
extractZip dir zipFile =
  sendIO $ Zip.withArchive (fromAbsFile zipFile) (Zip.unpackInto (fromAbsDir dir))
