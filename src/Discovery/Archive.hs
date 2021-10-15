module Discovery.Archive (
  discover,
  withArchive,
  extractRpm,
  extractTar,
  extractTarGz,
  extractZip,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Zip qualified as Zip
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.Diagnostics
import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.TaskPool (TaskPool, forkTask)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import Data.String.Conversion (toText)
import Discovery.Archive.RPM (extractRpm)
import Discovery.Walk
import Effect.ReadFS (ReadFS)
import Path
import Path.IO qualified as PIO
import Prelude hiding (zip)

-- | Given a function to run over unarchived contents, recursively unpack archives
discover :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Finally sig m, Has TaskPool sig m) => (Path Abs Dir -> m ()) -> Path Abs Dir -> m ()
discover go dir = context "Finding archives" $
  flip walk dir $ \_ _ files -> do
    -- To process an unpacked archive, run the provided function on the archive
    -- contents, and recursively call discover
    let process file unpackedDir = context (toText (fileName file)) $ do
          go unpackedDir
          discover go unpackedDir

    let tars = filter (\file -> ".tar" `isSuffixOf` fileName file) files
    traverse_ (\file -> forkTask $ withArchive extractTar file (process file)) tars

    let tarGzs = filter (\file -> ".tar.gz" `isSuffixOf` fileName file) files
    traverse_ (\file -> forkTask $ withArchive extractTarGz file (process file)) tarGzs

    let zips = filter (\file -> ".zip" `isSuffixOf` fileName file) files
    traverse_ (\file -> forkTask $ withArchive extractZip file (process file)) zips

    let jars = filter (\file -> ".jar" `isSuffixOf` fileName file) files
    traverse_ (\file -> forkTask $ withArchive extractZip file (process file)) jars

    let rpms = filter (\file -> ".rpm" `isSuffixOf` fileName file) files
    traverse_ (\file -> forkTask $ withArchive extractRpm file (process file)) rpms

    pure WalkContinue

-- | Extract an archive to a temporary directory, and run the provided callback
-- on the temporary directory. Archive contents are removed when the callback
-- finishes.
withArchive ::
  (Has (Lift IO) sig m, Has Finally sig m) =>
  -- | Archive extraction function
  (Path Abs Dir -> Path Abs File -> m ()) ->
  -- | Path to archive
  Path Abs File ->
  -- | Callback
  (Path Abs Dir -> m c) ->
  m c
withArchive extract file go = do
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
