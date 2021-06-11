module Discovery.Archive (
  discover,
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
import Discovery.Archive.RPM (extractRpm)
import Discovery.Walk
import Effect.ReadFS (ReadFS)
import Path
import Path.IO qualified as PIO
import Prelude hiding (zip)

-- Given a function to run over unarchived contents, unpack archives
discover :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Finally sig m, Has TaskPool sig m) => (Path Abs Dir -> m ()) -> Path Abs Dir -> m ()
discover go = walk $ \_ _ files -> do
  let tars = filter (\file -> ".tar" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkTask $ withArchive tar file go) tars

  let tarGzs = filter (\file -> ".tar.gz" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkTask $ withArchive tarGz file go) tarGzs

  let zips = filter (\file -> ".zip" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkTask $ withArchive zip file go) zips

  let jars = filter (\file -> ".jar" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkTask $ withArchive zip file go) jars

  let rpms = filter (\file -> ".rpm" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkTask $ withArchive extractRpm file go) rpms

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
  (Path Abs Dir -> m ()) ->
  m ()
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

tar :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
tar dir tarFile =
  sendIO $ Tar.unpack (fromAbsDir dir) . removeTarLinks . Tar.read =<< BL.readFile (fromAbsFile tarFile)

tarGz :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
tarGz dir tarGzFile =
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

zip :: Has (Lift IO) sig m => Path Abs Dir -> Path Abs File -> m ()
zip dir zipFile =
  sendIO $ Zip.withArchive (fromAbsFile zipFile) (Zip.unpackInto (fromAbsDir dir))
