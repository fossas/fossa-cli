module Strategy.Archive
  ( discover,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Control.Carrier.Diagnostics (FailureBundle (..), SomeDiagnostic (..))
import Control.Effect.Exception (SomeException, mask, try)
import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.Output (output)
import Control.Effect.TaskPool (forkTask)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import Discovery.Walk
import Path
import qualified Path.IO as PIO
import Strategy.Archive.RPM (extractRpm)
import Types
import Prelude hiding (zip)

-- Given a discover function to run over unarchived contents, discover projects
discover :: HasDiscover sig m => (Path Abs Dir -> m ()) -> Path Abs Dir -> m ()
discover go = walk $ \_ _ files -> do
  let tars = filter (\file -> ".tar" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkArchiveDiscover $ withArchive tar file go) tars

  let tarGzs = filter (\file -> ".tar.gz" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkArchiveDiscover $ withArchive tarGz file go) tarGzs

  let zips = filter (\file -> ".zip" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkArchiveDiscover $ withArchive zip file go) zips

  let jars = filter (\file -> ".jar" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkArchiveDiscover $ withArchive zip file go) jars

  let rpms = filter (\file -> ".rpm" `isSuffixOf` fileName file) files
  traverse_ (\file -> forkArchiveDiscover $ withArchive extractRpm file go) rpms

  pure WalkContinue

-- | Fork discovery of archive contents as a new task, catching exceptions
forkArchiveDiscover :: HasDiscover sig m => m () -> m ()
forkArchiveDiscover go = forkTask $ do
  mask $ \restore -> do
    (res :: Either SomeException a) <- try (restore go)
    case res of
      Left exc -> output (ProjectFailure ArchiveGroup "archive" (FailureBundle [] (SomeDiagnostic [] exc)))
      Right () -> pure ()

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
