{-# LANGUAGE GADTs #-}

module Container.TarballReadFs (
  runTarballReadFSIO,
) where

import Codec.Archive.Tar (
  Entry (entryContent),
  EntryContent (
    BlockDevice,
    CharacterDevice,
    Directory,
    HardLink,
    NamedPipe,
    NormalFile,
    OtherEntryType,
    SymbolicLink
  ),
 )
import Codec.Archive.Tar.Entry (LinkTarget, entryTarPath, fromLinkTargetToPosixPath)
import Codec.Archive.Tar.Index (TarEntryOffset, hReadEntry)
import Container.Tarball (filePathOf)
import Control.Carrier.Simple (interpret)
import Control.Effect.Exception (throw)
import Control.Effect.Lift (Has, Lift)
import Control.Exception (
  Exception (fromException, toException),
  IOException,
  SomeAsyncException (SomeAsyncException),
  catch,
  throwIO,
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.FileTree.IndexFileTree (
  SomeFileTree,
  doesDirExist,
  doesFileExist,
  lookupDir,
  lookupFileRef,
  resolveSymLinkRef,
 )
import Data.Hashable (hash)
import Data.Set qualified as Set
import Data.String.Conversion (
  ConvertUtf8 (decodeUtf8),
  LazyStrict (toStrict),
  ToText (toText),
  toString,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.ReadFS (
  DirID (..),
  ReadFSErr (CurrentDirError, FileReadError, ListDirError, ResolveError),
  ReadFSF (
    DoesDirExist,
    DoesFileExist,
    GetCurrentDir,
    GetIdentifier,
    ListDir,
    ReadContentsBS',
    ReadContentsBSLimit',
    ReadContentsText',
    ResolveDir',
    ResolveFile',
    ResolvePath
  ),
  ReadFSIOC,
  SomePath (SomeDir, SomeFile),
  catchingIO,
 )
import Path (Abs, Dir, File, SomeBase (..))

-- We use internal module, as we cannot use parse_B_T (e.g. parseAbsFile), etc. to craft
-- Path b t, since tarball paths are not representative of POSIX, or windows
-- and such, parse__B_T is not useful.
import Path.Internal (Path (Path))
import System.IO (IOMode (ReadMode), withFile)

maxHopsOf20 :: Int
maxHopsOf20 = 20

ensureSlashSuffix :: Text -> Text
ensureSlashSuffix t = if Text.isSuffixOf "/" t then t else (t <> "/")

readContentBS ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Int -> -- circuit breaker for symbolic link hops
  SomeBase File ->
  IO ByteString
readContentBS fs tarball hop target =
  case lookupFileRef (toText target) fs of
    Nothing ->
      throw . userError $
        "ReadContentBS: Could not find "
          <> (toString target)
          <> " in "
          <> (toString tarball)
    Just tarOffset ->
      withFile (toString tarball) ReadMode $ \handle -> do
        (getContent fs tarball hop) =<< hReadEntry handle tarOffset

getContent ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Int -> -- current hop count
  Entry ->
  IO ByteString
getContent fs tarball hop entry =
  case entryContent entry of
    NormalFile content _ -> pure $ toStrict content
    Directory -> throw $ userError "directory found, cannot get file content from a directory."
    NamedPipe -> throw $ userError "named pipe found, cannot get file content from a named pipe."
    BlockDevice _ _ -> throw $ userError "block device found, cannot get file content from block device."
    CharacterDevice _ _ -> throw $ userError "character device found, cannot get file content from character device."
    OtherEntryType{} -> throw $ userError "other entry type found, cannot get file content from other entry type."
    SymbolicLink target -> toTargetPath target
    HardLink target -> toTargetPath target
  where
    toTargetPath :: LinkTarget -> IO ByteString
    toTargetPath target =
      if hop > maxHopsOf20
        then throw . userError $ "following symbolic link led to more than maximum hops, current filepaths is: " <> (toString currPath)
        else
          do
            -- In Tarball links target path may be provided as absolute, relative, or absolute without leading /
            --
            -- e.g.
            --  etc/os-release => ../usr/lib/os-release
            --  etc/os-release => /usr/lib/os-release
            --  etc/os-release => usr/lib/os-release
            -- -
            let targetPath :: Path Abs File =
                  Path . toString $
                    resolveSymLinkRef currPath (toText . fromLinkTargetToPosixPath $ target)

            readContentBS fs tarball (hop + 1) (Abs targetPath)
            `ioOr` readContentBS fs tarball (hop + 1) (Abs . Path $ fromLinkTargetToPosixPath target)

    currPath :: Text
    currPath = toText . filePathOf . entryTarPath $ entry

readContentsBSLimit ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  SomeBase File ->
  Int ->
  IO ByteString
readContentsBSLimit fs tarball target limit =
  ByteString.take limit <$> (readContentBS fs tarball 0 target)

readContentText ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  SomeBase File ->
  IO Text
readContentText fs tarball target =
  decodeUtf8 <$> (readContentBS fs tarball 0 target)

resolveFile ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Path Abs Dir ->
  Text ->
  IO (Path Abs File)
resolveFile fs tarball dir target =
  if (doesFileExist candidatePath fs)
    then pure $ Path (toString candidatePath)
    else
      throw . userError $
        "ResolveFile: Could not find "
          <> (toString target)
          <> " in "
          <> (toString tarball)
  where
    candidatePath :: Text
    candidatePath = (ensureSlashSuffix . toText $ dir) <> target

resolveDir ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Path Abs Dir ->
  Text ->
  IO (Path Abs Dir)
resolveDir fs tarball dir target =
  if doesDirExist candidatePath fs
    then pure $ Path (toString candidatePath)
    else
      throw . userError $
        "ResolveDir: Could not find "
          <> (toString target)
          <> " in "
          <> (toString tarball)
  where
    candidatePath :: Text
    candidatePath = (ensureSlashSuffix . toText $ dir) <> (ensureSlashSuffix target)

listDir ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Path Abs Dir ->
  IO ([Path Abs Dir], [Path Abs File])
listDir fs tarball target =
  case lookupDir (toText target) fs of
    Nothing ->
      throw . userError $
        "ListDir: Could not find "
          <> (toString target)
          <> " in "
          <> (toString tarball)
    Just paths ->
      pure $
        foldr
          ( \a (dirs, files) ->
              let dirs' = if a `doesDirExist` fs then (Path . toString $ a) : dirs else dirs
                  files' = if a `doesFileExist` fs then (Path . toString $ a) : files else files
               in (dirs', files')
          )
          ([], [])
          (Set.toList paths)

getIdentifier ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Path Abs Dir ->
  IO DirID
getIdentifier fs tarball target = do
  case lookupFileRef candidatePath fs of
    Nothing -> do
      -- Tarball changeset do not have inode IDs!
      -- So we use hash of the directory filepath
      if doesDirExist candidatePath fs
        then pure $ DirID hashOfFilePath hashOfFilePath
        else
          throw . userError $
            "GetIdentifier: Could not find directory "
              <> toString target
              <> " in "
              <> toString tarball
    Just tarOffset -> pure $ DirID hashOfFilePath (toInteger tarOffset)
  where
    candidatePath :: Text
    candidatePath = (ensureSlashSuffix . toText $ target)

    hashOfFilePath :: Integer
    hashOfFilePath = toInteger . hash . toText $ target

resolvePath ::
  SomeFileTree TarEntryOffset ->
  Path Abs File ->
  Path Abs Dir ->
  FilePath ->
  IO SomePath
resolvePath fs tarball dir target = do
  case (doesDirExist candidatePath fs, doesFileExist candidatePath fs) of
    (True, _) -> pure $ SomeDir (Abs . Path . toString $ candidatePath)
    (_, True) -> pure $ SomeFile (Abs . Path . toString $ candidatePath)
    (_, _) -> throw $ userError $ "ResolvePath: Could not find " <> target <> " in " <> (toString tarball)
  where
    candidatePath :: Text
    candidatePath = (toText dir) <> "/" <> (toText target) <> "/"

-- | ReadFS based on virtual file tree and tarball archive for random seeks.
runTarballReadFSIO ::
  Has (Lift IO) sig m =>
  SomeFileTree TarEntryOffset -> -- Virtual FileTree Containing Filepaths with reference offsets
  Path Abs File -> -- Tarball file in which offsets are used for random seek
  ReadFSIOC m a ->
  m a
runTarballReadFSIO fs tarball = interpret $ \case
  ReadContentsBS' file ->
    readContentBS fs tarball 0 file
      `catchingIO` FileReadError (toString file)
  ReadContentsBSLimit' file limit ->
    readContentsBSLimit fs tarball file limit
      `catchingIO` FileReadError (toString file)
  ReadContentsText' file ->
    readContentText fs tarball file
      `catchingIO` FileReadError (toString file)
  ResolveFile' dir path ->
    resolveFile fs tarball dir path
      `catchingIO` ResolveError (toString dir) (toString path)
  ResolveDir' dir path ->
    resolveDir fs tarball dir path
      `catchingIO` ResolveError (toString dir) (toString path)
  ListDir dir ->
    listDir fs tarball dir
      `catchingIO` ListDirError (toString dir)
  GetIdentifier dir ->
    getIdentifier fs tarball dir
      `catchingIO` FileReadError (toString dir)
  ResolvePath root path ->
    resolvePath fs tarball root path
      `catchingIO` ResolveError (toString root) path
  GetCurrentDir -> pure . Left $ CurrentDirError "there is no current directory within tar!"
  DoesFileExist file -> pure $ doesFileExist (toText file) fs
  DoesDirExist dir -> pure $ doesDirExist (toText dir) fs

-- | Alternatives in IO monad handling IO exceptions.
ioOr :: IO a -> IO a -> IO a
ioOr a b = a `catch` (\(e :: IOException) -> if isSyncException e then b else throwIO e)
  where
    isSyncException e = case fromException (toException e) of
      Just (SomeAsyncException _) -> False
      Nothing -> True
