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
import Codec.Archive.Tar.Index (TarEntryOffset, hReadEntry)
import Control.Carrier.Simple (interpret)
import Control.Effect.Exception (throw)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.FileTree.IndexFileTree (
  SomeFileTree,
  doesDirExist,
  doesFileExist,
  lookupDir,
  lookupFileRef,
 )
import Data.Set qualified as Set
import Data.String.Conversion (
  ConvertUtf8 (decodeUtf8),
  LazyStrict (toStrict),
  ToText (toText),
  toString,
 )
import Data.Text (Text)
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
import Path (Abs, Dir, File, Path, SomeBase (..))
import Path.Internal.Posix (Path (Path))
import System.IO (IOMode (ReadMode), withFile)
import System.Random

readContentBS :: SomeFileTree TarEntryOffset -> Path Abs File -> SomeBase File -> IO ByteString
readContentBS fs tarball target = do
  fileRef <- lookupFileRef (toText target) fs
  case fileRef of
    Nothing -> throw $ userError $ "ReadContentBS: Could not find " <> (toString target) <> " in " <> (toString tarball)
    Just tarOffset ->
      withFile (toString tarball) ReadMode $ \handle ->
        (getContent fs tarball) =<< hReadEntry handle tarOffset

getContent :: SomeFileTree TarEntryOffset -> Path Abs File -> Entry -> IO ByteString
getContent _ _ entry =
  case entryContent entry of
    NormalFile content _ -> pure $ toStrict content
    Directory -> throw $ userError "directory found, cannot get file content of a directory."
    NamedPipe -> throw $ userError "named pipe found, cannot get file content of a named pipe."
    BlockDevice _ _ -> throw $ userError "block device found, cannot get file content of block device."
    CharacterDevice _ _ -> throw $ userError "character device found, cannot get file content of character device."
    OtherEntryType{} -> throw $ userError "other entry type found, cannot get file content of other entry type."
    -- TODO: Add Symbolic link support.
    SymbolicLink _ -> throw $ userError "symbolic link found, cannot get file content of symbolic link."
    HardLink _ -> throw $ userError "hard link found, cannot get file content of hard link."

readContentsBSLimit :: SomeFileTree TarEntryOffset -> Path Abs File -> SomeBase File -> Int -> IO ByteString
readContentsBSLimit fs tarball target limit = ByteString.take limit <$> (readContentBS fs tarball target)

readContentText :: SomeFileTree TarEntryOffset -> Path Abs File -> SomeBase File -> IO Text
readContentText fs tarball target = decodeUtf8 <$> (readContentBS fs tarball target)

resolveFile :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> Text -> IO (Path Abs File)
resolveFile fs tarball dir target = do
  fileExist <- doesFileExist candidatePath fs
  if fileExist
    then throw $ userError $ "ResolveFile: Could not find " <> (toString target) <> " in " <> (toString tarball)
    else pure $ Path (toString candidatePath)
  where
    candidatePath :: Text
    candidatePath = (toText dir) <> "/" <> target

resolveDir :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> Text -> IO (Path Abs Dir)
resolveDir fs tarball dir target = do
  dirExist <- doesDirExist candidatePath fs
  if dirExist
    then throw $ userError $ "ResolveDir: Could not find " <> (toString target) <> " in " <> (toString tarball)
    else pure $ Path (toString candidatePath)
  where
    candidatePath :: Text
    candidatePath = (toText dir) <> "/" <> target <> "/"

listDir :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File])
listDir fs tarball target = do
  dirListing <- lookupDir (toText target) fs
  case dirListing of
    Nothing -> throw $ userError $ "ListDir: Could not find " <> (toString target) <> " in " <> (toString tarball)
    Just paths -> do
      dirs <- filterM (`doesDirExist` fs) $ Set.toList paths
      files <- filterM (`doesDirExist` fs) $ Set.toList paths
      pure (map (Path . toString) dirs, map (Path . toString) files)

getIdentifier :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> IO DirID
getIdentifier fs tarball target = do
  fileRef <- lookupFileRef (toText target) fs
  case fileRef of
    Nothing -> do
      -- TODO: Fix when adding symbolic link support.
      -- Tarball changeset do not have inode IDs!
      rand1 <- randomIO :: IO Int -- EVIL
      rand2 <- randomIO :: IO Int -- EVIL
      dirExist <- doesDirExist (toText target) fs
      if dirExist
        then pure $ DirID (toInteger rand1) (toInteger rand2)
        else throw $ userError $ "GetIdentifier: Could not find directory " <> toString target <> " in " <> toString tarball
    Just tarOffset -> pure $ DirID (toInteger tarOffset) (toInteger tarOffset)

resolvePath :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> FilePath -> IO Effect.ReadFS.SomePath
resolvePath fs tarball dir target = do
  dirExist <- doesDirExist candidatePath fs
  fileExist <- doesFileExist candidatePath fs
  case (dirExist, fileExist) of
    (True, _) -> pure $ Effect.ReadFS.SomeDir (Abs (Path $ toString candidatePath))
    (_, True) -> pure $ Effect.ReadFS.SomeFile (Abs (Path $ toString candidatePath))
    (_, _) -> throw $ userError $ "ResolvePath: Could not find " <> target <> " in " <> (toString tarball)
  where
    candidatePath :: Text
    candidatePath = (toText dir) <> "/" <> (toText target) <> "/"

runTarballReadFSIO :: Has (Lift IO) sig m => (SomeFileTree TarEntryOffset) -> (Path Abs File) -> Effect.ReadFS.ReadFSIOC m a -> m a
runTarballReadFSIO fs tarball = interpret $ \case
  Effect.ReadFS.ReadContentsBS' file -> readContentBS fs tarball file `Effect.ReadFS.catchingIO` Effect.ReadFS.FileReadError (toString file)
  Effect.ReadFS.ReadContentsBSLimit' file limit -> readContentsBSLimit fs tarball file limit `Effect.ReadFS.catchingIO` Effect.ReadFS.FileReadError (toString file)
  Effect.ReadFS.ReadContentsText' file -> readContentText fs tarball file `Effect.ReadFS.catchingIO` Effect.ReadFS.FileReadError (toString file)
  Effect.ReadFS.ResolveFile' dir path -> resolveFile fs tarball dir path `Effect.ReadFS.catchingIO` Effect.ReadFS.ResolveError (toString dir) (toString path)
  Effect.ReadFS.ResolveDir' dir path -> resolveDir fs tarball dir path `Effect.ReadFS.catchingIO` Effect.ReadFS.ResolveError (toString dir) (toString path)
  Effect.ReadFS.ListDir dir -> listDir fs tarball dir `Effect.ReadFS.catchingIO` Effect.ReadFS.ListDirError (toString dir)
  Effect.ReadFS.GetIdentifier dir -> getIdentifier fs tarball dir `Effect.ReadFS.catchingIO` Effect.ReadFS.FileReadError (toString dir)
  Effect.ReadFS.ResolvePath root path -> resolvePath fs tarball root path `Effect.ReadFS.catchingIO` Effect.ReadFS.ResolveError (toString root) path
  Effect.ReadFS.GetCurrentDir -> pure (Left $ Effect.ReadFS.CurrentDirError "there is no current directory within tar!")
  Effect.ReadFS.DoesFileExist file -> sendIO $ doesFileExist (toText file) fs
  Effect.ReadFS.DoesDirExist dir -> sendIO $ doesDirExist (toText dir) fs
