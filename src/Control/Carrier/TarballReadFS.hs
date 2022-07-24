{-# LANGUAGE GADTs #-}

module Control.Carrier.TarballReadFS (runTarballReadFSIO) where

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
import Codec.Archive.Tar.Entry (Entry (entryTarPath), fromLinkTargetToPosixPath, fromTarPath)
import Codec.Archive.Tar.Index (TarEntryOffset, hReadEntry)
import Container.VirtualFS.VirtualMapsFS (
  SomeFileTree,
  doesDirExist,
  doesFileExist,
  lookupDir,
  lookupFileRef,
  resolveRelativePath,
 )
import Control.Carrier.Simple (interpret)
import Control.Effect.Exception (throw)
import Control.Effect.Lift (Has, Lift)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Data.String.Conversion (
  ConvertUtf8 (decodeUtf8),
  LazyStrict (toStrict),
  ToText (toText),
  toString,
 )
import Data.Text (Text)
import Effect.ReadFS (
  DirID (DirID, dirDeviceID, dirFileID),
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
readContentBS fs tarball target = case lookupFileRef (toText target) fs of
  Nothing -> throw $ userError $ "ReadContentBS: Could not find " <> (toString target) <> " in " <> (toString tarball)
  Just tarOffset ->
    withFile (toString tarball) ReadMode $ \handle ->
      (getContent fs tarball) =<< hReadEntry handle tarOffset

getContent :: SomeFileTree TarEntryOffset -> Path Abs File -> Entry -> IO ByteString
getContent fs tarball entry =
  case entryContent entry of
    NormalFile content _ -> pure $ toStrict content
    Directory -> throw $ userError "directory found, cannot get file content of a directory."
    NamedPipe -> throw $ userError "named pipe found, cannot get file content of a named pipe."
    BlockDevice _ _ -> throw $ userError "block device found, cannot get file content of block device."
    CharacterDevice _ _ -> throw $ userError "character device found, cannot get file content of character device."
    -- TODO: Add Symbolic link support.
    HardLink _ -> throw $ userError "hard link found, cannot get file content of hard link."
    SymbolicLink fp -> throw $ userError "symbolic link found, cannot get file content of symbolic link."
    OtherEntryType{} -> throw $ userError "other entry type found, cannot get file content of other entry type."

readContentsBSLimit :: SomeFileTree TarEntryOffset -> Path Abs File -> SomeBase File -> Int -> IO ByteString
readContentsBSLimit fs tarball target limit = ByteString.take limit <$> (readContentBS fs tarball target)

readContentText :: SomeFileTree TarEntryOffset -> Path Abs File -> SomeBase File -> IO Text
readContentText fs tarball target = decodeUtf8 <$> (readContentBS fs tarball target)

resolveFile :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> Text -> IO (Path Abs File)
resolveFile fs tarball dir target =
  if doesFileExist candidatePath fs
    then throw $ userError $ "ResolveFile: Could not find " <> (toString target) <> " in " <> (toString tarball)
    else pure $ Path (toString candidatePath)
  where
    candidatePath :: Text
    candidatePath = (toText dir) <> "/" <> target

resolveDir :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> Text -> IO (Path Abs Dir)
resolveDir fs tarball dir target =
  if doesDirExist candidatePath fs
    then throw $ userError $ "ResolveDir: Could not find " <> (toString target) <> " in " <> (toString tarball)
    else pure $ Path (toString candidatePath)
  where
    candidatePath :: Text
    candidatePath = (toText dir) <> "/" <> target <> "/"

listDir :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File])
listDir fs tarball target = case lookupDir (toText target) fs of
  Nothing -> throw $ userError $ "ListDir: Could not find " <> (toString target) <> " in " <> (toString tarball)
  Just paths ->
    pure
      ( map (Path . toString) $ filter (`doesDirExist` fs) $ Set.toList paths
      , map (Path . toString) $ filter (`doesFileExist` fs) $ Set.toList paths
      )

getIdentifier :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> IO Effect.ReadFS.DirID
getIdentifier fs tarball target = case lookupFileRef (toText target) fs of
  Nothing -> do
    -- TODO: Fix when adding symbolic link support.
    rand1 <- randomIO :: IO Int
    rand2 <- randomIO :: IO Int
    if doesDirExist (toText target) fs
      then pure $ Effect.ReadFS.DirID (toInteger rand1) (toInteger rand2)
      else throw $ userError $ "GetIdentifier: Could not find directory " <> toString target <> " in " <> toString tarball
  Just tarOffset -> pure $ Effect.ReadFS.DirID (toInteger tarOffset) (toInteger tarOffset)

resolvePath :: SomeFileTree TarEntryOffset -> Path Abs File -> Path Abs Dir -> FilePath -> IO Effect.ReadFS.SomePath
resolvePath fs tarball dir target = case (doesDirExist candidatePath fs, doesFileExist candidatePath fs) of
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
  Effect.ReadFS.DoesFileExist file -> pure $ doesFileExist (toText file) fs
  Effect.ReadFS.DoesDirExist dir -> pure $ doesDirExist (toText dir) fs
