module Container.Tarball (
  parse,
  mkFsFromChangeset,

  -- * for testing
  TarEntries (..),
  removeWhiteOut,
  mkEntries,
  mkImage,

  -- * utilities
  filePathOf,
) where

import Codec.Archive.Tar (
  Entry (entryContent),
  EntryContent (HardLink, NormalFile, SymbolicLink),
 )
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (Entry (entryTarPath), TarPath, entryPath, fromTarPathToPosixPath)
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Tar.Index (TarEntryOffset, nextEntryOffset)
import Container.Docker.ImageJson (ImageJson, decodeImageJson, getLayerIds)
import Container.Docker.Manifest (ManifestJson (..), decodeManifestJson, getImageJsonConfigFilePath, getLayerPaths, manifestFilename)
import Container.Errors (ContainerImgParsingError (..))
import Container.Types (
  ContainerFSChangeSet (InsertOrUpdate, Whiteout),
  ContainerImageRaw (ContainerImageRaw),
  ContainerLayer (..),
  LayerPath,
  mkLayerPath,
 )
import Control.Algebra (Has)
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Either (lefts, rights)
import Data.FileTree.IndexFileTree (SomeFileTree, empty, insert, remove, resolveSymLinkRef, toSomePath)
import Data.Foldable (foldlM)
import Data.List.NonEmpty qualified as NLE
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (ToText (toText), toString)
import Data.Text (Text, intercalate)
import Data.Text qualified as Text
import Debug.Pretty.Simple
import Debug.Trace (trace)
import Effect.Logger (Logger, Pretty (pretty), logDebug, logWarn)
import System.FilePath (hasTrailingPathSeparator)
import System.FilePath.Posix (normalise)

-- | Container of list of tar entries with their offset for random content read.
data TarEntries = TarEntries
  { entries :: Seq (Tar.Entry, TarEntryOffset)
  , prevOffset :: TarEntryOffset
  }
  deriving (Show)

-- | Parses Container Image from Tarball Byte string.
parse :: ByteStringLazy.ByteString -> Either (NLE.NonEmpty ContainerImgParsingError) ContainerImageRaw
parse content = case mkEntries $ Tar.read' content of
  Left err -> Left $ NLE.singleton err
  Right te -> do
    -- Exported docker image must have
    -- manifest file
    case getManifest te of
      Left err -> Left $ NLE.singleton err
      Right manifest -> do
        -- Use manifest to get image config which has
        -- layer and image hash
        case getImageJson (getImageJsonConfigFilePath manifest) te of
          Left err -> Left $ NLE.singleton err
          Right imgJson -> pTrace ("manifest path: " <> show imgJson) $ mkImage manifest imgJson te (getLayerPaths manifest)
  where
    getManifest :: TarEntries -> Either ContainerImgParsingError ManifestJson
    getManifest te = parseManifest =<< getFileContent te (toString manifestFilename)

    parseManifest :: ByteStringLazy.ByteString -> Either ContainerImgParsingError ManifestJson
    parseManifest bs = case decodeManifestJson bs of
      Left err -> Left $ ManifestJsonParsingFailed err
      Right manifest -> Right manifest

    getImageJson :: Text -> TarEntries -> Either ContainerImgParsingError ImageJson
    getImageJson imgJsonFp te = parseImageJson =<< getFileContent te (toString imgJsonFp)

    parseImageJson :: ByteStringLazy.ByteString -> Either ContainerImgParsingError ImageJson
    parseImageJson bs = case decodeImageJson bs of
      Left err -> Left $ ManifestJsonParsingFailed err
      Right imgJson -> Right imgJson

    getFileContent :: TarEntries -> FilePath -> Either ContainerImgParsingError ByteStringLazy.ByteString
    getFileContent (TarEntries te _) filepath = do
      tarFilePath <- first FilePathToTarPathConversion $ TarEntry.toTarPath False filepath
      pTraceM "Filepath: "
      pTraceShowM filepath
      case viewl $ Seq.filter (\(t, _) -> trace (show $ entryTarPath t) (entryTarPath t) == tarFilePath && isFile t) te of
        EmptyL -> Left $ TarballFileNotFound filepath
        (manifestEntryOffset :< _) -> case entryContent $ fst manifestEntryOffset of
          (NormalFile c _) -> Right c
          _ -> Left $ TarballFileNotFound filepath

mkEntries ::
  Tar.Entries Tar.FormatError ->
  Either ContainerImgParsingError TarEntries
mkEntries = build (TarEntries mempty 0)
  where
    build :: TarEntries -> Tar.Entries Tar.FormatError -> Either ContainerImgParsingError TarEntries
    build builder (Tar.Next e es) = build (addEntry e builder) es
    build builder Tar.Done = Right builder
    build _ (Tar.Fail err) = Left $ TarParserError err

    addEntry :: Tar.Entry -> TarEntries -> TarEntries
    addEntry entry tarEntries =
      tarEntries
        { entries = entries tarEntries |> (entry, prevOffset tarEntries)
        , prevOffset = nextEntryOffset entry (prevOffset tarEntries)
        }

mkImage ::
  ManifestJson ->
  ImageJson ->
  TarEntries ->
  NLE.NonEmpty FilePath ->
  Either (NLE.NonEmpty ContainerImgParsingError) ContainerImageRaw
mkImage manifest imgJson entries layerTarballPaths =
  case (errs, parsedLayers) of
    ((e : es), _) -> Left $ e NLE.:| es
    (_, []) -> Left $ NLE.singleton ContainerNoLayersDiscovered
    (_, (l : ls)) -> Right $ ContainerImageRaw (l NLE.:| ls) manifest
  where
    errs :: [ContainerImgParsingError]
    errs = lefts layers

    parsedLayers :: [ContainerLayer]
    parsedLayers = rights layers

    layers :: [Either ContainerImgParsingError ContainerLayer]
    layers = zipWith (curry $ mkLayer entries) (getLayerIds imgJson) (NLE.toList layerTarballPaths)

mkLayer :: TarEntries -> (Text, FilePath) -> Either ContainerImgParsingError ContainerLayer
mkLayer (TarEntries entries tarOffset) (layerId, layerTarball) =
  case viewl $ Seq.filter (\(t, _) -> (filePathOf . entryTarPath) t == layerTarball && (isFile t || isSymLink t)) entries of
    EmptyL -> Left $ TarMissingLayerTar layerTarball
    (layerTarballEntry :< _) -> case entryContent $ fst layerTarballEntry of
      (NormalFile c _) -> do
        let rawEntries = Tar.read' c
        case mkLayerFromOffset layerId (mkLayerPath layerTarball) (snd layerTarballEntry) rawEntries of
          Left err -> Left err
          Right layer -> Right layer

      -- Sometimes layer tar is alias to existing tarball file
      -- This occurs when same layer is used multiple times in container image.
      (SymbolicLink target) -> do
        mkLayer
          (TarEntries entries tarOffset)
          ( layerId
          , toString $
              resolveSymLinkRef
                (toText layerTarball)
                (toText $ TarEntry.fromLinkTargetToPosixPath target)
          )

      -- Layer tarball must be a file, or symbolic link to existing tar file.
      _ -> Left $ TarLayerNotAFile layerTarball

mkLayerFromOffset ::
  Text ->
  LayerPath ->
  TarEntryOffset ->
  Tar.Entries Tar.FormatError ->
  Either ContainerImgParsingError ContainerLayer
mkLayerFromOffset layerId layerPath imgOffset = build $ mempty{layerDigest = layerId}
  where
    build builder (Tar.Next e es) = build (addNextChangeSet imgOffset e builder) es
    build builder Tar.Done = Right builder
    build _ (Tar.Fail err) = Left $ TarParserError err

    addNextChangeSet :: TarEntryOffset -> Tar.Entry -> ContainerLayer -> ContainerLayer
    addNextChangeSet offset entry containerLayer =
      ContainerLayer
        { layerChangeSets = updateChangeSet offset entry containerLayer
        , lastOffset = nextEntryOffset entry (lastOffset containerLayer)
        , layerDigest = layerId
        , layerPath = Just layerPath
        }

    updateChangeSet :: TarEntryOffset -> Tar.Entry -> ContainerLayer -> Seq ContainerFSChangeSet
    updateChangeSet offset entry containerLayer =
      if isDoubleWhiteOut (filePathOf . entryTarPath $ entry)
        || ( not (isFileOrLinkTarget entry)
              && not (isWhiteOut $ filePathOf . entryTarPath $ entry)
           )
        then -- Do not capture Insert for non-files or non-symbolic links, as folders
        -- by themselves are not analysis relevant, and filepath information already contains
        -- relevant folder information.
          layerChangeSets containerLayer
        else
          (layerChangeSets containerLayer)
            |> (mkChangeSet (entryTarPath entry) (offset + lastOffset containerLayer + 1))

    mkChangeSet :: TarPath -> TarEntryOffset -> ContainerFSChangeSet
    mkChangeSet tarPath offset =
      if isWhiteOut $ filePathOf tarPath
        then Whiteout (removeWhiteOut . filePathOf $ tarPath)
        else InsertOrUpdate (filePathOf tarPath) offset

mkFsFromChangeset :: (Has Logger sig m) => ContainerLayer -> m (SomeFileTree TarEntryOffset)
mkFsFromChangeset ContainerLayer{layerChangeSets, layerDigest} = do
  logDebug $ "Building fs from layer " <> pretty layerDigest <> " changeset"
  (emptyDirs, tree) <- foldlM (flip applyChangeSet) (Set.empty, empty) layerChangeSets

  unless (null emptyDirs) $ do
    logWarn $
      "Empty directories found in change set, analysis may not be fully representative of container."
        <> "\nThis can happen when the path in the container is too long to have been stored in the tar image."
        <> "\nDirectories:"
        <> "\n"
        <> pretty (intercalate "\n" . map toText $ Set.toList emptyDirs)

  pure tree
  where
    applyChangeSet :: (Has Logger sig m) => ContainerFSChangeSet -> (Set FilePath, SomeFileTree TarEntryOffset) -> m (Set String, SomeFileTree TarEntryOffset)
    applyChangeSet (InsertOrUpdate path offset) (emptyDirs, tree) = do
      -- Do not insert directories directly, only files; @insert@ already handles creating directories on demand.
      -- This is done because if the tar has a file path length limit that is too short to store the full file path,
      -- inserting just the directory causes a path traversal error when using the resulting fs.
      if hasTrailingPathSeparator path
        then do
          pure (Set.insert path emptyDirs, tree)
        else do
          logDebug $ "[BuildLayer] InsertUpdate " <> pretty path
          pure (emptyDirs, insert (toSomePath . toText $ path) (Just offset) tree)
    applyChangeSet (Whiteout path) (emptyDirs, tree) = do
      logDebug $ "[BuildLayer] Remove " <> pretty path
      pure (emptyDirs, remove (toSomePath . toText $ path) tree)

-- | True if tar entry is for a file or a symlink, otherwise False
isFileOrLinkTarget :: Tar.Entry -> Bool
isFileOrLinkTarget e = isFile e || isSymLink e || isHardLink e

-- | True if tar entry is for a file with content, otherwise False.
isFile :: Tar.Entry -> Bool
isFile (TarEntry.Entry _ (NormalFile _ _) _ _ _ _) = True
isFile _ = False

-- | True if tar entry is for a symbolic link, otherwise False.
isSymLink :: Tar.Entry -> Bool
isSymLink (TarEntry.Entry _ (SymbolicLink _) _ _ _ _) = True
isSymLink _ = False

-- | True if tar entry is for a hard link, otherwise False.
isHardLink :: Tar.Entry -> Bool
isHardLink (TarEntry.Entry _ (HardLink _) _ _ _ _) = True
isHardLink _ = False

-- | True if tar path has double whiteout marker.
isDoubleWhiteOut :: FilePath -> Bool
isDoubleWhiteOut = fileNameHasPrefix ".wh..wh."

-- | True if tar path has whiteout marker.
isWhiteOut :: FilePath -> Bool
isWhiteOut = fileNameHasPrefix ".wh"

-- | True if tarpath's filename has provided prefix. Otherwise False.
fileNameHasPrefix :: Text -> FilePath -> Bool
fileNameHasPrefix prefix = Text.isPrefixOf prefix . fileNameOf . toText

-- | Filename from text path.
fileNameOf :: Text -> Text
fileNameOf path = snd $ Text.breakOnEnd "/" path

-- | Retrieves filepath from tar path.
filePathOf :: TarPath -> FilePath
filePathOf = normalise . fromTarPathToPosixPath

-- | Removes whiteout prefix from the filepath. If no whiteout prefix is detected returns Nothing.
--
-- Path with whiteout markers do not have "/" suffix in their filepath, even for directories.
--
-- >> removeWhiteOut "etc/hello.txt" = Nothing
-- >> removeWhiteOut "etc/.wh.hello.txt" = Just "etc/hello.txt"
-- >> removeWhiteOut "etc/w.h.os" = Just "etc/os"
removeWhiteOut :: FilePath -> FilePath
removeWhiteOut p = if isWhiteOut p then toString $ Text.replace ".wh." "" (toText p) else p
