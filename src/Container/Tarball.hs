{-# LANGUAGE BangPatterns #-}

module Container.Tarball (
  -- * for testing
  TarEntries (..),
  removeWhiteOut,
  mkEntries,
  mkImage,
) where

import Codec.Archive.Tar (
  Entry (entryContent),
  EntryContent (NormalFile, SymbolicLink),
  entryPath,
 )
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (Entry (entryTarPath), TarPath, fromTarPath)
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Tar.Index (TarEntryOffset, nextEntryOffset)
import Container.Errors (
  ContainerImgParsingError (
    ContainerNoLayersDiscovered,
    TarLayerNotAFile,
    TarMissingLayerTar,
    TarParserError
  ),
 )
import Container.Types (
  ContainerFSChangeSet (InsertOrUpdate, Whiteout),
  ContainerImageRaw (ContainerImageRaw),
  ContainerLayer (
    ContainerLayer,
    lastOffset,
    layerChangeSets
  ),
 )
import Data.Either (lefts, rights)
import Data.List.NonEmpty qualified as NLE
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (|>))
import Data.Sequence qualified as Seq
import Data.String.Conversion (ToText (toText), toString)
import Data.Text (Text)
import Data.Text qualified as Text

data TarEntries = TarEntries
  { entries :: !(Seq (Tar.Entry, TarEntryOffset))
  , prevOffset :: !TarEntryOffset
  }
  deriving (Show)

mkEntries ::
  Tar.Entries Tar.FormatError ->
  Either ContainerImgParsingError TarEntries
mkEntries = build (TarEntries mempty 0)
  where
    build :: TarEntries -> Tar.Entries Tar.FormatError -> Either ContainerImgParsingError TarEntries
    build !builder (Tar.Next e es) = build (addEntry e builder) es
    build !builder Tar.Done = Right builder
    build !_ (Tar.Fail err) = Left $ TarParserError err

    addEntry :: Tar.Entry -> TarEntries -> TarEntries
    addEntry entry tarEntries =
      tarEntries
        { entries = entries tarEntries |> (entry, prevOffset tarEntries)
        , prevOffset = nextEntryOffset entry (prevOffset tarEntries)
        }

mkImage ::
  TarEntries ->
  NLE.NonEmpty FilePath ->
  Either (NLE.NonEmpty ContainerImgParsingError) ContainerImageRaw
mkImage entries layerTarballPaths =
  case (errs, parsedLayers) of
    ((e : es), _) -> Left $ e NLE.:| es
    (_, []) -> Left $ NLE.singleton ContainerNoLayersDiscovered
    (_, (l : ls)) -> Right . ContainerImageRaw $ l NLE.:| ls
  where
    errs :: [ContainerImgParsingError]
    errs = lefts layers

    parsedLayers :: [ContainerLayer]
    parsedLayers = rights layers

    layers :: [Either ContainerImgParsingError ContainerLayer]
    layers = NLE.toList $ NLE.map (mkLayer entries) layerTarballPaths

mkLayer :: TarEntries -> FilePath -> Either ContainerImgParsingError ContainerLayer
mkLayer (TarEntries entries _) layerTarball =
  case viewl $ Seq.filter (\(t, _) -> entryPath t == layerTarball && isFile t) entries of
    EmptyL -> Left $ TarMissingLayerTar layerTarball
    (layerTarballEntry :< _) -> case entryContent $ fst layerTarballEntry of
      (NormalFile c _) -> do
        let rawEntries = Tar.read c
        case mkLayerFromOffset (snd layerTarballEntry) rawEntries of
          Left err -> Left err
          Right layer -> Right layer

      -- Layer tarball must be a file, they cannot be a directory
      -- or symlink to some other file.
      _ -> Left $ TarLayerNotAFile layerTarball

mkLayerFromOffset ::
  TarEntryOffset ->
  Tar.Entries Tar.FormatError ->
  Either ContainerImgParsingError ContainerLayer
mkLayerFromOffset imgOffset = build (ContainerLayer mempty 0)
  where
    build !builder (Tar.Next e es) = build (addNextChangeSet imgOffset e builder) es
    build !builder Tar.Done = Right builder
    build !_ (Tar.Fail err) = Left $ TarParserError err

    addNextChangeSet :: TarEntryOffset -> Tar.Entry -> ContainerLayer -> ContainerLayer
    addNextChangeSet offset entry containerLayer =
      ContainerLayer
        { layerChangeSets = updateChangeSet offset entry containerLayer
        , lastOffset = nextEntryOffset entry (lastOffset containerLayer)
        }

    updateChangeSet :: TarEntryOffset -> Tar.Entry -> ContainerLayer -> Seq ContainerFSChangeSet
    updateChangeSet offset entry containerLayer =
      if isDoubleWhiteOut (entryTarPath entry)
        || ( not (isFileOrSymLink entry)
              && not (isWhiteOut $ entryTarPath entry)
           )
        then -- Do not capture Insert for non-files or non-symbolic links, as folders
        -- by themselves are not analysis relevant, and filepath information already contains
        -- relevant folder information.
          layerChangeSets containerLayer
        else
          (layerChangeSets containerLayer)
            |> (mkChangeSet (entryTarPath entry) (offset + lastOffset containerLayer + 1))

    mkChangeSet :: TarPath -> TarEntryOffset -> ContainerFSChangeSet
    mkChangeSet path offset = case removeWhiteOut $ fromTarPath path of
      Nothing -> InsertOrUpdate (fromTarPath path) offset
      Just p -> Whiteout p

-- | True if tar entry is for a file or a symlink, otherwise False
isFileOrSymLink :: Tar.Entry -> Bool
isFileOrSymLink e = isFile e || isSymLink e

-- | True if tar entry is for a file with content, otherwise False.
isFile :: Tar.Entry -> Bool
isFile (TarEntry.Entry _ (NormalFile _ _) _ _ _ _) = True
isFile _ = False

-- | True if tar entry is for a symbolic link, otherwise False.
isSymLink :: Tar.Entry -> Bool
isSymLink (TarEntry.Entry _ (SymbolicLink _) _ _ _ _) = True
isSymLink _ = False

-- | True if tar path has double whiteout marker.
isDoubleWhiteOut :: TarPath -> Bool
isDoubleWhiteOut = fileNameHasPrefix ".wh..wh."

-- | True if tar path has whiteout marker.
isWhiteOut :: TarPath -> Bool
isWhiteOut = fileNameHasPrefix ".wh"

-- | True if tarpath's filename has provided prefix. Otherwise False.
fileNameHasPrefix :: Text -> TarPath -> Bool
fileNameHasPrefix prefix path = Text.isPrefixOf prefix (fileNameOf $ toText . fromTarPath $ path)

-- | Filename from text path.
fileNameOf :: Text -> Text
fileNameOf path = snd $ Text.breakOnEnd "/" path

-- | Removes whiteout prefix from the filepath. If no whiteout prefix is detected returns Nothing.
--
-- Path with whiteout markers do not have "/" suffix in their filepath, even for directories.
--
-- >> removeWhiteOut "etc/hello.txt" = Nothing
-- >> removeWhiteOut "etc/.wh.hello.txt" = Just "etc/hello.txt"
-- >> removeWhiteOut "etc/w.h.os" = Just "etc/os"
removeWhiteOut :: FilePath -> Maybe FilePath
removeWhiteOut path =
  if Text.isPrefixOf ".wh." (fileNameOf $ toText path)
    then Just $ toString $ Text.replace ".wh." "" (toText path)
    else Nothing
