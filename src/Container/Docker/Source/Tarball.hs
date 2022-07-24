{-# LANGUAGE BangPatterns #-}

module Container.Docker.Source.Tarball (
  parse,
  mkFsFromChangeset,
) where

import Codec.Archive.Tar (
  Entry (entryContent),
  EntryContent (NormalFile, OtherEntryType),
  entryPath,
 )
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (Entry (entryTarPath), TarPath, fromTarPath)
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Docker.Manifest (
  ManifestJson,
  decodeManifestJson,
  getLayerPaths,
  manifestFilename,
 )
import Container.Errors (
  ContainerImgParsingError (
    ManifestJsonNotFound,
    ManifestJsonParsingFailed,
    TarMissingLayerTar,
    TarParserError
  ),
 )
import Container.Types (
  ContainerFSChangeSet (InsertOrUpdate, Whiteout),
  ContainerImageRaw (ContainerImageRaw),
  ContainerLayer (ContainerLayer, lastOffset, layerChangeSets),
  removeWhiteOut,
 )
import Container.VirtualFS.VirtualMapsFS (SomeFileTree, empty, insert, remove, toSomePath)
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Either (partitionEithers)
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NLE
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (|>))
import Data.Sequence qualified as Seq
import Data.String.Conversion (ToText (toText), toString)

data TarEntries = TarEntries
  { entries :: !(Seq (Tar.Entry, TarEntryOffset))
  , prevOffset :: !TarEntryOffset
  }

parse :: ByteStringLazy.ByteString -> Either ContainerImgParsingError ContainerImageRaw
parse content = case getTarEntries $ Tar.read content of
  Left err -> Left err
  Right ewo -> do
    case getManifest ewo of
      Left err -> Left err
      Right mj -> do
        let !layers = getLayers mj ewo
        let (!parsingErrors, !containerLayerImgs) = partitionEithers (NLE.toList layers)

        if not . null $ parsingErrors
          then Left (head parsingErrors)
          else Right $ ContainerImageRaw mj (NLE.fromList containerLayerImgs)

getManifest :: TarEntries -> Either ContainerImgParsingError ManifestJson
getManifest (TarEntries te _) =
  case viewl $ Seq.filter (\(t, _) -> entryPath t == manifestFilePath && isFile t) te of
    EmptyL -> Left ManifestJsonNotFound
    -- From docker exported tarball, there is only singular manifest file.
    -- If there are multiple, choose the first one.
    (manifestEntryOffset :< _) -> case entryContent $ fst manifestEntryOffset of
      (NormalFile c _) -> case decodeManifestJson c of
        Left err -> Left $ ManifestJsonParsingFailed err
        Right manifest -> Right manifest
      -- Manifest json must be regular file, not some symlink or directory
      -- per spec. If it does exist but is not a normal file. Then, it is likely
      -- not valid manifest.
      _ -> Left ManifestJsonNotFound
  where
    manifestFilePath :: FilePath
    manifestFilePath = toString manifestFilename

isFile :: Tar.Entry -> Bool
isFile (TarEntry.Entry _ (NormalFile _ _) _ _ _ _) = True
isFile _ = False

getTarEntries :: Tar.Entries Tar.FormatError -> Either ContainerImgParsingError TarEntries
getTarEntries = build (TarEntries mempty 0)
  where
    build :: TarEntries -> Tar.Entries Tar.FormatError -> Either ContainerImgParsingError TarEntries
    build !factory (Tar.Next e es) = build (addTarEntry e factory) es
    build !factory Tar.Done = Right factory
    build !_ (Tar.Fail err) = Left $ TarParserError err

    addTarEntry :: Tar.Entry -> TarEntries -> TarEntries
    addTarEntry entry tarEntries =
      tarEntries
        { entries = entries tarEntries |> (entry, prevOffset tarEntries)
        , prevOffset = nextOffset entry (prevOffset tarEntries)
        }

getLayers :: ManifestJson -> TarEntries -> NLE.NonEmpty (Either ContainerImgParsingError ContainerLayer)
getLayers mj (TarEntries te _) = NLE.map f $ getLayerPaths mj
  where
    f :: FilePath -> Either ContainerImgParsingError ContainerLayer
    f path = case viewl $ Seq.filter (\(t, _) -> entryPath t == path && isFile t) te of
      EmptyL -> Left $ TarMissingLayerTar path
      (layerTarball :< _) -> case entryContent $ fst layerTarball of
        (NormalFile c _) -> do
          let rawEntries = Tar.read c
          case toChangeSet (snd layerTarball) rawEntries of
            Left cipe -> Left cipe
            Right cli -> Right cli
        _ -> Left ManifestJsonNotFound

-- This is modified code from: tar.
toChangeSet :: TarEntryOffset -> Tar.Entries Tar.FormatError -> Either ContainerImgParsingError ContainerLayer
toChangeSet imgOffset = go (ContainerLayer mempty 0)
  where
    go !builder (Tar.Next e es) = go (addNextChangeSet imgOffset e builder) es -- TODO: force eval
    go !builder Tar.Done = Right $! builder -- TODO: force eval
    go !_ (Tar.Fail err) = Left $ TarParserError err -- TODO: force eval
    addNextChangeSet :: TarEntryOffset -> Tar.Entry -> ContainerLayer -> ContainerLayer
    addNextChangeSet offset entry containerLayer =
      ContainerLayer
        { layerChangeSets =
            (layerChangeSets containerLayer) |> (mkChangeSet (entryTarPath entry) (offset + lastOffset containerLayer + 1))
        , lastOffset = nextOffset entry (lastOffset containerLayer)
        }

mkChangeSet :: TarPath -> TarEntryOffset -> ContainerFSChangeSet
mkChangeSet path offset = case removeWhiteOut (fromTarPath path) of
  Nothing -> InsertOrUpdate (fromTarPath path) offset
  Just pathWithoutWhiteout -> Whiteout pathWithoutWhiteout

-- | Calculates offset for entry using previous offset.
--
-- This is modified code from: tar.
nextOffset :: Tar.Entry -> TarEntryOffset -> TarEntryOffset
nextOffset entry offset =
  offset
    + 1
    + case entryContent entry of
      NormalFile _ size -> blocks size
      OtherEntryType _ _ size -> blocks size
      _ -> 0
  where
    blocks :: Int64 -> TarEntryOffset
    blocks size = fromIntegral (1 + (size - 1) `div` 512)

mkFsFromChangeset :: Seq ContainerFSChangeSet -> SomeFileTree TarEntryOffset
mkFsFromChangeset = foldr applyChangeSet empty
  where
    applyChangeSet :: ContainerFSChangeSet -> SomeFileTree TarEntryOffset -> SomeFileTree TarEntryOffset
    applyChangeSet (InsertOrUpdate path offset) tree = insert (toSomePath . toText $ path) (Just offset) tree
    applyChangeSet (Whiteout path) tree = remove (toSomePath . toText $ path) tree
