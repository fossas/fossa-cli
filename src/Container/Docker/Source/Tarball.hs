module Container.Docker.Source.Tarball (
    parse
) where

import Codec.Archive.Tar (
  Entry (entryContent),
  EntryContent (Directory, NormalFile, OtherEntryType),
  entryPath,
 )
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Tar.Index (TarEntryOffset)
import Container.Docker.Manifest (ManifestJson, decodeManifestJson, manifestFilename, getLayerPaths)
import Container.Errors (ContainerImgParsingError (ManifestJsonNotFound, ManifestJsonParsingFailed, TarParserError, TarMissingLayerTar))
import Data.Int (Int64)
import Data.Sequence (Seq ((:|>)), ViewL (EmptyL, (:<)), (|>), viewl)
import Data.Sequence qualified as Seq
import Data.String.Conversion (toString)
import qualified Data.List.NonEmpty as NLE
import Container.Types (ContainerLayerImage (layerChangeSets, lastOffset, ContainerLayerImage), ContainerImageRaw (ContainerImageRaw), ContainerFSChangeSet (InsertOrUpdate, Whiteout), removeWhiteOut)
import Codec.Archive.Tar.Entry (TarPath, Entry (entryTarPath), fromTarPath)
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.Either (partitionEithers)

-- | Sequence of entry and their associated offset.
-- Uses Seq to get O(1) peek and push.
newtype EntriesWithOffsets = EntriesWithOffsets (Seq (Tar.Entry, TarEntryOffset))

parse :: ByteStringLazy.ByteString -> Either ContainerImgParsingError ContainerImageRaw
parse content = case getEntriesWithOffsets $ Tar.read content of
    Left err -> Left err
    Right ewo -> do
        -- Ensure we have a valid manifest.json
        case getManifest ewo of
            Left err -> Left err
            Right mj -> do
                let layers = getLayerEntries mj ewo
                let (parsingErrors, containerLayerImgs) = partitionEithers (NLE.toList layers)

                if not . null $ parsingErrors
                    then Left (head parsingErrors)
                    else Right $ ContainerImageRaw mj containerLayerImgs

-- | Gets the manifest from tarball.
getManifest :: EntriesWithOffsets -> Either ContainerImgParsingError ManifestJson
getManifest (EntriesWithOffsets te) =
  case Seq.viewl $ Seq.filter (\(t, _) -> entryPath t == manifestFilePath && isFile t) te of
    EmptyL -> Left ManifestJsonNotFound
    (manifestEntryOffset :< _) -> case entryContent $ fst manifestEntryOffset of
      (NormalFile c _) -> case decodeManifestJson c of
        Left err -> Left $ ManifestJsonParsingFailed err
        Right manifest -> Right manifest
      _ -> Left ManifestJsonNotFound
  where
    manifestFilePath :: FilePath
    manifestFilePath = toString manifestFilename

getEntriesWithOffsets :: Tar.Entries Tar.FormatError -> Either ContainerImgParsingError EntriesWithOffsets
getEntriesWithOffsets = build (EntriesWithOffsets mempty)
  where
    build :: EntriesWithOffsets -> Tar.Entries Tar.FormatError -> Either ContainerImgParsingError EntriesWithOffsets
    build !factory (Tar.Next e es) = build (addTarEntry e factory) es
    build !factory Tar.Done = Right factory
    build !_ (Tar.Fail err) = Left $ TarParserError err

-- This is modified code from: tar.
addTarEntry :: Tar.Entry -> EntriesWithOffsets -> EntriesWithOffsets
addTarEntry e (EntriesWithOffsets builders) = EntriesWithOffsets $ builders |> (e, nextOffset e $ prevOffset builders)
  where
    prevOffset :: Seq (Tar.Entry, TarEntryOffset) -> TarEntryOffset
    prevOffset (_ :|> xe) = snd xe
    prevOffset _ = 0


getLayerEntries :: ManifestJson -> EntriesWithOffsets -> NLE.NonEmpty (Either ContainerImgParsingError ContainerLayerImage)
getLayerEntries mj (EntriesWithOffsets te) = NLE.map f $ getLayerPaths mj
  where
    f :: FilePath -> Either ContainerImgParsingError ContainerLayerImage
    f path = case Seq.viewl $ Seq.filter (\(t, _) -> entryPath t == path && isFile t) te of
        EmptyL -> Left $ TarMissingLayerTar path
        (layerTarball :< _) -> case entryContent $ fst layerTarball of
            (NormalFile c _) -> do
                let rawEntries = Tar.read c
                case toChangeSet (snd layerTarball) rawEntries of
                    Left cipe -> Left cipe
                    Right cli -> Right cli
            _ -> Left ManifestJsonNotFound

-- This is modified code from: tar.
toChangeSet :: TarEntryOffset -> Tar.Entries Tar.FormatError -> Either ContainerImgParsingError ContainerLayerImage
toChangeSet imgOffset = go (ContainerLayerImage mempty 0)
  where
    go !builder (Tar.Next e es) = go (addNextChangeSet imgOffset e builder) es
    go !builder Tar.Done = Right $! builder
    go !_ (Tar.Fail err) = Left $ TarParserError err

    addNextChangeSet :: TarEntryOffset -> Tar.Entry -> ContainerLayerImage -> ContainerLayerImage
    addNextChangeSet imgOffset entry containerLayer =
        ContainerLayerImage
            { layerChangeSets =
                layerChangeSets containerLayer ++ [mkChangeSet (entryTarPath entry) (imgOffset + lastOffset containerLayer + 1)]
            , lastOffset = nextOffset entry (lastOffset containerLayer)
            }

mkChangeSet :: TarPath -> TarEntryOffset -> ContainerFSChangeSet
mkChangeSet path offset = case removeWhiteOut (fromTarPath path) of
    Nothing -> InsertOrUpdate (fromTarPath path) offset
    Just pathWithoutWhiteout -> Whiteout pathWithoutWhiteout

isFile :: Tar.Entry -> Bool
isFile (TarEntry.Entry _ (NormalFile _ _) _ _ _ _) = True
isFile _ = False

isDirectory :: Tar.Entry -> Bool
isDirectory (TarEntry.Entry _ Directory _ _ _ _) = True
isDirectory _ = False

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
