{-# LANGUAGE RecordWildCards #-}

module Data.Rpm.DbHeaderBlob.Internal (
  -- * Main interface
  -- |You should not use this module directly. Use 'Data.Rpm.DbHeaderBlob'
  -- instead.
  PkgInfo (..),
  readPackageInfo,

  -- * Internal Types
  HeaderBlob (..),
  EntryInfo (..),
  IndexEntry (..),
  IndexCount (..),

  -- * Internal Functions
  readHeaderMetaData,
  getV3RegionCount,
  emptyRegionInfo,
  calcDataLength,
  readHeaderBlobTagData,

  -- * Header Tag Ids
  -- $tagIdDoc
  RpmTagType(..),
  rpmTagHeaderImg,
  rpmTagHeaderImmutable,
  rpmTagHeaderSignatures,
  regionTagCount,

  -- * Header Blob Type IDs
  -- $tagTypeDoc
  rpmStringType,
  regionTagType,
  rpmStringArrayType,
  rpmI18NstringType,
  rpmInt64Type,
  rpmInt32Type,
) where

import Control.Applicative (liftA2)
import Control.Monad (foldM, join, replicateM, unless, when)
import Data.Bifunctor (bimap, first)
import Data.Binary.Get (ByteOffset, Get, getInt32be, getWord32be, label, runGetOrFail)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BLS
import Data.Char (ord)
import Data.Either.Combinators (maybeToRight)
import Data.Int (Int32)
import Data.IntMap qualified as Map
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import Text.Printf (printf)

-- $tagIdDoc
-- Magic numbers for different tags included in a header blob

-- $tagIdDoc
rpmTagHeaderImmutable :: Int32
rpmTagHeaderImmutable = 63

rpmTagHeaderSignatures :: Int32
rpmTagHeaderSignatures = 62

rpmTagHeaderImg :: Int32
rpmTagHeaderImg = 61

rpmTagHeaderI18nTable :: Int32
rpmTagHeaderI18nTable = 100

rpmtagName :: Int
rpmtagName = 1000 -- string

rpmtagVersion :: Int
rpmtagVersion = 1001 -- string

rpmtagRelease :: Int
rpmtagRelease = 1002 -- string

rpmtagArch :: Int
rpmtagArch = 1022 -- string

-- $tagTypeDoc
-- Ids for types of data used in the header blobs. This is not exhaustive and
-- only includes types for data we actually read from the header. More types can
-- be found
-- [here](https://github.com/csasarak/go-rpmdb/blob/modified-cmd/pkg/rpmtags.go#L2)

-- $tagTypeDoc

rpmStringType :: Word32
rpmStringType = 6

rpmStringArrayType :: Word32
rpmStringArrayType = 8

rpmI18NstringType :: Word32
rpmI18NstringType = 9

rpmInt64Type :: Word32
rpmInt64Type = 5

rpmInt32Type :: Word32
rpmInt32Type = 4

-- |This corresponds to a type of 'RpmBin' but has a special meaning when the
-- first entry info has this type.
-- [Reference](https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L15)
regionTagType :: RpmTagType
regionTagType = RpmBin

-- $dataSizeDoc
-- Sizes for data that may be found in a header blob, in bytes.

-- |Size in bytes of an 'EntryInfo'.
entryInfoSize :: Int32
entryInfoSize = 16

-- |Maximum number of bytes allowed in a header. Defined
-- [here](https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L18).
headerMaxBytes :: Int32
headerMaxBytes = 256 * 1024 * 1024

data RpmTagType =
  RpmNull
  | RpmChar
  | RpmInt8
  | RpmInt16
  | RpmInt32
  | RpmInt64
  | RpmString
  | RpmBin
  | RpmStringArray
  | RpmI18nString
  -- | RpmNonExistentType
  deriving (Eq, Show, Ord)

-- | The size of each type in bytes, -1 if it's a string
-- and 1 if it's binary
headerTypeSize :: RpmTagType -> Int32
headerTypeSize ty = case ty of
  RpmNull        -> 0
  RpmChar        -> 1
  RpmInt8        -> 1
  RpmInt16       -> 2
  RpmInt32       -> 4
  RpmInt64       -> 8
  RpmString      -> -1
  RpmBin         -> 1
  RpmStringArray -> -1
  RpmI18nString  -> -1

intToHeaderType :: Word32 -> Maybe RpmTagType
intToHeaderType i =
  case i of
    0 -> Just RpmNull
    1 -> Just RpmChar
    2 -> Just RpmInt8
    3 -> Just RpmInt16
    4 -> Just RpmInt32
    5 -> Just RpmInt64
    6 -> Just RpmString
    7 -> Just RpmBin
    8 -> Just RpmStringArray
    9 -> Just RpmI18nString
    _ -> Nothing

-- |The number of 'EntryInfo's to expect in a v3 header region.
-- [Reference Implementation](https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L14).
regionTagCount :: Int32
-- Logically, I'm not sure why this is derived from the size in bytes of an
-- entryInfo in both the c code and the go code. It seems like the only
-- relationship between the two is they happen to both be the number 16.
regionTagCount = entryInfoSize

-- |General characteristics about a header blob. Official documentation can be
-- found [here](https://refspecs.linuxbase.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/pkgformat.html)
--
-- In summary, a header blob is a key-value data-store for different attributes
-- of an RPM package.  Each key-value pair is called a tag. For example, the
-- package name attribute for a package has @tag@ of @1000@ and is a string.
--
-- The first part of a header blob is an index which describes all of the tags that it
-- contains as well as their types. In this implementation, these entries are called 'EntryInfo'.
-- Following the index is a data area which contain the actual values for a tag.
--
-- One caveat for a header is that v4 headers are structured differently than v3
-- headers, but include the v3 original in a tag that describes where in the
-- blob the original v3 data region is. The v3 data is read in first in a
-- separate step before any additional info from the v4 part of the header. More
-- information can be found
-- [here](https://rpm-software-management.github.io/rpm/manual/hregions.html)
data HeaderBlob = HeaderBlob
  { -- |Count of entries in the index area
    indexCount :: IndexCount
  , -- |The size of the data area in bytes
    dataLength :: Int32
  , -- |The offset from the beginning of the blob data where tag values are
    dataStart :: Int32
  , -- |The offset where the data area ends
    dataEnd :: Int32
  , -- |The metadata entries for each tag
    entryInfos :: NonEmpty EntryInfo
  , -- |The number of EntryInfo's in the original v3 header region. 0 if none exists.
    regionIndexCount :: IndexCount
  }
  deriving (Show, Eq)

-- |Structure for metadata describing the type, location, and size of a tag value
-- pair in a 'HeaderBlob'
data EntryInfo = EntryInfo
  { -- |ID for which piece of RPM data this is. More information on available
    -- tags can be found [here](https://rpm-software-management.github.io/rpm/manual/tags.html).
    tag :: Int32
  , -- |The type of data that this tag is. See the
    -- [docs](https://refspecs.linuxbase.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/pkgformat.html)
    -- Table 24-5.
    tagType :: RpmTagType
  , -- |The offset from the beginning of 'HeaderBlob's 'dataStart' that data for
    -- this entry can be found.
    offset :: Int32
  , -- | The count of data items to expect. For simple scalar types like numbers
    -- or strings this will be 1. For composite types such as arrays it may be
    -- more.
    count :: Word32
  }
  deriving (Show, Eq, Ord)

-- | Read initial metadata items, such as data lengths, locations, and index entries from
-- a header blob.
--
-- This function is roughly equivalent to @hdrblobInit@ from the original [go
-- code](https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L105).
readHeaderMetaData :: BLS.ByteString -> Either (BLS.ByteString, ByteOffset, String) HeaderBlob
readHeaderMetaData bs =
  case runGetOrFail readHeader bs of
    Right (_, _, res) -> Right res
    Left l -> Left l
  where
    readHeader = do
      indexCount <- IndexCount <$> label "Read indexCount" getInt32be
      let indexCount' = fromIntegral indexCount

      when (indexCount < 1) $
        fail "region no tags error"

      dataLength <- label "Read dataLength" getInt32be
      let dataStart = calcDataStart indexCount'
      let pvLength = dataAndIndexLen + dataLength + indexCount' * entryInfoSize
      let dataEnd = dataStart + dataLength

      when (pvLength >= headerMaxBytes) $
        fail "blob size bad"

      entryInfos <- readEntries indexCount

      case getV3RegionCount entryInfos dataLength dataStart bs of
        Left s -> fail s
        Right regionIndexCount -> pure HeaderBlob{..}

    dataAndIndexLen :: Int32
    dataAndIndexLen = 8

    -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L116
    calcDataStart :: Int32 -> Int32
    calcDataStart indexLength =
      dataAndIndexLen
        + indexLength
          * entryInfoSize

-- |A count of entries in a tag index
newtype IndexCount = IndexCount Int32
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

emptyRegionInfo :: IndexCount
emptyRegionInfo = IndexCount 0

-- | Report the number entries in this blob corresponding to a v3 data region
-- and verify its correctness. In the case of a regular v3 blob, this will be
-- 0. In the case of v4 it may be more.
--
-- This is inspired by [hdrblobVerifyRegion](https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L263).
getV3RegionCount :: NonEmpty.NonEmpty EntryInfo -> Int32 -> Int32 -> BLS.ByteString -> Either String IndexCount
getV3RegionCount entryInfos dataLength dataStart blobData = do
  let EntryInfo{..} = NonEmpty.head entryInfos
  let regionTag =
        if tag
          `elem` [ rpmTagHeaderImg
                 , rpmTagHeaderSignatures
                 , rpmTagHeaderImmutable
                 ]
          then tag
          else 0

  if regionTag == 0
    then -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L276
      pure emptyRegionInfo
    else do
      unless (tagType == regionTagType && count == fromIntegral regionTagCount) $
        Left "invalid region tag"

      when (headerCheckRange dataLength (offset + regionTagCount)) $
        Left "invalid region offset"

      let regionEnd = dataStart + offset
      EntryInfo{offset = trailerOffset} <-
        runGetOrFail'
          (label "read trailer" readEntry)
          (bsSubString regionEnd (regionEnd + regionTagCount) blobData)

      -- The negation has a special meaning according to the original code.
      -- https://github.com/rpm-software-management/rpm/blob/061ba962297eba71ecb1b45a4133cbbd86f8450e/lib/header.c#L1843
      let negOffset = negate trailerOffset

      pure $
        IndexCount (negOffset `div` entryInfoSize)
  where
    trd (_, _, c) = c

    runGetOrFail' r = bimap trd trd . runGetOrFail r

    headerCheckRange :: Int32 -> Int32 -> Bool
    headerCheckRange dataLen offset = offset < 0 || offset > dataLen

-- | Metadata about an entry info
data IndexEntry =
  IndexEntry
  { -- | The expected length of the entryData, in bytes
    info :: EntryInfo
  , -- | The actual data for the entry
    entryLength :: Int32
  , entryData :: BLS.ByteString
  }
  deriving (Eq, Show, Ord)

-- |Given header metadata, read out the sub-slice of the bytestring blob that
-- corresponds to each 'EntryInfo'.
--
-- Rought equivalent to
-- [hdrblobImport](https://github.com/knqyf263/go-rpmdb/blob/a9e3110d8ee1fd3b0798a7abe8c59230bd265cd3/pkg/entry.go#L150)
-- in the original go code
readHeaderBlobTagData :: BLS.ByteString -> HeaderBlob -> Either String [IndexEntry]
readHeaderBlobTagData bs HeaderBlob{..} = do
  let firstEntry = NonEmpty.head entryInfos
  if tag firstEntry >= rpmTagHeaderI18nTable
    then
    -- v3 entries, these seem to be uncommon. They are distinct from > v4
    -- entries in that they don't have a specialized region for v3 data, which
    -- is why the function doesn't skip the first element of entryInfos
      bimap
        ("Failed to parse legacy index entries: " <>)
        fst
        (regionSwab bs (NonEmpty.toList entryInfos) dataStart dataEnd 0)
    else do
      -- v4 entries, since we need to read the legacy v3 entries first we'll
      -- try to pull out all 'IndexEntry's in just the v3 header region.
      let regionIndexCount' = if offset firstEntry == 0 then indexCount else regionIndexCount
      (indexEntries, ieReadLen) <- regionSwab bs (subList 1 (fromIntegral regionIndexCount') entryInfos) dataStart dataEnd 0

      (allEntries, dataLenWithDribble) <-
        if fromIntegral regionIndexCount' < length entryInfos
          then calculateDribbleEntries indexEntries regionIndexCount ieReadLen
          else Right (indexEntries, ieReadLen)

      let totalLen = dataLenWithDribble + regionTagCount
      if totalLen /= dataLength
        then Left $ printf "The caculated length (%d) is different than the data length (%d)" totalLen dataLength
        else Right allEntries
  where
    subList :: Int32 -> Int32 -> NonEmpty a -> [a]
    subList start end = take (fromIntegral $ end - start) . NonEmpty.drop (fromIntegral start)

    calculateDribbleEntries :: [IndexEntry] -> IndexCount -> Int32 -> Either String ([IndexEntry], Int32)
    calculateDribbleEntries indexEntries ril startDataLength = do
      let dribbleInfos = NonEmpty.drop (fromIntegral ril) entryInfos
      (dribbleEntries, ieReadLen) <- regionSwab bs dribbleInfos dataStart dataEnd startDataLength
      if ieReadLen < 0
        then Left "invalid length of dribble entries"
        else
          Right
            ( Map.elems
                . Map.fromList
                . map (\ie -> (fromIntegral . tag . info $ ie, ie))
                $ dribbleEntries <> indexEntries
            , ieReadLen
            )

-- Returns a list of index entries that were read as well as their length in bytes
regionSwab :: BLS.ByteString -> [EntryInfo] -> Int32 -> Int32 -> Int32 -> Either String ([IndexEntry], Int32)
regionSwab bs entryInfos dataStart dataEnd startDataLength = do
  foldM calcIndexEntry ([], startDataLength) (zip [0 ..] entryInfos)
  where
    calcIndexEntry (entries, runningDataLength) (idx, entryInfo) = do
      newIdxEntry <- swabEntry idx entryInfo
      alignDifference <- alignDiff (tagType entryInfo) runningDataLength
      let rdl = alignDifference + runningDataLength + entryLength newIdxEntry
      Right (newIdxEntry : entries, rdl)

    alignDiff :: RpmTagType -> Int32 -> Either String Int32
    alignDiff ty alignSize = do
      let tSize = headerTypeSize ty
      let diff = tSize - (alignSize `mod` tSize)
      if tSize > 1 && diff /= tSize
        then Right diff
        else Right 0

    entryInfosV :: V.Vector (Int, EntryInfo)
    entryInfosV = V.indexed . V.fromList $ entryInfos

    lastIdx :: Int32
    lastIdx = fromIntegral $ V.length entryInfosV - 1

    swabEntry :: Int32 -> EntryInfo -> Either String IndexEntry
    swabEntry i info = do
      let currOffset = offset info
      let start = dataStart + currOffset
      let tType = tagType info

      entryLength <-
        do
          if i < lastIdx && tType `elem` [RpmString, RpmStringArray, RpmI18nString]
            then case offset . snd <$> entryInfosV V.!? fromIntegral (i + 1) of
              -- this Nothing shouldn't happen since we checked it in
              -- the if exp above
              Nothing -> Left $ "Failed to read index " <> show i
              Just nextOffset -> Right $ nextOffset - currOffset
            else do
              dataLength <- calcDataLength bs tType (count info) start dataEnd
              if dataLength < 0
                then Left "invalid data length"
                else Right dataLength

      let end = start + entryLength
      if (start >= dataEnd)
        then do Left "invalid data offset"
        else
          Right
            IndexEntry
              { info = info
              , entryLength = entryLength
              , entryData = bsSubString start end bs
              }

-- | Calculate the length of a given piece of data in bytes
calcDataLength :: BLS.ByteString -> RpmTagType -> Word32 -> Int32 -> Int32 -> Either String Int32
calcDataLength bs ty count start dataEnd =
  case ty of
    RpmString -> if count /= 1 then
                   Left $ "count for string == " <> show count <> " it should == 1."
                 else
                   stringTagLength 1
    RpmStringArray -> stringTagLength count
    RpmI18nString -> stringTagLength count
    _ -> do let len = (fromIntegral count) * (headerTypeSize ty)
            if len < 0 || dataEnd > 0 && start + fromIntegral len > dataEnd
              then Left $ "Invalid calculated len: " <> show len
              else Right $ fromIntegral len

  where
    stringTagLength :: Word32 -> Either String Int32
    stringTagLength strCount =
      if start >= dataEnd
        then Left $ "String start (" <> show start <> ") >= end (" <> show dataEnd <> ")"
        else do
          let substr = bsSubString start dataEnd bs
          case take (fromIntegral strCount) . BLS.findIndices (== 0) $ substr of
            (x : xs) -> Right . fromIntegral $ NonEmpty.last (x NonEmpty.:| xs) + 1
            _ -> Left $ printf "Couldn't find ending null byte for %d string(s) in substring %s" strCount (show substr)

-- | Substring of a byte string. Includes stop, but not end for range
-- [start, end).
bsSubString :: Int32 -> Int32 -> BLS.ByteString -> BLS.ByteString
bsSubString start end = BLS.take (fromIntegral $ end - start) . BLS.drop (fromIntegral start)

readEntries :: IndexCount -> Get (NonEmpty EntryInfo)
readEntries indexLength =
  liftA2 (:|) readEntry $ replicateM (fromIntegral (indexLength - 1)) readEntry

readEntry :: Get EntryInfo
readEntry =
  -- The original code reads these as little endian:
  -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L127
  -- However, the original code looks like it simply copies bytes
  -- sequentially into a buffer that is then treated like an entryInfo
  -- structure. That means higher bytes in a word are least-significant,
  -- corresponding to big-endian when read individually. Big endian is also
  -- network order.
  EntryInfo
    <$> label "Read tag" getInt32be
    <*> label "Read type" readTagType
    <*> label "Read offset" getInt32be
    <*> label "Read count" getWord32be
  where readTagType =
          do tyNum <- getWord32be
             case intToHeaderType tyNum of
               Just ty -> pure ty
               Nothing -> fail $ "Invalid type number: " <> show tyNum


data PkgInfo = PkgInfo
  { pkgName :: Text
  , pkgVersion :: Text
  , pkgRelease :: Text
  , pkgArch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- More information than what gets extracted is available from the blob. See the
-- different tag values here:
-- https://github.com/csasarak/go-rpmdb/blob/modified-cmd/pkg/package.go#L50
getPkgInfo :: [IndexEntry] -> Either String PkgInfo
getPkgInfo ies =
  PkgInfo
    <$> readTag rpmtagName
    <*> readTag rpmtagVersion
    <*> readTag rpmtagRelease
    <*> Right (readOptionalTag rpmtagArch)
  where
    tagMap =
      fmap convertIndexEntryData
        . Map.fromList
        . map (\i -> (fromIntegral . tag . info $ i, i))
        $ ies

    readTag :: Int -> Either String Text
    readTag tag =
      maybeToRight (printf "Failed to read required tag %d" tag)
        . join
        . Map.lookup tag
        $ tagMap

    readOptionalTag :: Int -> Maybe Text
    readOptionalTag tag = join $ Map.lookup tag tagMap

convertIndexEntryData :: IndexEntry -> Maybe Text
convertIndexEntryData ie =
  case tagNum of
    RpmString -> Just dataAsText
    _         -> Nothing
  where
    eData :: BLS.ByteString
    eData = entryData ie

    tagNum :: RpmTagType
    tagNum = tagType . info $ ie

    nullChr :: Word8
    nullChr = fromIntegral . ord $ '\0'

    dataAsText :: Text
    dataAsText = decodeUtf8 . BS.dropWhileEnd (== nullChr) . BLS.toStrict $ eData

readPackageInfo :: BLS.ByteString -> Either String PkgInfo
readPackageInfo bs = do
  blob <- first (\(_, _, s) -> s) $ readHeaderMetaData bs
  ies <- readHeaderBlobTagData bs blob
  getPkgInfo ies
