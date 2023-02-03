{-# LANGUAGE RecordWildCards #-}

-- | Description: These are internal functions for parsing an RPM package header blob. The
-- main entry-point is 'readPackageInfo'.
module Data.Rpm.DbHeaderBlob.Internal (
  -- * Main interface

  -- | You should not use this module directly. Use 'Data.Rpm.DbHeaderBlob'
  -- instead.
  PkgInfo (..),
  readPackageInfo,

  -- * Internal Types
  HeaderBlob (..),
  EntryMetadata (..),
  TagValueData (..),
  IndexCount (..),
  RpmTagType (..),
  RpmTag (..),

  -- * Internal Functions
  readHeaderMetaData,
  getV3RegionCount,
  calcDataLength,
  readHeaderBlobTagData,

  -- * Magic numbers
  regionTagCount,
  regionTagType,
) where

import Control.Applicative (liftA2)
import Control.Monad (foldM, replicateM, unless, when)
import Data.Bifunctor (bimap, first)
import Data.Binary.Get (ByteOffset, Get, getInt32be, getWord32be, label, runGetOrFail)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BLS
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32)
import Text.Printf (printf)

-- | RPM tags (keys). This is not exhaustive, see the full list in the
-- [origin go-rpmdb implementation](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/rpmtags.go#L3).
data RpmTag
  = -- When modifying this type, it is important to order data constructors like their numbers.
    -- When checking the first entry in the index, tag  >= TagHeaderI18nTable (1000) indicates a v3 header.
    -- Adding new constructors out of order risks breaking that.
    -- New tags also need to be added to 'intToRpmTag' to be usable.
    TagHeaderImage -- 61
  | TagHeaderSignatures -- 62
  | TagHeaderImmutable -- 63
  | TagHeaderI18nTable -- 100
  | TagName -- 1000
  | TagVersion -- 1001
  | TagRelease -- 1002
  | TagEpoch -- 1003
  | TagArchitecture -- 1022
  | -- | Tag values that we aren't interested in that aren't in spec/reference implementation.
    -- This allows reading those tag entries without failing on unknown tags.
    -- If a new, known tag is needed in the future, that tag should get its own entry above.
    TagOther Int32
  deriving (Eq, Ord, Show)

intToRpmTag :: Int32 -> RpmTag
intToRpmTag t =
  case t of
    61 -> TagHeaderImage
    62 -> TagHeaderSignatures
    63 -> TagHeaderImmutable
    100 -> TagHeaderI18nTable
    1000 -> TagName
    1001 -> TagVersion
    1002 -> TagRelease
    1003 -> TagEpoch
    1022 -> TagArchitecture
    n -> TagOther n

-- | This is a type of 'RpmBin' but has a special meaning when the first entry info in the blob has this type.
-- [Reference](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L15).
regionTagType :: RpmTagType
regionTagType = RpmBin

-- | Size in bytes of an 'EntryMetadata'.
entryMetadataSize :: Int32
entryMetadataSize = 16

-- | Maximum number of bytes allowed in a header.
-- Defined [here](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L18).
headerMaxBytes :: Int32
headerMaxBytes = 256 * 1024 * 1024

-- | The number of 'EntryMetadata's to expect in a v3 header region.
-- [Reference Implementation](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L14).
regionTagCount :: Int32
-- In the reference implementation REGION_TAG_COUNT = sizeof(entryInfo).
-- One is a count and the other is a size in bytes.
-- Besides the fact that both = 16 I don't now why thatt is, so I chose not to replicate it here.
regionTagCount = 16

data RpmTagType
  = RpmNull
  | RpmChar
  | RpmInt8
  | RpmInt16
  | RpmInt32
  | RpmInt64
  | RpmString
  | RpmBin
  | RpmStringArray
  | RpmI18nString
  deriving (Eq, Show, Ord)

-- | The size of each type in bytes, -1 if it's a string
-- and 1 if it's binary
headerTypeSize :: RpmTagType -> Int32
headerTypeSize ty = case ty of
  RpmNull -> 0
  RpmChar -> 1
  RpmInt8 -> 1
  RpmInt16 -> 2
  RpmInt32 -> 4
  RpmInt64 -> 8
  RpmString -> -1
  RpmBin -> 1
  RpmStringArray -> -1
  RpmI18nString -> -1

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

-- | Structure representing the data we can read out of an RPM package header blob.
-- Official documentation can be
-- found [here](https://refspecs.linuxbase.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/pkgformat.html)
--
-- In summary, a header blob is a key-value data-store for different attributes of an RPM package.
-- Keys are called 'tags' in RPM docs/code. For example,
-- the package name attribute for a package has @tag@ of @1000@ and is a string.
--
-- The first part of a header blob is an index which describes all of the tags that it contains as well as their types.
-- In this implementation, these entries are called 'EntryMetadata',
-- but are called @entryInfo@ in the reference implementation.
-- Following the index is a data area which contain the actual values for a tag.
--
-- v4 headers are structured differently than v3 headers.
-- v4 headers include the v3 original v3 tags in a tag that describes where in the blob the v3 data region is.
-- The v3 data is read in first in a separate step before any additional info from the v4 part of the header.
-- More information can be found [here](https://rpm-software-management.github.io/rpm/manual/hregions.html)
data HeaderBlob = HeaderBlob
  { -- | Count of entries in the index area
    indexCount :: IndexCount
  , -- | The size of the data area in bytes
    dataLength :: Int32
  , -- | The offset from the beginning of the blob data where tag values are
    dataStart :: Int32
  , -- | The offset where the data area ends
    dataEnd :: Int32
  , -- | The metadata entries for each tag
    entryMetadatas :: NonEmpty EntryMetadata
  , -- | The number of 'EntryMetadata''s in the original v3 header region. 0 if none exists.
    regionIndexCount :: IndexCount
  }
  deriving (Show, Eq)

-- | Structure for describing the type, location, and size of a tag-value pair in a 'HeaderBlob'.
-- This roughly corresponds to [entryInfo](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L63) in the original Go code.
data EntryMetadata = EntryMetadata
  { -- | ID for which piece of RPM data this is. More information on available
    -- tags can be found [here](https://rpm-software-management.github.io/rpm/manual/tags.html).
    tag :: RpmTag
  , -- | The type of data that this tag is. See the
    -- [docs](https://refspecs.linuxbase.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/pkgformat.html)
    -- Table 24-5.
    tagType :: RpmTagType
  , -- | The offset from the beginning of 'HeaderBlob's 'dataStart' that data for
    -- this entry can be found.
    offset :: Int32
  , -- | The count of data items to expect. For simple scalar types like numbers
    -- or strings this will be 1. For composite types such as arrays it may be
    -- more.
    count :: Word32
  }
  deriving (Show, Eq, Ord)

-- | Read initial metadata items, such as data lengths, locations, and index entries from a header blob.
-- This function is roughly equivalent to @hdrblobInit@ from the original [go code](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L105).
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
      let pvLength = dataAndIndexLen + dataLength + indexCount' * entryMetadataSize
      let dataEnd = dataStart + dataLength

      when (pvLength >= headerMaxBytes) $
        fail "blob size bad"

      entryMetadatas <- readEntries indexCount

      case getV3RegionCount entryMetadatas dataLength dataStart bs of
        Left s -> fail s
        Right regionIndexCount -> pure HeaderBlob{..}

    dataAndIndexLen :: Int32
    dataAndIndexLen = 8

    -- https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L116
    calcDataStart :: Int32 -> Int32
    calcDataStart indexLength =
      dataAndIndexLen
        + indexLength
          * entryMetadataSize

-- | A count of entries in a tag index
newtype IndexCount = IndexCount Int32
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

-- | Report the number entries in this blob corresponding to a v3 data region and verify the region's correctness.
-- In the case of a regular v3 blob, this is 0. In the case of v4 it is non-zero.
-- This is analagous to [hdrblobVerifyRegion](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L263).
getV3RegionCount :: NonEmpty.NonEmpty EntryMetadata -> Int32 -> Int32 -> BLS.ByteString -> Either String IndexCount
getV3RegionCount entryMetadatas dataLength dataStart blobData = do
  let EntryMetadata{..} = NonEmpty.head entryMetadatas
  if tag --  any of these 3 tags would indicate a v4 header, thus no v3 region
    `notElem` [ TagHeaderImage
              , TagHeaderSignatures
              , TagHeaderImmutable
              ]
    then -- This is a v3 header, so no special v3 region.
    -- https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L276
      pure 0
    else do
      -- There is a v3 region because this is a v4 header
      unless (tagType == regionTagType && count == fromIntegral regionTagCount) $
        Left "invalid region tag"

      when (headerCheckRange dataLength (offset + regionTagCount)) $
        Left "invalid region offset"

      let regionEnd = dataStart + offset
      EntryMetadata{offset = trailerOffset} <-
        runGetOrFail'
          (label "read trailer" readEntry)
          (bsSubString regionEnd (regionEnd + regionTagCount) blobData)

      -- The negation has a special meaning according to the original code.
      -- https://github.com/rpm-software-management/rpm/blob/061ba962297eba71ecb1b45a4133cbbd86f8450e/lib/header.c#L1843
      let negOffset = negate trailerOffset

      pure $
        IndexCount (negOffset `div` entryMetadataSize)
  where
    headerCheckRange :: Int32 -> Int32 -> Bool
    headerCheckRange dataLen offset = offset < 0 || offset > dataLen

trd :: (a, b, c) -> c
trd (_, _, c) = c

-- |'runGetOrFail' but don't include parse locations/remaining data in output
runGetOrFail' :: Get d -> BLS.ByteString -> Either String d
runGetOrFail' r = bimap trd trd . runGetOrFail r

-- | Container for metadata as well as the byte data for an entry in the header blob.
-- This type is equivalent to [indexEntry](https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L70) in the original Go code.
data TagValueData = TagValueData
  { -- | The expected length of the entryData, in bytes
    info :: EntryMetadata
  , entryLength :: Int32
  , -- | The actual data for the entry
    entryData :: BLS.ByteString
  }
  deriving (Eq, Show, Ord)

-- | Read out the sub-slice of the bytestring blob that corresponds to each 'EntryMetadata'.
-- This function will respect both v3 and v4 headers.
-- Roughly equivalent to [hdrblobImport](https://github.com/knqyf263/go-rpmdb/blob/a9e3110d8ee1fd3b0798a7abe8c59230bd265cd3/pkg/entry.go#L150) in the original go code
readHeaderBlobTagData :: BLS.ByteString -> HeaderBlob -> Either String [TagValueData]
readHeaderBlobTagData bs HeaderBlob{..} = do
  let firstEntry = NonEmpty.head entryMetadatas
  if tag firstEntry >= TagHeaderI18nTable
    then -- v3 entries, these seem to be uncommon. They are distinct from > v4
    -- entries in that they don't have a specialized region for v3 data, which
    -- is why the function doesn't skip the first element of entryMetadatas

      bimap
        ("Failed to parse legacy index entries: " <>)
        fst
        (extractEntryData bs (NonEmpty.toList entryMetadatas) dataStart dataEnd 0)
    else do
      -- v4 entries, since we need to read the legacy v3 entries first we'll
      -- try to pull out all 'TagValueData's in just the v3 header region.
      let regionIndexCount' = if offset firstEntry == 0 then indexCount else regionIndexCount
      (tagValueData, ieReadLen) <- extractEntryData bs (subList 1 (fromIntegral regionIndexCount') entryMetadatas) dataStart dataEnd 0

      (allEntries, dataLenWithDribble) <-
        if fromIntegral regionIndexCount' < length entryMetadatas
          then calculateDribbleEntries tagValueData regionIndexCount ieReadLen
          else Right (tagValueData, ieReadLen)

      let totalLen = dataLenWithDribble + regionTagCount
      if totalLen /= dataLength
        then Left $ printf "The calculated length (%d) is different than the data length (%d)" totalLen dataLength
        else Right allEntries
  where
    subList :: Int32 -> Int32 -> NonEmpty a -> [a]
    subList start end = take (fromIntegral $ end - start) . NonEmpty.drop (fromIntegral start)

    calculateDribbleEntries :: [TagValueData] -> IndexCount -> Int32 -> Either String ([TagValueData], Int32)
    calculateDribbleEntries tagValueDatas ril startDataLength = do
      let dribbleInfos = NonEmpty.drop (fromIntegral ril) entryMetadatas
      (dribbleEntries, ieReadLen) <- extractEntryData bs dribbleInfos dataStart dataEnd startDataLength
      if ieReadLen < 0
        then Left "invalid length of dribble entries"
        else
          Right
            ( Map.elems
                . Map.fromList
                . map (\ie -> (tag . info $ ie, ie))
                $ dribbleEntries <> tagValueDatas
            , ieReadLen
            )

-- Returns a list of index entries that were read as well as their combined length in bytes.
-- This is analogous to [regionSwab](https://github.com/knqyf263/go-rpmdb/blob/c11b1c45/pkg/entry.go#L336)
extractEntryData :: BLS.ByteString -> [EntryMetadata] -> Int32 -> Int32 -> Int32 -> Either String ([TagValueData], Int32)
extractEntryData bs entryMetadatas dataStart dataEnd startDataLength = do
  foldM getTagValueData ([], startDataLength) (zip [0 ..] entryMetadatas)
  where
    getTagValueData (entries, runningDataLength) (idx, entryMeta) = do
      newIdxEntry <- extractEntry idx entryMeta
      alignDifference <- alignDiff (tagType entryMeta) runningDataLength
      let rdl = alignDifference + runningDataLength + entryLength newIdxEntry
      Right (newIdxEntry : entries, rdl)

    alignDiff :: RpmTagType -> Int32 -> Either String Int32
    alignDiff ty alignSize = do
      let tSize = headerTypeSize ty
      let diff = tSize - (alignSize `mod` tSize)
      if tSize > 1 && diff /= tSize
        then Right diff
        else Right 0

    entryMetadatasV :: V.Vector (Int, EntryMetadata)
    entryMetadatasV = V.indexed . V.fromList $ entryMetadatas

    lastIdx :: Int32
    lastIdx = fromIntegral $ V.length entryMetadatasV - 1

    extractEntry :: Int32 -> EntryMetadata -> Either String TagValueData
    extractEntry i info = do
      let currOffset = offset info
      let start = dataStart + currOffset
      let tType = tagType info

      entryLength <-
        do
          if i < lastIdx && tType `elem` [RpmString, RpmStringArray, RpmI18nString]
            then case offset . snd <$> entryMetadatasV V.!? fromIntegral (i + 1) of
              -- this Nothing shouldn't happen since we checked it in the if exp above
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
            TagValueData
              { info = info
              , entryLength = entryLength
              , entryData = bsSubString start end bs
              }

-- | Calculate the length of a given piece of data in bytes
calcDataLength :: BLS.ByteString -> RpmTagType -> Word32 -> Int32 -> Int32 -> Either String Int32
calcDataLength bs ty count start dataEnd =
  case ty of
    RpmString ->
      if count /= 1
        then Left $ "count for string == " <> show count <> " it should == 1."
        else stringTagLength 1
    RpmStringArray -> stringTagLength count
    RpmI18nString -> stringTagLength count
    _ -> do
      let len = (fromIntegral count) * (headerTypeSize ty)
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

readEntries :: IndexCount -> Get (NonEmpty EntryMetadata)
readEntries indexLength =
  liftA2 (:|) readEntry $ replicateM (fromIntegral (indexLength - 1)) readEntry

readEntry :: Get EntryMetadata
readEntry =
  -- The original code reads these as little endian: https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/entry.go#L127
  -- However, it simply copies bytes sequentially into a buffer that is then treated as an entryInfo/entryMetadata structure.
  -- That means higher bytes in a word are least-significant or big-endian.
  -- Big endian is also network order.
  EntryMetadata
    <$> label "Read tag" (intToRpmTag <$> getInt32be)
    <*> label "Read type" readTagType
    <*> label "Read offset" getInt32be
    <*> label "Read count" getWord32be
  where
    readTagType =
      do
        tyNum <- getWord32be
        case intToHeaderType tyNum of
          Just ty -> pure ty
          Nothing -> fail $ "Invalid type number: " <> show tyNum

-- | Result data from reading a header blob. Everything is maybe because
-- not every package has everything, even though they're
data PkgInfo = PkgInfo
  { pkgName :: Maybe Text
  , pkgVersion :: Maybe Text
  , pkgRelease :: Maybe Text
  , pkgArch :: Maybe Text
  , -- | This package epoch won't match the printout from go-rpmdb.
    -- In that code the epoch is stored as an [int pointer](https://github.com/knqyf263/go-rpmdb/blob/c11b1c45080aec5141fea92cd1577f8aa1c8d2fc/pkg/package.go#L15).
    -- According to the tag [documentation](https://rpm-software-management.github.io/rpm/manual/tags.html) it is int32.
    -- There seems to be code which does dereference it properly,
    -- so this appears to be a bug in the print statements than in the implementation.
    pkgEpoch :: Maybe Int32
  }
  deriving (Eq, Ord, Show)

-- More information than what is returned in a 'PkgInfo' is available from the blob.
-- See the different tag values [here] (https://github.com/knqyf263/go-rpmdb/blob/9f953f9/pkg/package.go#L50)
getPkgInfo :: [TagValueData] -> Either String PkgInfo
getPkgInfo ies =
  PkgInfo
    <$> readTextTag TagName
    <*> readTextTag TagVersion
    <*> readTextTag TagRelease
    <*> readTextTag TagArchitecture
    <*> readInt32Tag TagEpoch
  where
    tagMap :: Map.Map RpmTag TagValueData
    tagMap =
      Map.fromList
        . map (\i -> (tag . info $ i, i))
        $ ies

    readTextTag :: RpmTag -> Either String (Maybe Text)
    readTextTag = readTag parseTextTag

    readInt32Tag :: RpmTag -> Either String (Maybe Int32)
    readInt32Tag = readTag parseInt32Tag

    readTag :: (TagValueData -> Either String a) -> RpmTag -> Either String (Maybe a)
    readTag tagParse tag =
      traverse tagParse (Map.lookup tag tagMap)

    parseTextTag :: TagValueData -> Either String Text
    parseTextTag TagValueData{..} =
      if (tagType info) /= RpmString
        then Left $ "Expected RpmString type for " <> show (tag info) <> " got " <> (show (tagType info))
        else Right . decodeUtf8 . BS.dropWhileEnd (== 0) . BLS.toStrict $ entryData

    parseInt32Tag :: TagValueData -> Either String Int32
    parseInt32Tag TagValueData{..} =
      if (tagType info) /= RpmInt32
        then
          Left $
            "Expected RpmInt32 type for "
              <> show (tag info)
              <> " got "
              <> show (tagType info)
        else runGetOrFail' getInt32be entryData

-- | Attempt to read a ByteString containing the data for a single RPM package header blob.
readPackageInfo :: BLS.ByteString -> Either String PkgInfo
readPackageInfo bs = do
  blob <- first (\(_, _, s) -> s) $ readHeaderMetaData bs
  ies <- readHeaderBlobTagData bs blob
  getPkgInfo ies
