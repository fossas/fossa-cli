{-# LANGUAGE RecordWildCards #-}

module Data.Rpm.DbHeaderBlob.Internal (
  readPackageInfo,
  PkgInfo (..),
  -- everything below here is exposed for testing
  HeaderBlob (..),
  EntryInfo (..),
  IndexEntry (..),
  RegionInfo (..),
  readHeaderMetaData,
  hdrblobVerifyRegion,
  emptyRegionInfo,
  calcDataLength,
  -- consts
  rpmTagHeaderImg,
  rpmTagHeaderImmutable,
  rpmTagHeaderSignatures,
  rpmStringType,
  regionTagType,
  regionTagCount,
  hdrblobImport,
  rpmStringArrayType,
  rpmI18NstringType,
  rpmInt64Type,
  rpmInt32Type,
) where

import Control.Applicative (liftA2)
import Control.Monad (foldM, join, replicateM, unless, when)
import Data.Bifunctor (bimap, first)
import Data.Binary.Get (ByteOffset, Get, getInt32be, getWord32be, label, runGetOrFail)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BLS
import Data.Char (ord)
import Data.Int (Int32)
import Data.IntMap qualified as Map
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import Text.Printf (printf)

-- Constants from
-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/rpmtags.go#L3
rpmTagHeaderImmutable :: Int32
rpmTagHeaderImmutable = 63

rpmTagHeaderSignatures :: Int32
rpmTagHeaderSignatures = 62

rpmTagHeaderImg :: Int32
rpmTagHeaderImg = 61

rpmTagHeaderI18nTable :: Int32
rpmTagHeaderI18nTable = 100

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

-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L71
-- Size in bytes.
entryInfoSize :: Int32
entryInfoSize = 16

-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L18
headerMaxBytes :: Int32
headerMaxBytes = 256 * 1024 * 1024

-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L14
regionTagCount :: Int32
regionTagCount = entryInfoSize

-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L15
regionTagType :: Word32
regionTagType = 7

typeSizes :: V.Vector Int32
typeSizes =
  V.fromList
    [ 0 -- RPM_NULL_TYPE
    , 1 -- RPM_CHAR_TYPE
    , 1 -- RPM_INT8_TYPE
    , 2 -- RPM_INT16_TYPE
    , 4 -- RPM_INT32_TYPE
    , 8 -- RPM_INT64_TYPE
    , -1 -- RPM_STRING_TYPE
    , 1 -- RPM_BIN_TYPE
    , -1 -- RPM_STRING_ARRAY_TYPE
    , -1 -- RPM_I18NSTRING_TYPE
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

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
    indexCount :: Int32
  , -- |The size of the data area in bytes
    dataLength :: Int32
  , -- |The offset from the beginning of the blob data where tag values are
    dataStart :: Int32
  , -- |The offset where the data area ends
    dataEnd :: Int32
  , -- |The metadata entries for each tag
    entryInfos :: NonEmpty EntryInfo
  , -- |The number of EntryInfo's in the original v3 header region. 0 if none exists.
    regionIndexCount :: Int32
  }
  deriving (Show, Eq)

data EntryInfo = EntryInfo
  { -- |ID for which piece of RPM data this is. More information on available
    -- tags can be found here: https://rpm-software-management.github.io/rpm/manual/tags.html
    tag :: Int32
  , -- |The type of data that this tag is. See https://refspecs.linuxbase.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/pkgformat.html
    -- Table 24-5.
    tagType :: Word32
  , -- |The offset from the beginning of 'HeaderBlob's 'dataStart' that data for
    -- this entry can be found.
    offset :: Int32
  , -- | The count of data items to expect. For simple scalar types like numbers
    -- or strings this will be 1. For composite types such as arrays it may be
    -- more.
    count :: Word32
  }
  deriving (Show, Ord, Eq)

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
      indexCount <- label "Read indexCount" getInt32be

      when (indexCount < 1) $
        fail "region no tags error"

      dataLength <- label "Read dataLength" getInt32be
      let dataStart = calcDataStart indexCount
      let pvLength = dataAndIndexLen + dataLength + indexCount * entryInfoSize
      let dataEnd = dataStart + dataLength

      when (pvLength >= headerMaxBytes) $
        fail "blob size bad"

      entryInfos <- readEntries indexCount

      case hdrblobVerifyRegion entryInfos dataLength dataStart bs of
        Left s -> fail s
        Right
          RegionInfo
            { rIl = regionIndexCount
            } -> pure HeaderBlob{..}

    dataAndIndexLen :: Int32
    dataAndIndexLen = 8

    -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L116
    calcDataStart :: Int32 -> Int32
    calcDataStart indexLength =
      dataAndIndexLen
        + indexLength
          * entryInfoSize

newtype RegionInfo = RegionInfo
  { rIl :: Int32
  }
  deriving (Show, Eq)

emptyRegionInfo :: RegionInfo
emptyRegionInfo =
  RegionInfo
    { rIl = 0
    }

-- This is inspired by hdrblobVerifyRegion:
-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L263
hdrblobVerifyRegion :: NonEmpty.NonEmpty EntryInfo -> Int32 -> Int32 -> BLS.ByteString -> Either String RegionInfo
hdrblobVerifyRegion entryInfos dataLength dataStart blobData = do
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

      when (hdrCheckRange dataLength (offset + regionTagCount)) $
        Left "invalid region offset"

      let regionEnd = dataStart + offset
      EntryInfo{offset = trailerOffset} <-
        runGetOrFail'
          (label "read trailer" readEntry)
          (bsSubString regionEnd (regionEnd + regionTagCount) blobData)

      -- The negation has a special meaning according to the original code.
      -- https://github.com/rpm-software-management/rpm/blob/061ba962297eba71ecb1b45a4133cbbd86f8450e/lib/header.c#L1843
      let negOffset = negate trailerOffset

      pure
        emptyRegionInfo
          { rIl = negOffset `div` entryInfoSize
          }
  where
    trd (_, _, c) = c

    runGetOrFail' r = bimap trd trd . runGetOrFail r

    hdrCheckRange :: Int32 -> Int32 -> Bool
    hdrCheckRange dataLen offset = offset < 0 || offset > dataLen

data IndexEntry = IndexEntry
  { info :: EntryInfo
  , entryLength :: Int32
  , entryData :: BLS.ByteString
  }
  deriving (Eq, Ord, Show)

-- Inspired by https://github.com/knqyf263/go-rpmdb/blob/a9e3110d8ee1fd3b0798a7abe8c59230bd265cd3/pkg/entry.go#L150
hdrblobImport :: BLS.ByteString -> HeaderBlob -> Either String [IndexEntry]
hdrblobImport bs HeaderBlob{..} = do
  let firstEntry = NonEmpty.head entryInfos

  if tag firstEntry >= rpmTagHeaderI18nTable
    then -- v3 entries, these seem to be uncommon

      bimap
        (printf "Failed to parse legacy index entries: %s")
        fst
        (regionSwab bs (NonEmpty.toList entryInfos) dataStart dataEnd 0)
    else do
      -- v4 entries
      let regionIndexLength' = if offset firstEntry == 0 then indexCount else regionIndexCount
      (indexEntries, ieReadLen) <- regionSwab bs (subList 1 regionIndexLength' entryInfos) dataStart dataEnd 0

      (allEntries, dataLenWithDribble) <-
        if fromIntegral regionIndexLength' < length entryInfos
          then calculateDribbleEntries indexEntries regionIndexLength' ieReadLen
          else Right (indexEntries, ieReadLen)

      let totalLen = dataLenWithDribble + regionTagCount
      if totalLen /= dataLength
        then Left $ printf "The caculated length (%d) is different than the data length (%d)" totalLen dataLength
        else Right allEntries
  where
    subList :: Int32 -> Int32 -> NonEmpty a -> [a]
    subList start end = take (fromIntegral $ end - start) . NonEmpty.drop (fromIntegral start)

    calculateDribbleEntries :: [IndexEntry] -> Int32 -> Int32 -> Either String ([IndexEntry], Int32)
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

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing = Left b

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

    alignDiff :: Word32 -> Int32 -> Either String Int32
    alignDiff ty alignSize = do
      tSize <- maybeToEither ("aligning diff: tag type " <> show ty <> " is invalid") (typeSizes V.!? fromIntegral ty)
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
          typeSize <- maybeToEither ("No such type size: " <> show tType) (typeSizes V.!? fromIntegral tType)
          if i < lastIdx && typeSize == -1
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
calcDataLength :: BLS.ByteString -> Word32 -> Word32 -> Int32 -> Int32 -> Either String Int32
calcDataLength bs ty count start dataEnd
  | ty == rpmStringType =
    if count /= 1
      then Left $ "count for string == " <> show count <> " it should == 1."
      else strtaglen 1
  | ty `elem` [rpmStringArrayType, rpmI18NstringType] = strtaglen count
  | otherwise = do
    t <- maybeToEither ("Nonexistent typesize: " <> show ty) (typeSizes V.!? fromIntegral ty)
    if t == -1
      then Left $ "Typesize " <> show ty <> " can't be -1"
      else do
        -- typeSizes has 16 members, so it should never fail to read one for (ty .&. 0xf)
        let mLen = ((fromIntegral count) *) <$> (typeSizes V.!? (fromIntegral ty .&. 0xf))
        len <- maybeToEither ("Nonexistent tag type " <> show ty) mLen
        if len < 0 || dataEnd > 0 && start + fromIntegral len > dataEnd
          then Left $ "Invalid calculated len: " <> show len
          else Right $ fromIntegral len
  where
    strtaglen :: Word32 -> Either String Int32
    strtaglen strCount =
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

readEntries :: Int32 -> Get (NonEmpty EntryInfo)
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
    <*> label "Read type" getWord32be
    <*> label "Read offset" getInt32be
    <*> label "Read count" getWord32be

data PkgInfo = PkgInfo
  { pkgName :: Text
  , pkgVersion :: Text
  , pkgRelease :: Text
  , pkgArch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

rpmtagName :: Int
rpmtagName = 1000 -- string

rpmtagVersion :: Int
rpmtagVersion = 1001 -- string

rpmtagRelease :: Int
rpmtagRelease = 1002 -- string

rpmtagArch :: Int
rpmtagArch = 1022 -- string

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
      maybeToEither (printf "Failed to read required tag %d" tag)
        . join
        . Map.lookup tag
        $ tagMap

    readOptionalTag :: Int -> Maybe Text
    readOptionalTag tag = join $ Map.lookup tag tagMap

convertIndexEntryData :: IndexEntry -> Maybe Text
convertIndexEntryData ie =
  if tagNum == rpmStringType
    then Just dataAsText
    else Nothing
  where
    eData :: BLS.ByteString
    eData = entryData ie

    tagNum :: Word32
    tagNum = tagType . info $ ie

    nullChr :: Word8
    nullChr = fromIntegral . ord $ '\0'

    dataAsText :: Text
    dataAsText = decodeUtf8 . BS.dropWhileEnd (== nullChr) . BLS.toStrict $ eData

readPackageInfo :: BLS.ByteString -> Either String PkgInfo
readPackageInfo bs = do
  blob <- first (\(_, _, s) -> s) $ readHeaderMetaData bs
  ies <- hdrblobImport bs blob
  getPkgInfo ies
