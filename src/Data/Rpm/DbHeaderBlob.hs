{-# LANGUAGE RecordWildCards #-}

module Data.Rpm.DbHeaderBlob (
  headerBlobInit,
  HeaderBlob (..),
  EntryInfo (..),
  IndexEntry (..),
  RegionInfo (..),
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
import Control.Monad (foldM, replicateM, unless, when)
import Data.Bifunctor (bimap)
import Data.Binary.Get (ByteOffset, Get, getInt32be, getWord32be, label, runGetOrFail)
import Data.Bits ((.&.))
import Data.ByteString.Lazy qualified as BLS
import Data.Foldable (foldrM)
import Data.Int (Int32)
import Data.IntMap qualified as Map
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Vector qualified as V
import Data.Word (Word32)
import Debug.Trace (traceM, traceShowM)
import Text.Printf (printf)
import Data.List (findIndices)

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
    [ 0 -- !< RPM_NULL_TYPE
    , 1 -- !< RPM_CHAR_TYPE
    , 1 -- !< RPM_INT8_TYPE
    , 2 -- !< RPM_INT16_TYPE
    , 4 -- !< RPM_INT32_TYPE
    , 8 -- !< RPM_INT64_TYPE
    , -1 -- !< RPM_STRING_TYPE
    , 1 -- !< RPM_BIN_TYPE
    , -1 -- !< RPM_STRING_ARRAY_TYPE
    , -1 -- !< RPM_I18NSTRING_TYPE
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]

-- A package blob is laid out like this:
--
-- Blob:
-- indexLength - int32
-- dataLength - int32
-- entryInfo region - indexLength * 128 bits (indexLength entry infos)
--
-- entryInfo:
-- tag - int32
-- tagType - uint32
-- offset - int32
-- count - uint32

data HeaderBlob = HeaderBlob
  { indexLength :: Int32
  , dataLength :: Int32
  , dataStart :: Int32
  , dataEnd :: Int32
  , pvLength :: Int32 -- I do not know what this is
  , entryInfos :: NonEmpty EntryInfo
  , regionDataLength :: Int32
  , regionIndexLength :: Int32
  }
  deriving (Show, Eq)

data EntryInfo = EntryInfo
  { tag :: Int32
  , tagType :: Word32
  , offset :: Int32
  , count :: Word32
  }
  deriving (Show, Ord, Eq)

-- data BlobParseError
--   = IndexLength
--   | DataLength

-- data BlobParseContext = BlobParseContext
--   { byteString :: BLS.ByteString
--   , offset :: ByteOffset
--   , error :: BlobParseError
--   }

-- -- This might raise an error, but is never meant to be used outside this module.
-- strToError :: String -> BlobParseError
-- strToError = \case
--   "indexLen" -> IndexLength
--   "dataLen" -> DataLength
--   _ -> Prelude.error "strToError should not be used outside its module. This error indicates a bug in Data.Rpm.DbHeaderBlob."

-- data BlobInitErr
--   = Parse (BLS.ByteString, ByteOffset, String)
--   | IndexLength
--   deriving (Eq)

-- instance Show BlobInitErr where
--   show (Parse t) = "Parse " <> show t
--   -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L120
--   show IndexLength = "Index length too small"

-- Inspired by https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L105
headerBlobInit :: BLS.ByteString -> Either (BLS.ByteString, ByteOffset, String) HeaderBlob
headerBlobInit bs =
  case runGetOrFail readHeader bs of
    Right (_, _, res) -> Right res
    Left l -> Left l
  where
    readHeader = do
      indexLength <- label "Read indexLength" getInt32be

      when (indexLength < 1) $
        fail "region no tags error"

      dataLength <- label "Read dataLength" getInt32be
      let dataStart = calcDataStart indexLength
      let pvLength = dataAndIndexLen + dataLength + indexLength * entryInfoSize
      let dataEnd = dataStart + dataLength

      when (pvLength >= headerMaxBytes) $
        fail "blob size bad"

      entryInfos <- readEntries indexLength

      case hdrblobVerifyRegion entryInfos dataLength dataStart bs of
        Left s -> fail s
        Right
          RegionInfo
            { rDl = regionDataLength
            , rIl = regionIndexLength
            } -> pure HeaderBlob{..}

    dataAndIndexLen :: Int32
    dataAndIndexLen = 8

    -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L116
    calcDataStart :: Int32 -> Int32
    calcDataStart indexLength =
      dataAndIndexLen
        + indexLength
        * entryInfoSize

data RegionInfo = RegionInfo
  { rDl :: Int32
  , rIl :: Int32
  }
  deriving (Show, Eq)

emptyRegionInfo :: RegionInfo
emptyRegionInfo =
  RegionInfo
    { rDl = 0
    , rIl = 0
    }

-- -- This is inspired by hdrblobVerifyRegion:
-- -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L263 In the
-- -- interest of getting an MVP done quickly, this currently doesn't do
-- -- verification, it just calculates any data that future processes might need.
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
    then -- go defaults numbers to 0, so I think the effect of bailing out early in
    -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L276
    -- is to have all region information be 0.
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
          { rDl = offset + regionTagCount
          , rIl = negOffset `div` entryInfoSize
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

-- it looks like we have the first few region info in headerblob correct,
-- next make sure we're passing the right things to region swab as in the go code

hdrblobImport :: HeaderBlob -> BLS.ByteString -> Either String [IndexEntry]
hdrblobImport HeaderBlob{..} bs = do
  let firstEntry = NonEmpty.head entryInfos
  traceM $ printf "entryinfos with tag 1001: %s" $ show (NonEmpty.filter (\i -> tag i == 1001) entryInfos)
  if tag firstEntry >= rpmTagHeaderI18nTable
    then Left "Not implemented yet"
    else do
      traceM $ printf "First few entryInfo %s " $ show (NonEmpty.take 4 entryInfos)
      let regionIndexLength' = if offset firstEntry == 0 then indexLength else regionIndexLength
      traceM $ printf "regionIndexLen' %d" regionIndexLength'
      (indexEntries, ieReadLen) <- regionSwab bs (subList 1 regionIndexLength' entryInfos) dataStart dataEnd 0
      traceM $ printf "initial read len %d" ieReadLen
      traceM $ printf "blob dl %d" dataLength

      (allEntries, dataLenWithDribble) <-
        if fromIntegral regionIndexLength' < length entryInfos
          then calculateDribbleEntries indexEntries regionIndexLength' ieReadLen
          else Right (indexEntries, ieReadLen)

      traceM $ printf "Data len with dribble %d" dataLenWithDribble

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
      traceM $ printf "First dribble info %s" (show . head $ dribbleInfos)
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
  traceShowM $ "first entryInfo" <> show (head entryInfos)
  traceShowM $ "start " <> show dataStart
  traceShowM $ "end " <> show dataEnd
  traceM $ printf "last entryInfo %s" (show . last $ entryInfos)
  traceM $ printf "Start swab dl %d" startDataLength
  foldM calcIndexEntry ([], startDataLength) (zip [0 ..] entryInfos)
  where
    calcIndexEntry (entries, runningDataLength) (idx, entryInfo) = do
      when (tag entryInfo == 1001) $
        traceM "Found 1001 tag!"
      newIdxEntry <- swabEntry idx entryInfo
      -- traceM $ printf "entering alignDiff %d %d" (tagType entryInfo) runningDataLength
      alignDifference <- alignDiff (tagType entryInfo) runningDataLength
      let rdl = alignDifference + runningDataLength + entryLength newIdxEntry
      Right (newIdxEntry : entries, rdl)

    alignDiff :: Word32 -> Int32 -> Either String Int32
    alignDiff ty alignSize = do
      traceM $ printf "Aligning with %d and %d" ty alignSize
      tSize <- maybeToEither ("aligning diff: tag type " <> show ty <> " is invalid") (typeSizes V.!? fromIntegral ty)
      let diff = tSize - (alignSize `mod` tSize)
      traceM $ printf "diff %d" diff
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
        then Left "invalid data offset"
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
        else
          let indices = take (fromIntegral strCount) . BLS.findIndices (== 0) . bsSubString start dataEnd $ bs
           in Right . fromIntegral $ last indices + 1

-- | Substring of a byte string. Includes stop, but not end for range
-- [start, end).
bsSubString :: Int32 -> Int32 -> BLS.ByteString -> BLS.ByteString
bsSubString start end = BLS.take (fromIntegral $ end - start) . BLS.drop (fromIntegral start)

-- TODO: Should I keep processing even if one of these fails to read?
-- Since they're just lined up next to each other a failure to read in one
-- place likely means we've gone too far off the rails anyways.
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
