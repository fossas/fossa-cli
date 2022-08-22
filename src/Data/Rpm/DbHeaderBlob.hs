{-# LANGUAGE RecordWildCards #-}

module Data.Rpm.DbHeaderBlob (
  headerBlobInit,
  HeaderBlob (..),
  EntryInfo (..),
  RegionInfo (..),
  hdrblobVerifyRegion,
  emptyRegionInfo,
  -- consts
  rpmTagHeaderImg,
  rpmTagHeaderImmutable,
  rpmTagHeaderSignatures,
  regionTagType,
  regionTagCount,
) where

import Control.Applicative (liftA2)
import Control.Monad (replicateM, unless, when)
import Data.Bifunctor (bimap)
import Data.Binary.Get (ByteOffset, Get, getInt32be, getWord32be, label, runGetOrFail)
import Data.ByteString.Lazy qualified as BLS
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word32)

-- Constants from
-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/rpmtags.go#L3
rpmTagHeaderImmutable :: Int32
rpmTagHeaderImmutable = 63

rpmTagHeaderSignatures :: Int32
rpmTagHeaderSignatures = 62

rpmTagHeaderImg :: Int32
rpmTagHeaderImg = 61

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
  deriving (Show, Eq)

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
    -- do
    -- when (not $ hdrCheckRange )
    trd (_, _, c) = c

    runGetOrFail' r = bimap trd trd . runGetOrFail r

    hdrCheckRange :: Int32 -> Int32 -> Bool
    hdrCheckRange dataLen offset = offset < 0 || offset > dataLen

-- next steps: try to parse an entry out of the trailer, then finish creating
-- the regionInfo

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

-- data IndexEntry = IndexEntry {}

-- rpmTagHeaderI18nTable :: Int32
-- rpmTagHeaderI18nTable = 100

-- hdrBlobImport :: BLS.ByteString -> HeaderBlob -> [IndexEntry]
-- hdrBlobImport bs HeaderBlob{..} =
--   if tag (entryInfos !! 1) >= rpmTagHeaderI18nTable then
--     error "Not implemented yet"
--   else
--     regionSwab bs entryInfos
