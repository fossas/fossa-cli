{-# LANGUAGE RecordWildCards #-}

module Data.Rpm.DbHeaderBlob (
  headerBlobInit,
  HeaderBlob (..),
  EntryInfo (..),
) where

import Control.Monad (replicateM, when)
import Data.Binary.Get (ByteOffset, Get, getInt32be, getWord32be, label, runGetOrFail)
import Data.ByteString.Lazy qualified as BLS
import Data.Int (Int32)
import Data.Word (Word32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Control.Applicative (liftA2)

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
  }
  deriving (Show, Eq)

data EntryInfo = EntryInfo
  { tag :: Int32
  , tagType :: Word32
  , offset :: Int32
  , count :: Word32
  }
  deriving (Show, Eq)

-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L71
entryInfoSize :: Int32
entryInfoSize = 128

-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L18
headerMaxBytes :: Int32
headerMaxBytes = 256 * 1024 * 1024

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
      let dataStart = calcDataStart dataLength
      let pvLength = dataAndIndexLen + dataLength + indexLength * entryInfoSize
      let dataEnd = dataStart + dataLength

      when (pvLength >= headerMaxBytes) $
        fail "blob size bad"

      entryInfos <- readEntries indexLength

      -- TODO: There are a few extra verification steps that the go version does here
      -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L139
      pure $ HeaderBlob{..}

    dataAndIndexLen :: Int32
    dataAndIndexLen = 32 + 32

    -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L116
    calcDataStart :: Int32 -> Int32
    calcDataStart indexLength =
      dataAndIndexLen
        + indexLength
        * entryInfoSize

readEntries :: Int32 -> Get (NonEmpty EntryInfo)
readEntries indexLength = do
  liftA2 (:|) readEntry $ replicateM (fromIntegral (indexLength - 1)) readEntry
  where
    readEntry :: Get EntryInfo
    readEntry =
      -- The original code reads these as little endian:
      -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L127
      -- However, the original code looks like it simply copies bytes
      -- sequentially into a buffer that is then treated like an entryInfo
      -- structure. That means higher bytes in a word are least-significant,
      -- corresponding to big-endian when read individually.
      EntryInfo
        <$> label "Read tag" getInt32be
        <*> label "Read type" getWord32be
        <*> label "Read offset" getInt32be
        <*> label "Read count" getWord32be
