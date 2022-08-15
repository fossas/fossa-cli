{-# LANGUAGE RecordWildCards #-}

module Data.Rpm.DbHeaderBlob (headerBlobInit
                             , HeaderBlob(..)) where

import Data.Binary.Get (ByteOffset, getInt32be, runGetOrFail, label, getInt32le)
import Data.ByteString.Lazy qualified as BLS
import Data.Int (Int32)

data HeaderBlob = HeaderBlob
  { indexLength :: Int32
  , dataLength :: Int32
  , dataStart :: Int32
  , pvLength :: Int32 -- I do not know what this is
  }
  deriving (Show, Eq)

-- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L71
entryInfoSize :: Int32
entryInfoSize = 128

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

-- Inspired by https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L105
headerBlobInit :: BLS.ByteString -> Either (BLS.ByteString, ByteOffset, String) HeaderBlob
headerBlobInit bs =
  case runGetOrFail readHeader bs of
    Right (_, _, res) -> Right res
    Left l -> Left l
  where
    dataAndIndexLen :: Int32
    dataAndIndexLen = 32 + 32

    -- https://github.com/knqyf263/go-rpmdb/blob/master/pkg/entry.go#L116
    calcDataStart :: Int32 -> Int32
    calcDataStart indexLength = dataAndIndexLen +
                                indexLength
                                * entryInfoSize

    readHeader = do
      indexLength <- label "Read indexLength" getInt32be
      dataLength <- label "Read dataLength" getInt32be
      let dataStart = calcDataStart dataLength
      let pvLength = dataAndIndexLen + dataLength + indexLength * entryInfoSize
      pure $ HeaderBlob{..}
