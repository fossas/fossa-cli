module Data.BerkeleyDB (
  entries,
  entriesWith,
  BdbEntry,
) where

import Control.Algebra (Has)
import Control.Carrier.Diagnostics (runDiagnosticsIO, withResult)
import Control.Effect.Diagnostics (Diagnostics, context, fromEither)
import Control.Effect.Lift (Lift, sendIO)
import Data.Bits (Bits (shiftL))
import Data.ByteString (ByteString)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word32, Word8)
import Effect.ReadFS (ReadFS, readContentsBSLimit, readContentsParser, readContentsParserBS)
import GHC.Base (Alternative)
import Path (Abs, File, Path)
import Text.Megaparsec (
  MonadParsec (eof, notFollowedBy, takeWhileP),
  Parsec,
  anySingle,
  chunk,
  empty,
  many,
  manyTill,
  single,
  some,
  someTill,
  takeP,
  try,
  (<|>),
 )
import Text.Megaparsec.Byte.Binary (word32be, word32le, word8)
import Text.Megaparsec.Char.Lexer (lexeme)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void ByteString

data ByteOrder = BigEndian | LittleEndian

-- | An entry in BerkeleyDB. Each entry must be parsed.
newtype BdbEntry = BdbEntry
  { bdbEntryValue :: ByteString
  }
  deriving (Eq, Ord, Show)

entries :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m [BdbEntry]
entries = entriesWith pure

entriesWith :: (Has Diagnostics sig m, Has ReadFS sig m) => (BdbEntry -> m a) -> Path Abs File -> m [a]
entriesWith parseEntry file = context ("Parse BDB file: " <> toText file) $ do
  metadata <- context "metadata" $ readContentsParserBS parseBdbMeta file

  undefined

-- The metadata header.
-- Combines the "generic" and "hash" sections from the Go source because it's simpler to do so here.
--
-- Adapted from: https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/generic_page.go#L10-L28
--          and: https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/hash_metadata_page.go#L10-L20
-- Original source: https://github.com/berkeleydb/libdb/blob/5b7b02ae052442626af54c176335b67ecc613a30/src/dbinc/db_page.h#L73
--             and: https://github.com/berkeleydb/libdb/blob/5b7b02ae052442626af54c176335b67ecc613a30/src/dbinc/db_page.h#L130
data BdbMetadata = BdbMetadata
  { -- The byte order used to parse this metadata.
    bdbMetaByteOrder :: ByteOrder
  , -- Generic block
    bdbMetaLSN :: [Word8] -- 8 bytes long
  , bdbMetaPageNumber :: Word32
  , bdbMetaMagic :: Word32
  , bdbMetaVersion :: Word32
  , bdbMetaPageSize :: Word32
  , bdbMetaEncryptionAlg :: Word8
  , bdbMetaPageType :: Word8
  , bdbMetaMetaFlags :: Word8
  , bdbMetaUnused1 :: Word8
  , bdbMetaFree :: Word32
  , bdbMetaLastPageNo :: Word32
  , bdbMetaNParts :: Word32
  , bdbMetaKeyCount :: Word32
  , bdbMetaRecordCount :: Word32
  , bdbMetaFlags :: Word32
  , bdbMetaUniqueFileId :: [Word8] -- 19 bytes long
  -- HashMetadata block
  , bdbMetaMaxBucket :: Word32
  , bdbMetaHighMask :: Word32
  , bdbMetaLowMask :: Word32
  , bdbMetaFillFactor :: Word32
  , bdbMetaNumKeys :: Word32
  , bdbMetaCharKeyHash :: Word32
  }

parseBdbMeta :: Parser BdbMetadata
parseBdbMeta = try (parseBdbMeta' LittleEndian) <|> parseBdbMeta' BigEndian

parseBdbMeta' :: ByteOrder -> Parser BdbMetadata
parseBdbMeta' order = do
  meta <-
    BdbMetadata order
      <$> parseByteN 8
      <*> word32
      <*> word32
      <*> word32
      <*> word32
      <*> word8
      <*> word8
      <*> word8
      <*> word8
      <*> word32
      <*> word32
      <*> word32
      <*> word32
      <*> word32
      <*> word32
      <*> parseByteN 19
      <*> word32
      <*> word32
      <*> word32
      <*> word32
      <*> word32
      <*> word32
  validateMeta meta
  where
    word32 :: Parser Word32
    word32 = word32' order

validateMeta :: (MonadFail m) => BdbMetadata -> m BdbMetadata
validateMeta meta = validateMetaMagic meta >>= validateMetaPageType >>= validateMetaEncryptionAlg >>= validatePageSize

validateMetaMagic :: (MonadFail m) => BdbMetadata -> m BdbMetadata
validateMetaMagic meta@BdbMetadata{bdbMetaMagic} = validate' expect "magic number" meta bdbMetaMagic
  where
    -- From: https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L6
    expect :: Word32
    expect = 0x00061561

validateMetaPageType :: (MonadFail m) => BdbMetadata -> m BdbMetadata
validateMetaPageType meta@BdbMetadata{bdbMetaPageType} = validate' expect "page type" meta bdbMetaPageType
  where
    -- From: https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L18
    expect :: Word8
    expect = 8

validateMetaEncryptionAlg :: (MonadFail m) => BdbMetadata -> m BdbMetadata
validateMetaEncryptionAlg meta@BdbMetadata{bdbMetaEncryptionAlg} = validate' expect "encryption alg" meta bdbMetaEncryptionAlg
  where
    -- From: https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/constants.go#L4
    expect :: Word8
    expect = 0

validatePageSize :: (MonadFail m) => BdbMetadata -> m BdbMetadata
validatePageSize meta@BdbMetadata{bdbMetaPageSize} =
  if bdbMetaPageSize `elem` valid
    then pure meta
    else fail $ "invalid page size: " <> show bdbMetaPageSize
  where
    -- From: https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go#L11-L20
    valid :: [Word32]
    valid = [shiftL 512 x | x <- [0 .. 7]]

validate' :: (MonadFail m, Eq a, Show a) => a -> String -> b -> a -> m b
validate' expect msg meta test =
  if test /= expect
    then fail $ "invalid " <> msg <> ": " <> show test
    else pure meta

word32' :: ByteOrder -> Parser Word32
word32' LittleEndian = word32le
word32' BigEndian = word32be

parseByteN :: Int -> Parser [Word8]
parseByteN n = take n <$> many word8
