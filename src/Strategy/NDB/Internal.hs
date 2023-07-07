{-# LANGUAGE RecordWildCards #-}

-- | This implementation of the NDB format is heavily based on the Go project here:
--   https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go
--
--   The "New" Database format by RPM is not well documented outside the original source code, located here:
--   https://github.com/rpm-software-management/rpm/blob/rpm-4.17.0-release/lib/backend/ndb/rpmpkg.c
--
--   Packages.db File Format Summary:
--   ================================
--
--   32 bytes "NDB Header": Format Magic header, with version number etc. Provides the
--   Slot Pages count "SlotNPages".
--
--   Immediately following the NDB Header is an array of "SlotNPages" count Slot Pages.
--   Each Slot Page is exactly 4k in size and contains NDB_SlotEntriesPerPage individual Slots.
--
--   Each Slot Entry can be referring to a Package with an identifier or be a free slot entry (Package index is zero).
--   If a Slot Entry is non-free, the BlkOffset points to the "Block".
--
--   The "Block" has a "Blob Header", directly followed by the "Blob" (the actual package headers)
--   and a Blob "tail" of up to 16 bytes. The "Blob" is checksummed using Adler32 (ignored by this implementation).
module Strategy.NDB.Internal (
  readNDB,
  NdbEntry (..),
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, fromEitherParser, fromEitherShow)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Rpm.DbHeaderBlob (PkgInfo (..), readPackageInfo)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word32, Word8)
import Effect.ReadFS (ReadFS, readContentsBS)
import Numeric (showHex)
import Path (Abs, File, Path)
import Text.Megaparsec (
  Parsec,
  count,
  runParser,
  some,
  (<?>),
 )
import Text.Megaparsec.Byte.Binary (word32le, word8)

-- | An entry in the database, consisting of the architecture, package, and version.
data NdbEntry = NdbEntry
  { ndbEntryArch :: Text
  , ndbEntryPackage :: Text
  , ndbEntryVersion :: Text
  , ndbEntryEpoch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | FOSSA _requires_ that architecture is provided: https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages
parsePkgInfo :: (Has Diagnostics sig m) => PkgInfo -> m NdbEntry
parsePkgInfo (PkgInfo (Just pkgName) (Just pkgVersion) (Just pkgRelease) (Just pkgArch) pkgEpoch) = pure $ NdbEntry pkgArch pkgName (pkgVersion <> "-" <> pkgRelease) (fmap (toText . show) pkgEpoch)
parsePkgInfo pkg = fatalText . toText $ "package '" <> show pkg <> "' is missing one or more fields; all fields are required"

-- | Packages are read as a JSON array of base64 strings.
readNDB ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs File ->
  m [NdbEntry]
readNDB file = do
  blobs <- context "read blobs" $ readNDB' file
  entries <- context "parse blobs" . traverse fromEitherShow $ readPackageInfo <$> fmap BSL.fromStrict blobs
  context "parse package info" $ traverse parsePkgInfo entries

readNDB' ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs File ->
  m [ByteString]
readNDB' file = do
  -- The Go code this module is based on uses a file handle, but doing that is rough in Haskell.
  -- Instead this code buffers the entire file and then treats the resulting ByteString as a seekable cursor.
  --
  -- This works because:
  -- - The file is small enough to reasonably buffer.
  -- - The parsers operate on ByteString.
  -- - The parsers are lazy, only reading the minimum amount of data to work.
  -- - Slicing a ByteString is O(1).
  --
  -- If any of the above change in the future this may need to be rethought.
  content <- readContentsBS file

  -- Equivalent to 'Open': https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L93-L127
  header <- fromEitherParser $ runParser parseRawNdb "ndb header" content
  entries <- fromEitherParser . runParser (parseSlotEntries header) "slot entries" $ slice ndbHeaderLength content

  -- Equivalent to 'Read', without the needless channel: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L129-L192
  readBlobs content $ filter ndbSlotEntryShouldProcess entries
  where
    readBlobs :: (Has Diagnostics sig m) => ByteString -> [NdbSlotEntry] -> m [ByteString]
    readBlobs content (entry : entries) = do
      let content' = slice (ndbSlotBlkOffset entry * ndbBlobHeaderSize) content
      blobHeader <- fromEitherParser $ runParser (parseNdbBlobHeader entry) "blob header" content'
      blob <- fromEitherParser . runParser (parseNdbBlob blobHeader) "blob" $ slice ndbBlobHeaderSize content'
      rest <- readBlobs content entries
      pure $ blob : rest
    readBlobs _ [] = pure []

type Parser = Parsec Void ByteString

data NdbHeader = NdbHeader
  { ndbHeaderMagic :: Word32
  , ndbHeaderVersion :: Word32
  , ndbHeaderGeneration :: Word32
  , ndbHeaderSlotNPages :: Word32
  }
  deriving (Show)

data NdbSlotEntry = NdbSlotEntry
  { ndbSlotMagic :: Word32
  , ndbSlotPkgIndex :: Word32
  , ndbSlotBlkOffset :: Word32
  , ndbSlotBlkCount :: Word32
  }
  deriving (Show)

data NdbBlobHeader = NdbBlobHeader
  { ndbBlobHeaderMagic :: Word32
  , ndbBlobHeaderPkgIndex :: Word32
  , ndbBlobHeaderChecksum :: Word32
  , ndbBlobHeaderLen :: Word32
  }
  deriving (Show)

-- | Parse the slot entries in the header section of the DB.
-- The original structure contains the file handle too, but as mentioned in 'readNdb'' this code doesn't use a file handle,
-- so instead it just parses the entries themselves instead of a container type.
--
-- Structure: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L82-L85
-- Parse: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L93-L127
-- Validation is handled by the child parsers.
parseRawNdb :: Parser NdbHeader
parseRawNdb = parseNdbHeader <?> "header"

parseSlotEntries :: NdbHeader -> Parser [NdbSlotEntry]
parseSlotEntries header = count (ndbHeaderEntryCount header) (parseNdbSlotEntry <?> "slot entry")

-- | Parse the header for the database.
-- Due to the way in which this module buffers the file content for parsers, parsing the unused portion of the struct is technically not needed.
-- It's kept in for completeness, so that in the future if this module needs to move to handle based parsing it can do so with less surprises.
--
-- Structure: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L60-L66
-- Parse: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L99-L103
-- Validate: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L105-L113
parseNdbHeader :: Parser NdbHeader
parseNdbHeader =
  NdbHeader
    <$> (word32le <?> "magic")
    <*> (word32le <?> "version")
    <*> (word32le <?> "generation")
    <*> (word32le <?> "slot pages count")
    <* (parseBytesRaw 16 <?> "unused")
    >>= validate
  where
    validate :: (MonadFail m) => NdbHeader -> m NdbHeader
    validate NdbHeader{..} | ndbHeaderSlotNPages == 0 = fail "expected >0 pages but got zero"
    validate NdbHeader{..} | ndbHeaderSlotNPages > ndbHeaderMaxPages = fail $ "slot page limit exceeded: " <> show ndbHeaderSlotNPages
    validate NdbHeader{..} | ndbHeaderVersion /= ndbHeaderSupportedVersion = fail $ "expected version '0' but got: " <> show ndbHeaderVersion
    validate NdbHeader{..} | ndbHeaderMagic /= ndbHeaderMagicExpected = fail $ "expected magic '" <> showHex' ndbHeaderMagicExpected <> "' but got: " <> showHex' ndbHeaderMagic
    validate header = pure header

-- | Parse a slot entry.
--
-- Structure: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L68-L73
-- Parse: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L116-L117
-- Validate: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L138-L145
parseNdbSlotEntry :: Parser NdbSlotEntry
parseNdbSlotEntry =
  NdbSlotEntry
    <$> (word32le <?> "magic")
    <*> (word32le <?> "package index")
    <*> (word32le <?> "block offset")
    <*> (word32le <?> "block count")
    >>= validate
  where
    validate :: (MonadFail m) => NdbSlotEntry -> m NdbSlotEntry
    validate NdbSlotEntry{..} | ndbSlotMagic /= ndbSlotMagicExpected = fail $ "expected magic '" <> showHex' ndbSlotMagicExpected <> "' but got: " <> showHex' ndbSlotMagic
    validate entry = pure entry

-- | Parse the header for a specific blob.
--
-- Structure: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L75-L80
-- Parse: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L159-L167
-- Validate: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L168-L178
parseNdbBlobHeader :: NdbSlotEntry -> Parser NdbBlobHeader
parseNdbBlobHeader slot =
  NdbBlobHeader
    <$> (word32le <?> "magic")
    <*> (word32le <?> "package index")
    <*> (word32le <?> "checksum")
    <*> (word32le <?> "len")
    >>= validate slot
  where
    validate :: (MonadFail m) => NdbSlotEntry -> NdbBlobHeader -> m NdbBlobHeader
    validate _ NdbBlobHeader{..} | ndbBlobHeaderMagic /= ndbBlobMagicExpected = fail $ "expected magic '" <> showHex' ndbBlobMagicExpected <> "' but got: " <> showHex' ndbBlobHeaderMagic
    validate NdbSlotEntry{..} NdbBlobHeader{..} | ndbSlotPkgIndex /= ndbBlobHeaderPkgIndex = fail $ "expected pkg index '" <> show ndbSlotPkgIndex <> "', but got: " <> show ndbBlobHeaderPkgIndex
    validate _ header = pure header

-- | Parse a blob.
-- Contains no real structure (other than that it is, in Haskell terms, a '[Word8]') or validation.
--
-- Parse: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L182-L187
parseNdbBlob :: NdbBlobHeader -> Parser ByteString
parseNdbBlob NdbBlobHeader{..} = BS.pack <$> parseBytesRaw (fromIntegral ndbBlobHeaderLen)

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L88
ndbHeaderMagicExpected :: Word32
ndbHeaderMagicExpected = 1349349458

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L138
ndbSlotMagicExpected :: Word32
ndbSlotMagicExpected = 1953459283

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L168
ndbBlobMagicExpected :: Word32
ndbBlobMagicExpected = 1398959170

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L111
ndbHeaderMaxPages :: Word32
ndbHeaderMaxPages = 2048

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L89
ndbHeaderSupportedVersion :: Word32
ndbHeaderSupportedVersion = 0

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L87
ndbSlotEntriesPerPage :: Word32
ndbSlotEntriesPerPage = 256

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L135
ndbBlobHeaderSize :: Word32
ndbBlobHeaderSize = 16

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L116
ndbHeaderEntryCount :: NdbHeader -> Int
ndbHeaderEntryCount NdbHeader{ndbHeaderSlotNPages} = fromIntegral $ ndbHeaderSlotNPages * ndbSlotEntriesPerPage - 2

-- | https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L146-L149
ndbSlotEntryShouldProcess :: NdbSlotEntry -> Bool
ndbSlotEntryShouldProcess NdbSlotEntry{ndbSlotPkgIndex} = ndbSlotPkgIndex /= 0

ndbHeaderLength :: Int
ndbHeaderLength = 32

-- | Just read raw bytes. Fails if the read is short.
parseBytesRaw :: Int -> Parser [Word8]
parseBytesRaw n = do
  buf <- take n <$> some word8
  if length buf == n
    then pure buf
    else fail $ "short read: expected " <> show n <> " bytes, read " <> show (length buf)

-- | Convenience for 'showHex' to avoid having to provide an empty string every time.
showHex' :: (Integral a) => a -> String
showHex' a = showHex a ""

-- | Parsers are lazy, so use unbounded slices.
slice :: (Integral a) => a -> ByteString -> ByteString
slice = BS.drop . fromIntegral
