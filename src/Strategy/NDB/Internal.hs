-- | Reads NDB databases.
--
-- See also:
-- - Go parser: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go
-- - Original C parser with useful comments: https://github.com/rpm-software-management/rpm/blob/3e74e8ba2dd5e76a5353d238dc7fc38651ce27b3/lib/backend/ndb/rpmpkg.c
module Strategy.NDB.Internal (
  readNDB,
  NdbEntry (..),
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fromEitherParser)
import Control.Monad (void, when)
import Data.Bits (zeroBits)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (ord)
import Data.Rpm.DbHeaderBlob (PkgInfo (..), readPackageInfo)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word32)
import Effect.ReadFS (ReadFS, readContentsBS)
import Path (Abs, File, Path)
import Text.Megaparsec (
  Parsec,
  count,
  eof,
  label,
  runParser,
  takeP,
  (<?>),
 )
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Byte.Binary (word32le)

-- | Read an NDB database. These are commonly found at @\/var\/lib\/rpm\/Packages.db@.
--
-- Reading this database provides us with a list of installed RPM packages. It
-- does not provide edges between package dependencies.
readNDB :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m [NdbEntry]
readNDB file = do
  contents <- readContentsBS file
  fromEitherParser $ runParser parseNDB (show file) contents

-- When parsing ByteStrings, the associated token is a Word8 (a byte).
--
-- See also:
-- - https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:Token
-- - https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:ShareInput
type Parser = Parsec Void ByteString

-- | An entry in an NDB database, consisting of the architecture, package, and
-- version.
data NdbEntry = NdbEntry
  { ndbEntryArch :: Text
  , ndbEntryPackage :: Text
  , ndbEntryVersion :: Text
  , ndbEntryEpoch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | A parser for NDB databases.
--
-- An NDB database is a binary file with the following structure:
--
--   1. The header of the database. This is 16 bytes long, and contains some
--      metadata and the count of the number of "slot pages".
--   2. 16 bytes of unused data.
--   3. A sequence of "slot pages". Each slot page is 4096 bytes in size, and
--      contains 256 slots that are 16 bytes each. Each slot represents a single
--      installed package, and a pointer to the "block" for this package. If the
--      number of installed packages is not a multiple of the slot page size, a
--      slot page might have a bunch of empty slots at the end of it.
--   4. A sequence of "blocks". Blocks contain an RPM package's actual header
--      data. Blocks are variable-length, densely packed, and always padded to
--      be 16-aligned.
--
-- For details on each type (e.g. slots, blocks, etc.), see the comment for the
-- associated parser. To see this structure for yourself, find an RPM NDB file
-- (e.g. @docker run registry.suse.com\/bci\/bci-base@) and examine the hex dump
-- of its @\/var\/lib\/rpm\/Packages.db@ (e.g. @hexdump -Cv Packages.db@).
--
-- See also: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L34-L58
parseNDB :: Parser [NdbEntry]
parseNDB = do
  -- Parse the header.
  header <- parseHeader
  (label "unused" $ void $ takeP Nothing 16)
  -- Parse the slot pages.
  slots <- parseSlots header
  -- Parse the blobs.
  blobs <- parseBlobs slots
  eof
  pure blobs

-- | The header of an NDB database.
newtype NdbHeader = NdbHeader
  { ndbHeaderSlotNPages :: Word32
  }
  deriving (Show)

-- | The header of an NDB database contains, in order:
--
--   1. A magic sequence (@\"RpmP\"@) (4 bytes).
--   2. An NDB header version (we only support version 0) (4 bytes).
--   3. An NDB header generation (I have no idea what this is for) (4 bytes).
--   4. A count of "slot pages" in this NDB database (4 bytes).
--
-- Of these, we really only need the slot page count, but we parse the others
-- for clarity.
parseHeader :: Parser NdbHeader
parseHeader =
  label "header" $ NdbHeader <$> (magic *> version *> generation *> slotPagesCount)
  where
    magic = label "magic" $ parseMagic ['R', 'p', 'm', 'P']
    version = label "version" $ void $ string $ BS.pack $ replicate 4 zeroBits
    generation = take32 <?> "generation"
    slotPagesCount = word32le <?> "slot pages count"

-- | A "slot" representing a package in an NDB database.
data NdbSlotEntry = NdbSlotEntry
  { ndbSlotPkgIndex :: Word32
  , ndbSlotBlkOffset :: Word32
  , ndbSlotBlkCount :: Word32
  }
  deriving (Show)

-- | Parse all the slots of an NDB database. The header tells us how many slots
-- to expect to parse.
--
-- We filter out all slots where the package index is 0, because those slots are
-- empty padding at the end of a not-fully-filled slot page. The first slot
-- begins with index 1 and each subsequent slot has an incrementing index, so
-- all valid slots will always have package indexes greater than 0.
parseSlots :: NdbHeader -> Parser [NdbSlotEntry]
parseSlots NdbHeader{ndbHeaderSlotNPages} =
  label "slots" $ takeWhile ((/= 0) . ndbSlotPkgIndex) <$> count (fromIntegral $ slotsToParse ndbHeaderSlotNPages) parseSlot
  where
    slotsPerPage = 256

    -- The first page only 254 slots, since its first two slots are overridden
    -- with the NDB database's header.
    --
    -- See also: https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L115
    slotsToParse pages = pages * slotsPerPage - 2

-- | Each slot contains, in order:
--
--   1. A magic sequence (@\"Slot\"@) (4 bytes).
--   2. A package index, identifying the package entry of this slot (4 bytes).
--   3. A block offset, indicating where the block's data begins, counted in
--      16-byte rows. All blocks begin at 16-aligned addresses (4 bytes).
--   4. A block count, indicating the total length of the block, counted in
--      16-byte rows (4 bytes).
parseSlot :: Parser NdbSlotEntry
parseSlot =
  label "slot" $
    NdbSlotEntry
      <$> (magic *> word32le <?> "package index")
      <*> (word32le <?> "block offset")
      <*> (word32le <?> "block count")
  where
    magic = label "magic" $ parseMagic ['S', 'l', 'o', 't']

-- | Parse all blobs of an NDB database. The slots tell us how many blobs to
-- expect to parse, and how to parse them.
parseBlobs :: [NdbSlotEntry] -> Parser [NdbEntry]
parseBlobs = label "blobs" . traverse parseBlob

-- | Each blob contains, in order:
--
--   1. A magic sequence (@\"BlbS\"@) (4 bytes).
--   2. The package index of this blob's package, which matches its
--      corresponding slot's package index (4 bytes).
--   3. The generation of this package (I have no idea what this is for) (4 bytes).
--   4. The length of the blob's variable-length data in bytes (4 bytes).
--   5. The blob's data, which contains the package's actual headers
--      (variable-length).
--   6. The blob's "tail", which is zero-padded so that the blob will end on a
--      16-aligned address (0-15 bytes).
--   7. An Adler-32 (RFC 1950) checksum of this blob's data (32 bytes).
--   8. The length of the blob's variable-length data in bytes, identical to
--      field 4 (4 bytes).
--   9. An ending magic sequence (@\"BlbE\"@) (4 bytes).
--
-- We then pass the blob's data to 'readPackageInfo', which is provides the
-- logic for parsing the package headers themselves. This logic is shared
-- between several different RPM backends. RPM supports different database
-- storage backends, each of which shares a content format for its package
-- headers (which are text) but stores and indexes data differently.
--
-- For layout documentation, see: https://github.com/rpm-software-management/rpm/blob/3e74e8ba2dd5e76a5353d238dc7fc38651ce27b3/lib/backend/ndb/rpmpkg.c#L512-L513
parseBlob :: NdbSlotEntry -> Parser NdbEntry
parseBlob NdbSlotEntry{ndbSlotPkgIndex, ndbSlotBlkCount} = label "blob" $ do
  -- Parse header.
  magicS
  index <- word32le <?> "package index"
  when (index /= ndbSlotPkgIndex) $ fail $ "expected pkg index '" <> show ndbSlotPkgIndex <> "', but got: " <> show index
  take32 <?> "generation"
  len <- (subtract 16) . fromIntegral <$> word32le <?> "len"

  -- Parse body.
  --
  -- We subtract 16 here because this length is from the start of the blob
  -- (including the 16 byte header) to the end of its data.
  blob <- takeP Nothing (len - 16) <?> "blob"

  -- Parse tail.
  --
  -- Padding = total blob size - length from start to end-of-data - length of constant-size tail.
  let tailLength = (fromIntegral ndbSlotBlkCount) * 16 - fromIntegral len - 12
  label "tail" $ void $ takeP Nothing tailLength
  take32 <?> "checksum"
  take32 <?> "tail len"
  magicE

  -- Parse body contents.
  parseBlobData blob
  where
    magicS = parseMagic ['B', 'l', 'b', 'S'] <?> "magicS"
    magicE = parseMagic ['B', 'l', 'b', 'E'] <?> "magicE"

    -- FOSSA _requires_ that architecture is provided: https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages
    parseBlobData blob = case readPackageInfo $ BSL.fromStrict blob of
      Left err -> fail err
      Right (PkgInfo (Just pkgName) (Just pkgVersion) (Just pkgRelease) (Just pkgArch) pkgEpoch) -> pure $ NdbEntry pkgArch pkgName (pkgVersion <> "-" <> pkgRelease) (fmap (toText . show) pkgEpoch)
      Right pkg -> fail $ "package '" <> show pkg <> "' is missing one or more fields; all fields are required"

-- | Take a single Word32 (i.e. 32 bits == 4 bytes) without parsing it.
take32 :: Parser ()
take32 = void $ takeP Nothing 4

-- | Parse a series of exact 'Word8'-sized 'Char's.
parseMagic :: [Char] -> Parser ()
parseMagic = void . string . BS.pack . fmap (fromIntegral . ord)
