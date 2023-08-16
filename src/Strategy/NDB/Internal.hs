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
import Control.Effect.Diagnostics (Diagnostics, fromEitherParser, fromEitherShow)
import Control.Monad (void, when)
import Data.Bits (zeroBits)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (ord)
import Data.Functor.Extra ((<$$>))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Rpm.DbHeaderBlob (PkgInfo (..), readPackageInfo)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word32)
import Effect.ReadFS (ReadFS, readContentsBS)
import Path (Abs, File, Path)
import Text.Megaparsec (Parsec, count, getOffset, label, runParser, takeP, (<?>))
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Byte.Binary (word32le)

-- | Read an NDB database. These are commonly found at
-- @\/var\/lib\/rpm\/Packages.db@.
--
-- Reading this database provides us with a list of installed RPM packages. It
-- does not provide edges between package dependencies.
readNDB :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m [NdbEntry]
readNDB file = do
  contents <- readContentsBS file
  blobs <- fromEitherParser $ BSL.fromStrict <$$> runParser parseNDB (show file) contents
  entries <- traverse fromEitherShow $ readPackageInfo <$> blobs
  pure $ mapMaybe parsePkgInfo entries
  where
    -- FOSSA _requires_ that architecture is provided: https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages
    --
    -- RPM packages that don't have all the required fields are dropped from the
    -- parsed list.
    parsePkgInfo :: PkgInfo -> Maybe NdbEntry
    parsePkgInfo (PkgInfo (Just pkgName) (Just pkgVersion) (Just pkgRelease) (Just pkgArch) pkgEpoch) =
      Just $ NdbEntry pkgArch pkgName (pkgVersion <> "-" <> pkgRelease) (fmap (toText . show) pkgEpoch)
    parsePkgInfo _ = Nothing

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
--      data. Blocks are variable-length and always padded to be 16-aligned.
--      Note that blocks are not densely packed! As you uninstall and upgrade
--      packages, old blocks can be resized to be smaller without any other
--      blocks being moved. This means that gaps filled with garbage data can be
--      created between blocks.
--
-- For details on each type (e.g. slots, blocks, etc.), see the comment for the
-- associated parser. To see this structure for yourself, find an RPM NDB file
-- (e.g. @docker run registry.suse.com\/bci\/bci-base@) and examine the hex dump
-- of its @\/var\/lib\/rpm\/Packages.db@ (e.g. @hexdump -Cv Packages.db@).
--
-- See also:
-- - Go parser (comments have some errors): https://github.com/knqyf263/go-rpmdb/blob/1369b2ee40b762e48586531810d5b2564e2c1063/pkg/ndb/ndb.go#L34-L58
-- - Original C parser: https://github.com/rpm-software-management/rpm/blob/rpm-4.17.0-release/lib/backend/ndb/rpmpkg.c
--   - Look at @rpmpkg{Get,Put}Internal@ and @rpmpkg{Read,Write}Blob@.
parseNDB :: Parser [ByteString]
parseNDB = do
  -- Parse the header.
  header <- parseHeader
  tossBytes 16 <?> "unused"
  -- Parse the slot pages.
  slots <- parseSlots header
  -- Parse the blobs.
  --
  -- We don't parse EOF after this because we might not have actually reached
  -- EOF. The last valid block might have extra unused or garbage blocks
  -- following it depending on the database has been used.
  parseBlocks slots

-- | The header of an NDB database.
newtype NdbHeader = NdbHeader
  { ndbHeaderSlotNPages :: Word32
  }
  deriving (Show)

-- | NDB databases begin with a header. Headers contain, in order:
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
    generation = toss4 <?> "generation"
    slotPagesCount = take4 <?> "slot pages count"

-- | A "slot" representing a package in an NDB database.
data NdbSlotEntry = NdbSlotEntry
  { ndbSlotPkgIndex :: Word32
  , ndbSlotBlkOffset :: Word32
  , ndbSlotBlkCount :: Word32
  }
  deriving (Show)

-- | Parse all the slots of an NDB database. The header tells us how many slot
-- pages to expect to parse. Slots are located immediately after headers.
--
-- We filter out all slots where the package index is 0, because those slots do
-- not point to valid blocks. They might be unused slots at the end of the page,
-- or they might be slots that used to be occupied but whose packages have since
-- been uninstalled.
parseSlots :: NdbHeader -> Parser [NdbSlotEntry]
parseSlots NdbHeader{ndbHeaderSlotNPages} =
  label "slots" $ filter ((/= 0) . ndbSlotPkgIndex) <$> count (fromIntegral $ slotsToParse ndbHeaderSlotNPages) parseSlot
  where
    slotsPerPage = 256

    -- The first page has only 254 slots, since its first two slots are
    -- overridden with the NDB database's header.
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
--
-- We use slot data to know when blocks start and how long blocks are.
parseSlot :: Parser NdbSlotEntry
parseSlot =
  label "slot" $
    NdbSlotEntry
      <$> (magic *> take4 <?> "package index")
      <*> (take4 <?> "block offset")
      <*> (take4 <?> "block count")
  where
    magic = label "magic" $ parseMagic ['S', 'l', 'o', 't']

-- | Parse all blocks of an NDB database. The slots tell us how many blocks to
-- expect to parse, and how to parse them.
--
-- Since blocks are not contiguous, but we still want to parse every block in a
-- single pass, we order the parsed slots by block offset. We then parse each
-- block in offset order, skipping to the block's offset if needed at the
-- beginning of each block.
parseBlocks :: [NdbSlotEntry] -> Parser [ByteString]
parseBlocks = label "blocks" . traverse parseBlock . sortOn ndbSlotBlkOffset

-- | Each block contains, in order:
--
--   1. A magic sequence (@\"BlbS\"@) (4 bytes).
--   2. The package index of this block's package, which matches its
--      corresponding slot's package index (4 bytes).
--   3. The generation of this package (I have no idea what this is for) (4
--      bytes).
--   4. The length of the block's variable-length data blob in bytes (4 bytes).
--   5. The blob data, which contains the package's actual headers
--      (variable-length).
--   6. The block's "tail", which is padded so that the block will end on a
--      16-aligned address (0-15 bytes).
--   7. An Adler-32 (RFC 1950) checksum of this block's data blob (32 bytes).
--   8. The length of the block's variable-length data blob in bytes, identical
--      to field 4 (4 bytes).
--   9. An ending magic sequence (@\"BlbE\"@) (4 bytes).
--
-- We later pass the parsed blob to 'readPackageInfo', which provides the logic
-- for parsing the package headers themselves. This logic is shared between
-- several different RPM backends. RPM supports different database storage
-- backends, each of which shares a content format for its package headers
-- (which are text) but stores and indexes data differently.
--
-- For layout documentation, see: https://github.com/rpm-software-management/rpm/blob/3e74e8ba2dd5e76a5353d238dc7fc38651ce27b3/lib/backend/ndb/rpmpkg.c#L512-L513
parseBlock :: NdbSlotEntry -> Parser ByteString
parseBlock NdbSlotEntry{ndbSlotPkgIndex, ndbSlotBlkOffset, ndbSlotBlkCount} = label "block" $ do
  -- Jump to block offset if needed. This is sometimes needed because there is a
  -- gap between where the previous block ended and where the current block
  -- begins.
  currentOffset <- getOffset
  let blockStartOffset = (fromIntegral ndbSlotBlkOffset) * rowSize
      gap = blockStartOffset - currentOffset
  when (gap > 0) $ tossBytes gap

  -- Parse header.
  magicS
  index <- take4 <?> "package index"
  when (index /= ndbSlotPkgIndex) $ fail $ "expected pkg index '" <> show ndbSlotPkgIndex <> "', but got: " <> show index
  toss4 <?> "generation"
  len <- fromIntegral <$> take4 <?> "len"

  -- Parse blob.
  --
  -- We subtract 16 here because this length is from the start of the block
  -- (including the 16 byte header) to the end of its blob.
  blob <- takeBytes (len - 16) <?> "blob"

  -- Parse tail.
  --
  -- Padding = total block size - length from start to end-of-blob - length of constant-size tail.
  let tailLength = (fromIntegral ndbSlotBlkCount) * rowSize - fromIntegral len - 12
  tossBytes tailLength <?> "tail"
  toss4 <?> "checksum"
  toss4 <?> "tail len"
  magicE

  -- Return blob directly. We parse the blob contents later so that errors in
  -- constructing the locator don't appear as confusing parsing errors.
  pure blob
  where
    magicS = parseMagic ['B', 'l', 'b', 'S'] <?> "magicS"
    magicE = parseMagic ['B', 'l', 'b', 'E'] <?> "magicE"

-- | The size of a "row" in the NDB format.
rowSize :: Int
rowSize = 16

-- | Consume a certain number of bytes.
takeBytes :: Int -> Parser ByteString
takeBytes = takeP Nothing

-- | Consume and discard a certain number of bytes.
tossBytes :: Int -> Parser ()
tossBytes = void . takeBytes

-- | Consume and discard 4 bytes (a common field size in NDB).
toss4 :: Parser ()
toss4 = tossBytes 4

-- | Consume 4 bytes (a common field size in NDB).
--
-- This is an alias for 'word32le' because the different unit sizes (a 'Word32'
-- has 32 _bits_, which is equal to 4 _bytes_) kept tripping me up.
take4 :: Parser Word32
take4 = word32le

-- | Parse a series of exact 'Word8'-sized 'Char's.
parseMagic :: [Char] -> Parser ()
parseMagic = void . string . BS.pack . fmap (fromIntegral . ord)
