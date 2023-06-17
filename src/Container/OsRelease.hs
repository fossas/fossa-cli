-- | Parses system information identifier for linux based operating systems.
--
-- Typically such information is stored in following paths:
--
--  * /etc/os-release (common)
--  * /usr/lib/os-release (common)
--  * /etc/initrd-release
--  * /usr/lib/extension-release.d/extension-release.IMAGE
--
-- The format of os-release is a newline-separated list of environment-like
-- shell-compatible variable assignments.
--
-- ## Example
-- ----------
--
-- NAME="Alpine Linux"
-- ID=alpine
-- VERSION_ID=3.15.4
-- PRETTY_NAME="Alpine Linux v3.15"
-- HOME_URL="https://alpinelinux.org/"
-- BUG_REPORT_URL="https://bugs.alpinelinux.org/"
--
-- ## Fields of Interest
-- ---------------------
--
-- ID
--
--  A lower-case string (no spaces or other characters outside of 0–9, a–z, ".", "_" and "-") identifying the operating system,
--  excluding any version information and suitable for processing by scripts or usage in generated filenames.
--  If not set, a default of "ID=linux" may be used. Note that even though this string may not include characters
--  that require shell quoting, quoting may nevertheless be used.
--
--
-- VERSION_ID
--
--  A lower-case string (mostly numeric, no spaces or other characters outside of 0–9, a–z, ".", "_" and "-")
--  identifying the operating system version, excluding any OS name information or release code name, and
--  suitable for processing by scripts or usage in generated filenames. This field is optional.
--
-- ## Spec
-- --------
--
-- * Variable assignment values are enclosed in double or single quotes if they include spaces, semicolons or
--   other special characters outside of A–Z, a–z, 0–9.
--
-- * Shell special characters ("$", quotes, backslash, backtick) must be escaped with backslashes,
--   following shell style. All strings should be in UTF-8 encoding, and non-printable characters
--   should not be used. Concatenation of multiple individually quoted strings is not supported.
--
-- * Lines beginning with "#" are treated as comments.
--
-- * Blank lines are permitted and ignored.
module Container.OsRelease (
  OsInfo (..),
  getOsInfo,

  -- * for testing
  osReleaseParser,
  busyBoxParser,
  centOsOldFmtParser,
) where

import Control.Effect.Diagnostics (Diagnostics, fatal, (<||>))
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Foldable (asum)
import Data.Map qualified as Map
import Data.SemVer qualified as SemVer
import Data.String.Conversion (ToString (toString), toText)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (OnDecodeError)
import Data.Void (Void)
import Effect.ReadFS (
  Has,
  ReadFS,
  ReadFSErr (FileParseError),
  readContentsBS,
  readContentsParser,
 )
import GHC.Generics (Generic)
import Path (Abs, File)
import Path.Internal (Path (Path))
import Text.Megaparsec (
  MonadParsec (takeWhileP, try),
  Parsec,
  anySingle,
  chunk,
  empty,
  errorBundlePretty,
  many,
  optional,
  runParser,
  skipManyTill,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lexer.space
    (void $ some $ char ' ' <|> char '\t' <|> char '\n')
    (Lexer.skipLineComment "#") -- os-release content can have line comments!
    empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

-- | Os Release Information of a system.
data OsInfo = OsInfo
  { nameId :: Text -- name identifier, e.g. alpine
  , version :: Text -- version identifier e.g. 3.1.4
  }
  deriving (Show, Ord, Eq, Generic)

instance ToJSON OsInfo

getOsInfo :: (Has ReadFS sig m, Has Diagnostics sig m) => m OsInfo
getOsInfo = parseEtcOsRelease <||> parseSystemReleaseCpe <||> parseBinBusyBox

parseEtcOsRelease :: (Has ReadFS sig m, Has Diagnostics sig m) => m OsInfo
parseEtcOsRelease = readContentsParser osInfoParser (Path "etc/os-release")

parseSystemReleaseCpe :: (Has ReadFS sig m, Has Diagnostics sig m) => m OsInfo
parseSystemReleaseCpe = readContentsParser centOsOldFmtParser (Path "etc/system-release-cpe")

parseBinBusyBox :: (Has ReadFS sig m, Has Diagnostics sig m) => m OsInfo
parseBinBusyBox = do
  binBusyBoxContent <- decodeUtf8With lenientDecode <$> (readContentsBS binBusyBox)
  case runParser busyBoxParser (toString binBusyBox) binBusyBoxContent of
    Left err -> fatal $ FileParseError (toString binBusyBox) (toText (errorBundlePretty err))
    Right a -> pure a
  where
    binBusyBox :: Path Abs File
    binBusyBox = (Path "bin/busybox")

    -- Replace an invalid input byte with the Unicode replacement (U+FFFD).
    lenientDecode :: OnDecodeError
    lenientDecode _ _ = Just '\xfffd'

-- | Parses Os Release Information.
osInfoParser :: Parser OsInfo
osInfoParser =
  try osReleaseParser
    <|> try centOsOldFmtParser
    <|> busyBoxParser

initComments :: Parser ()
initComments = void $ many $ (symbol "#") *> takeWhileP (Just "character") (/= '\n') <* "\n"

-- NOTE: For telemetry purposes we use a pre-existing library to do this.
-- If we find issues in this parser in the future, consider using that.

-- | Parses os-release file for OS release information.
osReleaseParser :: Parser OsInfo
osReleaseParser = do
  -- consume any initial comments as we have yet
  -- to consume parser using line comment aware lexer!
  _ <- try . optional $ lexeme initComments
  properties <- propertiesParser
  let nameId =
        asum
          ( map (`Map.lookup` properties) ["ID"]
              ++ [Just "linux"] -- We should default to linux as last resort per spec!
          )
  let versionId = Map.lookup "VERSION_ID" properties

  case (nameId, versionId) of
    (Just name, Just version) -> pure $ OsInfo name version
    (Just _, Nothing) -> fail "could not identify os version"
    (Nothing, Just _) -> fail "could not identify os name"
    (Nothing, Nothing) -> fail "could not identify os name or version"
  where
    -- >> parseTest propertiesParser "A=B\nC=D" = fromList [(A, B), (C, D)]
    -- >> parseTest propertiesParser "A=B\n\nC=D" = fromList [(A, B), (C, D)]
    propertiesParser :: Parser (Map.Map Text Text)
    propertiesParser = Map.fromList <$> many (lexeme propertyParser)

    -- >> parseTest propertyParser "A=B" = (A, B)
    -- >> parseTest propertyParser "A='B'" = (A, B)
    propertyParser :: Parser (Text, Text)
    propertyParser = (,) <$> (keyParser <* char '=') <*> valueParser

    keyParser :: Parser Text
    keyParser = toText <$> (some (alphaNumChar <|> char '_' <|> char '-'))

    -- >> parseTest valueParser "'1.0.2'" = 1.0.2
    -- >> parseTest valueParser "\"Abc\"" = Abc
    valueParser :: Parser Text
    valueParser = lexeme $ do
      quote <- optional (char '"' <|> char '\'')
      case quote of
        Nothing -> toText <$> some (alphaNumChar <|> char '_' <|> char '-' <|> char '.')
        Just _ -> do
          value <- takeWhileP (Just "entry value") (not . (`elem` ("\r\n\"\'" :: String)))
          void $ optional (char '"' <|> char '\'')
          pure value

-- | Parses content of /bin/busybox to identify OsInfo.
--
-- Busybox distribution does not contain /etc/os-release file,
-- or equivalent. We identify by parsing /bin/busybox content.
--
-- Reference:
--  https://unix.stackexchange.com/questions/15895/how-do-i-check-busybox-version-from-busybox
busyBoxParser :: Parser OsInfo
busyBoxParser = do
  maybePrefix <- optional $ try prefix
  case maybePrefix of
    Nothing -> fail "could not find keyword BusyBox to infer release information"
    Just _ -> do
      busyBoxVersion <- SemVer.fromText <$> versionText
      case busyBoxVersion of
        Left _ -> fail "could not retrieve release version for busybox"
        Right ver -> pure $ OsInfo "busybox" (toText $ SemVer.toString ver)
  where
    prefix :: Parser Text
    prefix = skipManyTill anySingle (symbol "BusyBox v")

    versionText :: Parser Text
    versionText =
      takeWhileP
        (Just "busybox version")
        (not . (`elem` (" " :: String)))

-- | Parses older centos distribution for os information.
--
-- Since os-release standard is relatively new (only released in 2012),
-- some older distros do not adhere to this guideline, and have custom
-- specification to denote Os release information.
--
-- >> cat /etc/system-release-cpe
-- cpe:/o:centos:linux:6:GA
centOsOldFmtParser :: Parser OsInfo
centOsOldFmtParser = OsInfo <$> name <*> version
  where
    name :: Parser Text
    name = chunk "cpe:/o:" *> tillColon <* chunk ":linux:"

    version :: Parser Text
    version = tillColon

    tillColon :: Parser Text
    tillColon =
      takeWhileP
        (Just "entry till :")
        (not . (`elem` (":\r\n\"" :: String)))
