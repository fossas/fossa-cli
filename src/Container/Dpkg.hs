{-# LANGUAGE TemplateHaskell #-}

module Container.Dpkg (
  analyzeDpkgEntries,
  analyzeDpkgEntriesScoped,
  DpkgEntry (..),
  -- | For testing
  dpkgEntryParser,
) where

import App.Fossa.VSI.DynLinked.Util (fsRoot, runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, (<||>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.ReadFS (
  ReadFS,
  readContentsParser,
 )
import Path (Abs, Dir, Path, mkRelDir, mkRelFile, (</>))
import Text.Megaparsec (
  MonadParsec (eof, notFollowedBy, takeWhileP),
  Parsec,
  anySingle,
  empty,
  many,
  single,
  someTill,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer (lexeme)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

-- | An entry in the dpkg database, consisting of the architecture, package, and version.
data DpkgEntry = DpkgEntry
  { dpkgEntryArch :: Text
  , dpkgEntryPackage :: Text
  , dpkgEntryVersion :: Text
  }
  deriving (Eq, Ord, Show)

-- | Analyze dpkg entries from the root of the file system.
-- Searches for: @var/lib/dpkg/{status|status.d}@ from the root of the file system.
analyzeDpkgEntries :: (Has ReadFS sig m, Has Diagnostics sig m) => m ([DpkgEntry])
analyzeDpkgEntries = analyzeDpkgEntriesScoped fsRoot

-- | Analyze dpkg entries from the provided root.
-- Searches for: @var/lib/dpkg/{status|status.d}@ from the provided directory.
analyzeDpkgEntriesScoped :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([DpkgEntry])
analyzeDpkgEntriesScoped root | runningLinux = parseStatus root <||> parseStatusD root
analyzeDpkgEntriesScoped _ = pure []

parseStatus :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([DpkgEntry])
parseStatus root = readContentsParser dpkgEntriesParser $ dpkgStatusDir root </> $(mkRelFile "status")

parseStatusD :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([DpkgEntry])
parseStatusD root = readContentsParser dpkgEntriesParser $ dpkgStatusDir root </> $(mkRelFile "status.d")

dpkgStatusDir :: Path Abs Dir -> Path Abs Dir
dpkgStatusDir root = root </> $(mkRelDir "var") </> $(mkRelDir "lib") </> $(mkRelDir "dpkg")

-- | Parses a dpkg status file.
dpkgEntriesParser :: Parser [DpkgEntry]
dpkgEntriesParser = many dpkgEntryParser <* eof

-- | Parses a dpkg status entry.
dpkgEntryParser :: Parser DpkgEntry
dpkgEntryParser = do
  properties <- propertiesParser
  let package = Map.lookup "Package" properties
  let version = Map.lookup "Version" properties
  let arch = Map.lookup "Architecture" properties

  case (package, version, arch) of
    (Just package', Just version', Just arch') ->
      pure $
        DpkgEntry
          { dpkgEntryArch = arch'
          , dpkgEntryPackage = package'
          , dpkgEntryVersion = version'
          }
    _ -> fail $ buildFailure package version arch
  where
    buildFailure :: Maybe Text -> Maybe Text -> Maybe Text -> String
    buildFailure package version arch =
      "parse all of required 'Package', 'Version', 'Architecture' keys."
        <> " Package: "
        <> show package
        <> "; Version: "
        <> show version
        <> "; Arch: "
        <> show arch

    -- Collect Key: Value pairs into a map.
    propertiesParser :: Parser (Map Text Text)
    propertiesParser = Map.fromList <$> many (lexeme sc propertyParser)

    -- Collect Key: Value pairs into actual tuples.
    propertyParser :: Parser (Text, Text)
    propertyParser = (,) <$> (keyParser <* symbol ":") <*> valueParser <* sc

    -- Keys are easy: just parse anything up to the ':' literal.
    keyParser :: Parser Text
    keyParser = takeWhileP Nothing (/= ':')

    -- Value fields are either single line or multi line.
    -- There's no documentation I could find on this,
    -- but from observeration it appears that a single space indent is used
    -- to form a multi-line value, with intentionally blank lines consisting
    -- of a single literal dot after the space.
    -- Given this, we first try to parse a single line, and if that fails
    -- we try to parse a multiline. If that fails, fail the parse.
    valueParser :: Parser Text
    valueParser = try singleLineValueParser <|> multiLineValueParser

    -- A single line value is one that is terminated on a newline and is not
    -- followed by a space on the next line.
    -- This is not documented, and was simply observed- see valueParser for details.
    singleLineValueParser :: Parser Text
    singleLineValueParser = takeWhileP Nothing (/= '\n') <* single '\n' <* notFollowedBy (single ' ')

    -- Parse and record anything until the end of this multiline value.
    -- So far we don't actually use any of the multiline values, so no further processing
    -- is performed, we just read them.
    multiLineValueParser :: Parser Text
    multiLineValueParser = do
      parsed <- someTill anySingle eov
      pure $ toText parsed

    -- End of [multiline] value is reached when either a double newline is encountered,
    -- or the end of the file is encountered.
    -- This is not documented, and was simply observed- see valueParser for details.
    eov :: Parser Text
    eov = try $ (symbol "\n\n") <|> (eof >> "")

sc :: Parser ()
sc = Lexer.space space1 empty empty

-- | Parse for the provided symbol, then consume any trailing spaces.
symbol :: Text -> Parser Text
symbol = Lexer.symbol sc
