{-# LANGUAGE TemplateHaskell #-}

module Container.Dpkg (
  analyzeDpkgEntries,
  analyzeDpkgEntriesScoped,
  DpkgEntry (..),
) where

import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, (<||>))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Effect.ReadFS (
  ReadFS,
  readContentsParser,
 )
import Path (Abs, Dir, Path, mkAbsDir, mkRelDir, mkRelFile, (</>))
import Text.Megaparsec (
  MonadParsec (notFollowedBy, takeWhileP),
  Parsec,
  empty,
  many,
 )
import Text.Megaparsec.Char (space1)
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
analyzeDpkgEntries = analyzeDpkgEntriesScoped $(mkAbsDir "/")

-- | Analyze dpkg entries from the provided root.
-- Searches for: @var/lib/dpkg/{status|status.d}@ from the provided directory.
analyzeDpkgEntriesScoped :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([DpkgEntry])
analyzeDpkgEntriesScoped root | runningLinux = parseStatus root <||> parseStatusD root
analyzeDpkgEntriesScoped _ = pure []

parseStatus :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([DpkgEntry])
parseStatus root = readContentsParser entriesParser $ root </> $(mkRelDir "var") </> $(mkRelDir "lib") </> $(mkRelDir "dpkg") </> $(mkRelFile "status")

parseStatusD :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([DpkgEntry])
parseStatusD root = readContentsParser entriesParser $ root </> $(mkRelDir "var") </> $(mkRelDir "lib") </> $(mkRelDir "dpkg") </> $(mkRelFile "status.d")

-- | Parses a dpkg status file.
entriesParser :: Parser [DpkgEntry]
entriesParser = many entryParser

-- | Parses a dpkg status entry.
entryParser :: Parser DpkgEntry
entryParser = do
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

    -- >> parseTest propertiesParser "A: B\nC: D" = fromList [(A, B), (C, D)]
    propertiesParser :: Parser (Map.Map Text Text)
    propertiesParser = Map.fromList <$> many (lexeme propertyParser)

    -- >> parseTest propertyParser "A: B" = (A, B)
    propertyParser :: Parser (Text, Text)
    propertyParser = (,) <$> (keyParser <* symbol ":") <*> valueParser

    -- keyParser :: Parser Text
    -- keyParser = toText <$> (some (alphaNumChar <|> char '-'))
    keyParser :: Parser Text
    keyParser = takeWhileP Nothing (/= ':')

    -- >> parseTest valueParser "1.0.2" = 1.0.2
    -- >> parseTest valueParser "This is a\n long line" = This is a\n long line
    valueParser :: Parser Text
    valueParser = takeWhileP Nothing (/= '\n') <* notFollowedBy (symbol "\n ")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

sc :: Parser ()
sc = Lexer.space space1 empty empty

-- | Parse for the provided symbol, then consume any trailing spaces.
symbol :: Text -> Parser Text
symbol = Lexer.symbol sc
