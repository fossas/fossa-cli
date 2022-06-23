-- | Parsing functions for APK files.
-- APK is the package manager for Alpine Linux (https://alpinelinux.org/).
--
-- The database itself is just a flat file with entries in text.  These are
-- specified in a key-value format with obscure names.  See
-- https://wiki.alpinelinux.org/wiki/Apk_spec#Syntax.
module Strategy.AlpineLinux.Parser (installedPackagesDatabaseParser, PackageError (..)) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Strategy.AlpineLinux.Types (AlpinePackage (..))
import Text.Megaparsec (
  MonadParsec (eof, takeWhileP),
  Parsec,
  many,
  optional,
  some,
  try,
 )
import Text.Megaparsec.Char (char, eol, letterChar)

-- | Errors that can occur even in a valid file.  Packages are a lose collection
-- of fields, but it unclear which are required.  These error represent fields
-- we expect, but which may be missing in unusual circumstances.
data PackageError
  = MissingPackageName
  | MissingPackageArchitecture {packageName :: Text}
  | MissingPackageVersion {packageName :: Text}
  deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

-- | Key for the package-name field.
packageNameKey :: Text
packageNameKey = "P"

-- | Key for the architecture field.
architectureKey :: Text
architectureKey = "A"

-- | Key for the version field.
versionKey :: Text
versionKey = "V"

-- | Parses zero or more occurrences of a construct with separators between.
--
-- This differs from `Control.Monad.Combinators.sepBy` because it will backtrack
-- failed repetitions.  The generic version has no notion of backtracking so if
-- the separator consumes input after the last entry then the whole parser will
-- fail because it will fail to parse another entry after the separator.
--
-- TODO: If this is more generally useful we should move it to a common location.
--
-- The Megaparsec project chose not to include this in their library:
--    https://github.com/mrkkrp/megaparsec/issues/401
-- This implementation is written to mirror the source of the generic version:
--   https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/src/Control.Monad.Combinators.html#sepBy
sepByWithTry :: MonadParsec e s m => m a1 -> m a2 -> m [a1]
sepByWithTry p sep = do
  r <- optional (try p)
  case r of
    Nothing -> pure []
    Just x -> (x :) <$> many (try (sep >> p))

-- | Parses one or more occurrences of a construct with separators between.
--
-- See `sepByWithTry` for an explanation and motivation.
--
-- This implementation is written to mirror the source of the generic version:
--   https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/src/Control.Monad.Combinators.html#sepBy1
sepByWithTry1 :: MonadParsec e s m => m a1 -> m a2 -> m (NonEmpty a1)
sepByWithTry1 p sep = do
  x <- try p
  (x :|) <$> many (try (sep >> p))

-- | Parses an Alpine Linux installed-packages database file.
-- This returns a list of packages or errors that represent malformed packages.
--
-- Packages are separated from one another by one-or-more newlines.  The end of
-- a file may have any number of trailing newlines.
installedPackagesDatabaseParser :: Parser [Either PackageError AlpinePackage]
installedPackagesDatabaseParser =
  many eol *> packageParser `sepByWithTry` (eol *> some eol) <* many eol <* eof

-- | Parses a single package or error about that package.
--
-- Each package is a list of line-separated properties.  Each property is of the
-- form @key:value@.
packageParser :: Parser (Either PackageError AlpinePackage)
packageParser = do
  entries <- propertiesParser
  case (Map.lookup packageNameKey entries, Map.lookup architectureKey entries, Map.lookup versionKey entries) of
    (Just name, Just arch, Just version) ->
      pure . Right $
        AlpinePackage
          { alpinePackageName = name
          , alpinePackageArchitecture = arch
          , alpinePackageVersion = version
          }
    (Nothing, _, _) ->
      pure . Left $ MissingPackageName
    (Just name, Nothing, _) ->
      pure . Left $ MissingPackageArchitecture name
    (Just name, _, Nothing) ->
      pure . Left $ MissingPackageVersion name

-- | Parses a list of properties into a map.
-- Note: Some properties can be repeated.  This map will only have the last value.
-- However, none of the properties we are interested in are repeatable.
propertiesParser :: Parser (Map.Map Text Text)
propertiesParser =
  Map.fromList . NE.toList <$> propertyParser `sepByWithTry1` eol

-- | Parses a single property.
propertyParser :: Parser (Text, Text)
propertyParser = (,) <$> (keyParser <* char ':') <*> valueParser

-- | Parses a property key.  Keys are assumed to be a single letter.
keyParser :: Parser Text
keyParser = toText <$> some letterChar

-- | Parses a property value.  The value may be empty.
valueParser :: Parser Text
valueParser = takeWhileP (Just "entry value") (not . (`elem` ("\r\n" :: String)))
