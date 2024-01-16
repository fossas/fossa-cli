-- | Parsing functions for APK files.
-- APK is the package manager for Alpine Linux (https://alpinelinux.org/).
--
-- The database contains many properties of the packages, but we are only
-- interested in a few that we pull out by name.  As of writing that's the
-- package name, architecture and version.
--
-- - The @database@ itself is just a flat file of text.
-- - A @database@ is a list of @package@s, separated by two (or more?) new lines.
-- - A @package@ is a list of @property@s, separated by a new line.
-- - A @property@ is a @key@ and @value@ separated by a colon.
-- - A @key@ is a letter.  Meaning of keys is in
--   https://wiki.alpinelinux.org/wiki/Apk_spec.  They are not all unique.
-- - A @value@ is any text other than a new line.
--
-- Here's an example of the package definition for libssl.
--
-- C:Q1/KZ00qDHWZ5cj3AWG/DPdACRNYI=
-- P:libssl1.1
-- V:1.1.1n-r0
-- A:x86_64
-- S:213209
-- I:540672
-- T:SSL shared libraries
-- U:https://www.openssl.org/
-- L:OpenSSL
-- o:openssl
-- m:Timo Teras <timo.teras@iki.fi>
-- t:1647383879
-- c:455e966899a9358fc94f5bce633afe8a1942095c
-- D:so:libc.musl-x86_64.so.1 so:libcrypto.so.1.1
-- p:so:libssl.so.1.1=1.1
-- r:libressl
-- F:lib
-- R:libssl.so.1.1
-- a:0:0:755
-- Z:Q1xNjj7jxvOj3lDRd3sRXzHowTUsQ=
-- F:usr
-- F:usr/lib
-- R:libssl.so.1.1
-- a:0:0:777
-- Z:Q18j35pe3yp6HOgMih1wlGP1/mm2c=
module Strategy.AlpineLinux.Parser (installedPackagesDatabaseParser, PackageError (..)) where

import Control.Effect.Diagnostics (ToDiagnostic (renderDiagnostic))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Diag.Diagnostic (DiagnosticInfo (..))
import Effect.Logger (pretty)
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

-- | Errors that can occur even in a valid file.  Packages are a loose
-- collection of fields, but it is unclear which are required.  These errors
-- represent fields we expect, but which may be missing in unusual
-- circumstances.
data PackageError
  = MissingPackageName
  | MissingPackageArchitecture {packageName :: Text}
  | MissingPackageVersion {packageName :: Text}
  deriving (Show, Eq, Ord)

instance ToDiagnostic PackageError where
  renderDiagnostic MissingPackageName = do
    let header = "Could not identify alpine package name"
    DiagnosticInfo (Just header) Nothing Nothing Nothing Nothing Nothing Nothing
  renderDiagnostic (MissingPackageArchitecture name) = do
    let header = "Could not identify architecture associated with " <> name
    DiagnosticInfo (Just header) Nothing Nothing Nothing Nothing Nothing Nothing
  renderDiagnostic (MissingPackageVersion name) = do
    let header = "Could not identify version associated with " <> name
    DiagnosticInfo (Just header) Nothing Nothing Nothing Nothing Nothing Nothing

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
sepByWithTry :: MonadParsec e s m => m a -> m b -> m [a]
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
sepByWithTry1 :: MonadParsec e s m => m a -> m b -> m (NonEmpty a)
sepByWithTry1 p sep = do
  x <- try p
  (x :|) <$> many (try (sep >> p))

-- | Parses an Alpine Linux installed-packages database file.
--
-- This returns a list of packages or errors that represent malformed packages.
--
-- Packages are separated from one another by one-or-more newlines.  The end of
-- a file may have any number of trailing newlines.
--
-- Example:
--
-- > P:libssl1.1
-- > V:1.1.1n-r0
-- > A:x86_64
-- >
-- > P:ssl_client
-- > V:1.34.1-r4
-- > A:x86_64
-- >
installedPackagesDatabaseParser :: Parser [Either PackageError AlpinePackage]
installedPackagesDatabaseParser =
  many eol *> packageParser `sepByWithTry` (eol *> some eol) <* many eol <* eof

-- | Parses a single package or error about that package.
--
-- Each package is a list of line-separated properties.  Each property is of the
-- form @key:value@.
--
-- Example:
--
-- > P:libssl1.1
-- > V:1.1.1n-r0
-- > A:x86_64
packageParser :: Parser (Either PackageError AlpinePackage)
packageParser = do
  entries <- propertiesParser
  case (Map.lookup packageNameKey entries, Map.lookup architectureKey entries, Map.lookup versionKey entries) of
    (Just name, Just arch, Just version) ->
      pure . Right $
        AlpinePackage
          { alpinePackageName = Text.strip name
          , alpinePackageArchitecture = Text.strip arch
          , alpinePackageVersion = Text.strip version
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
--
-- >>> parse propertiesParser "" "P:libcrypto1.1\r\nT:Crypto library from openssl"
-- Right (fromList [("P","libcrypto1.1"),("T","Crypto library from openssl")])
propertiesParser :: Parser (Map.Map Text Text)
propertiesParser =
  Map.fromList . NE.toList <$> propertyParser `sepByWithTry1` eol

-- | Parses a single property.
--
-- >>> parse propertyParser "" "P:libcrypto1.1"
-- Right ("P","libcrypto1.1")
--
-- >>> parse propertyParser "" "T:Crypto library from openssl"
-- Right ("T","Crypto library from openssl")
--
-- >>> parse propertyParser "" "U:https://www.openssl.org/"
-- Right ("U","https://www.openssl.org/")
propertyParser :: Parser (Text, Text)
propertyParser = (,) <$> (keyParser <* char ':') <*> valueParser

-- | Parses a property key.  Keys appear to be a single letter, but multiple is supported.
--
-- >>> parse keyParser "" "A"
-- Right "A"
--
-- >>> parse keyParser "" "foo"
-- Right "foo"
keyParser :: Parser Text
keyParser = toText <$> some letterChar

-- | Parses a property value.  The value may be empty.
--
-- >>> parse valueParser "" "libcrypto1.1"
-- Right "libcrypto1.1"
--
-- >>> parse valueParser "" "so:libc.musl-x86_64.so.1"
-- Right "so:libc.musl-x86_64.so.1"
--
-- >>> parse valueParser "" "Crypto library from openssl"
-- Right "Crypto library from openssl"
--
-- >>> parse valueParser "" "https://www.openssl.org/"
-- Right "https://www.openssl.org/"
valueParser :: Parser Text
valueParser = takeWhileP (Just "entry value") (not . (`elem` ("\r\n" :: String)))
