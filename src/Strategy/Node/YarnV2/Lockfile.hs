{-# LANGUAGE RecordWildCards #-}

-- | Types and decoders for the elements found in a yarn v2 lockfile
--
-- See the yarnv2 devdocs for an overview
module Strategy.Node.YarnV2.Lockfile (
  YarnLockfile (..),
  Locator (..),
  Descriptor (..),
  PackageDescription (..),
  tryParseDescriptor,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey (..),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  Value (Object),
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Extra (TextLike (..))
import Data.Aeson.KeyMap qualified as Object
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Strategy.Node.PackageJson (NodePackage (..))
import Text.Megaparsec (
  MonadParsec (takeWhile1P),
  Parsec,
  errorBundlePretty,
  optional,
  runParser,
  takeRest,
 )
import Text.Megaparsec.Char (char)

---------- Types

newtype YarnLockfile = YarnLockfile (Map [Descriptor] PackageDescription)
  deriving (Eq, Ord, Show)

data Locator = Locator
  { locatorScope :: Maybe Text
  , locatorName :: Text
  , locatorReference :: Text
  }
  deriving (Eq, Ord, Show)

data Descriptor = Descriptor
  { descriptorScope :: Maybe Text
  , descriptorName :: Text
  , descriptorRange :: Text
  }
  deriving (Eq, Ord, Show)

data PackageDescription = PackageDescription
  { descVersion :: Text
  , descResolution :: Locator
  , descDependencies :: [Descriptor]
  }
  deriving (Eq, Ord, Show)

---------- Decoding from JSON

-- | The yarn lockfile is a @Map [Descriptor] PackageDescription@ with one
-- additional top-level field: @__metadata@
--
-- To decode the lockfile, we kill the metadata field, and run parseJSON again
-- on the object
instance FromJSON YarnLockfile where
  parseJSON = withObject "YarnLockfile" (fmap YarnLockfile . parseJSON . Object . Object.delete "__metadata")

-- | See 'packageRefP'/'locatorP' below
instance FromJSON Locator where
  parseJSON = withText "Locator" (tryParse locatorP)

-- | See 'packageRefP'/'descriptorP' below
instance FromJSON Descriptor where
  parseJSON = withText "Descriptor" (tryParse descriptorP)

-- | Each key at the top level in a yarn lockfile is a comma-separated list of
-- descriptors, in string form.
--
-- Fortunately, Aeson provides a mechanism for us to represent this:
-- 'FromJSONKey', which is used in the 'FromJSON' instance for 'Map'. It allows
-- us to arbitrarily decode any type as a Map key (assuming an 'Ord' instance,
-- of course).
--
-- See 'parsePackageKeys' for the implementation of the key parser
instance FromJSONKey Descriptor where
  fromJSONKey = FromJSONKeyTextParser (tryParse descriptorP)
  fromJSONKeyList = FromJSONKeyTextParser parsePackageKeys

-- | A comma-separated list of descriptors
--
-- See: https://github.com/yarnpkg/berry/blob/8afcaa2a954e196d6cd997f8ba506f776df83b1f/packages/yarnpkg-core/sources/Project.ts#L303
parsePackageKeys :: MonadFail m => Text -> m [Descriptor]
parsePackageKeys = traverse (tryParse descriptorP) . splitTrim ","

-- | 'Data.Text.splitOn', but trims surrounding whitespace from the results
splitTrim :: Text -> Text -> [Text]
splitTrim needle = map Text.strip . Text.splitOn needle

instance FromJSON PackageDescription where
  parseJSON = withObject "PackageDescription" $ \obj ->
    PackageDescription
      <$> obj .: "version"
      <*> obj .: "resolution"
      <*> (obj .:? "dependencies" .!= Map.empty >>= parseDependencyDescriptors . Map.map unTextLike)

-- | Rather than storing dependencies as a flat list of Descriptors, the yarn
-- lockfile stores them as key/value pairs, split on the last "@" in a
-- descriptor. This is identical to how they'd be found in a @package.json@
--
-- We re-construct the raw descriptor by rejoining on "@" before running the parser
parseDependencyDescriptors :: MonadFail m => Map Text Text -> m [Descriptor]
parseDependencyDescriptors = traverse (\(name, range) -> tryParse descriptorP (name <> "@" <> range)) . Map.toList

---------- Text field Parsers

type Parser = Parsec Void Text

tryParseDescriptor :: NodePackage -> Maybe Descriptor
tryParseDescriptor NodePackage{..} = tryParse descriptorP $ pkgName <> "@" <> pkgConstraint

tryParse :: MonadFail m => Parser a -> Text -> m a
tryParse p = either (fail . errorBundlePretty) pure . runParser p ""

-- | Locator and Descriptor fields are both parsed identically.
--
-- From yarn's structUtils.tryParseDescriptor/tryParselocator (in strict mode):
--
-- @
--     string.match(/^(?:@([^/]+?)\/)?([^/]+?)(?:@(.+))$/)
-- @
--
-- ..with the three matched fields referring to:
-- - package scope
-- - package name
-- - package range (descriptor) or package reference (locator)
--
-- See: https://github.com/yarnpkg/berry/blob/8afcaa2a954e196d6cd997f8ba506f776df83b1f/packages/yarnpkg-core/sources/structUtils.ts#L333-L425
packageRefP :: Parser (Maybe Text, Text, Text)
packageRefP = do
  scope <- optional $ char '@' *> segment "Scope" <* char '/'
  package <- segment "Package"
  _ <- char '@'
  rest <- takeRest
  pure (scope, package, rest)
  where
    segment :: String -> Parser Text
    segment name = takeWhile1P (Just name) (\c -> c /= '/' && c /= '@')

descriptorP :: Parser Descriptor
descriptorP = (\(scope, package, range) -> Descriptor scope package range) <$> packageRefP

locatorP :: Parser Locator
locatorP = (\(scope, package, reference) -> Locator scope package reference) <$> packageRefP
